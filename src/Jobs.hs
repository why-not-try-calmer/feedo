{-# LANGUAGE FlexibleContexts #-}

module Jobs (startNotifs, startJobs) where

import Control.Concurrent (
  readChan,
  threadDelay,
 )
import Control.Concurrent.Async (async, forConcurrently, forConcurrently_)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (modifyIORef', readIORef)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Mem (makeDigestsFromMem, withChatsFromMem)
import Mongo (evalDb, markNotified, saveToLog)
import Notifications (alertAdmin)
import Redis
import Replies (
  mkDigestUrl,
  mkReply,
 )
import Requests (reply, runSend_)
import TgActions (isChatOfType)
import TgramInJson (ChatType (Channel))
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Types
import Utils (renderDbError)

{- Background tasks -}

runForever_ :: (Exception e) => IO () -> (e -> IO ()) -> IO ()
{- Utility to fork a runtime thread that will run forever (i.e. absorbing all exceptions -}
runForever_ action handler = void . async . forever $ catch action handler

checkDelay :: (Ord b, Num b, Show b) => b -> (String, b)
checkDelay delay
  | delay < 10 = ("10 secs", 10000000)
  | delay > 30 = ("30 secs", 30000000)
  | otherwise = (show delay, delay)

startNotifs :: (MonadReader AppConfig m, MonadIO m) => m ()
{- Forks a thread and tasks it with checking every minute
if any chat need a digest or follow -}
startNotifs =
  ask >>= \env ->
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        onError (SomeException err) = do
          let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
          alertAdmin (postjobs env) report
        -- sending digests + follows
        send_tg_notif hmap now = forConcurrently (HMS.toList hmap) $
          \(cid, (c, batch)) ->
            let sets = sub_settings c
             in case batch of
                  Follows fs -> do
                    reply tok cid (mkReply (FromFollow fs sets)) (postjobs env)
                    pure (cid, map f_link fs)
                  Digests ds -> do
                    let (ftitles, flinks, fitems) =
                          foldr
                            ( \f (one, two, three) ->
                                (f_title f : one, f_link f : two, three ++ f_items f)
                            )
                            ([], [], [])
                            ds
                        ftitles' = S.toList . S.fromList $ ftitles
                        flinks' = S.toList . S.fromList $ flinks
                        digest = Digest Nothing now fitems flinks' ftitles'
                    res <- evalDb env $ WriteDigest digest
                    let mb_digest_link r = case r of
                          Right (DbDigestId _id) -> Just $ mkDigestUrl (base_url env) _id
                          _ -> Nothing
                    reply tok cid (mkReply (FromDigest ds (mb_digest_link res) sets)) (postjobs env)
                    pure (cid, map f_link ds)
        notify = do
          -- rebuilding feeds and collecting notifications
          from_cache_payload <- runApp env makeDigestsFromMem
          case from_cache_payload of
            Right (CacheDigests notif_hmap) -> do
              -- skipping sending on a firt run
              my_last_run <- readIORef $ last_worker_run env
              now <- getCurrentTime
              case my_last_run of
                Nothing -> do
                  -- mark chats as notified on first run
                  -- but do not send digests to avoid double-sending
                  let notified_chats = HMS.keys notif_hmap
                  markNotified env notified_chats now
                  modifyIORef' (last_worker_run env) $ \_ -> Just now
                Just _ -> do
                  -- this time sending digests, follows & search notifications
                  notified_chats <- map fst <$> send_tg_notif notif_hmap now
                  markNotified env notified_chats now
                  modifyIORef' (last_worker_run env) $ \_ -> Just now
            Left err -> alertAdmin (postjobs env) ("notifier: failed to acquire notification package and got this error:" `T.append` err)
            -- to avoid an incomplete pattern
            _ -> pure ()
        wait_action = do
          threadDelay interval
          putStrLn "startNotifs: Woke up"
          notify
          putStrLn "startNotifs: Notified. Going back to sleep"
        handler e = onError e >> notify
     in liftIO $ runForever_ wait_action handler >> putStrLn "startNotifs: started"

startJobs :: (MonadReader AppConfig m, MonadIO m) => m ()
{- Forks a runtime thread and tasks it with handling as they come all post-processing jobs -}
startJobs = do
  env <- ask
  liftIO $ runForever_ (action env) (handler env) >> putStrLn "startJobs: started"
 where
  action env = do
    job <- readChan (postjobs env)
    print $ "startJobs: job received: " `T.append` (T.pack . show $ job)
    execute env job
    putStrLn "startJobs: job complete!"
  handler env (SomeException e) = do
    print $ "Failed to execute job: " `T.append` (T.pack . show $ e)
    alertAdmin (postjobs env) . reportFailed $ e
  reportFailed e = "postProcJobs: Exception met : " `T.append` (T.pack . show $ e)

interpolateCidInTxt :: (Show p) => T.Text -> p -> T.Text -> T.Text
interpolateCidInTxt before cid after = before `T.append` (T.pack . show $ cid) `T.append` after

execute :: AppConfig -> Job -> IO ()
execute env (JobArchive feeds now) = do
  -- archiving items
  putStrLn "Jobs received: JobArchive"
  evalDb env (ArchiveItems feeds) >>= \case
    Left err ->
      let msg = "Unable to archive items. Reason: " `T.append` renderDbError err
       in execute env $ JobTgAlertAdmin msg
    _ -> putStrLn "successfully ran job"
  -- cleaning more than 1 month old archives
  void $ evalDb env (PruneOld $ addUTCTime (-2592000) now)
execute env (JobLog item) = saveToLog env item
execute env (JobPin cid mid) = do
  runSend_ (bot_token . tg_config $ env) "pinChatMessage" (PinMessage cid mid) >>= \case
    Left _ ->
      let msg = interpolateCidInTxt "Tried to pin a message in (chat_id) " cid " but failed. Either the message was removed already, or perhaps the chat is a channel and I am not allowed to delete edit messages in it?"
       in execute env (JobTgAlertAdmin msg)
    _ -> pure ()
execute env (JobPurge cid) = void $ runApp env (withChatsFromMem Purge cid)
execute env (JobRemoveMsg cid mid delay) = do
  let (msg, checked_delay) = checkDelay delay
  putStrLn ("Removing message in " ++ msg)
  do
    threadDelay checked_delay
    runSend_ (bot_token . tg_config $ env) "deleteMessage" (DeleteMessage cid mid) >>= \case
      Left _ ->
        execute env
          . JobTgAlertAdmin
          . interpolateCidInTxt "Tried to delete a message in (chat_id) " cid
          $ " but failed. Either the message was removed already, or perhaps  is a channel and I am not allowed to delete edit messages in it?"
      _ -> pure ()
execute env (JobSetPagination cid mid pages mb_link) =
  let to_db = evalDb env $ InsertPages cid mid pages mb_link
      to_cache = withCache $ CacheSetPages cid mid pages mb_link
   in runApp env (to_db >> to_cache) >>= \case
        Right _ -> pure ()
        _ ->
          let report = "Failed to update Redis on this key: " `T.append` T.append (T.pack . show $ cid) (T.pack . show $ mid)
           in execute env $ JobTgAlertAdmin report
execute env (JobTgAlertAdmin contents) = do
  let msg = ServiceReply $ "Feedo is sending an alert: " `T.append` contents
  reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) msg (postjobs env)
execute env (JobTgAlertChats chat_ids contents) =
  let msg = ServiceReply contents
      tok = bot_token . tg_config $ env
      jobs = postjobs env
   in forConcurrently_ chat_ids $ \cid -> do
        verdict <- isChatOfType tok cid Channel
        unless (verdict == Right True) $ reply tok cid msg jobs
