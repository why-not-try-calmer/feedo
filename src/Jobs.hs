{-# LANGUAGE FlexibleContexts #-}

module Jobs (startNotifs, startJobs) where

import Control.Concurrent (
  readChan,
  threadDelay,
 )
import Control.Concurrent.Async (async, forConcurrently, forConcurrently_, wait, withAsync)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Data.Foldable (foldl', for_)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (modifyIORef', readIORef)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Traversable (for)
import Database.MongoDB (Select (select), find, rest, (=:))
import Mem (makeDigests, withChatsFromMem)
import Mongo (HasMongo (withDb), MongoDoc (readDoc), bsonToAdmin, evalDb, markNotified, saveToLog)
import Notifications (alertAdmin)
import Redis
import Replies (
  mkDigestUrl,
  mkReply,
  render,
 )
import Requests (reply, runSend_)
import TgActions (isChatOfType)
import TgramInJson (ChatType (Channel))
import TgramOutJson (ChatId, Outbound (DeleteMessage, PinMessage), TgRequestMethod (TgDeleteMessage, TgPinChatMessage))
import Types

{- Background tasks -}

runForever_ :: (Exception e) => IO () -> (e -> IO ()) -> IO ()
{- Utility to fork a runtime thread that will run forever (i.e. absorbing all exceptions -}
runForever_ action handler = void . async . forever $ catch action handler

checkDelay :: (Ord b, Num b, Show b) => b -> (String, b)
checkDelay delay
  | delay < 10 = ("10 secs", 10000000)
  | delay > 30 = ("30 secs", 30000000)
  | otherwise = (show delay, delay)

getPrebatch :: (HasMongo m, MonadIO m) => m Prebatch
getPrebatch = do
  let getChats now = find (select ["sub_next_digest" =: ["$lt" =: now]] "chats")
  now <- liftIO getCurrentTime
  docs <- withDb (getChats now >>= rest)
  case docs of
    Left err -> undefined
    Right bson_chats ->
      let (chats, flinks) =
            foldl'
              ( \(cs, fs) doc ->
                  let c = readDoc doc :: SubChat
                   in (c : cs, fs `S.union` sub_feeds_links c)
              )
              ([], S.empty)
              bson_chats
       in pure $ foldl' (\acc l -> let subs = filter (\c -> l `S.member` sub_feeds_links c) chats in HMS.insert l subs acc) HMS.empty flinks

-- prepareDigest :: (HasMongo m, MonadIO m) => HMS.HashMap ChatId SubChat -> m (HMS.HashMap FeedLink [(ChatId, SubChat)])
-- prepareDigest = do
--   let getFeedLinks

startNotifs :: (MonadIO m) => App m ()
{- Forks a thread and tasks it with checking every minute
if any chat need a digest or follow -}
startNotifs =
  ask >>= \env ->
    let interval = worker_interval env
        onError (SomeException err) = do
          let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
          alertAdmin (postjobs env) report
        -- sending digests + follows
        send_tg_notif hmap now = forConcurrently (HMS.toList hmap) $
          \(cid, (c, batch)) ->
            let sets = sub_settings c
             in case batch of
                  Follows fs -> do
                    runApp env $ reply cid (mkReply (FromFollow fs sets))
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
                    res <- runApp env $ evalDb $ WriteDigest digest
                    let mb_digest_link r = case r of
                          Right (DbDigestId _id) -> Just $ mkDigestUrl (base_url env) _id
                          _ -> Nothing
                    runApp env $ reply cid (mkReply (FromDigest ds (mb_digest_link res) sets))
                    pure (cid, map f_link ds)
        notify = do
          -- rebuilding feeds and collecting notifications
          digests <- makeDigests
          case digests of
            Right (CacheDigests notif_hmap) -> do
              -- skipping sending on a firt run
              (last_run, now) <- liftIO $ do
                lr <- readIORef $ last_worker_run env
                now <- getCurrentTime
                pure (lr, now)
              case last_run of
                Nothing -> do
                  -- mark chats as notified on first run
                  -- but do not send digests to avoid double-sending
                  let notified_chats = HMS.keys notif_hmap
                  markNotified notified_chats now
                  liftIO $ modifyIORef' (last_worker_run env) $ \_ -> Just now
                Just _ -> do
                  -- this time sending digests, follows & search notifications
                  notified_chats <- liftIO $ map fst <$> send_tg_notif notif_hmap now
                  markNotified notified_chats now
                  liftIO $ modifyIORef' (last_worker_run env) $ \_ -> Just now
            Left err -> alertAdmin (postjobs env) ("notifier: failed to acquire notification package and got this error:" `T.append` err)
            -- to avoid an incomplete pattern
            _ -> pure ()
        wait_action = do
          threadDelay interval
          putStrLn "startNotifs: Woke up"
          runApp env notify
          putStrLn "startNotifs: Notified. Going back to sleep"
        handler e = onError e >> runApp env notify
     in liftIO $ runForever_ wait_action handler >> putStrLn "startNotifs: started"

startJobs :: (MonadIO m) => App m ()
{- Forks a runtime thread and tasks it with handling as they come all post-processing jobs -}
startJobs = do
  env <- ask
  liftIO $ do
    runForever_ (action env) (handler env)
    putStrLn "startJobs: started"
 where
  action env = do
    job <- readChan $ postjobs env
    putStrLn $ "startJobs: job received: " ++ (take 20 . show $ job)
    forkExecute env job
    putStrLn "startJobs: job complete!"
  handler env (SomeException e) = do
    putStrLn $ "Failed to run job: " ++ (take 20 . show $ e)
    alertAdmin (postjobs env) . reportFailed $ e
  reportFailed e = "postProcJobs: Exception met : " `T.append` (T.pack . show $ e)

interpolateCidInTxt :: (Show p) => T.Text -> p -> T.Text -> T.Text
interpolateCidInTxt before cid after = before `T.append` (T.pack . show $ cid) `T.append` after

forkExecute :: (MonadIO m) => AppConfig -> Job -> m ()
forkExecute env job =
  let todo = runApp env $ go job
   in liftIO $ withAsync todo wait
 where
  go (JobArchive feeds now) = do
    -- archiving items
    liftIO $ putStrLn "Jobs received: JobArchive"
    evalDb (ArchiveItems feeds) >>= \case
      Left err ->
        let msg = "Unable to archive items. Reason: " `T.append` render err
         in go $ JobTgAlertAdmin msg
      _ -> liftIO $ putStrLn "successfully ran job"
    -- cleaning more than 1 month old archives
    void $ evalDb (PruneOld $ addUTCTime (-2592000) now)
  go (JobLog items) = mapM_ saveToLog items
  go (JobPin cid mid) = do
    runSend_ (bot_token . tg_config $ env) TgPinChatMessage (PinMessage cid mid) >>= \case
      Left _ ->
        let msg = interpolateCidInTxt "Tried to pin a message in (chat_id) " cid " but failed. Either the message was removed already, or perhaps the chat is a channel and I am not allowed to delete edit messages in it?"
         in go (JobTgAlertAdmin msg)
      _ -> pure ()
  go (JobPurge cid) = void $ withChatsFromMem Purge Nothing cid
  go (JobRemoveMsg cid mid delay) = do
    let (msg, checked_delay) = checkDelay delay
    liftIO $ putStrLn ("Removing message in " ++ msg)
    do
      liftIO $ threadDelay checked_delay
      runSend_ (bot_token . tg_config $ env) TgDeleteMessage (DeleteMessage cid mid) >>= \case
        Left _ ->
          go
            . JobTgAlertAdmin
            . interpolateCidInTxt "Tried to delete a message in (chat_id) " cid
            $ " but failed. Either the message was removed already, or perhaps  is a channel and I am not allowed to delete edit messages in it?"
        _ -> pure ()
  go (JobSetPagination cid mid pages mb_link) =
    let to_db = evalDb $ InsertPages cid mid pages mb_link
        to_cache = withKeyStore $ CacheSetPages cid mid pages mb_link
     in to_db >> to_cache >>= \case
          Right _ -> pure ()
          _ ->
            let report = "Failed to update Redis on this key: " `T.append` T.append (T.pack . show $ cid) (T.pack . show $ mid)
             in go $ JobTgAlertAdmin report
  go (JobTgAlertAdmin contents) = do
    let msg = ServiceReply $ "Feedo is sending an alert: " `T.append` contents
    reply (alert_chat . tg_config $ env) msg
  go (JobTgAlertChats chat_ids contents) = do
    let msg = ServiceReply contents
        tok = bot_token . tg_config $ env
    liftIO $ forConcurrently_ chat_ids $ \cid -> do
      verdict <- isChatOfType tok cid Channel
      unless (verdict == Right True) $ runApp env $ reply cid msg
