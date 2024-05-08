module Jobs (startNotifs, startJobs) where

import Chats (withChat)
import Control.Concurrent (
  readChan,
  threadDelay,
 )
import Control.Concurrent.Async (async, forConcurrently, forConcurrently_)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Digests (makeDigests)
import Mongo (evalDb, saveToLog)
import Redis
import Replies (
  mkDigestUrl,
  mkReply,
  render,
 )
import Requests (alertAdmin, reply, runSend_)
import TgActions (isChatOfType)
import TgramInJson (ChatType (Channel))
import TgramOutJson (Outbound (DeleteMessage, PinMessage), TgRequestMethod (TgDeleteMessage, TgPinChatMessage))
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

startNotifs :: (MonadIO m) => App m ()
{- Forks a thread and tasks it with checking every minute
if any chat need a digest or follow -}
startNotifs =
  ask >>= \env ->
    let interval = worker_interval env
        onError (SomeException err) = do
          let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
          alertAdmin (postjobs env) report
        send_tg_notif hmap now = forConcurrently (HMS.toList hmap) $ \(cid, (c, batch)) -> do
          unless (null batch) $
            let sets = sub_settings c
                (ftitles, flinks, fitems) =
                  foldr
                    ( \f (one, two, three) ->
                        (f_title f : one, f_link f : two, three ++ f_items f)
                    )
                    ([], [], [])
                    batch
                ftitles' = S.toList . S.fromList $ ftitles
                flinks' = S.toList . S.fromList $ flinks
                digest = Digest Nothing now fitems flinks' ftitles'
                mb_digest_link r = case r of
                  Right (DbDigestId _id) -> Just $ mkDigestUrl (base_url env) _id
                  _ -> Nothing
            in  runApp env $ do
                  write_res <- evalDb $ WriteDigest digest
                  reply cid $ mkReply $ FromDigest batch (mb_digest_link write_res) sets
          pure (cid, map f_link batch)
        notify = do
          digests <- makeDigests
          case digests of
            Left err -> alertAdmin (postjobs env) ("notifier: failed to acquire notification package and got this error:" `T.append` err)
            Right (CacheDigests batches) -> do
              (n, notified) <- liftIO $ do
                now <- getCurrentTime
                notified_chats <- map fst <$> send_tg_notif batches now
                pure (now, notified_chats)
              res <- evalDb $ BumpNotified notified n
              case res of
                Left bump_error -> alertAdmin (postjobs env) ("notifier: failed to acquire notification package and got this error:" `T.append` render bump_error)
                Right _ -> pure ()
            something_else -> liftIO . print $ show something_else
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
   in liftIO . void . async $ todo
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
  go (JobPurge cid) = void $ withChat Purge Nothing cid
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
