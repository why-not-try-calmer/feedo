{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App (..), AppConfig (..), DbAction (..), DbRes (..), Feed (f_link), FeedsAction (..), FeedsRes (..), Job (..), LogItem (LogItem), Reply (ServiceReply), ServerConfig (..), Settings (..), SubChat (..), ToReply (..), renderDbError, runApp)
import Backend (evalFeedsAct)
import Control.Concurrent
  ( modifyMVar_,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (async, concurrently, forConcurrently, forConcurrently_)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime, systemToUTCTime)
import Database (evalDb, saveToLog)
import Replies
  ( toReply,
  )
import Requests (reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Utils (findNextTime, renderTimeSlices)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ catch action handler

notifier :: MonadIO m => App m ()
notifier = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        onError (SomeException err) = do
            let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
            writeChan (postjobs env) . JobTgAlert $ report
        send_tg_notif payload = forConcurrently (HMS.toList payload) $
            \(cid, (settings, feed_items)) ->
                reply tok cid (toReply (FromFeedsItems feed_items) (Just settings)) (postjobs env)
                >> pure (cid, map (f_link . fst) feed_items)
        send_tg_search payload = forConcurrently_ (HMS.toList payload) $
            \(cid, keys_items) -> reply tok cid (toReply (FromSearchRes keys_items) Nothing) (postjobs env)
        send_notifs update search = fst <$> concurrently (send_tg_notif update) (send_tg_search search)
        updated_notified_chats notified_chats chats now =
            HMS.mapWithKey (\cid c -> if cid `elem` notified_chats then c {
                sub_last_notification = Just now,
                sub_next_notification = Just $ findNextTime now (settings_batch_interval . sub_settings $ c)
            } else c) chats
        notify = do
            now <- getCurrentTime
            t1 <- systemSeconds <$> getSystemTime
            -- rebuilding feeds and dispatching notifications
            res <- runApp (env { last_worker_run = Just now }) $ evalFeedsAct Refresh
            case res of
                FeedBatches update_notif search_notif -> do
                    t2 <- systemSeconds <$> getSystemTime
                    -- sending update & search notifications
                    notified_chats_feeds <- send_notifs update_notif search_notif
                    t3 <- systemSeconds <$> getSystemTime
                    -- preparing updates
                    let notified_chats = map fst notified_chats_feeds
                        read_feeds = S.toList . S.fromList . foldMap snd $ notified_chats_feeds
                    -- increasing reads count
                    writeChan (postjobs env) $ JobIncReadsJob read_feeds
                    -- updating chats
                    modifyMVar_ (subs_state env) $ \subs ->
                        let updated_chats = updated_notified_chats notified_chats subs now
                        in  evalDb env (UpsertChats updated_chats) >>= \case
                            DbErr err -> do
                                writeChan (postjobs env) $ JobTgAlert $ "notifier: failed to \
                                    \ save updated chats to db because of this error" `T.append` renderDbError err
                                pure subs
                            DbOk -> pure updated_chats
                            _ -> pure subs
                    (t4, later) <- (\t -> (systemSeconds t, systemToUTCTime t)) <$> getSystemTime
                    let item = LogItem
                            later
                            "notifier"
                            ("ran successfully" `T.append` renderTimeSlices [t1, t2, t3, t4]
                                ["running Refresh", "sending notification messages", "updating db & memory"])
                            (realToFrac $ t4 - t1)
                    writeChan (postjobs env) $ JobLog item
                FeedsError err ->
                    writeChan (postjobs env) $ JobTgAlert $ "notifier: \
                        \ failed to acquire notification package and got this error: "
                        `T.append` renderDbError err
                -- probably pulled a 'FeedsOk'
                _ -> pure ()
        wait_action = threadDelay interval >> notify
        handler e = onError e >> notify
    liftIO $ runForever_ wait_action handler

postProcJobs :: MonadIO m => App m ()
postProcJobs = ask >>= \env ->
    let tok = bot_token . tg_config $ env
        jobs = postjobs env
        action = readChan (postjobs env) >>= \case
            JobIncReadsJob links -> fork $ runApp env $ evalFeedsAct (IncReadsF links)
            JobLog item -> fork $ saveToLog env item
            JobPin cid mid -> fork $ do
                reqSend_ tok "pinChatMessage" (PinMessage cid mid) >>= \case
                    Left _ -> writeChan jobs . JobTgAlert . with_cid_txt "Tried to pin a message in (chat_id) " cid $
                        " but failed. Either the message was removed already, or perhaps the chat is a channel and I am not allowed to delete edit messages in it?"
                    Right _ -> pure ()
            JobRemoveMsg cid mid delay -> do
                let (msg, checked_delay) = check_delay delay
                putStrLn ("Removing message in " ++ msg)
                fork $ do
                    threadDelay checked_delay
                    reqSend_ tok "deleteMessage" (DeleteMessage cid mid) >>= \case
                        Left _ -> writeChan jobs . JobTgAlert . with_cid_txt "Tried to delete a message in (chat_id) " cid $
                            " but failed. Either the message was removed already, or perhaps  is a channel and I am not allowed to delete edit messages in it?"
                        Right _ -> pure ()
            JobTgAlert contents -> fork $ do
                let msg = ServiceReply $ "feedfarer2 is sending an alert: " `T.append` contents
                print $ "postProcJobs: JobTgAlert " `T.append` (T.pack . show $ contents)
                reply tok (alert_chat . tg_config $ env) msg jobs
        handler (SomeException e) = do
            let report = "postProcJobs: Exception met : " `T.append` (T.pack . show $ e)
            writeChan (postjobs env) . JobTgAlert $ report
            print $ "postProcJobs bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling postProcJobs now."
    in  liftIO $ runForever_ action handler
    where
        fork = void . async
        check_delay delay
            | delay < 10 = ("10 secs", 10000000)
            | delay > 30 = ("30 secs", 30000000)
            | otherwise = (show delay, delay)
        with_cid_txt before cid after = before `T.append` (T.pack . show $ cid) `T.append` after



