{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App (..), AppConfig (..), DbAction (..), DbRes (..), FeedsAction (..), FeedsRes (..), Job (..), LogItem (LogItem), Reply (ServiceReply), ServerConfig (..), Settings (..), SubChat (..), ToReply (..), runApp, renderDbError)
import Backend (evalFeedsAct, updateEngine)
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
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Database (evalDb, saveToLog)
import Replies
  ( toReply,
  )
import Requests (reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Utils (findNextTime)

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
                >> pure cid
        send_tg_search payload = forConcurrently_ (HMS.toList payload) $
            \(cid, keys_items) -> reply tok cid (toReply (FromSearchRes keys_items) Nothing) (postjobs env)
        send_notifs update search = fst <$> concurrently (send_tg_notif update) (send_tg_search search)
        updated_chats now notified_chats subs =
            HMS.map (\c -> if sub_chatid c `elem` notified_chats then c { sub_last_notification = Just now }
            else c) subs
        notify = do
            t1 <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            res <- runApp (env { last_worker_run = Just t1 }) $ evalFeedsAct Refresh
            case res of
                FeedBatches update_notif search_notif -> do
                    -- sending update & search notifications
                    notified_chats <- send_notifs update_notif search_notif
                    -- updating chats
                    modifyMVar_ (subs_state env) $ \subs -> do
                        void $ evalDb env (UpsertChats $ updated_chats t1 notified_chats subs)
                        pure (updated_chats t1 notified_chats subs)
                    t2 <- getCurrentTime
                    let item = LogItem t2 "notifier" "ran successfully" . realToFrac $ diffUTCTime t1 t2
                    writeChan (postjobs env) (JobLog item)
                FeedsError err -> 
                    writeChan (postjobs env) . JobTgAlert $ "notifier: \
                        \ failed to acquire notification package and got this error"
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
            JobUpdateEngine feeds -> fork $ updateEngine (search_engine env) feeds
            JobUpdateSchedules chat_ids -> fork $ do
                now <- getCurrentTime
                modifyMVar_ (subs_state env) $ \chats_hmap ->
                    let relevant = HMS.filter (\c -> sub_chatid c `elem` chat_ids) chats_hmap
                        updated_chats = HMS.map (\c -> c {
                            sub_last_notification = Just now,
                            sub_next_notification = Just $ findNextTime now (settings_batch_interval . sub_settings $ c)}
                            ) relevant
                    in  evalDb env (UpsertChats updated_chats) >>= \case
                        DbOk -> pure $ HMS.union updated_chats chats_hmap
                        _ -> pure chats_hmap
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



