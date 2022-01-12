{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App, AppConfig (last_worker_run, postjobs, search_engine, subs_state, tg_config, worker_interval), DbAction (UpsertChats), DbRes (DbOk), FeedsAction (IncReadsF, RefreshNotifyF), FeedsRes (FeedBatches), Job (JobIncReadsJob, JobLog, JobPin, JobRemoveMsg, JobTgAlert, JobUpdateEngine, JobUpdateSchedules), LogItem (LogItem), Reply (ServiceReply), ServerConfig (alert_chat, bot_token), Settings (settings_batch_interval), SubChat (sub_chatid, sub_last_notification, sub_next_notification, sub_settings), runApp)
import Backend (evalFeedsAct, updateEngine)
import Control.Concurrent
  ( modifyMVar_,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (async, forConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Database (evalDb, saveToLog)
import Replies
  ( ToReply (FromFeedsItems),
    toReply,
  )
import Requests (reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Utils (findNextTime)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ catch action handler

refresher :: MonadIO m => App m ()
refresher = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        -- sending alert over Telegram
        onError (SomeException err) = do
            let report = "refresher: exception met : " `T.append` (T.pack . show $ err)
            writeChan (postjobs env) . JobTgAlert $ report
        action = do
            t1 <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            runApp (env { last_worker_run = Just t1 }) $ evalFeedsAct RefreshNotifyF >>= \case
                FeedBatches payload -> liftIO $ do
                    -- sending notifications
                    notified_chats <- forConcurrently (HMS.toList payload) $ 
                        \(cid, (settings, feed_items)) ->
                            reply tok cid (toReply (FromFeedsItems feed_items) (Just settings)) (postjobs env)
                            >> pure cid
                    -- updating chats
                    modifyMVar_ (subs_state env) $ \subs ->
                        let updated_chats = HMS.map (\c -> if sub_chatid c `elem` notified_chats then c { sub_last_notification = Just t1 } else c) subs
                        in  evalDb env (UpsertChats updated_chats) >> pure updated_chats
                    t2 <- getCurrentTime
                    let diff = diffUTCTime t1 t2
                        item = LogItem t2 "refresher" "ran successfully" (realToFrac diff)
                    writeChan (postjobs env) (JobLog item)
                _ -> liftIO $ writeChan (postjobs env) . JobTgAlert $ "refresher: failed to acquire notification package"
        wait_action = threadDelay interval >> action
        handler e = onError e >> action
    liftIO $ runForever_ wait_action handler

postProcJobs :: MonadIO m => App m ()
postProcJobs = ask >>= \env ->
    let tok = bot_token . tg_config $ env
        jobs = postjobs env
        with_cid_txt before cid after = before `T.append` (T.pack . show $ cid) `T.append` after
        action = readChan (postjobs env) >>= \case
            JobIncReadsJob links -> fork $ runApp env $ evalFeedsAct (IncReadsF links)
            JobLog item -> fork $ saveToLog env item
            JobPin cid mid -> fork $ do
                reqSend_ tok "pinChatMessage" (PinMessage cid mid) >>= \case
                    Left _ -> writeChan jobs . JobTgAlert . with_cid_txt "Tried to pin a message in (chat_id) " cid $
                        " but failed. Either the message was removed already, or perhaps the chat is a channel and I am not allowed to delete edit messages in it?"
                    Right _ -> pure ()
            JobRemoveMsg cid mid delay -> fork $ do
                threadDelay $ fromMaybe 30000000 delay
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
    where fork = void . async
        
