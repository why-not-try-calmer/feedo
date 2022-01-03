{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App, AppConfig (last_worker_run, subs_state, tasks_queue, tg_config, worker_interval), DbAction (UpsertChats), FeedsAction (IncReadsF, RefreshNotifyF), FeedsRes (FeedBatches), Job (IncReadsJob, Log, TgAlert, UpdateNotificationTimes), ServerConfig (alert_chat, bot_token), SubChat (sub_chatid, sub_last_notification, sub_settings, sub_next_notification), runApp, DbRes (DbOk), ChatSettings (settings_batch_interval))
import Backend (evalFeedsAct)
import Control.Concurrent
    ( readChan, writeChan, modifyMVar_, threadDelay )
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database (evalDb, saveToLog)
import Replies
    ( Reply(PlainReply), toReply, FromContents(FromFeedsItems) )
import Requests (reply)
import Utils (findNextTime)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ go
    where   go = catch action handler

runRefresh :: MonadIO m => App m ()
runRefresh = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        -- sending alert over Telegram
        onError (SomeException err) = do
            let report = "runRefresh: Exception met : " `T.append` (T.pack . show $ err)
            writeChan (tasks_queue env) . TgAlert $ report
            print $ "runRefresh bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling runRefresh now."
        action = do
            now <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            runApp (env { last_worker_run = Just now }) $ evalFeedsAct RefreshNotifyF >>= \case
                FeedBatches payload -> liftIO $ do
                    -- sending notifications
                    notified_chats <- mapConcurrently
                        (\(cid, feed_items) ->
                        reply tok cid (toReply . FromFeedsItems $ feed_items) >> pure cid) $ HMS.toList payload
                    -- updating chats
                    modifyMVar_ (subs_state env) $ \subs ->
                        let updated_chats = HMS.map (\c -> if sub_chatid c `elem` notified_chats then c { sub_last_notification = Just now } else c) subs
                        in  runApp env $ evalDb (UpsertChats updated_chats) >> pure updated_chats
                    writeChan (tasks_queue env) $ UpdateNotificationTimes notified_chats
                _ -> liftIO $ writeChan (tasks_queue env) . TgAlert $ "runRefresh: Worked failed to acquire notification package"
        wait_action = threadDelay interval >> action
        handler e = onError e >> action
    liftIO $ runForever_ wait_action handler

runJobs :: MonadIO m => App m ()
runJobs = ask >>= \env ->
    let action = readChan (tasks_queue env) >>= \case
            IncReadsJob links -> void . runApp env $ evalFeedsAct (IncReadsF links)
            TgAlert contents ->
                let msg = PlainReply $ "feedfarer2 is sending an alert: " `T.append` contents
                in  reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) msg
            Log item -> runApp env $ saveToLog item
            UpdateNotificationTimes chat_ids -> 
                getCurrentTime >>= \now -> 
                modifyMVar_ (subs_state env) $ \chats_hmap -> 
                    let relevant = HMS.filter (\c -> sub_chatid c `elem` chat_ids) chats_hmap
                        updated_chats = HMS.map (\c -> c { 
                            sub_last_notification = Just now,
                            sub_next_notification = findNextTime (settings_batch_interval . sub_settings $ c) now}
                            ) relevant
                    in  runApp env $ evalDb (UpsertChats updated_chats) >>= \case
                        DbOk -> pure $ HMS.union updated_chats chats_hmap
                        _ -> pure chats_hmap
        handler (SomeException e) = do
            let report = "runDbTasks: Exception met : " `T.append` (T.pack . show $ e)
            writeChan (tasks_queue env) . TgAlert $ report
            print $ "runDbTasks bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling run_worker now."
    in  liftIO $ runForever_ action handler
