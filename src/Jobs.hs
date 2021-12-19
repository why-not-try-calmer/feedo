module Jobs where

import AppTypes (App, AppConfig (last_worker_run, subs_state, tasks_queue, tg_config, worker_interval), FeedsAction (IncReadsF, RefreshNotifyF), FeedsRes (FeedBatches), Job (IncReadsJob, TgAlert, Log), ServerConfig (alert_chat, bot_token), SubChat (sub_chatid, sub_last_notification), db_config, runApp)
import Backend (evalFeedsAct)
import Control.Concurrent
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database (getValidCreds, saveToLog)
import Database.MongoDB.Query (Failure (ConnectionFailure))
import Replies

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ go
    where   go = catch action handler

runRefresh :: MonadIO m => App m ()
runRefresh = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        -- regenerating connection handler upon failed connection
        onError (ConnectionFailure _) = do
            let conn = db_config env
            creds <- readIORef conn
            refreshed <- getValidCreds creds
            atomicModifyIORef' conn $ const (refreshed, ())
        -- sending alert over Telegram
        onError err = do
            let report = "runRefresh: Exception met : " `T.append` (T.pack . show $ err)
            writeChan (tasks_queue env) . TgAlert $ report
            print $ "runRefresh bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling runRefresh now."
        action = do
            now <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            runApp (env {last_worker_run = Just now}) $ evalFeedsAct RefreshNotifyF >>= \case
                FeedBatches p -> liftIO $ do
                    let listed = HMS.toList p
                    -- sending notifications
                    notified_chats <- mapConcurrently (\(cid, feed_items) ->
                        reply tok cid (toReply . FromFeedsItems $ feed_items) >> pure cid) listed
                    -- updating feeds
                    modifyMVar_ (subs_state env) $
                        pure . HMS.map (\c ->
                            if sub_chatid c `elem` notified_chats then c { sub_last_notification = Just now }
                            else c)
                _ -> liftIO $ writeChan (tasks_queue env) . TgAlert $ "runRefresh: Worked failed to acquire notification package"
        wait_action = threadDelay interval >> action
        handler e = onError e >> action
    liftIO $ runForever_ wait_action handler

runJobs :: MonadIO m => App m ()
runJobs = ask >>= \env ->
    let q = tasks_queue env
        action = readChan q >>= \case
            IncReadsJob links -> void . runApp env $ evalFeedsAct (IncReadsF links)
            TgAlert contents ->
                let msg = PlainReply $ "feedfarer2 is sending an alert: " `T.append` contents
                in  reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) msg
            Log item -> readIORef (db_config env) >>= \config -> saveToLog config item
        handler (SomeException e) = do
            let report = "runDbTasks: Exception met : " `T.append` (T.pack . show $ e)
            writeChan (tasks_queue env) . TgAlert $ report
            print $ "runDbTasks bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling run_worker now."
    in  liftIO $ runForever_ action handler