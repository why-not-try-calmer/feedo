module Jobs where

import AppTypes (App, AppConfig (last_worker_run, mongo_config, server_config, subs_state, tasks_queue, worker_interval), FeedsAction (IncReadsF, RefreshNotifyF), FeedsRes (FeedBatches), Job (IncReadsJob), LogItem (LogItem), ServerConfig (bot_token), SubChat (sub_chatid, sub_last_notification), runApp)
import Backend (evalFeedsAct)
import Control.Concurrent
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database (saveToLog)
import Replies

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ go
    where   go = action `catch` handler

runRefresh :: MonadIO m => App m ()
runRefresh = do
    env <- ask
    let tok = bot_token . server_config $ env
        interval = worker_interval env
        handler (SomeException e) = getCurrentTime >>= \now -> do
            let report = LogItem now "runRefresh" ("Exception met : " `T.append` (T.pack . show $ e)) "error"
            runApp env $ saveToLog (mongo_config  env) report
            print $ "runRefresh bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling runRefresh now."
        action = do
            threadDelay interval
            now <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            runApp (env {last_worker_run = Just now}) $ do
                evalFeedsAct RefreshNotifyF >>= \case
                    FeedBatches p -> 
                        liftIO ( do
                            let listed = HMS.toList p
                            notified_chats <- mapConcurrently (\(cid, feed_items) ->
                                reply tok cid (toReply . FromFeedsItems $ feed_items) >> pure cid) listed
                            modifyMVar_ (subs_state env) $
                                pure . HMS.map (\c ->
                                    if sub_chatid c `elem` notified_chats
                                    then c { sub_last_notification = Just now }
                                    else c)    
                        ) >> saveToLog (mongo_config env) (LogItem now "runRefresh" (T.append "Refresh worker tried to send notification packages: " (T.pack . show $ p)) "worker_error")
                    _ -> saveToLog (mongo_config env) $ LogItem now "runRefresh" "Worked failed to acquire notification package." "worker_error"
    liftIO $ runForever_ action handler

runJobs :: MonadIO m => App m ()
runJobs = ask >>= \env ->
    let q = tasks_queue env
        action = readChan q >>= \case
            IncReadsJob links -> void . runApp env $ evalFeedsAct (IncReadsF links)
        handler (SomeException e) = getCurrentTime >>= \now ->
            let report = LogItem now "runDbTasks" ("Exception met : " `T.append` (T.pack . show $ e)) "error"
            in  do
                runApp env $ saveToLog (mongo_config  env) report
                print $ "runDbTasks bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling run_worker now."
    in  liftIO $ runForever_ action handler