{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App, AppConfig (last_worker_run, postjobs, subs_state, tg_config, worker_interval), ChatSettings (settings_batch_interval), DbAction (UpsertChats), DbRes (DbOk), FeedsAction (IncReadsF, RefreshNotifyF), FeedsRes (FeedBatches), Job (IncReadsJob, Log, RemoveMsg, TgAlert, UpdateSchedules), LogItem (LogItem), ServerConfig (alert_chat, bot_token), SubChat (sub_chatid, sub_last_notification, sub_next_notification, sub_settings), runApp)
import Backend (evalFeedsAct)
import Control.Concurrent
  ( modifyMVar_,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (async, mapConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Database (evalDb, saveToLog)
import Replies
  ( FromContents (FromFeedsItems),
    Reply (PlainReply),
    toReply,
  )
import Requests (reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage))
import Utils (findNextTime)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ go
    where   go = catch action handler

refresher :: MonadIO m => App m ()
refresher = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        -- sending alert over Telegram
        onError (SomeException err) = do
            let report = "refresher: exception met : " `T.append` (T.pack . show $ err)
            writeChan (postjobs env) . TgAlert $ report
        action = do
            t1 <- getCurrentTime
            -- rebuilding feeds and dispatching notifications
            runApp (env { last_worker_run = Just t1 }) $ evalFeedsAct RefreshNotifyF >>= \case
                FeedBatches payload -> liftIO $ do
                    -- sending notifications
                    notified_chats <- mapConcurrently
                        (\(cid, feed_items) ->
                        reply tok cid (toReply . FromFeedsItems $ feed_items) >> pure cid) $ HMS.toList payload
                    -- updating chats
                    modifyMVar_ (subs_state env) $ \subs ->
                        let updated_chats = HMS.map (\c -> if sub_chatid c `elem` notified_chats then c { sub_last_notification = Just t1 } else c) subs
                        in  evalDb env (UpsertChats updated_chats) >> pure updated_chats
                    t2 <- getCurrentTime
                    let diff = diffUTCTime t1 t2
                        item = LogItem t2 "refresher" ("refresher ran successfully within " `T.append` (T.pack . show $ diff))
                    writeChan (postjobs env) (Log item)
                _ -> liftIO $ writeChan (postjobs env) . TgAlert $ "refresher: failed to acquire notification package"
        wait_action = threadDelay interval >> action
        handler e = onError e >> action
    liftIO $ runForever_ wait_action handler

postProcJobs :: MonadIO m => App m ()
postProcJobs = ask >>= \env ->
    let action = readChan (postjobs env) >>= \case
            IncReadsJob links -> void . runApp env $ evalFeedsAct (IncReadsF links)
            Log item -> saveToLog env item
            RemoveMsg cid mid -> liftIO . void . async $ do
                threadDelay 30000000
                reqSend_ (bot_token . tg_config $ env) "deleteMessage" (DeleteMessage cid mid) >>= \case
                    Left err -> print err
                    Right _ -> pure ()
            TgAlert contents -> do
                let msg = PlainReply $ "feedfarer2 is sending an alert: " `T.append` contents
                print $ "postProcJobs: TgAlert " `T.append` (T.pack . show $ contents)
                reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) msg
            UpdateSchedules chat_ids -> 
                getCurrentTime >>= \now -> 
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
            writeChan (postjobs env) . TgAlert $ report
            print $ "postProcJobs bumped on exception " `T.append` (T.pack . show $ report) `T.append` "Rescheduling postProcJobs now."
    in  liftIO $ runForever_ action handler
