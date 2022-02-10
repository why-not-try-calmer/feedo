{-# LANGUAGE FlexibleContexts #-}
module Jobs where

import AppTypes (App (..), AppConfig (..), DbAction (..), DbRes (..), Digest (Digest), Feed (f_link, f_title), FeedsAction (..), FeedsRes (..), Job (..), LogItem (LogPerf, log_at, log_message, log_refresh, log_sending_notif, log_total, log_updating), Reply (ServiceReply), ServerConfig (..), Settings (..), SubChat (..), Replies (..), renderDbError, runApp, Item)
import Backend (evalFeeds)
import Control.Concurrent
  ( modifyMVar_,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (async, concurrently, forConcurrently, forConcurrently_)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Data.Foldable (Foldable (foldl'))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime, systemToUTCTime)
import Database (evalDb, saveToLog)
import Replies
  ( mkDigestUrl,
    mkReply,
  )
import Requests (reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Utils (findNextTime, hash, scanTimeSlices)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ catch action handler

procNotif :: MonadIO m => App m ()
procNotif = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        onError (SomeException err) = do
            let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
            writeChan (postjobs env) . JobTgAlert $ report
        -- to send search notifications
        send_tg_search search_payload = forConcurrently_ (HMS.toList search_payload) $
            \(cid, DbSearchRes keys results) -> reply tok cid (mkReply (FromSearchRes keys results)) (postjobs env)
        -- to send follow notifications
        send_tg_follow follow_payload = forConcurrently_ (HMS.toList follow_payload) $
            \(cid, (settings, feeds_items)) -> reply tok cid 
                (mkReply (FromFollow feeds_items settings))
                (postjobs env)
        -- writing the last digest to the db
        -- sending digest notifications
        -- to send digest notifications
        send_tg_notif digests_follows now = forConcurrently (HMS.toList digests_follows) $
            \(cid, (c, f_digests, f_follows)) ->
                let _id = hash . show $ now
                    digests_items = foldMap snd f_digests :: [Item]
                    (ftitles, flinks) = foldl' (\(!ts, !fs) (!f,_) -> (f_title f:ts, f_link f:fs)) ([],[]) f_digests
                    ftitles' = S.toList . S.fromList $ ftitles
                    flinks' = S.toList . S.fromList $ flinks
                    digest = Digest _id now digests_items flinks' ftitles'
                    mb_digest_link res = case res of 
                        DbOk -> Just $ mkDigestUrl _id
                        _ -> Nothing 
                in  do
                    res <- evalDb env $ WriteDigest digest
                    reply tok cid 
                        (mkReply (FromDigest f_digests (mb_digest_link res) (sub_settings c)))
                        (postjobs env)
                    pure (cid, map (f_link . fst) f_digests)
        updated_notified_chats notified_chats chats now =
            HMS.mapWithKey (\cid c -> if cid `elem` notified_chats then 
                    let start = case settings_digest_start . sub_settings $ c of
                            Nothing -> Nothing 
                            Just s -> if s < now then Nothing else Just s
                    -- updating last, next, and consuming 'settings_digest_start'
                    in  c {
                            sub_last_digest = Just now,
                            sub_next_digest = Just $ findNextTime now (settings_digest_interval . sub_settings $ c),
                            sub_settings = (sub_settings c) { settings_digest_start = start } 
                        } 
                    else c) chats
        notify = do
            now <- getCurrentTime
            t1 <- systemSeconds <$> getSystemTime
            -- rebuilding feeds and dispatching notifications
            res <- runApp (env { last_worker_run = Just now }) $ evalFeeds Refresh
            case res of
                FeedDigests notif_hmap search_notif -> do
                    t2 <- systemSeconds <$> getSystemTime
                    -- sending update & search notifications
                    notified_chats_feeds <- fst <$> concurrently (send_tg_notif notif_hmap now) (send_tg_search search_notif)
                    t3 <- systemSeconds <$> getSystemTime
                    -- preparing updates
                    let notified_chats = map fst notified_chats_feeds
                        read_feeds = S.toList . S.fromList . foldMap snd $ notified_chats_feeds
                    -- increasing reads count
                    writeChan (postjobs env) $ JobIncReadsJob read_feeds
                    -- confirming notifications against locally stored + database chats
                    modifyMVar_ (subs_state env) $ \subs ->
                        let updated_chats = updated_notified_chats notified_chats subs now
                        -- updating db
                        in  evalDb env (UpsertChats updated_chats) >>= \case
                            DbErr err -> do
                                writeChan (postjobs env) $ JobTgAlert $ "notifier: failed to \
                                    \ save updated chats to db because of this error" `T.append` renderDbError err
                                pure subs
                            DbOk -> pure updated_chats
                            _ -> pure subs
                    (t4, later) <- (\t -> (systemSeconds t, systemToUTCTime t)) <$> getSystemTime
                    let perf = scanTimeSlices [t1, t2, t3, t4]
                    when (length perf == 3) $ do
                        let from_keys = T.intercalate ", " . map (T.pack . show) . HMS.keys
                            (report1, report2) = (from_keys notif_hmap, from_keys search_notif)
                            msg = "notifier ran on update notif package for "
                                `T.append` report1
                                `T.append` " and search notif package for "
                                `T.append` report2
                            item = LogPerf {
                            log_message = msg,
                            log_at = later,
                            log_refresh = head perf,
                            log_sending_notif = perf !! 1,
                            log_updating = perf !! 2,
                            log_total = sum perf
                            }
                        -- writing logs
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
            JobArchive feeds now -> fork $ do
                -- archiving items
                evalDb env (ArchiveItems feeds) >>= \case
                    DbErr err -> writeChan (postjobs env) . JobTgAlert $
                        "Failed to upsert these feeds: "
                        `T.append` T.intercalate ", " (map f_link feeds)
                        `T.append` " because of "
                        `T.append` renderDbError err
                    _ -> pure ()
                -- cleaning more than 1 month old archives
                void $ evalDb env (PruneOld $ addUTCTime (-2592000) now)
            JobIncReadsJob links -> fork $ runApp env $ evalFeeds (IncReadsF links)
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



