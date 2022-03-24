{-# LANGUAGE FlexibleContexts #-}

module Jobs where

import AppTypes (AppConfig (..), Batch (Digests, Follows), CacheAction (CacheRefresh, CacheSetPages), DbAction (..), DbRes (..), Digest (Digest), Feed (f_items, f_link, f_title), FeedLink, FromCache (CacheDigests), Job (..), LogItem (LogPerf, log_at, log_message, log_refresh, log_sending_notif, log_total, log_updating), PageOne (PageOne), Replies (..), Reply (EditReply, ServiceReply), ServerConfig (..), SubChat (..), renderDbError, runApp)
import Backend (markNotified)
import Broker (HasCache (withCache))
import Control.Concurrent
  ( readChan,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (async, forConcurrently)
import Control.Exception (Exception, SomeException (SomeException), catch)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.System (SystemTime (systemSeconds), getSystemTime, systemToUTCTime)
import Mongo (evalDb, saveToLog)
import Replies
  ( mkDigestUrl,
    mkReply,
  )
import Requests (mkKeyboard, reply, reqSend_)
import TgramOutJson (Outbound (DeleteMessage, PinMessage))
import Utils (scanTimeSlices)

{- Background tasks -}

runForever_ :: Exception e => IO () -> (e -> IO ()) -> IO ()
runForever_ action handler = void . async . forever $ catch action handler

procNotif :: (MonadReader AppConfig m, MonadIO m) => m ()
procNotif = do
    env <- ask
    let tok = bot_token . tg_config $ env
        interval = worker_interval env
        onError (SomeException err) = do
            let report = "notifier: exception met : " `T.append` (T.pack . show $ err)
            writeChan (postjobs env) . JobTgAlert $ report
        -- sending digests + follows
        send_tg_notif hmap now = forConcurrently (HMS.toList hmap) $
            \(cid, (c, batch)) -> let sets = sub_settings c in case batch of
                Follows fs -> do
                    reply tok cid (mkReply (FromFollow fs sets)) (postjobs env)
                    pure (cid, map f_link fs)
                Digests ds -> do
                    let (ftitles, flinks, fitems) = foldr (\f (one, two, three) ->
                            (f_title f:one, f_link f:two, three ++ f_items f)) ([],[],[]) ds
                        ftitles' = S.toList . S.fromList $ ftitles
                        flinks' = S.toList . S.fromList $ flinks
                        digest = Digest Nothing now fitems flinks' ftitles'
                    res <- evalDb env $ WriteDigest digest
                    let mb_digest_link r = case r of
                            DbDigestId _id -> Just $ mkDigestUrl (base_url env) _id
                            _ -> Nothing
                    reply tok cid (mkReply (FromDigest ds (mb_digest_link res) sets)) (postjobs env)
                    pure (cid, map f_link ds)
        notify = do
            now <- getCurrentTime
            t1 <- systemSeconds <$> getSystemTime
            -- every six hours, concurrently flipping back all pages
            when (maybe False (\t -> diffUTCTime now t > 3600) (last_worker_run env)) (writeChan (postjobs env) JobFlipPages)
            -- rebuilding feeds and collecting notifications
            res <- runApp (env { last_worker_run = Just now }) $ withCache CacheRefresh
            case res of
                Right (CacheDigests notif_hmap) -> do
                    t2 <- systemSeconds <$> getSystemTime
                    -- sending digests, follows & search notifications
                    notified_chats_feeds <- send_tg_notif notif_hmap now
                    t3 <- systemSeconds <$> getSystemTime
                    -- confirming notifications against locally stored + database chats
                    let notified_chats = map fst notified_chats_feeds
                        read_feeds =
                            S.toList .
                            S.fromList .
                            foldMap snd $ notified_chats_feeds :: [FeedLink]
                    markNotified env notified_chats now
                    -- increasing reads count
                    writeChan (postjobs env) $ JobIncReadsJob read_feeds
                    -- wrapping up performance stats
                    (t4, later) <- (\t -> (systemSeconds t, systemToUTCTime t)) <$> getSystemTime
                    let perf = scanTimeSlices [t1, t2, t3, t4]
                    when (length perf == 3) $ do
                        let from_keys = T.intercalate ", " . map (T.pack . show) . HMS.keys
                            msg = "notifier ran on update notif package for " `T.append` from_keys notif_hmap
                            item = LogPerf {
                            log_message = msg,
                            log_at = later,
                            log_refresh = head perf,
                            log_sending_notif = perf !! 1,
                            log_updating = perf !! 2,
                            log_total = sum perf
                            }
                        -- sending logs
                        writeChan (postjobs env) $ JobLog item
                Left err ->
                    writeChan (postjobs env) $ JobTgAlert $ "notifier: \
                        \ failed to acquire notification package and got this error: "
                        `T.append` err
                -- to avoid an incomplete pattern
                _ -> pure ()
        wait_action = threadDelay interval >> notify
        handler e = onError e >> notify
    liftIO $ runForever_ wait_action handler

postProcJobs :: (MonadReader AppConfig m, MonadIO m) => m ()
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
            JobFlipPages -> fork $ do
                -- flipping back all pages to page 1
                res <- evalDb env GetAllPages
                case res of
                    DbPagesOne pages -> for_ pages $ 
                        \(PageOne p cid mid n mb_url) ->
                            let rep = EditReply mid p True (mkKeyboard 1 n mb_url)
                            in  reply (bot_token . tg_config $ env) cid rep $ postjobs env
                    DbErr err -> writeChan (postjobs env) $ JobTgAlert (renderDbError err)
                    _ -> pure ()
            JobIncReadsJob links -> fork $ evalDb env (IncReads links)
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
            JobSetPagination cid mid pages mb_link -> fork $
                let to_db = evalDb env $ InsertPages cid mid pages mb_link
                    to_cache = withCache $ CacheSetPages cid mid pages mb_link
                in  runApp env (to_db >> to_cache) >>= \case
                    Right _ -> pure ()
                    _ ->
                        let report = "Failed to update Redis on this key: " `T.append` T.append (T.pack . show $ cid) (T.pack . show $ mid)
                        in  writeChan (postjobs env) (JobTgAlert report)
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