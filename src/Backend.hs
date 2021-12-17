{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Backend where

import AppTypes
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.List (intersect, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX
import Database (evalMongoAct)
import Parser (getFeedFromHref, rebuildFeed)
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import Utils (partitionEither)
import Data.IORef (readIORef)

{- Subscriptions -}

freshLastXDays :: Int -> UTCTime -> [Item] -> [Item]
freshLastXDays days now items =
    let x = fromIntegral $ days * 86400
        x_days_ago = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - x
    in  filter (\i -> i_pubdate i > x_days_ago) items

tooManySubs :: Int -> SubChats -> ChatId -> Bool
tooManySubs upper_bound chats cid = case HMS.lookup cid chats of
    Nothing -> False
    Just chat ->
        let diff = upper_bound - length (sub_feeds_links chat)
        in  diff < 0

notificationBatches :: KnownFeeds -> SubChats -> UTCTime -> HMS.HashMap ChatId FeedItems
notificationBatches feeds_hmap subs current_time =
    HMS.foldl' (\notif chat ->
        let subscribed_to = sub_feeds_links chat
            relevant_chat_feeds = HMS.filter (\f -> f_link f `elem` subscribed_to) feeds_hmap
            relevant_chat_items = HMS.foldl' (\acc f ->
                let fresh_filtered = filterItemsWith (sub_settings chat) current_time (sub_last_notification chat) $ f_items f
                in  if null fresh_filtered then acc else (f, fresh_filtered):acc) [] relevant_chat_feeds
        in  if null relevant_chat_items then notif else HMS.insert (sub_chatid chat) relevant_chat_items notif
    ) HMS.empty subs
    where
        filterItemsWith :: FeedSettings -> UTCTime -> Maybe UTCTime -> [Item] -> [Item]
        filterItemsWith _ _ Nothing items = items
        filterItemsWith FeedSettings{..} now (Just last_time) items =
            if      not settings_batch
            then    filter (with_filters [blacklist, time_window]) items
            else    take size .
                    filter (with_filters [blacklist, time_window]) $ items
            where
                with_filters fs i = all ($ i) fs
                blacklist i = not . any (`T.isInfixOf` i_desc i) $ filters_blacklist settings_filters
                time_window i = case settings_batch_interval of
                    Nothing -> last_time < i_pubdate i
                    Just d -> now < addUTCTime d (i_pubdate i)
                size = if settings_batch_size == 0 then length items else settings_batch_size

defaultFeedSettings :: FeedSettings
defaultFeedSettings = FeedSettings {
        settings_filters = Filters [],
        settings_batch = False,
        settings_batch_size = 15,
        settings_batch_interval = Just 9000
    }

mergeFeedSettings :: UnParsedFeedSettings -> FeedSettings -> FeedSettings
mergeFeedSettings keyvals orig =
    let keys = Map.keys keyvals
        updater = FeedSettings {
            settings_filters = Filters $ maybe [] (T.splitOn ",") $ Map.lookup "whitelist" keyvals,
            settings_batch = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "batch" keyvals,
            settings_batch_size =
                let mbread = readMaybe . T.unpack =<<
                        Map.lookup "batch_size" keyvals
                in  fromMaybe 10 mbread,
            settings_batch_interval = maybe (Just 9000) (\t ->
                let res = realToFrac <$> (readMaybe . T.unpack $ t :: Maybe Integer) :: Maybe NominalDiffTime
                in  res) $ Map.lookup "batch_interval" keyvals
        }
    in  orig {
            settings_filters = if "blacklist" `elem` keys then settings_filters updater else settings_filters orig,
            settings_batch = if "batch" `elem` keys then settings_batch updater else settings_batch orig,
            settings_batch_size = if "batch_size" `elem` keys then settings_batch_size updater else settings_batch_size orig,
            settings_batch_interval = if "batch_interval" `elem` keys then settings_batch_interval updater else settings_batch_interval orig
        }

withChat :: MonadIO m => UserAction -> ChatId -> App m (Either UserError ())
withChat action cid = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config -> modifyMVar (subs_state env) (`afterDb` config) where
    afterDb hmap config = case HMS.lookup cid hmap of
        Nothing -> case action of
            Sub links ->
                let created_c = SubChat cid [] Nothing (S.fromList links) defaultFeedSettings False
                    inserted_m = HMS.insert cid created_c hmap
                in  evalMongoAct config (UpsertChat created_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (inserted_m, Right ())
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
        Just c -> case action of
            Sub links ->
                let updated_c = c { sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
                    updated_m = HMS.insert cid updated_c hmap
                in  evalMongoAct config (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (updated_m, Right ())
            UnSub links ->
                let updated_c = c { sub_feeds_links = S.filter (`notElem` unFeedRefs links) $ sub_feeds_links c }
                    updated_m = HMS.insert cid updated_c hmap
                in  evalMongoAct config (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (updated_m, Right ())
            Purge -> evalMongoAct config (DeleteChat cid) >>= \case
                DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                _ -> pure (HMS.delete cid hmap, Right ())
            SetSubFeedSettings unparsed ->
                let updated_settings = mergeFeedSettings unparsed $ sub_settings c
                    updated_c = c { sub_settings = updated_settings }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalMongoAct config (UpsertChat updated_c) >>= \case
                    DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                    _ -> pure (updated_cs, Right ())
            Pause pause_or_resume ->
                let updated_c = c { sub_is_paused = pause_or_resume }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalMongoAct config (UpsertChat updated_c) >>= \case
                    DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
                    _ -> pure (updated_cs, Right ())
            _ -> pure (hmap, Right ())

loadChats :: MonadIO m => App m ()
loadChats = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config -> modifyMVar_ (subs_state env) $ \chats_hmap ->
    evalMongoAct config GetAllChats >>= \case
        DbChats chats -> pure . HMS.fromList . map (\c -> (sub_chatid c, c)) $ chats
        _ -> pure chats_hmap

evalFeedsAct :: MonadIO m => FeedsAction -> App m (FeedsRes a)
evalFeedsAct (InitF start_urls) = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config -> do
    res <- mapConcurrently getFeedFromHref start_urls
    case sequence res of
        Left err -> print err >> pure FeedsOk
        Right refreshed_feeds -> do
            evalMongoAct config (UpsertFeeds refreshed_feeds) >>= \case
                DbErr err -> print $ renderDbError err
                _ -> pure ()
            pure FeedsOk
evalFeedsAct LoadF = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config ->
    evalMongoAct config Get100Feeds >>= \case
        DbFeeds feeds -> do
            modifyMVar_ (feeds_state env) $ \_ -> pure . HMS.fromList $ map (\f -> (f_link f, f)) feeds
            pure FeedsOk
        _ -> pure $ FeedsError FailedToLoadFeeds
evalFeedsAct (AddF feeds) = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config -> do
    res <- liftIO . modifyMVar (feeds_state env) $ \app_hmap ->
        let user_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
        in  evalMongoAct config (UpsertFeeds feeds) >>= \case
                DbOk -> pure (HMS.union user_hmap app_hmap, Just ())
                _ -> pure (app_hmap, Nothing)
    case res of
        Nothing -> pure $ FeedsError FailedToStoreAll
        Just _ -> pure FeedsOk
evalFeedsAct (RemoveF links) = ask >>= \env -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \app_hmap ->
        let deleted = HMS.filter (\f -> f_link f `notElem` links) app_hmap
        in  pure deleted
    pure FeedsOk
evalFeedsAct RefreshNotifyF = ask >>= \env -> do
    (chats, config) <- (,) <$> liftIO (readMVar $ subs_state env) <*> liftIO (readIORef $ db_config env)
    res <- liftIO . modifyMVar (feeds_state env) $
    -- update chats MVar just in case we were able to produce an interesting result
        \feeds_hmap -> do
            -- collecting HMap of feeds with any subscribers
            let feeds_to_subs = HMS.foldl' (\hmap sub ->
                    let links = S.toList $ sub_feeds_links sub
                        cid = sub_chatid sub
                    in  if null links then hmap else updateWith cid links hmap) HMS.empty chats
                subbed_to = HMS.keys feeds_to_subs
            -- rebuilding all feeds with any subscribers
            eitherUpdated <- mapConcurrently rebuildFeed subbed_to
            now <- getCurrentTime
            let (failed, succeeded) = partitionEither eitherUpdated
                updated_subscribed_to_feeds = HMS.fromList $
                    map (\f -> (f_link f, f)) succeeded
                -- building HMap of chat_ids with the relevant feed & items
                notif = notificationBatches updated_subscribed_to_feeds (HMS.filter (not . sub_is_paused) chats) now
                is_within_top100_reads =
                    take 100 .
                    map f_link .
                    sortBy (comparing $ Down . f_reads) $ HMS.elems feeds_hmap
                -- keeping only top 100 most read with feeds in memory provided they have any subscribers
                to_keep_in_memory = HMS.filter
                    (\f -> f_link f `elem` (subbed_to `intersect` is_within_top100_reads))
                    feeds_hmap
            unless (null failed) (print $ "Failed to update theses feeds: " `T.append` T.intercalate " " failed)
            evalMongoAct config (UpsertFeeds succeeded) >>= \case
                DbErr _ -> pure (feeds_hmap, Nothing)
                _ -> pure (to_keep_in_memory, Just (notif, subbed_to))
    case res of
        Nothing -> pure . FeedsError . FailedToUpdateFeeds $ " at evalFeedsAct.RefreshNotifyF"
        Just (notif, subbed_to) -> do
            -- extracting results for dispatching; in order:
                --  notification to send to individual chats
                --  batch updates to write to the db
                --  read counts to write to the db + memory
            liftIO $ writeChan (tasks_queue env) $ IncReadsJob subbed_to
            -- returning notifications to the calling thread
            pure $ FeedBatches notif
    where
        updateWith _ [] hmap = hmap
        updateWith cid (f:fs) hmap = updateWith cid fs (updateOrInsert cid f hmap)
        updateOrInsert cid f hmap = case HMS.lookup f hmap of
            Nothing -> HMS.insert f (S.singleton cid) hmap
            Just cids -> HMS.insert f (S.insert cid cids) hmap
evalFeedsAct (GetAllXDays links days) = do
    env <- ask
    (feeds, now) <- liftIO $ (,) <$> (readMVar . feeds_state $ env) <*> getCurrentTime
    pure . FeedLinkBatch . foldFeeds feeds $ now
    where
        foldFeeds feeds now = HMS.foldl' (\acc f -> if f_link f `notElem` links then acc else collect now f acc) [] feeds
        collect now f acc =
            let fresh = freshLastXDays days now $ f_items f
            in  if null fresh then acc else (f_link f, fresh):acc
evalFeedsAct (IncReadsF links) = ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \hmap ->
        evalMongoAct config (IncReads links) >>= \case
            DbOk -> pure $ HMS.map (\f -> f { f_reads = 1 + f_reads f }) hmap
            _ -> pure hmap
    pure FeedsOk