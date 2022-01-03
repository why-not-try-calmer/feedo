{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Backend where

import AppTypes
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX
import Database (interpretDb, Db (interpretDb))
import Parser (getFeedFromHref, rebuildFeed)
import Search (initSearchWith)
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import Utils (freshLastXDays, partitionEither, removeByUserIdx)

notifFor :: KnownFeeds -> SubChats -> HMS.HashMap ChatId FeedItems
notifFor feeds_hmap subs_hmap = HMS.foldl' (\acc f ->
    let layer = HMS.foldl' (\hmapping c ->
            if f_link f `notElem` sub_feeds_links c || null (fresh_filtered c f) then hmapping
            else
                let v = (f, fresh_filtered c f)
                in  HMS.alter (\case 
                    Nothing -> Just [v]
                    Just vs -> Just (v:vs)) (sub_chatid c) hmapping) acc subs_hmap
    in  HMS.union layer acc) HMS.empty feeds_hmap
    where
        fresh_filtered c f = filterItemsWith (sub_settings c) (sub_last_notification c) $ f_items f
        filterItemsWith _ Nothing items = items
        filterItemsWith ChatSettings{..} (Just last_time) items =
            take settings_batch_size .
            filter (with_filters [blacklist, fresh]) $ items
            where
                with_filters fs i = all ($ i) fs
                blacklist i = not . any
                    (\l -> T.toCaseFold l `T.isInfixOf` (i_desc i `T.append` i_link i)) $
                        filters_blacklist settings_filters
                fresh i = last_time < i_pubdate i

defaultChatSettings :: ChatSettings
defaultChatSettings = ChatSettings {
        settings_filters = Filters [] [],
        settings_batch_size = 15,
        settings_batch_interval = Secs 9000,
        settings_is_paused = False
    }

mergeSettings :: ParsedChatSettings -> ChatSettings -> ChatSettings
{-
Expected fields:
- blacklist: term, term, term...
- whitelist: term, term, term...
- batch_at: hhmm, hhmm...
- batch_every: n
- paused: true | false
-}
mergeSettings keyvals orig =
    let updater = ChatSettings {
            settings_filters =
                let blacklist = maybe [] (T.splitOn ",") $ Map.lookup "blacklist" keyvals
                    whitelist = maybe [] (T.splitOn ",") $ Map.lookup "whitelist" keyvals
                in  Filters blacklist whitelist,
            settings_batch_size =
                let mbread = readMaybe . T.unpack =<<
                        Map.lookup "batch_size" keyvals
                in  fromMaybe 10 mbread,
            settings_batch_interval =
                case Map.lookup "batch_every" keyvals of
                    Nothing -> case Map.lookup "batch_at" keyvals of
                        Nothing -> dflt
                        Just txt ->
                            let datetimes = T.splitOn "," txt
                                collected = foldl' step [] datetimes
                            in if null collected then dflt else HM collected
                    Just mbint -> maybe dflt Secs (readMaybe . T.unpack $ mbint),
            settings_is_paused = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "paused" keyvals
        }
    in  orig {
            settings_filters = if "blacklist" `elem` keys then settings_filters updater else settings_filters orig,
            settings_batch_size = if "batch_size" `elem` keys then settings_batch_size updater else settings_batch_size orig,
            settings_batch_interval =
                if any (`elem` keys) ["batch_at", "batch_every"] then settings_batch_interval updater
                else settings_batch_interval orig,
            settings_is_paused = if "paused" `elem` keys then settings_is_paused updater else settings_is_paused orig
        }
    where
        keys = Map.keys keyvals
        dflt = Secs 9000
        step acc val =
            let (hh, mm) = T.splitAt 2 val
                (m1, m2) = T.splitAt 1 mm
                mm' =
                    if m1 == "0" then readMaybe . T.unpack $ m2 :: Maybe Int
                    else readMaybe . T.unpack $ mm :: Maybe Int
                hh' = readMaybe . T.unpack $ hh :: Maybe Int
            in  case sequence [hh', mm'] of
                Nothing -> []
                Just parsed ->
                    if length parsed /= 2 then [] else
                        let (h, m) = (head parsed, last parsed)
                        in  toAcc h m acc
        toAcc h m acc
            |   h < 0 || h > 24 = []
            |   m < 0 || m > 60 = []
            |   otherwise = acc ++ [(h, m)]

withChat :: MonadIO m => UserAction -> ChatId -> App m (Either UserError ())
withChat action cid =
    ask >>= \env -> liftIO $ do
    config <- readIORef $ db_config env
    modifyMVar (subs_state env) (`afterDb` config)
    where
    afterDb hmap config = case HMS.lookup cid hmap of
        Nothing -> case action of
            Sub links ->
                let created_c = SubChat cid Nothing Nothing (S.fromList links) defaultChatSettings
                    inserted_m = HMS.insert cid created_c hmap
                in  interpretDb config (UpsertChat created_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (inserted_m, Right ())
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
        Just c -> case action of
            Reset ->
                let updated_c = c { sub_settings = defaultChatSettings }
                    update_m = HMS.update (\_ -> Just updated_c) cid hmap
                in  interpretDb config (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ())
            Sub links ->
                let updated_c = c { sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
                    updated_m = HMS.insert cid updated_c hmap
                in  interpretDb config (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (updated_m, Right ())
            UnSub refs -> do
                let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u:us, is); ById i -> (us, i:is)) ([],[]) refs
                    update_db c' = interpretDb config (UpsertChat c') >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (HMS.insert cid c' hmap, Right ())
                if not (null byurls) && not (null byids) then pure (hmap, Left . BadInput $ "You cannot mix references by urls and by ids in the same command.")
                else
                    if null byurls then case removeByUserIdx (S.toList . sub_feeds_links $ c) byids of
                    Nothing -> pure (hmap, Left . BadInput $ "Invalid references. Make sure to use /list to get a list of valid references.")
                    Just removed ->
                        let updated_c = c { sub_feeds_links = S.fromList removed }
                        in  update_db updated_c
                    else
                        let updated_c = c { sub_feeds_links = S.filter (`notElem` byurls) $ sub_feeds_links c}
                        in  update_db updated_c
            Purge -> interpretDb config (DeleteChat cid) >>= \case
                DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                _ -> pure (HMS.delete cid hmap, Right ())
            SetSubFeedSettings unparsed ->
                let updated_settings = mergeSettings unparsed $ sub_settings c
                    updated_c = c { sub_settings = updated_settings }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  interpretDb config (UpsertChat updated_c) >>= \case
                    DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                    _ -> pure (updated_cs, Right ())
            Pause pause_or_resume ->
                let updated_sets = (sub_settings c) { settings_is_paused = pause_or_resume }
                    updated_c = c { sub_settings = updated_sets }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  interpretDb config (UpsertChat updated_c) >>= \case
                    DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
                    _ -> pure (updated_cs, Right ())
            _ -> pure (hmap, Right ())

loadChats :: MonadIO m => App m ()
loadChats =
    ask >>= \env -> liftIO $ readIORef (db_config env) >>= \config ->
    modifyMVar_ (subs_state env) $ \chats_hmap ->
    interpretDb config GetAllChats >>= \case
        DbChats chats -> pure . HMS.fromList . map (\c -> (sub_chatid c, c)) $ chats
        _ -> pure chats_hmap

updateEngine :: MVar ([KeyedItem], FeedsSearch)-> [Feed] -> IO ()
updateEngine mvar items =
    let is_search = initSearchWith items
    in  tryTakeMVar mvar >> putMVar mvar is_search

evalFeedsAct :: MonadIO m => FeedsAction -> App m (FeedsRes a)
evalFeedsAct (InitF start_urls) = do
    env <- ask
    config <- liftIO . readIORef $ db_config env
    res <- liftIO $ mapConcurrently getFeedFromHref start_urls
    case sequence res of
        Left err -> liftIO $ print err >> pure FeedsOk
        Right refreshed_feeds -> do
            dbres <- interpretDb config $ UpsertFeeds refreshed_feeds
            case dbres of
                DbErr err -> liftIO $ print $ renderDbError err
                _ -> pure ()
            pure FeedsOk
evalFeedsAct LoadF = do
    env <- ask
    config <- liftIO $ readIORef (db_config env)
    interpretDb config Get100Feeds >>= \case
        DbFeeds feeds -> do
            liftIO $ modifyMVar_ (feeds_state env) $ \_ ->
                let feeds_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
                in  updateEngine (search_engine env) feeds >> pure feeds_hmap
            pure FeedsOk
        _ -> pure $ FeedsError FailedToLoadFeeds
evalFeedsAct (AddF feeds) = do
    env <- ask
    config <- liftIO $ readIORef (db_config env)
    res <- liftIO $ modifyMVar (feeds_state env) $ \app_hmap ->
        let user_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
        in  interpretDb config (UpsertFeeds feeds) >>= \case
                DbOk ->
                    updateEngine (search_engine env) feeds >>
                    pure (HMS.union user_hmap app_hmap, Just ())
                _ -> pure (app_hmap, Nothing)
    case res of
        Nothing -> pure $ FeedsError FailedToStoreAll
        Just _ -> pure FeedsOk
evalFeedsAct (RemoveF links) = ask >>= \env -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \app_hmap ->
        let deleted = HMS.filter (\f -> f_link f `notElem` links) app_hmap
        in  pure deleted
    pure FeedsOk
evalFeedsAct RefreshNotifyF = ask >>= \env -> liftIO $ do
    chats <- readMVar $ subs_state env
    config <- readIORef $ db_config env
    modifyMVar (feeds_state env) $ \feeds_hmap -> do
        -- update chats MVar just in case we were able to produce an interesting result
        now <- getCurrentTime
        -- collecting hmap of feeds with relevantly related chats
        let relevant_feedlinks = collect_relevant_feedlinks chats
        -- rebuilding all feeds with any subscribers
        eitherUpdated <- mapConcurrently rebuildFeed relevant_feedlinks
        let (failed, succeeded) = partitionEither eitherUpdated
            updated_feeds = HMS.fromList $ map (\f -> (f_link f, f)) succeeded
            -- building hmap of chat_ids with the relevant feed & items
            relevant_chats = HMS.filter (\c -> 
                (not . settings_is_paused . sub_settings $ c) && 
                maybe True (< now) (sub_next_notification c)) chats
            notif = notifFor updated_feeds relevant_chats
            -- keeping only top 100 most read with feeds in memory from thee rebuilt ones that have any subscribers
            to_keep_in_memory = HMS.filter (\f -> f_link f `elem` within_top100_reads succeeded) updated_feeds
        unless (null failed) (writeChan (tasks_queue env) $ TgAlert $
            "Failed to update theses feeds: " `T.append` T.intercalate " " failed)
        -- saving to memory & db
        interpretDb config (UpsertFeeds succeeded) >>= \case
            DbOk -> do
                -- updating search engine on successful save to database.
                let upto l = T.pack . show $ diffUTCTime l now
                updateEngine (search_engine env) (HMS.elems to_keep_in_memory)
                -- increasing reads count
                writeChan (tasks_queue env) . IncReadsJob $ relevant_feedlinks
                later <- getCurrentTime
                -- logging
                writeChan (tasks_queue env)
                    (Log $ LogItem later "interpretDb.UpserFeeds"
                    ("Ran worker job (refreshing all feeds + search engine) in " `T.append` upto later) "info")
                pure (to_keep_in_memory, FeedBatches notif)
            _ -> pure (feeds_hmap, FeedsError . FailedToUpdate $ " at evalFeedsAct.RefreshNotifyF")
    where
        within_top100_reads succeeded =
            take 100 .
            map f_link .
            sortBy (comparing $ Down . f_reads) $ succeeded
        collect_relevant_feedlinks chats =
            HMS.keys $
            HMS.foldl' (\hmap sub ->
                let links = S.toList $ sub_feeds_links sub
                    cid = sub_chatid sub
                in  if null links
                    then hmap
                    else updateWith cid links hmap) HMS.empty chats
        updateWith _ [] hmap = hmap
        updateWith !cid (f:fs) !hmap = updateWith cid fs $
            HMS.alter (\case
                Nothing -> Just $ S.singleton cid
                Just cids -> Just $ S.insert cid cids) f hmap
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
        interpretDb config (IncReads links) >>= \case
            DbOk -> pure $ HMS.map (\f -> f { f_reads = 1 + f_reads f }) hmap
            _ -> pure hmap
    pure FeedsOk