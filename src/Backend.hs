{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Backend where

import AppTypes
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sortBy)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Database (Db (evalDb), evalDb)
import Parser (getFeedFromHref, rebuildFeed)
import Search (initSearchWith)
import TgramOutJson (ChatId)
import Utils (defaultChatSettings, freshLastXDays, mergeSettings, notifFor, partitionEither, removeByUserIdx)

withChat :: MonadIO m => UserAction -> ChatId -> App m (Either UserError ())
withChat action cid = ask >>= \env -> liftIO $ do
    modifyMVar (subs_state env) (`afterDb` env)
    where
    afterDb hmap env = case HMS.lookup cid hmap of
        Nothing -> case action of
            Sub links ->
                let created_c = SubChat cid Nothing Nothing (S.fromList links) defaultChatSettings
                    inserted_m = HMS.insert cid created_c hmap
                in  evalDb env (UpsertChat created_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (inserted_m, Right ())
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
        Just c -> case action of
            Reset ->
                let updated_c = c { sub_settings = defaultChatSettings }
                    update_m = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ())
            Sub links ->
                let updated_c = c { sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
                    updated_m = HMS.insert cid updated_c hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (updated_m, Right ())
            UnSub refs -> do
                let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u:us, is); ById i -> (us, i:is)) ([],[]) refs
                    update_db c' = evalDb env (UpsertChat c') >>= \case
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
            Purge -> evalDb env (DeleteChat cid) >>= \case
                DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                _ -> pure (HMS.delete cid hmap, Right ())
            SetSubFeedSettings unparsed ->
                let updated_settings = mergeSettings [] unparsed $ sub_settings c
                    updated_c = c { sub_settings = updated_settings }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                    DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                    _ -> pure (updated_cs, Right ())
            Pause pause_or_resume ->
                let updated_sets = (sub_settings c) { settings_is_paused = pause_or_resume }
                    updated_c = c { sub_settings = updated_sets }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                    DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
                    _ -> pure (updated_cs, Right ())
            _ -> pure (hmap, Right ())

loadChats :: MonadIO m => App m ()
loadChats =
    ask >>= \env -> liftIO $ modifyMVar_ (subs_state env) $ \chats_hmap ->
    evalDb env GetAllChats >>= \case
        DbChats chats -> pure . HMS.fromList . map (\c -> (sub_chatid c, c)) $ chats
        _ -> pure chats_hmap

updateEngine :: MVar ([KeyedItem], FeedsSearch)-> [Feed] -> IO ()
updateEngine mvar items =
    let is_search = initSearchWith items
    in  tryTakeMVar mvar >> putMVar mvar is_search

evalFeedsAct :: MonadIO m => FeedsAction -> App m (FeedsRes a)
evalFeedsAct (InitF start_urls) = do
    env <- ask
    res <- liftIO $ mapConcurrently getFeedFromHref start_urls
    case sequence res of
        Left err -> liftIO $ print err >> pure FeedsOk
        Right refreshed_feeds -> do
            dbres <- evalDb env $ UpsertFeeds refreshed_feeds
            case dbres of
                DbErr err -> liftIO $ print $ renderDbError err
                _ -> pure ()
            pure FeedsOk
evalFeedsAct LoadF = do
    env <- ask
    evalDb env Get100Feeds >>= \case
        DbFeeds feeds -> do
            liftIO $ modifyMVar_ (feeds_state env) $ \_ ->
                let feeds_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
                in  updateEngine (search_engine env) feeds >> pure feeds_hmap
            pure FeedsOk
        _ -> pure $ FeedsError FailedToLoadFeeds
evalFeedsAct (AddF feeds) = do
    env <- ask
    res <- liftIO $ modifyMVar (feeds_state env) $ \app_hmap ->
        let user_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
        in  evalDb env (UpsertFeeds feeds) >>= \case
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
    modifyMVar (feeds_state env) $ \feeds_hmap -> do
        -- update chats MVar just in case we were able to produce an interesting result
        now <- getCurrentTime
        -- collecting hmap of feeds with relevantly related chats
        let relevant_feedlinks = collect_subscribed_to_feeds chats
        -- rebuilding all feeds with any subscribers
        eitherUpdated <- mapConcurrently rebuildFeed relevant_feedlinks
        let (failed, succeeded) = partitionEither eitherUpdated
            updated_feeds = HMS.fromList $ map (\f -> (f_link f, f)) succeeded
            -- notifying only non paused && due for notification
            relevant_chats = HMS.filter (\c ->
                (not . settings_is_paused . sub_settings $ c) &&
                maybe True (< now) (sub_next_notification c)) chats
            notif = notifFor updated_feeds relevant_chats
            -- keeping only top 100 most read with feeds in memory from thee rebuilt ones that have any subscribers
            to_keep_in_memory = HMS.filter (\f -> f_link f `elem` within_top100_reads succeeded) updated_feeds
        unless (null failed) (writeChan (postjobs env) $ JobTgAlert $
            "Failed to update theses feeds: " `T.append` T.intercalate " " failed)
        -- saving to memory & db
        evalDb env (UpsertFeeds succeeded) >>= \case
            DbOk -> do
                -- updating search engine on successful save to database.
                updateEngine (search_engine env) (HMS.elems to_keep_in_memory)
                -- increasing reads count
                writeChan (postjobs env) . JobIncReadsJob $ relevant_feedlinks
                -- computing next run
                writeChan (postjobs env) . JobUpdateSchedules $ HMS.keys relevant_chats
                -- refreshing feeds mvar
                pure (to_keep_in_memory, FeedBatches notif)
            _ -> pure (feeds_hmap, FeedsError . FailedToUpdate $ " at evalFeedsAct.RefreshNotifyF")
    where
        within_top100_reads succeeded =
            take 100 .
            map f_link .
            sortBy (comparing $ Down . f_reads) $ succeeded
        collect_subscribed_to_feeds chats =
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
evalFeedsAct (IncReadsF links) = ask >>= \env -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \hmap ->
        evalDb env (IncReads links) >>= \case
            DbOk -> pure $ HMS.map (\f -> f { f_reads = 1 + f_reads f }) hmap
            _ -> pure hmap
    pure FeedsOk