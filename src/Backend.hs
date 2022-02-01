{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Backend where
import AppTypes
import Control.Concurrent
import Control.Concurrent.Async (forConcurrently, mapConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Database (Db (evalDb), evalDb)
import Parsing (getFeedFromHref, rebuildFeed)
import TgramOutJson (ChatId)
import Utils (defaultChatSettings, findNextTime, freshLastXDays, notifFor, partitionEither, removeByUserIdx, updateSettings)

withChat :: MonadIO m => UserAction -> ChatId -> App m (Either UserError ())
withChat action cid = ask >>= \env -> liftIO $ do
    modifyMVar (subs_state env) (`afterDb` env)
    where
    afterDb hmap env = case HMS.lookup cid hmap of
        Nothing -> case action of
            Sub links ->
                let created_c = SubChat cid Nothing Nothing (S.fromList links) defaultChatSettings
                in  getCurrentTime >>= \now ->
                        let updated_c = created_c { sub_next_notification =
                                Just $ findNextTime now (settings_batch_interval . sub_settings $ created_c)
                            }
                            inserted_m = HMS.insert cid updated_c hmap
                        in  evalDb env (UpsertChat updated_c) >>= \case
                            DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                            _ -> pure (inserted_m, Right ())
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
        Just c -> case action of
            Migrate to ->
                let updated_c = c { sub_chatid = to }
                    update_m = HMS.update(\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to migrate this chat." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ())
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
            SetChatSettings parsed ->
                let updated_settings = updateSettings parsed $ sub_settings c
                    updated_next_notification now = Just . findNextTime now . settings_batch_interval $ updated_settings
                in  getCurrentTime >>= \now ->
                        let updated_c = c {
                                sub_next_notification = updated_next_notification now,
                                sub_settings = updated_settings
                            }
                            updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                        in  evalDb env (UpsertChat updated_c) >>= \case
                            DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                            _ -> pure (updated_cs, Right ())
            Pause pause_or_resume ->
                let updated_sets = (sub_settings c) { settings_paused = pause_or_resume }
                    updated_c = c { sub_settings = updated_sets }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                    DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
                    _ -> pure (updated_cs, Right ())
            _ -> pure (hmap, Right ())

loadChats :: MonadIO m => App m ()
loadChats = ask >>= \env -> liftIO $ modifyMVar_ (subs_state env) $
    \chats_hmap -> do
        now <- getCurrentTime
        evalDb env GetAllChats >>= \case
            DbChats chats -> pure $ update_chats chats now
            _ -> pure chats_hmap
    where
        update_chats chats now = HMS.fromList $ map (\c ->
            let c' = c { sub_next_notification = Just $ findNextTime now (settings_batch_interval . sub_settings $ c) }
            in  (sub_chatid c, c')) chats

evalFeeds :: MonadIO m => FeedsAction -> App m FeedsRes
evalFeeds (InitF start_urls) = do
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
evalFeeds LoadF =
    ask >>= \env ->
    evalDb env Get100Feeds >>= \case
        DbFeeds feeds -> do
            liftIO $ modifyMVar_ (feeds_state env) $ \_ ->
                pure $ HMS.fromList $ map (\f -> (f_link f, f)) feeds
            evalDb env (ArchiveItems feeds) >>= \case
                DbOk -> pure FeedsOk
                _ -> liftIO (putStrLn "Failed to copy feeds!") >> pure FeedsOk
        _ -> pure $ FeedsError FailedToLoadFeeds
evalFeeds (AddF feeds) = do
    env <- ask
    res <- liftIO $ modifyMVar (feeds_state env) $ \app_hmap ->
        let user_hmap = HMS.fromList $ map (\f -> (f_link f, f)) feeds
        in  getCurrentTime >>= \now -> evalDb env (UpsertFeeds feeds) >>= \case
                DbOk -> do
                    writeChan (postjobs env) $ JobArchive feeds now
                    pure (HMS.union user_hmap app_hmap, Just ())
                _ -> pure (app_hmap, Nothing)
    case res of
        Nothing -> pure . FeedsError $ FailedToUpdate (T.intercalate ", " (map f_link feeds)) "could not be added."
        Just _ -> pure FeedsOk
evalFeeds (RemoveF links) = ask >>= \env -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \app_hmap ->
        let deleted = HMS.filter (\f -> f_link f `notElem` links) app_hmap
        in  pure deleted
    pure FeedsOk
evalFeeds (GetAllXDays links days) = do
    env <- ask
    (feeds, now) <- liftIO $ (,) <$> (readMVar . feeds_state $ env) <*> getCurrentTime
    pure . FeedLinkBatch . foldFeeds feeds $ now
    where
        foldFeeds feeds now = HMS.foldl' (\acc f -> if f_link f `notElem` links then acc else collect now f acc) [] feeds
        collect now f acc =
            let fresh = freshLastXDays days now $ f_items f
            in  if null fresh then acc else (f_link f, fresh):acc
evalFeeds (IncReadsF links) = ask >>= \env -> do
    liftIO $ modifyMVar_ (feeds_state env) $ \hmap ->
        evalDb env (IncReads links) >>= \case
            DbOk -> pure $ HMS.map (\f -> f { f_reads = 1 + f_reads f }) hmap
            _ -> pure hmap
    pure FeedsOk
evalFeeds Refresh = ask >>= \env -> liftIO $ do
    chats <- readMVar $ subs_state env
    now <- getCurrentTime
    let (due_chats, flinks) = dueChatsFeeds chats now
    -- stop here if no chat is due
    if null due_chats then pure FeedsOk else do
        -- else rebuilding all feeds with any subscribers
        eitherUpdated <- mapConcurrently rebuildFeed flinks
        let (failed, succeeded) = partitionEither eitherUpdated
        -- handling case of some feeds not rebuilding
        unless (null failed) (writeChan (postjobs env) . JobTgAlert $
            "Failed to update theses feeds: " `T.append` T.intercalate ", " failed)
        -- updating memory on successful db write
        modifyMVar (feeds_state env) $ \old_feeds -> evalDb env (UpsertFeeds succeeded) >>= \case
            DbErr e ->
                let err = FeedsError e
                in  pure (old_feeds, err)
            _ ->
                let fresh_feeds = HMS.fromList $ map (\f -> (f_link f, f)) succeeded
                    to_keep_in_memory = HMS.union fresh_feeds old_feeds
                    -- creating update notification payload
                    notif = notifFor to_keep_in_memory due_chats
                    -- preparing search notification payload
                    scheduled_searches = HMS.foldlWithKey' (\hmap cid chat ->
                        let keywords = match_searchset . settings_word_matches . sub_settings $ chat
                            scope = match_only_search_results . settings_word_matches . sub_settings $ chat
                        in  if S.null keywords then hmap else HMS.insert cid (keywords, scope) hmap) HMS.empty due_chats
                in do
                -- performing db search
                dbres <- forM scheduled_searches $ \(kws, sc) -> evalDb env $ DbSearch kws sc
                -- archiving
                writeChan (postjobs env) $ JobArchive succeeded now
                -- returning to calling thread
                pure (to_keep_in_memory, FeedBatches notif dbres)

dueChatsFeeds :: SubChats -> UTCTime -> (SubChats, [FeedLink])
dueChatsFeeds chats now =
    let (chats', links') = foldl' (\(!hmap, !links) c ->
            let chat_id = sub_chatid c
                settings = sub_settings c
                next_notif = sub_next_notification c
                last_notif = sub_last_notification c
                c_links = sub_feeds_links c
                interval = settings_batch_interval settings
            in  if (not . settings_paused $ settings) && 
                    nextIsNow last_notif next_notif interval
            then (HMS.insert chat_id c hmap, S.union c_links links)
            else (hmap, links)) (HMS.empty, S.empty) chats
    in  (chats', S.toList links')
    where
        nextIsNow Nothing Nothing _ = True
        nextIsNow Nothing (Just next_t) _ = next_t < now
        nextIsNow (Just last_t) _ i = findNextTime last_t i < now

regenFeeds :: MonadIO m => SubChats -> App m (Either T.Text ())
regenFeeds chats = ask >>= \env ->
    let urls = S.toList $ HMS.foldl' (\acc c -> sub_feeds_links c `S.union` acc) S.empty chats
    in  liftIO $ forConcurrently urls rebuildFeed >>= \res -> case sequence res of
        Left err -> pure . Left $ err
        Right feeds -> evalDb env (UpsertFeeds feeds) >>= \case
            DbErr err -> pure . Left . renderDbError $ err
            _ -> pure . Right $ ()