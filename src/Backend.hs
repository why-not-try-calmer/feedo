{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend where
import AppTypes
import Control.Concurrent
import Control.Concurrent.Async (forConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX
import Mongo (HasMongo (evalDb), evalDb)
import Parsing (rebuildFeed)
import TgramOutJson (ChatId)
import Utils (defaultChatSettings, findNextTime, removeByUserIdx, sortItems, updateSettings)

withChat :: MonadIO m => UserAction -> ChatId -> App m (Either UserError ChatRes)
withChat action cid = do
    env <- ask
    res <- liftIO $ modifyMVar (subs_state env) (`afterDb` env)
    case res of
        Left err -> pure $ Left err
        Right ChatOk -> pure $ Right ChatOk
        Right r -> pure . Right $ r
    where
    afterDb hmap env = case HMS.lookup cid hmap of
        Nothing ->
            let initialized now flinks linked_to = SubChat {
                    sub_chatid = cid,
                    sub_last_digest = Nothing,
                    sub_next_digest = Just $ findNextTime now (settings_digest_interval defaultChatSettings),
                    sub_linked_to = linked_to,
                    sub_feeds_links = S.fromList flinks,
                    sub_settings = defaultChatSettings
                }
                inserted c = HMS.insert cid c hmap
                saveToDb c = evalDb env $ UpsertChat c
            in  case action of
                    Link target_id -> getCurrentTime >>= \now ->
                        let new_chat = initialized now [] (Just target_id)
                        in  saveToDb new_chat >>= \case
                            DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat: " `T.append` renderDbError err)
                            _ -> pure (inserted new_chat, Right ChatOk)
                    Sub links -> getCurrentTime >>= \now ->
                        let new_chat = initialized now links Nothing
                        in  saveToDb new_chat >>= \case
                                DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                                _ -> pure (inserted new_chat, Right ChatOk)
                    _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
        Just c -> case action of
            Link target_id ->
                let updated_c = c { sub_linked_to = Just target_id }
                    update_m = HMS.update(\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat to a new chat_id." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ChatOk)
            Migrate to ->
                let updated_c = c { sub_chatid = to }
                    update_m = HMS.update(\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to migrate this chat." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ChatOk)
            Reset ->
                let updated_c = c { sub_settings = defaultChatSettings }
                    update_m = HMS.update (\_ -> Just updated_c) cid hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` renderDbError err)
                        _ -> pure (update_m, Right ChatOk)
            Sub links ->
                let updated_c = c { sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
                    updated_m = HMS.insert cid updated_c hmap
                in  evalDb env (UpsertChat updated_c) >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (updated_m, Right ChatOk)
            UnSub refs -> do
                let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u:us, is); ById i -> (us, i:is)) ([],[]) refs
                    update_db c' = evalDb env (UpsertChat c') >>= \case
                        DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                        _ -> pure (HMS.insert cid c' hmap, Right ChatOk)
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
                _ -> pure (HMS.delete cid hmap, Right ChatOk)
            SetChatSettings s ->
                let updated_settings = case s of
                        Parsed p -> updateSettings p $ sub_settings c
                        Immediate settings -> settings
                    updated_next_notification now =
                        let start = fromMaybe now $ settings_digest_start updated_settings
                        in  Just . findNextTime start . settings_digest_interval $ updated_settings
                in  getCurrentTime >>= \now ->
                        let updated_c = c {
                                sub_next_digest = updated_next_notification now,
                                sub_settings = updated_settings
                            }
                            updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                        in  evalDb env (UpsertChat updated_c) >>= \case
                            DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                            _ -> pure (updated_cs, Right . ChatUpdated $ updated_c)
            Pause pause_or_resume ->
                let updated_sets = (sub_settings c) { settings_paused = pause_or_resume }
                    updated_c = c { sub_settings = updated_sets }
                    updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
                in  do
                    res <- evalDb env (UpsertChat updated_c)
                    case res of
                        DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
                        _ -> pure (updated_cs, Right ChatOk)
            _ -> pure (hmap, Right ChatOk)

loadChats :: (MonadReader AppConfig m, MonadIO m) => m ()
loadChats = ask >>= \env -> liftIO $ modifyMVar_ (subs_state env) $
    \chats_hmap -> do
        now <- getCurrentTime
        evalDb env GetAllChats >>= \case
            DbChats chats -> pure $ update_chats chats now
            _ -> pure chats_hmap
    where
        update_chats chats now = HMS.fromList $ map (\c ->
            let interval = settings_digest_interval . sub_settings $ c
                t = case sub_last_digest c of
                    Nothing -> findNextTime now interval
                    Just last_t -> findNextTime last_t interval
                c' = c { sub_next_digest = Just t }
            in  (sub_chatid c, c')) chats

collectDue ::
    SubChats ->
    Maybe UTCTime ->
    UTCTime ->
    HMS.HashMap ChatId (SubChat, BatchRecipe)
-- Peeling off conditions for notifications:
-- new start or new digest or new follow
collectDue chats last_run now =
    foldl' (\hmap c@SubChat{..} ->
        let interval = settings_digest_interval sub_settings
        in  if settings_paused sub_settings
            then hmap
            else case settings_digest_start sub_settings of
                Nothing ->
                    if nextWasNow sub_next_digest sub_last_digest interval
                    -- 'digests' take priority over 'follow notifications'
                    then HMS.insert sub_chatid (c, DigestFeedLinks $ S.toList sub_feeds_links) hmap
                    else
                        if settings_follow sub_settings then case last_run of
                            Nothing -> HMS.insert sub_chatid (c, FollowFeedLinks $ S.toList sub_feeds_links) hmap
                            Just t ->
                                if addUTCTime 1200 t < now
                                then HMS.insert sub_chatid (c, FollowFeedLinks $ S.toList sub_feeds_links) hmap
                                else hmap
                        else hmap
                Just new ->
                    if new < now
                    then HMS.insert sub_chatid (c, DigestFeedLinks $ S.toList sub_feeds_links) hmap
                    else hmap
            ) HMS.empty chats
    where
        nextWasNow Nothing Nothing _ = True
        nextWasNow (Just next_t) _ _ = next_t < now
        nextWasNow Nothing (Just last_t) i = findNextTime last_t i < now

markNotified :: (MonadIO m, HasMongo IO) => AppConfig -> [ChatId] -> UTCTime -> m ()
-- marking input chats as notified 
markNotified env notified_chats now = liftIO $ modifyMVar_ (subs_state env) $ \subs ->
    let updated_chats = updated_notified_chats notified_chats subs
    in  evalDb env (UpsertChats updated_chats) >>= \case
        DbErr err -> do
            writeChan (postjobs env) $ JobTgAlert $ "notifier: failed to \
                \ save updated chats to db because of this error" `T.append` renderDbError err
            pure subs
        DbOk -> pure updated_chats
        _ -> pure subs
    where
        updated_notified_chats notified = HMS.mapWithKey (\cid c ->
            if cid `elem` notified
            then
                let next = Just . findNextTime now . settings_digest_interval . sub_settings $ c
                in  -- updating last, next, and consuming 'settings_digest_start'
                    case settings_digest_start . sub_settings $ c of
                    Nothing -> c { sub_last_digest = Just now, sub_next_digest = next }
                    Just _ -> c {
                        sub_last_digest = Just now,
                        sub_next_digest = next,
                        sub_settings = (sub_settings c) { settings_digest_start = Nothing }
                        }
            else c)
regenFeeds :: (MonadIO m, MonadReader AppConfig m) => m ()
regenFeeds = do
    env <- ask
    chats <- liftIO . readMVar $ subs_state env
    let urls = S.toList $ HMS.foldl' (\acc c -> sub_feeds_links c `S.union` acc) S.empty chats
        report err = writeChan (postjobs env) . JobTgAlert $ "Failed to regen feeds for this reason: " `T.append` err
    liftIO $ forConcurrently urls rebuildFeed >>= \res -> case sequence res of
        Left err -> report err
        Right feeds -> evalDb env (UpsertFeeds (map sortItems feeds)) >>= \case
            DbErr err -> report $ renderDbError err
            _ -> pure ()