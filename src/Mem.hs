{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Mem where

import Control.Concurrent
import Control.Concurrent.Async (forConcurrently, mapConcurrently)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import Mongo (HasMongo (evalDb), evalDb)
import Notifications
import Parsing (rebuildFeed)
import Redis (HasRedis)
import TgramOutJson (ChatId)
import Types
import Utils (defaultChatSettings, feedsFromList, partitionEither, removeByUserIdx, renderDbError, sortItems, updateSettings)

withChatsFromMem :: (MonadReader AppConfig m, MonadIO m) => UserAction -> ChatId -> m (Either Error ChatRes)
withChatsFromMem action cid = do
  env <- ask
  res <- liftIO $ modifyMVar (subs_state env) (`afterDb` env)
  case res of
    Left err -> pure $ Left err
    Right ChatOk -> pure $ Right ChatOk
    Right r -> pure . Right $ r
 where
  afterDb hmap env = case HMS.lookup cid hmap of
    Nothing ->
      let initialized now flinks linked_to =
            SubChat
              { sub_chatid = cid
              , sub_last_digest = Nothing
              , sub_next_digest = Just $ findNextTime now (settings_digest_interval defaultChatSettings)
              , sub_linked_to = linked_to
              , sub_feeds_links = S.fromList flinks
              , sub_settings = defaultChatSettings
              }
          inserted c = HMS.insert cid c hmap
          saveToDb c = evalDb env $ UpsertChat c
       in case action of
            Link target_id ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now [] (Just target_id)
                 in saveToDb new_chat >>= \case
                      DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat: " `T.append` renderDbError err)
                      _ -> pure (inserted new_chat, Right ChatOk)
            Sub links ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now links Nothing
                 in saveToDb new_chat >>= \case
                      DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                      _ -> pure (inserted new_chat, Right ChatOk)
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
    Just c -> case action of
      Link target_id ->
        let updated_c = c{sub_linked_to = Just target_id}
            update_m = HMS.update (\_ -> Just updated_c) cid hmap
         in evalDb env (UpsertChat updated_c) >>= \case
              DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat to a new chat_id." `T.append` renderDbError err)
              _ -> pure (update_m, Right ChatOk)
      Migrate to ->
        let updated_c = c{sub_chatid = to}
            update_m = HMS.insert cid updated_c hmap
         in evalDb env (UpsertChat updated_c) >>= \case
              DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to migrate this chat." `T.append` renderDbError err)
              _ -> pure (update_m, Right ChatOk)
      Reset ->
        let updated_c = c{sub_settings = defaultChatSettings}
            update_m = HMS.update (\_ -> Just updated_c) cid hmap
         in evalDb env (UpsertChat updated_c) >>= \case
              DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` renderDbError err)
              _ -> pure (update_m, Right ChatOk)
      Sub links ->
        let updated_c = c{sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
            updated_m = HMS.insert cid updated_c hmap
         in evalDb env (UpsertChat updated_c) >>= \case
              DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
              _ -> pure (updated_m, Right ChatOk)
      UnSub refs -> do
        let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u : us, is); ById i -> (us, i : is)) ([], []) refs
            update_db c' =
              evalDb env (UpsertChat c') >>= \case
                DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
                _ -> pure (HMS.insert cid c' hmap, Right ChatOk)
        if not (null byurls) && not (null byids)
          then pure (hmap, Left . BadInput $ "You cannot mix references by urls and by ids in the same command.")
          else
            if null byurls
              then case removeByUserIdx (S.toList . sub_feeds_links $ c) byids of
                Nothing -> pure (hmap, Left . BadInput $ "Invalid references. Make sure to use /list to get a list of valid references.")
                Just removed ->
                  let updated_c = c{sub_feeds_links = S.fromList removed}
                   in update_db updated_c
              else
                let updated_c = c{sub_feeds_links = S.filter (`notElem` byurls) $ sub_feeds_links c}
                 in update_db updated_c
      Purge ->
        evalDb env (DeleteChat cid) >>= \case
          DbErr err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` renderDbError err)
          _ -> pure (HMS.delete cid hmap, Right ChatOk)
      SetChatSettings s ->
        let updated_settings = case s of
              Parsed p -> updateSettings p $ sub_settings c
              Immediate settings -> settings
            updated_next_notification now =
              let start = fromMaybe now $ settings_digest_start updated_settings
               in Just . findNextTime start . settings_digest_interval $ updated_settings
         in getCurrentTime >>= \now ->
              let updated_c =
                    c
                      { sub_next_digest = updated_next_notification now
                      , sub_settings = updated_settings
                      }
                  updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
               in evalDb env (UpsertChat updated_c) >>= \case
                    DbErr _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                    _ -> pure (updated_cs, Right . ChatUpdated $ updated_c)
      Pause pause_or_resume ->
        let updated_sets = (sub_settings c){settings_paused = pause_or_resume}
            updated_c = c{sub_settings = updated_sets}
            updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
         in evalDb env (UpsertChat updated_c) >>= \case
              DbErr err -> pure (hmap, Left . UpdateError . renderDbError $ err)
              _ -> pure (updated_cs, Right ChatOk)
      _ -> pure (hmap, Right ChatOk)

loadChatsIntoMem :: (MonadReader AppConfig m, MonadIO m) => m ()
loadChatsIntoMem =
  ask >>= \env -> liftIO
    $ modifyMVar_ (subs_state env)
    $ \chats_hmap -> do
      now <- getCurrentTime
      putStrLn "Trying to loading chats now"
      evalDb env GetAllChats >>= \case
        DbChats chats -> pure $ update_chats chats now
        DbErr err -> do
          print $ renderDbError err
          pure chats_hmap
        _ -> pure chats_hmap
 where
  update_chats chats now =
    HMS.fromList
      $ map
        ( \c ->
            let interval = settings_digest_interval . sub_settings $ c
                t = case sub_last_digest c of
                  Nothing -> findNextTime now interval
                  Just last_t -> findNextTime last_t interval
                c' = c{sub_next_digest = Just t}
             in (sub_chatid c, c')
        )
        chats

getFeedsFromMem :: (MonadIO m, MonadReader AppConfig m) => m [Feed]
getFeedsFromMem = do
  env <- ask
  chats <- liftIO . readMVar $ subs_state env
  let urls = S.toList $ HMS.foldl' (\acc c -> sub_feeds_links c `S.union` acc) S.empty chats
      report err = writeChan (postjobs env) . JobTgAlertAdmin $ "Failed to regen feeds for this reason: " `T.append` err
  liftIO $ do
    (failed, feeds) <- partitionEither <$> forConcurrently urls rebuildFeed
    unless (null failed) (report $ "getFeedsFromMem: Unable to rebuild these feeds" `T.append` T.intercalate ", " (map r_url failed))
    pure . map sortItems $ feeds

rebuildFeedsFromMem ::
  (MonadReader AppConfig m, HasRedis m, HasMongo m, MonadIO m) =>
  [FeedLink] ->
  UTCTime ->
  m (Either T.Text (HMS.HashMap T.Text Feed))
rebuildFeedsFromMem flinks now =
  ask >>= \env -> liftIO $ do
    (failed, succeeded) <- fetch_feeds
    -- if any failed, 'punish' offenders
    -- that is, increment blacklist and remove urls guilty of more 3 failures
    unless (null failed) (act_on_failed env failed succeeded)
    -- bubble up failure if no feed could be rebuilt, else return the feeds
    if null succeeded then pure $ Left "Failed to fetch feeds" else pure (Right . feedsFromList $ succeeded)
 where
  fetch_feeds = partitionEither <$> mapConcurrently rebuildFeed flinks
  act_on_failed env failed succeeded = punish env failed >> writeChan (postjobs env) (JobArchive succeeded now)
  punish env failed = do
    -- get all failing URLs
    punishable <- update_blacklist env failed
    -- get all ids of chats subscribed to a failing URL
    punished <- update_subchats env punishable
    -- alert admin
    let report = "Failed to update these feeds: " `T.append` T.intercalate ", " punishable
    writeChan (postjobs env) $ JobTgAlertAdmin (report `T.append` ". Blacklist was updated accordingly.")
    -- alert chats
    let msg = "This chat has been found to use a faulty RSS endpoint. It will be removed from your subscriptions."
    writeChan (postjobs env) (JobTgAlertChats punished msg)
  update_blacklist env failed = modifyMVar (blacklist env) $ \hmap -> pure (updateBlackList hmap failed, get_faulty_urls hmap)
   where
    updateBlackList = foldl' edit_blacklist
    edit_blacklist hmap (FeedError u st_code er_msg _) =
      HMS.alter
        ( \case
            Nothing -> Just (BlackListedUrl now er_msg st_code 1)
            Just bl -> Just $ bl{last_attempt = now, offenses = offenses bl + 1}
        )
        u
        hmap
  get_faulty_urls = map fst . HMS.toList . HMS.filter (\bl -> offenses bl == 3)
  update_subchats env urls = modifyMVar (subs_state env) $ \hmap -> pure (updateSubChats hmap urls, get_offending_chats hmap)
   where
    get_offending_chats = foldl' (\acc subchat -> if any (`S.member` sub_feeds_links subchat) urls then sub_chatid subchat : acc else acc) []
    updateSubChats = foldl' edit_subchats
    edit_subchats hmap feedlink = HMS.map (\subchat -> let filtered = S.delete feedlink (sub_feeds_links subchat) in subchat{sub_feeds_links = filtered}) hmap

makeDigestsFromMem :: (MonadIO m, MonadReader AppConfig m, HasRedis m, HasMongo m) => m (Either T.Text FromCache)
makeDigestsFromMem = do
  env <- ask
  (chats, now, last_run) <- liftIO $ do
    chats <- readMVar $ subs_state env
    now <- getCurrentTime
    last_run <- readIORef $ last_worker_run env
    pure (chats, now, last_run)
  -- checking due chats
  let pre = preNotifier now last_run chats
  -- handling due chats
  if null $ feeds_to_refresh pre
    then pure $ Right CacheOk
    else
      rebuildFeedsFromMem (feeds_to_refresh pre) now >>= \case
        Left err -> pure $ Left err
        Right rebuilt -> do
          -- sometimes a digest would contain items with the same timestamps, but
          -- we can filter them out through a simple comparison
          last_batch <- get_last_batch rebuilt env
          -- caching
          res <- evalDb env $ UpsertFeeds $ HMS.elems rebuilt
          -- creating update notification payload, with 'last_run' used only for 'follow notifications'
          let post = postNotifier rebuilt last_batch pre
          liftIO $ do
            -- ensuring caching worked
            case res of
              DbErr e -> writeChan (postjobs env) . JobTgAlertAdmin $ renderDbError e
              _ -> pure ()
            -- saving to mongo
            evalDb env (ArchiveItems $ HMS.elems rebuilt) >>= \case
              DbErr err -> writeChan (postjobs env) (JobTgAlertAdmin $ "Unable to archive items for this reason: " `T.append` renderDbError err)
              DbOk -> pure ()
              _ -> pure ()
            -- logging possibly too aggressive union
            unless (null $ discarded_items_links post)
              $ writeChan (postjobs env)
              . JobLog
              $ LogMissing (discarded_items_links post) (length $ discarded_items_links post) now
            -- log notifiers
            writeChan (postjobs env)
              $ JobLog
              $ LogNotifiers (HMS.map fst . batch_recipes $ pre) (HMS.map fst . batches $ post)
            -- Rust??
            where_is_rust env pre post
          pure . Right $ CacheDigests $ batches post
 where
  get_last_batch rebuilt env =
    evalDb env (GetSomeFeeds $ feedlinksWithMissingPubdates rebuilt) >>= \case
      DbFeeds fs -> pure $ map i_link $ foldMap f_items fs
      _ -> pure mempty
  where_is_rust env Pre{..} Post{..} =
    let rust_in = filter (T.isInfixOf "rust")
        to_refresh = rust_in feeds_to_refresh
        discarded = rust_in discarded_items_links
        recipes =
          foldl'
            ( \acc (_, v) -> case v of
                FollowFeedLinks fs -> acc ++ rust_in fs
                DigestFeedLinks ds -> acc ++ rust_in ds
            )
            []
            batch_recipes
        report =
          writeChan (postjobs env)
            . JobLog
            $ LogDiscardedToRefreshRecipes
              to_refresh
              discarded
              recipes
     in unless (all null [to_refresh, discarded, recipes]) report
  where_is_rust _ _ _ = undefined
