{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Mem where

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Mongo (HasMongo (evalDb), evalDb)
import Notifications
import Parsing (rebuildFeed)
import Replies (render)
import System.Timeout (timeout)
import TgramOutJson (ChatId, UserId)
import Types
import Utils (defaultChatSettings, feedsFromList, partitionEither, removeByUserIdx, sortItems, updateSettings)

withChatsFromMem :: (MonadIO m) => UserAction -> Maybe UserId -> ChatId -> App m (Either TgEvalError ChatRes)
withChatsFromMem action maybe_userid cid = do
  env <- ask
  res <- liftIO $ modifyMVarMasked (subs_state env) (`withHmap` env)
  case res of
    Left err -> pure $ Left err
    Right ChatOk -> pure $ Right ChatOk
    Right r -> pure . Right $ r
 where
  withHmap hmap env = case HMS.lookup cid hmap of
    Nothing ->
      let initialized now flinks linked_to =
            SubChat
              { sub_chatid = cid
              , sub_last_digest = Nothing
              , sub_next_digest = Just $ findNextTime now (settings_digest_interval defaultChatSettings)
              , sub_linked_to = linked_to
              , sub_feeds_links = S.fromList flinks
              , sub_settings = defaultChatSettings
              , sub_active_admins = HMS.empty
              }
          inserted c = HMS.insert cid c hmap
          saveToDb c = evalDb $ UpsertChat c
       in case action of
            Link target_id ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now [] (Just target_id)
                 in runApp env $
                      saveToDb new_chat
                        >>= \case
                          Left err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat: " `T.append` render err)
                          _ -> pure (inserted new_chat, Right ChatOk)
            Sub links ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now links Nothing
                 in runApp env $
                      saveToDb new_chat
                        >>= \case
                          Left err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                          _ -> pure (inserted new_chat, Right ChatOk)
            _ -> pure (hmap, Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
    Just c -> case action of
      Link target_id ->
        let updated_c = c{sub_linked_to = Just target_id}
            update_m = HMS.update (\_ -> Just updated_c) cid hmap
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (hmap, Left . UpdateError $ "Db refused to link this chat to a new chat_id." `T.append` render err)
                  _ -> pure (update_m, Right ChatOk)
      Migrate to ->
        let updated_c = c{sub_chatid = to}
            update_m = HMS.insert cid updated_c hmap
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (hmap, Left . UpdateError $ "Db refused to migrate this chat." `T.append` render err)
                  _ -> pure (update_m, Right ChatOk)
      Reset ->
        let updated_c = c{sub_settings = defaultChatSettings}
            update_m = HMS.update (\_ -> Just updated_c) cid hmap
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (hmap, Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` render err)
                  _ -> pure (update_m, Right ChatOk)
      Sub links ->
        let updated_c = c{sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
            updated_m = HMS.insert cid updated_c hmap
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                  _ -> pure (updated_m, Right ChatOk)
      UnSub refs -> do
        let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u : us, is); ById i -> (us, i : is)) ([], []) refs
            update_db c' =
              runApp env $
                evalDb (UpsertChat c')
                  >>= \case
                    Left err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                    _ -> pure (HMS.insert cid c' hmap, Right ChatOk)
        if not (null byurls) && not (null byids)
          then pure (hmap, Left . BadRef $ "You cannot mix references by urls and by ids in the same command.")
          else
            if null byurls
              then case removeByUserIdx (S.toList . sub_feeds_links $ c) byids of
                Nothing -> pure (hmap, Left . BadRef $ "Invalid references. Make sure to use /list to get a list of valid references.")
                Just removed ->
                  let updated_c = c{sub_feeds_links = S.fromList removed}
                   in update_db updated_c
              else
                let updated_c = c{sub_feeds_links = S.filter (`notElem` byurls) $ sub_feeds_links c}
                 in update_db updated_c
      Purge ->
        runApp env $
          evalDb (DeleteChat cid)
            >>= \case
              Left err -> pure (hmap, Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
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
                      , sub_active_admins =
                          let prev = sub_active_admins c
                           in maybe prev (\uid -> HMS.insert uid now prev) maybe_userid
                      }
                  updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
               in runApp env $
                    evalDb (UpsertChat updated_c)
                      >>= \case
                        Left _ -> pure (hmap, Left . UpdateError $ "Db refuse to update settings.")
                        _ -> pure (updated_cs, Right . ChatUpdated $ updated_c)
      Pause pause_or_resume ->
        let updated_sets = (sub_settings c){settings_paused = pause_or_resume}
            updated_c = c{sub_settings = updated_sets}
            updated_cs = HMS.update (\_ -> Just updated_c) cid hmap
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (hmap, Left . UpdateError . render $ err)
                  _ -> pure (updated_cs, Right ChatOk)
      _ -> pure (hmap, Right ChatOk)

loadChatsIntoMem :: (MonadIO m) => App m ()
loadChatsIntoMem = do
  env <- ask
  liftIO $
    modifyMVarMasked_ (subs_state env) $
      \chats_hmap -> do
        now <- getCurrentTime
        putStrLn "Trying to loading chats now"
        runApp env $
          evalDb GetAllChats
            >>= \case
              Right (DbChats chats) -> pure $ update_chats chats now
              Left err -> do
                liftIO $ putStrLn $ T.unpack $ render err
                pure chats_hmap
              _ -> pure chats_hmap
 where
  update_chats chats now =
    HMS.fromList $
      map
        ( \c ->
            let interval = settings_digest_interval . sub_settings $ c
                t = case sub_last_digest c of
                  Nothing -> findNextTime now interval
                  Just last_t -> findNextTime last_t interval
                c' = c{sub_next_digest = Just t}
             in (sub_chatid c, c')
        )
        chats

rebuildAllFeedsFromMem :: (MonadIO m) => App m [Feed]
rebuildAllFeedsFromMem = do
  env <- ask
  chats <- liftIO . readMVar $ subs_state env
  let urls = S.toList $ HMS.foldl' (\acc c -> sub_feeds_links c `S.union` acc) S.empty chats
      report err = alertAdmin (postjobs env) $ "Failed to regen feeds for this reason: " `T.append` err
      action = partitionEither <$> mapConcurrently rebuildFeed urls
  liftIO $ do
    res <- withAsync action $ \th -> timeout 5000000 $ wait th
    case res of
      Nothing -> pure []
      Just (failed, feeds) -> do
        unless (null failed) (report $ "rebuildAllFeedsFromMem: Unable to rebuild these feeds: " `T.append` T.intercalate ", " (map r_url failed))
        pure . map sortItems $ feeds

rebuildSomeFeedsFromMem ::
  (MonadReader AppConfig m, MonadIO m) =>
  [FeedLink] ->
  m (Either T.Text (HMS.HashMap T.Text Feed))
rebuildSomeFeedsFromMem flinks =
  ask >>= \env -> liftIO $ do
    (failed, succeeded) <- fetch_feeds
    -- if any failed, 'punish' offenders
    -- that is, increment blacklist and remove urls guilty of more 3 failures
    unless (null failed) (logAndAlert env failed)
    -- bubble up failure if no feed could be rebuilt, else return the feeds
    if null succeeded then pure $ Left "Failed to fetch feeds! Could not rebuild." else pure (Right . feedsFromList $ succeeded)
 where
  fetch_feeds = partitionEither <$> mapConcurrently rebuildFeed flinks
  logAndAlert env failed = do
    writeChan (postjobs env) (JobLog $ map LogFailed failed)
    writeChan (postjobs env) (JobTgAlertAdmin $ "Failed to fetch " `T.append` T.pack (show $ length failed) `T.append` " feeds")

makeDigestsFromMem :: (MonadIO m) => App m (Either T.Text FromCache)
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
      rebuildSomeFeedsFromMem (feeds_to_refresh pre) >>= \case
        Left err -> pure $ Left err
        Right rebuilt -> do
          -- sometimes a digest would contain items with the same timestamps, but
          -- we can filter them out through a simple comparison
          last_batch <- get_last_batch rebuilt
          -- caching
          res <- evalDb $ UpsertFeeds $ HMS.elems rebuilt
          -- creating update notification payload, with 'last_run' used only for 'follow notifications'
          let post = postNotifier rebuilt last_batch pre
          liftIO $ do
            -- ensuring caching worked
            case res of
              Left e -> alertAdmin (postjobs env) $ render e
              _ -> pure ()
            -- saving to mongo
            runApp env $
              evalDb (ArchiveItems $ HMS.elems rebuilt)
                >>= \case
                  Left err -> alertAdmin (postjobs env) $ "Unable to archive items for this reason: " `T.append` render err
                  _ -> pure ()
            -- logging possibly too aggressive union
            unless (null $ discarded_items_links post)
              $ writeChan (postjobs env)
                . JobLog
              $ pure
              $ LogMissing (discarded_items_links post) (length $ discarded_items_links post) now
            -- Rust??
            where_is_rust env pre post now
          pure . Right $ CacheDigests $ batches post
 where
  get_last_batch rebuilt =
    evalDb (GetSomeFeeds $ feedlinksWithMissingPubdates rebuilt) >>= \case
      Right (DbFeeds fs) -> pure $ map i_link $ foldMap f_items fs
      _ -> pure mempty
  where_is_rust env Pre{..} Post{..} now =
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
            $ pure
            $ LogDiscardedToRefreshRecipes
              to_refresh
              discarded
              recipes
              now
     in unless (all null [to_refresh, discarded, recipes]) report
  where_is_rust _ _ _ _ = undefined
