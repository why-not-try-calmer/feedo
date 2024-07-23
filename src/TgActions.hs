{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TgActions (isUserAdmin, isChatOfType, interpretCmd, processCbq, evalTgAct, subFeed) where

import Chats (getChats, withChat)
import Control.Concurrent (writeChan)
import Control.Concurrent.Async (concurrently, mapConcurrently, mapConcurrently_)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Data.Functor
import qualified Data.HashMap.Strict as HMS
import Data.List (find, foldl', sort)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Feeds (eitherUrlScheme, getFeedFromUrlScheme, rebuildFeed)
import Mongo (evalDb)
import Network.HTTP.Req (JsonResponse, renderUrl, responseBody)
import Redis
import Replies (Replies (FromAbout), mkReply, render)
import Requests (TgReqM (runSend), alertAdmin, answer, mkKeyboard, reply, runSend)
import Settings
import Text.Read (readMaybe)
import TgramInJson
import TgramOutJson
import Types (
  App,
  AppConfig (app_version, base_url, postjobs, tg_config),
  BotToken,
  CacheAction (..),
  ChatRes (ChatUpdated),
  DbAction (..),
  DbError (BadQuery),
  DbResults (..),
  Feed (f_items, f_link),
  FeedError (r_url),
  FeedLink,
  FeedRef (ById, ByUrl),
  FromCache (CachePage),
  InterpreterErr (..),
  Item (i_pubdate),
  Job (JobTgAlertAdmin),
  Replies (
    FromAdmin,
    FromAnnounce,
    FromChangelog,
    FromChat,
    FromChatFeeds,
    FromCmds,
    FromDigest,
    FromFeedDetails,
    FromFeedItems,
    FromFeedLinkItems,
    FromSearchRes,
    FromStart
  ),
  Reply (EditReply, ServiceReply),
  ServerConfig (alert_chat, bot_token),
  SettingsUpdater (Parsed),
  SubChat (sub_feeds_links, sub_linked_to, sub_settings),
  TgEvalError (
    BadRef,
    ChatNotPrivate,
    DbQueryError,
    MaxFeedsAlready,
    NotAdmin,
    NotFoundChat,
    NotFoundFeed,
    NotSubscribed,
    TelegramErr,
    UserNotAdmin
  ),
  UserAction (..),
  runApp,
 )
import Utils (maybeUserIdx, partitionEither, toFeedRef, tooManySubs, unFeedRefs)

isUserAdmin :: (TgReqM m) => BotToken -> UserId -> ChatId -> m (Either TgEvalError Bool)
isUserAdmin tok uid cid =
  if uid == cid
    then pure . Right $ True
    else
      getChatAdmins >>= \case
        Left err -> pure . Left . TelegramErr $ err
        Right res_chat_type ->
          let res_cms = responseBody res_chat_type :: TgGetChatMembersResponse
              chat_members = resp_cm_result res_cms :: [ChatMember]
           in pure . Right $ if_admin chat_members
 where
  getChatAdmins = runSend tok TgGetChatAdministrators $ GetChatMessage cid
  if_admin = foldr is_admin False
  is_admin member acc
    | uid /= uid' = acc
    | status `elem` [Administrator, Creator] = True
    | otherwise = False
   where
    (uid', status) = getUserStatus member

isChatOfType :: (MonadIO m) => BotToken -> ChatId -> ChatType -> m (Either TgEvalError Bool)
isChatOfType tok cid ty =
  liftIO $
    getChatType
      >>= \case
        Left err -> pure . Left . TelegramErr $ err
        Right res_chat ->
          let chat_resp = responseBody res_chat :: TgGetChatResponse
              chat_full_info = resp_result chat_resp :: ChatFullInfo
           in pure . Right $ cfi_type chat_full_info == ty
 where
  getChatType = runSend tok TgGetChat $ GetChatMessage cid

exitNotAuth :: (Applicative f, Show a) => a -> f (Either TgEvalError b)
exitNotAuth = pure . Left . NotAdmin . T.pack . show

interpretCmd :: T.Text -> Either InterpreterErr UserAction
interpretCmd contents
  | cmd == "/about" = Right About
  | cmd == "/admin" =
      if length args /= 1
        then Left $ InterpreterErr "/admin takes exactly one argument: the chat_id of the chat or channel to be administrate."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
          Nothing -> Left . InterpreterErr $ "The value passed to /admin could not be parsed into a valid chat_id."
          Just chat_or_channel_id -> Right $ AskForLogin chat_or_channel_id
  | cmd == "/announce" =
      if null args
        then Left . InterpreterErr $ "/announce takes exactly 1 argument, the text to broadcast to all users."
        else Right . Announce . T.concat $ args
  | cmd == "/changelog" = Right Changelog
  | cmd == "/feed" || cmd == "/f" =
      if length args == 1
        then case toFeedRef args of
          Left err -> Left err
          Right single_ref -> Right . FeedInfo . head $ single_ref
        else
          if length args == 2
            then case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /feed (with 2 arguments) could not be parsed into a valid chat_id."
              Just channel_id -> case toFeedRef . tail $ args of
                Left err -> Left err
                Right single_ref -> Right . AboutChannel channel_id . head $ single_ref
            else Left . InterpreterErr $ "/feed must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
  | cmd == "/fresh" =
      if length args /= 1
        then Left . InterpreterErr $ "/fresh takes exactly 1 argument, standing for number of days."
        else case readMaybe . T.unpack . head $ args :: Maybe Int of
          Nothing -> Left . InterpreterErr $ "/fresh's argument must be a valid integer."
          Just i -> Right $ GetLastXDaysItems i
  | cmd == "/help" = Right RenderCmds
  | cmd == "/items" || cmd == "/i" =
      if length args == 1
        then case toFeedRef args of
          Left err -> Left err
          Right single_ref -> Right . GetItems . head $ single_ref
        else
          if length args == 2
            then case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /items (with 2 arguments) could not be parsed into a valid chat_id."
              Just channel_id -> case toFeedRef . tail $ args of
                Left err -> Left err
                Right single_ref -> Right . GetChannelItems channel_id . head $ single_ref
            else Left . InterpreterErr $ "/items must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
  | cmd == "/link" =
      if length args /= 1
        then Left . InterpreterErr $ "/link takes exactly 1 argument, standing for a chat_id."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
          Nothing -> Left . InterpreterErr $ "The first value passed to /purge could not be parsed into a valid chat_id."
          Just n -> Right . Link $ n
  | cmd == "/list" =
      if null args
        then Right ListSubs
        else
          if length args > 1
            then Left . InterpreterErr $ "/list cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /list could not be parsed into a valid chat_id."
              Just n -> Right . ListSubsChannel $ n
  | cmd == "/migrate" =
      if null args
        then
          Left
            . InterpreterErr
            $ "/migrate takes at least one argument, standing for the chat_id of the destination you want to migrate this chat's settings to. \
              \ If you call /migrate with 2 arguments, the first should be the chat_id of the origin and the second the chat_id of the destination."
        else
          let (x : xs) = args
           in case readMaybe . T.unpack $ x :: Maybe ChatId of
                Nothing -> Left . InterpreterErr $ "The first value passed to /migrate could not be parsed into a valid chat_id."
                Just n ->
                  if null xs
                    then Right $ Migrate n
                    else case readMaybe . T.unpack . head $ xs :: Maybe ChatId of
                      Nothing -> Left . InterpreterErr $ "The second value passed to /migrate could not be parsed into a valid chat_id."
                      Just m -> Right $ MigrateChannel n m
  | cmd == "/pause" || cmd == "/p" =
      if null args
        then Right . Pause $ True
        else
          if length args > 1
            then Left . InterpreterErr $ "/pause cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /pause could not be parsed into a valid chat_id."
              Just n -> Right . PauseChannel n $ True
  | cmd == "/purge" =
      if null args
        then Right Purge
        else
          if length args > 1
            then Left . InterpreterErr $ "/purge cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /purge could not be parsed into a valid chat_id."
              Just n -> Right . PurgeChannel $ n
  | cmd == "/resume" =
      if null args
        then Right . Pause $ False
        else
          if length args > 1
            then Left . InterpreterErr $ "/resume cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /resume could not be parsed into a valid chat_id."
              Just n -> Right . PauseChannel n $ False
  | cmd == "/reset" =
      if null args
        then Right Reset
        else
          if length args > 1
            then Left . InterpreterErr $ "/reset cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /reset could not be parsed into a valid chat_id."
              Just n -> Right . ResetChannel $ n
  | cmd == "/search" || cmd == "/se" =
      if null args
        then Left . InterpreterErr $ "/search requires at least one keyword. Separate keywords with a space."
        else Right . Search $ args
  | cmd == "/set" =
      let body = tail . T.lines $ contents
       in if null args
            then Right GetSubchatSettings
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> case parseSettings body of
                Left err -> Left . InterpreterErr $ err
                Right settings -> Right . SetChatSettings . Parsed $ settings
              Just n ->
                if null $ tail args
                  then Right $ GetSubchannelSettings n
                  else case parseSettings body of
                    Left err -> Left . InterpreterErr $ err
                    Right settings -> Right $ SetChannelSettings n settings
  | cmd == "/start" = Right Start
  | cmd == "/sub" || cmd == "/s" =
      if null args
        then Left . InterpreterErr $ "/sub <optional: channel id> <mandatory: list of url feeds>"
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
          Nothing -> Right . Sub $ args
          Just n ->
            if null $ tail args
              then Left . InterpreterErr $ "Cannot subscribe to an empty list urls..."
              else Right . SubChannel n $ tail args
  | cmd == "/testdigest" =
      if null args
        then Right TestDigest
        else
          if length args > 1
            then Left . InterpreterErr $ "/testdigest cannot take more than one (optional) argument (channel_id)."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
              Nothing -> Left . InterpreterErr $ "The first value passed to /list could not be parsed into a valid chat_id."
              Just n -> Right . TestDigestChannel $ n
  | cmd == "/unsub" =
      if null args
        then Left . InterpreterErr $ "/unsub <optional: channel id> <mandatory: list of #s or url feeds>"
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
          Nothing -> case toFeedRef args of
            Left err -> Left err
            Right refs -> Right . UnSub $ refs
          Just n ->
            if abs n < 10000
              then case toFeedRef args of
                Left err -> Left err
                Right refs -> Right . UnSub $ refs
              else case toFeedRef $ tail args of
                Left err -> Left err
                Right refs -> Right . UnSubChannel n $ refs
  | otherwise = Left $ UnknownCommand cmd args
 where
  (cmd, args) =
    let (h' : t) = T.words contents
        (h : _) = T.splitOn "@" h'
     in (h, t)

subFeed :: (MonadIO m) => ChatId -> [FeedLink] -> App m Reply
subFeed cid = startWithValidateUrls
 where
  startWithValidateUrls urls = case mapM eitherUrlScheme urls of
    Left err -> prepareResponseWith . render $ err
    Right valid_urls -> getFeeds valid_urls
  getFeeds valid_urls =
    evalDb GetAllFeeds >>= \case
      Left err -> prepareResponseWith $ "Unable to acquire feeds. Reason: " `T.append` render err
      Right (DbFeeds old_feeds) -> do
        let new_valid_urls = getLinks old_feeds
            old_schemes = getScheme old_feeds
        unless (null new_valid_urls) (void $ withChat (Sub $ getLinks old_feeds) Nothing cid)
        fetchMore old_schemes new_valid_urls
      _ -> prepareResponseWith "Unknown error when trying to subscribe."
   where
    urls = map renderUrl valid_urls
    getLinks old_feeds = map f_link $ filter (\f -> f_link f `elem` urls) old_feeds
    getScheme old_feeds = filter (\u -> renderUrl u `notElem` getLinks old_feeds) valid_urls
  fetchMore [] new_valid_urls = prepareResponseWith $ "Successfully subscribed to " `T.append` T.intercalate ", " new_valid_urls
  fetchMore new_url_schemes old_links = do
    (failed, built_feeds) <- liftIO $ partitionEither <$> mapConcurrently getFeedFromUrlScheme new_url_schemes
    let (feeds, warnings) = foldl' (\(fs, ws) (f, w) -> (f : fs, w : ws)) ([], []) built_feeds
        all_links = old_links ++ map f_link feeds
    if null built_feeds
      then prepareResponseWith $ "No feed could be built; reason(s): " `T.append` T.intercalate ", " failed
      else saveFeeds feeds warnings all_links failed
  saveFeeds feeds warnings all_links failed =
    evalDb (UpsertFeeds feeds) >>= \case
      Left err -> prepareResponseWith $ render err
      _ -> updateChatInMem feeds warnings all_links failed
  updateChatInMem feeds warnings all_links failed =
    let to_sub_to = map f_link feeds
     in withChat (Sub to_sub_to) Nothing cid >>= \case
          Left err -> prepareResponseWith $ render err
          Right _ -> prepareResponse warnings all_links failed
  prepareResponse warnings all_links failed =
    let failed_txt = ". Failed to subscribe to these feeds: " `T.append` T.intercalate ", " failed
        ok_txt = "Added and subscribed to these feeds: " `T.append` T.intercalate ", " all_links
        warnings_text = case sequenceA warnings of
          Nothing -> mempty
          Just ws -> "However, the following warnings were raised: " `T.append` T.intercalate ", " ws
        contents = (if null failed then ok_txt `T.append` warnings_text else T.append ok_txt failed_txt)
     in prepareResponseWith contents
  prepareResponseWith txt = pure $ ServiceReply txt

evalTgAct ::
  (MonadIO m) =>
  UserId ->
  UserAction ->
  ChatId ->
  App m (Either TgEvalError Reply)
evalTgAct _ About _ =
  ask >>= \env ->
    pure . Right . mkReply . FromAbout . app_version $ env
evalTgAct _ (FeedInfo ref) cid = do
  chats_hmap <- getChats
  case HMS.lookup cid chats_hmap of
    Nothing -> pure . Left $ NotFoundChat
    Just c ->
      let subs = case sub_linked_to c of
            Nothing -> sort . S.toList . sub_feeds_links $ c
            Just cid' -> case HMS.lookup cid' chats_hmap of
              Nothing -> []
              Just c' -> sort . S.toList . sub_feeds_links $ c'
       in if null subs
            then pure . Left $ NotSubscribed
            else
              evalDb (GetSomeFeeds subs) >>= \case
                Right (DbFeeds fs) ->
                  let found = case ref of ById n -> maybeUserIdx subs n; ByUrl u -> Just u
                   in case found of
                        Nothing -> pure . Left $ NotFoundFeed mempty
                        Just u -> case find (\f -> f_link f == u) fs of
                          Nothing -> pure . Left $ NotFoundFeed mempty
                          Just f -> pure . Right . mkReply $ FromFeedDetails f
                _ -> pure . Left $ NotFoundFeed mempty
evalTgAct uid (Announce txt) admin_chat =
  ask >>= \env ->
    let tok = bot_token . tg_config $ env
        admin_id = alert_chat . tg_config $ env
        look cid = runSend tok TgGetChat $ GetChatMessage cid
     in if admin_id /= admin_chat
          then exitNotAuth uid
          else do
            chats <- getChats
            (failed, succeeded) <- liftIO $ partitionEither <$> mapConcurrently look (HMS.keys chats)
            let non_channels = are_non_channels succeeded
            unless (null failed) $
              alertAdmin (postjobs env) $
                "Telegram didn't told us the type of these chats: "
                  `T.append` (T.pack . show $ failed)
            if null non_channels
              then pure . Right . ServiceReply $ "No non-channel chats identified. Aborting."
              else
                let rep = mkReply $ FromAnnounce txt
                 in do
                      liftIO (mapConcurrently_ (\cid' -> runApp env $ reply cid' rep) non_channels)
                      pure . Right $ ServiceReply $ "Tried to broadcast an announce to " `T.append` (T.pack . show . length $ non_channels) `T.append` " chats."
 where
  are_non_channels :: [JsonResponse TgGetChatResponse] -> [ChatId]
  are_non_channels =
    foldl'
      ( \acc r ->
          let res_c = responseBody r
              chat_full_info = resp_result res_c
           in if not $ resp_ok res_c
                then acc
                else case cfi_type chat_full_info of Channel -> acc; _ -> cfi_chat_id chat_full_info : acc
      )
      []
evalTgAct uid (AskForLogin target_id) cid = do
  env <- ask
  let tok = bot_token . tg_config $ env
  isChatOfType tok cid Private >>= \case
    Left err -> pure . Left $ err
    Right private ->
      if not private
        then pure . Left $ ChatNotPrivate
        else
          isUserAdmin tok uid target_id >>= \case
            Left err -> pure . Left $ err
            Right ok ->
              if not ok
                then pure . Left $ UserNotAdmin
                else do
                  evalDb (DbAskForLogin uid cid) >>= \case
                    Right (DbToken h) -> pure . Right . mkReply . FromAdmin (base_url env) $ h
                    _ ->
                      pure
                        . Right
                        . mkReply
                        . FromAdmin (base_url env)
                        $ "Unable to log you in. Are you sure you are an admin of this chat?"
evalTgAct uid (AboutChannel channel_id ref) _ = evalTgAct uid (FeedInfo ref) channel_id
evalTgAct _ Changelog _ = pure . Right $ mkReply FromChangelog
evalTgAct uid (GetChannelItems channel_id ref) _ = evalTgAct uid (GetItems ref) channel_id
evalTgAct _ (GetItems ref) cid = do
  chats_hmap <- getChats
  feeds_hmap <-
    evalDb GetAllFeeds >>= \case
      Right (DbFeeds fs) -> pure $ HMS.fromList $ map (\f -> (f_link f, f)) fs
      _ -> pure HMS.empty
  -- get items by url or id depending on whether the
  -- user user a number or a string referencing the target feed
  case ref of
    ByUrl url -> urlPath url feeds_hmap chats_hmap
    ById _id -> case HMS.lookup cid chats_hmap of
      Nothing -> pure . Right . ServiceReply . render $ NotFoundChat
      Just c -> case maybeUserIdx (sort . S.toList $ sub_feeds_links c) _id of
        Nothing -> pure . Left . BadRef $ T.pack . show $ _id
        Just r -> urlPath r feeds_hmap chats_hmap
 where
  urlPath url f_hmap c_hmap = case HMS.lookup url f_hmap of
    Nothing -> pure . Left . NotFoundFeed $ url
    Just f ->
      if hasSubToFeed (f_link f) c_hmap
        then pure . Right $ mkReply (FromFeedItems f)
        else pure . Right . ServiceReply $ "It appears that you are not subscribed to this feed. Use /sub or talk to your chat administrator about it."
  hasSubToFeed flink chats_hmap = maybe False (\c -> flink `elem` sub_feeds_links c) (HMS.lookup cid chats_hmap)
evalTgAct _ (GetLastXDaysItems n) cid = do
  chats_hmap <- getChats
  case HMS.lookup cid chats_hmap of
    Nothing -> pure . Right . ServiceReply $ "Apparently this chat is not subscribed to any feed yet. Use /sub or talk to an admin!"
    Just c ->
      let subscribed = case sub_linked_to c of
            Nothing -> S.toList . sub_feeds_links $ c
            Just cid' -> case HMS.lookup cid' chats_hmap of
              Nothing -> []
              Just c' -> S.toList . sub_feeds_links $ c'
       in if null subscribed
            then pure . Right . ServiceReply $ "Apparently this chat is not subscribed to any feed yet. Use /sub or talk to an admin!"
            else
              evalDb (GetXDays subscribed n) >>= \case
                Right (DbLinkDigest res) -> pure . Right $ mkReply (FromFeedLinkItems res)
                _ -> pure . Right . ServiceReply $ "Unable to find any feed for this chat."
evalTgAct uid (GetSubchannelSettings channel_id) _ = evalTgAct uid GetSubchatSettings channel_id
evalTgAct _ GetSubchatSettings cid =
  getChats >>= \hmap ->
    case HMS.lookup cid hmap of
      Nothing -> pure . Left $ NotFoundChat
      Just ch -> pure . Right . ServiceReply . render $ ch
evalTgAct _ ListSubs cid = do
  chats_hmap <- getChats
  case HMS.lookup cid chats_hmap of
    Nothing -> pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
    Just c ->
      let subs = case sub_linked_to c of
            Nothing -> sort . S.toList . sub_feeds_links $ c
            Just cid' -> case HMS.lookup cid' chats_hmap of
              Nothing -> []
              Just c' -> sort . S.toList . sub_feeds_links $ c'
       in if null subs
            then pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
            else
              evalDb GetAllFeeds >>= \case
                Right (DbFeeds fs) ->
                  let hmap = HMS.fromList $ map (\f -> (f_link f, f)) fs
                   in case mapM (`HMS.lookup` hmap) subs of
                        Nothing ->
                          pure
                            . Left
                            . NotFoundFeed
                            $ "Unable to find these feeds "
                              `T.append` T.intercalate " " subs
                        Just feeds -> pure . Right $ mkReply (FromChatFeeds c feeds)
                _ ->
                  pure
                    . Left
                    . NotFoundFeed
                    $ "Unable to find these feeds "
                      `T.append` T.intercalate " " subs
evalTgAct uid (Link target_id) cid = do
  env <- ask
  verdict <- liftIO $ mapConcurrently (isUserAdmin (bot_token . tg_config $ env) uid) [cid, target_id]
  case and <$> sequence verdict of
    Left err -> pure . Left $ err
    Right authorized ->
      if not authorized
        then exitNotAuth uid
        else
          withChat (Link target_id) Nothing cid >>= \case
            Left err -> pure . Right . ServiceReply . render $ err
            Right _ -> pure . Right . ServiceReply $ succeeded
 where
  succeeded = "Successfully linked the two chats!"
evalTgAct uid (ListSubsChannel chan_id) _ = evalTgAct uid ListSubs chan_id
evalTgAct uid (Migrate to) cid = do
  env <- ask
  let tok = bot_token . tg_config $ env
  mb_ok <- liftIO $ do
    (one, two) <- concurrently (isUserAdmin tok uid cid) (isUserAdmin tok uid to)
    pure $ (&&) <$> one <*> two
  case mb_ok of
    Left err -> pure . Left $ err
    Right ok ->
      if not ok
        then
          pure
            . Right
            . ServiceReply
            $ "Not authorized. Make sure you have admin permissions \
              \ in both the origin and the destination."
        else
          migrate >>= \case
            Left err -> restore >> onErr err
            Right _ -> onOk
 where
  migrate = sequence <$> sequence [withChat (Migrate to) Nothing cid, withChat Purge Nothing cid]
  restore = withChat (Migrate cid) Nothing to >> withChat Purge Nothing to
  onOk =
    pure
      . Right
      . ServiceReply
      $ "Successfully migrated "
        `T.append` (T.pack . show $ cid)
        `T.append` " to "
        `T.append` (T.pack . show $ to)
  onErr err = pure . Right . ServiceReply $ render err
evalTgAct uid (MigrateChannel fr to) _ = evalTgAct uid (Migrate to) fr
evalTgAct uid (Pause pause_or_resume) cid =
  ask >>= \env ->
    let tok = bot_token . tg_config $ env
     in isUserAdmin tok uid cid >>= \case
          Left err -> pure . Left $ err
          Right verdict ->
            if not verdict
              then exitNotAuth uid
              else
                withChat (Pause pause_or_resume) Nothing cid >>= \case
                  Left err -> pure . Right . ServiceReply . render $ err
                  Right _ -> pure . Right . ServiceReply $ succeeded
 where
  succeeded =
    if pause_or_resume
      then "All notifications to this chat are now suspended. Use /resume to resume."
      else "Resuming notifications. This chat is receiving messages again."
evalTgAct uid (PauseChannel chan_id pause_or_resume) _ = evalTgAct uid (Pause pause_or_resume) chan_id
evalTgAct uid Purge cid = do
  env <- ask
  let tok = bot_token . tg_config $ env
  isUserAdmin tok uid cid >>= \case
    Left err -> pure . Left $ err
    Right verdict ->
      if not verdict
        then exitNotAuth uid
        else
          withChat Purge Nothing cid >>= \case
            Left err -> pure . Right . ServiceReply $ "Unable to purge this chat" `T.append` render err
            Right _ -> pure . Right . ServiceReply $ "Successfully purged the chat from the database."
evalTgAct uid (PurgeChannel chan_id) _ = evalTgAct uid Purge chan_id
evalTgAct _ Start cid =
  let existing_chat = getChats <&> HMS.lookup cid
   in existing_chat >>= \case Just c -> start_with_reload c; _ -> fresh_start
 where
  fresh_start = pure . Right . mkReply $ FromStart
  start_with_reload c =
    pure
      . Right
      $ mkReply
        ( FromChat
            c
            "Welcome back! This chat was already using this bot in the past;\
            \ find the last settings below. Use /help is to display the list of commands.\n---\n"
        )
evalTgAct uid (Sub feeds_urls) cid = do
  env <- ask
  chats <- getChats
  let tok = bot_token . tg_config $ env
  -- fails if 50 feeds subscribed to already
  if tooManySubs 50 chats cid
    then exitTooMany
    else
      isUserAdmin tok uid cid >>= \case
        Left err -> pure . Left $ err
        Right verdict ->
          if not verdict
            then exitNotAuth uid
            else subFeed cid feeds_urls <&> Right
 where
  exitTooMany = pure . Left . MaxFeedsAlready $ "As of now, chats are not allowed to subscribe to more than 25 feeds."
evalTgAct _ RenderCmds _ = pure . Right $ mkReply FromCmds
evalTgAct uid Reset cid = do
  env <- ask
  let tok = bot_token . tg_config $ env
  isUserAdmin tok uid cid >>= \case
    Left err -> pure . Left $ err
    Right verdict ->
      if not verdict
        then exitNotAuth uid
        else
          withChat Reset Nothing cid >>= \case
            Left err -> pure . Right . ServiceReply $ render err
            Right _ -> pure . Right . ServiceReply $ "Chat settings set to defaults."
evalTgAct uid (ResetChannel chan_id) _ = evalTgAct uid Reset chan_id
evalTgAct _ (Search keywords) cid =
  getChats >>= \hmap -> case HMS.lookup cid hmap of
    Nothing -> pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
    Just c ->
      let scope = case sub_linked_to c of
            Nothing -> S.toList . sub_feeds_links $ c
            Just cid' -> case HMS.lookup cid' hmap of
              Nothing -> []
              Just c' -> S.toList . sub_feeds_links $ c'
       in if null scope
            then pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet. Subscribe to a feed to be able to search its items."
            else
              evalDb (DbSearch (S.fromList keywords) (S.fromList scope) Nothing) >>= \case
                Right (DbSearchRes keys _ res) -> pure . Right $ mkReply (FromSearchRes keys res)
                _ -> pure . Left . DbQueryError . BadQuery $ "The database was not able to run your query."
evalTgAct uid (SetChannelSettings chan_id settings) _ = evalTgAct uid (SetChatSettings $ Parsed settings) chan_id
evalTgAct uid (SetChatSettings settings) cid =
  ask >>= \env ->
    let tok = bot_token . tg_config $ env
     in isUserAdmin tok uid cid >>= \case
          Left err -> pure . Left $ err
          Right verdict ->
            if not verdict
              then exitNotAuth uid
              else
                withChat (SetChatSettings settings) Nothing cid >>= \case
                  Left err -> pure . Right . ServiceReply $ "Unable to udpate this chat settings" `T.append` render err
                  Right (ChatUpdated c) -> pure . Right $ mkReply (FromChat c "Ok. New settings below.\n---\n")
                  _ -> pure . Left . DbQueryError . BadQuery $ "Unable to update the settings for this chat. Please try again later."
evalTgAct uid (SubChannel chan_id urls) _ =
  ask >>= \env ->
    let tok = bot_token . tg_config $ env
     in isUserAdmin tok uid chan_id >>= \case
          Left err -> pure . Left $ err
          Right verdict ->
            if not verdict
              then pure . Left . NotAdmin $ "Only users with administrative rights in the target channel is allowed to link the bot to the channel."
              else subFeed chan_id urls >>= \rep -> pure . Right $ rep
evalTgAct uid TestDigest cid =
  ask >>= \env ->
    isUserAdmin (bot_token . tg_config $ env) uid cid >>= \case
      Left _ -> pure . Right . ServiceReply $ "Unable to test the digest for this chat, as Telegram didn't let us check if you were an admin."
      Right is_admin ->
        if not is_admin
          then exitNotAuth uid
          else
            getChats >>= \chats -> case HMS.lookup cid chats of
              Nothing -> pure . Left $ NotFoundChat
              Just c -> do
                let feeds = sub_feeds_links c
                (now, failed, succeeded) <- liftIO $ do
                  now <- getCurrentTime
                  (failed, succeeded) <- partitionEither <$> mapConcurrently rebuildFeed (S.toList feeds)
                  pure (now, failed, succeeded)
                if null failed
                  then
                    pure
                      . Right
                      . mkReply
                      $ FromDigest (new_since_last_week now succeeded) Nothing (sub_settings c)
                  else pure . Right . ServiceReply $ "Unable to construct these feeds: " `T.append` T.intercalate ", " (map r_url failed)
 where
  new_since_last_week now =
    let last_week = addUTCTime (-604800)
        step fs feed =
          let filtered = filter (\i -> i_pubdate i > last_week now) $ f_items feed
           in if null filtered then fs else feed{f_items = take 3 filtered} : fs
     in foldl' step []
evalTgAct uid (TestDigestChannel chan_id) _ = evalTgAct uid TestDigest chan_id
evalTgAct uid (UnSub feeds) cid =
  ask >>= \env ->
    isUserAdmin (bot_token . tg_config $ env) uid cid >>= \case
      Left _ -> pure . Right . ServiceReply $ "TgEvalError occured when requesting Telegram. Try again."
      Right is_admin ->
        if not is_admin
          then exitNotAuth uid
          else
            withChat (UnSub feeds) Nothing cid >>= \case
              Left err -> pure . Left $ err
              Right _ -> pure . Right . ServiceReply $ "Successfully unsubscribed from " `T.append` T.intercalate " " (unFeedRefs feeds)
evalTgAct uid (UnSubChannel chan_id feeds) _ = evalTgAct uid (UnSub feeds) chan_id

processCbq :: (MonadReader AppConfig m, TgReqM m) => CallbackQuery -> m ()
processCbq cbq =
  ask >>= \env ->
    let send_result n (Right (CachePage p i mb_url)) =
          let rep = EditReply mid p True (mkKeyboard n i mb_url)
           in reply cid rep
        send_result _ (Left err) = report err
        send_result _ _ = pure ()
        report err = liftIO $ writeChan (postjobs env) $ JobTgAlertAdmin err
        send_answer =
          answer (bot_token . tg_config $ env) mkAnswer >>= \case
            Left err -> report err
            _ -> pure ()
        get_page n = withKeyStore $ CacheGetPage cid mid n
     in case cbq_data cbq
              >>= readMaybe
                . T.unpack ::
              Maybe Int of
          Nothing -> pure ()
          Just n ->
            liftIO $
              concurrently (runApp env $ get_page n) send_answer
                >>= runApp env
                  . send_result n
                  . fst
 where
  cid = chat_id . chat . fromJust . cbq_message $ cbq
  mid = message_id . fromJust . cbq_message $ cbq
  mkAnswer = AnswerCallbackQuery (cbq_id cbq) Nothing Nothing Nothing Nothing
