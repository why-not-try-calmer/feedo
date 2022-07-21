{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TgActions where

import Backend (withChat)
import Broker (HasCache (withCache), getAllFeeds)
import Control.Concurrent (Chan, readMVar, writeChan)
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
import Mongo (evalDb)
import Network.HTTP.Req (JsonResponse, renderUrl, responseBody)
import Parsing (eitherUrlScheme, getFeedFromUrlScheme, parseSettings, rebuildFeed)
import Replies (mkReply, render)
import Requests (TgReqM (runSend), answer, mkKeyboard, reply, runSend, setWebhook)
import Text.Read (readMaybe)
import TgramInJson
import TgramOutJson
import Types
import Utils (maybeUserIdx, partitionEither, renderUserError, toFeedRef, tooManySubs, unFeedRefs)

registerWebhook :: AppConfig -> IO ()
registerWebhook config =
    let tok = bot_token . tg_config $ config
        webhook = webhook_url . tg_config $ config
     in putStrLn "Trying to set webhook" >> setWebhook tok webhook
            >> print ("Webhook successfully set at " `T.append` webhook)

checkIfAdmin :: TgReqM m => BotToken -> UserId -> ChatId -> m (Maybe Bool)
checkIfAdmin tok uid cid =
    checkIfPrivate tok cid >>= \case
        Nothing -> pure Nothing
        Just ok ->
            if ok
                then pure $ Just True
                else
                    getChatAdmins >>= \case
                        Left err -> do
                            liftIO $ print err
                            pure Nothing
                        Right res_chat_type ->
                            let res_cms = responseBody res_chat_type :: TgGetChatMembersResponse
                                chat_members = resp_cm_result res_cms :: [ChatMember]
                             in pure . Just $ if_admin chat_members
  where
    getChatAdmins = runSend tok "getChatAdministrators" $ GetChat cid
    if_admin = foldr is_admin False
    is_admin member acc
        | uid /= (user_id . cm_user $ member) = acc
        | "administrator" == cm_status member || "creator" == cm_status member = True
        | otherwise = False

checkIfPrivate :: MonadIO m => BotToken -> ChatId -> m (Maybe Bool)
checkIfPrivate tok cid =
    liftIO $
        getChatType >>= \case
            Left err -> do
                liftIO $ print err
                pure Nothing
            Right res_chat ->
                let chat_resp = responseBody res_chat :: TgGetChatResponse
                    c = resp_result chat_resp :: Chat
                 in pure . Just $ chat_type c == Private
  where
    getChatType = runSend tok "getChat" $ GetChat cid

exitNotAuth :: (Applicative f, Show a) => a -> f (Either UserError b)
exitNotAuth = pure . Left . NotAdmin . T.pack . show

interpretCmd :: T.Text -> Either UserError UserAction
interpretCmd contents
    | cmd == "/admin" =
        if length args /= 1
            then Left $ BadInput "/admin takes exactly one argument: the chat_id of the chat or channel to be administrate."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                Nothing -> Left . BadInput $ "The value passed to /admin could not be parsed into a valid chat_id."
                Just chat_or_channel_id -> Right $ AskForLogin chat_or_channel_id
    | cmd == "/announce" =
        if null args
            then Left . BadInput $ "/announce takes exactly 1 argument, the text to broadcast to all users."
            else Right . Announce . T.concat $ args
    | cmd == "/changelog" = Right Changelog
    | cmd == "/feed" || cmd == "/f" =
        if length args == 1
            then case toFeedRef args of
                Left err -> Left err
                Right single_ref -> Right . About . head $ single_ref
            else
                if length args == 2
                    then case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /feed (with 2 arguments) could not be parsed into a valid chat_id."
                        Just channel_id -> case toFeedRef . tail $ args of
                            Left err -> Left err
                            Right single_ref -> Right . AboutChannel channel_id . head $ single_ref
                    else Left . BadInput $ "/feed must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
    | cmd == "/fresh" =
        if length args /= 1
            then Left . BadInput $ "/fresh takes exactly 1 argument, standing for number of days."
            else case readMaybe . T.unpack . head $ args :: Maybe Int of
                Nothing -> Left . BadInput $ "/fresh's argument must be a valid integer."
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
                        Nothing -> Left . BadInput $ "The first value passed to /items (with 2 arguments) could not be parsed into a valid chat_id."
                        Just channel_id -> case toFeedRef . tail $ args of
                            Left err -> Left err
                            Right single_ref -> Right . GetChannelItems channel_id . head $ single_ref
                    else Left . BadInput $ "/items must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
    | cmd == "/link" =
        if length args /= 1
            then Left . BadInput $ "/link takes exactly 1 argument, standing for a chat_id."
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                Nothing -> Left . BadInput $ "The first value passed to /purge could not be parsed into a valid chat_id."
                Just n -> Right . Link $ n
    | cmd == "/list" =
        if null args
            then Right ListSubs
            else
                if length args > 1
                    then Left . BadInput $ "/list cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /list could not be parsed into a valid chat_id."
                        Just n -> Right . ListSubsChannel $ n
    | cmd == "/migrate" =
        if null args
            then
                Left . BadInput $
                    "/migrate takes at least one argument, standing for the chat_id of the destination you want to migrate this chat's settings to. \
                    \ If you call /migrate with 2 arguments, the first should be the chat_id of the origin and the second the chat_id of the destination."
            else
                let (x : xs) = args
                 in case readMaybe . T.unpack $ x :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /migrate could not be parsed into a valid chat_id."
                        Just n ->
                            if null xs
                                then Right $ Migrate n
                                else case readMaybe . T.unpack . head $ xs :: Maybe ChatId of
                                    Nothing -> Left . BadInput $ "The second value passed to /migrate could not be parsed into a valid chat_id."
                                    Just m -> Right $ MigrateChannel n m
    | cmd == "/pause" || cmd == "/p" =
        if null args
            then Right . Pause $ True
            else
                if length args > 1
                    then Left . BadInput $ "/pause cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /pause could not be parsed into a valid chat_id."
                        Just n -> Right . PauseChannel n $ True
    | cmd == "/purge" =
        if null args
            then Right Purge
            else
                if length args > 1
                    then Left . BadInput $ "/purge cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /purge could not be parsed into a valid chat_id."
                        Just n -> Right . PurgeChannel $ n
    | cmd == "/resume" =
        if null args
            then Right . Pause $ False
            else
                if length args > 1
                    then Left . BadInput $ "/resume cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /resume could not be parsed into a valid chat_id."
                        Just n -> Right . PauseChannel n $ False
    | cmd == "/reset" =
        if null args
            then Right Reset
            else
                if length args > 1
                    then Left . BadInput $ "/reset cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /reset could not be parsed into a valid chat_id."
                        Just n -> Right . ResetChannel $ n
    | cmd == "/search" || cmd == "/se" =
        if null args
            then Left . BadInput $ "/search requires at least one keyword. Separate keywords with a space."
            else Right . Search $ args
    | cmd == "/set" =
        let body = tail . T.lines $ contents
         in if null args
                then Right GetSubchatSettings
                else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                    Nothing -> case parseSettings body of
                        Left err -> Left . BadInput $ err
                        Right settings -> Right . SetChatSettings . Parsed $ settings
                    Just n ->
                        if null $ tail args
                            then Right $ GetSubchannelSettings n
                            else case parseSettings body of
                                Left err -> Left . BadInput $ err
                                Right settings -> Right $ SetChannelSettings n settings
    | cmd == "/start" = Right Start
    | cmd == "/sub" || cmd == "/s" =
        if null args
            then Left . BadInput $ "/sub <optional: channel id> <mandatory: list of url feeds>"
            else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                Nothing -> Right . Sub $ args
                Just n ->
                    if null $ tail args
                        then Left . BadInput $ "Cannot subscribe to an empty list urls..."
                        else Right . SubChannel n $ tail args
    | cmd == "/testdigest" =
        if null args
            then Right TestDigest
            else
                if length args > 1
                    then Left . BadInput $ "/testdigest cannot take more than one (optional) argument (channel_id)."
                    else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
                        Nothing -> Left . BadInput $ "The first value passed to /list could not be parsed into a valid chat_id."
                        Just n -> Right . TestDigestChannel $ n
    | cmd == "/unsub" =
        if null args
            then Left . BadInput $ "/unsub <optional: channel id> <mandatory: list of #s or url feeds>"
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
    | otherwise = Left $ Ignore contents
  where
    (cmd, args) =
        let (h' : t) = T.words contents
            (h : _) = T.splitOn "@" h'
         in (h, t)

testChannel :: TgReqM m => BotToken -> ChatId -> Chan Job -> m (Either UserError ())
testChannel tok chan_id jobs =
    -- tries sending a message to the given channel
    -- if the response rewards the test with a message_id, it's won.
    runSend tok "sendMessage" (OutboundMessage chan_id "Channel linked successfully. This message will be removed in 10s." Nothing Nothing Nothing) >>= \case
        Left _ -> pure . Left . NotAdmin $ "Unable to post to " `T.append` (T.pack . show $ chan_id) `T.append` ". Make sure the bot has administrative rights in that channel."
        Right resp ->
            let res = responseBody resp :: TgGetMessageResponse
                mid = message_id . resp_msg_result $ res
             in if not $ resp_msg_ok res
                    then pure $ Left TelegramErr
                    else -- tries removing the test message
                    do
                        liftIO $ writeChan jobs . JobRemoveMsg chan_id mid $ 30
                        pure (Right ())

subFeed :: MonadIO m => ChatId -> [FeedLink] -> App m Reply
subFeed cid feeds_urls = do
    env <- ask
    -- check url scheme
    case mapM eitherUrlScheme feeds_urls of
        Left err -> respondWith . renderUserError $ err
        Right valid_urls ->
            -- sort out already existent feeds to minimize network call
            -- , and subscribe chat to them
            getAllFeeds env >>= \case
                Left _ -> respondWith "Unable to acquire feeds."
                Right old_feeds ->
                    let urls = map renderUrl valid_urls
                        old_keys = HMS.keys $ HMS.filter (\f -> f_link f `elem` urls) old_feeds
                        new_url_schemes = filter (\u -> renderUrl u `notElem` old_keys) valid_urls
                     in do
                            unless (null old_keys) (void $ withChat (Sub old_keys) cid)
                            if null new_url_schemes
                                then
                                    respondWith $
                                        "Successfully subscribed to " `T.append` T.intercalate ", " old_keys
                                else -- fetches feeds at remaining urls

                                    liftIO (mapConcurrently getFeedFromUrlScheme new_url_schemes) >>= \r ->
                                        -- add feeds
                                        let (failed, built_feeds) = partitionEither r
                                            (feeds, warnings) = foldl' (\(fs, ws) (f, w) -> (f : fs, w : ws)) ([], []) built_feeds
                                            all_links = old_keys ++ map f_link feeds
                                         in -- exiting early if no feed could be built
                                            if null built_feeds
                                                then respondWith $ "No feed could be built; reason(s): " `T.append` T.intercalate "," failed
                                                else
                                                    withCache (CachePushFeeds feeds) >>= \case
                                                        Left err -> respondWith err
                                                        Right _ ->
                                                            let to_sub_to = map f_link feeds
                                                             in withChat (Sub to_sub_to) cid >>= \case
                                                                    Left err -> respondWith $ renderUserError err
                                                                    Right _ ->
                                                                        let failed_text = ". Failed to subscribe to these feeds: " `T.append` T.intercalate ", " failed
                                                                            ok_text = "Added and subscribed to these feeds: " `T.append` T.intercalate ", " all_links
                                                                            warnings_text = case sequenceA warnings of
                                                                                Nothing -> mempty
                                                                                Just ws -> "However, the following warnings were raised: " `T.append` T.intercalate ", " ws
                                                                         in respondWith (if null failed then ok_text `T.append` warnings_text else T.append ok_text failed_text)
  where
    respondWith err = pure $ ServiceReply err

evalTgAct ::
    MonadIO m =>
    UserId ->
    UserAction ->
    ChatId ->
    App m (Either UserError Reply)
evalTgAct _ (About ref) cid =
    ask >>= \env -> do
        chats_hmap <- liftIO . readMVar $ subs_state env
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
                            withCache (CachePullFeeds subs) >>= \case
                                Right (CacheFeeds fs) ->
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
            look cid' = runSend tok "getChat" $ GetChat cid'
            fetch_chat_types = liftIO $ readMVar (subs_state env) >>= mapConcurrently look . HMS.keys
         in if admin_id /= admin_chat
                then exitNotAuth uid
                else
                    fetch_chat_types >>= \resp -> do
                        let (failed, succeeded) = partitionEither resp
                            non_channels = are_non_channels succeeded
                        unless (null failed) (liftIO $ writeChan (postjobs env) (JobTgAlert $ "Telegram didn't told us the type of these chats: " `T.append` (T.pack . show $ failed)))
                        if null non_channels
                            then pure . Right . ServiceReply $ "No non-channel chats identified. Aborting."
                            else
                                let rep = mkReply $ FromAnnounce txt
                                 in do
                                        liftIO (mapConcurrently_ (\cid' -> reply (bot_token . tg_config $ env) cid' rep (postjobs env)) non_channels)
                                        pure . Right $ ServiceReply $ "Tried to broadcast an announce to " `T.append` (T.pack . show . length $ non_channels) `T.append` " chats."
  where
    are_non_channels :: [JsonResponse TgGetChatResponse] -> [ChatId]
    are_non_channels =
        foldl'
            ( \acc r ->
                let res_c = responseBody r
                    c = resp_result res_c
                 in if not $ resp_ok res_c
                        then acc
                        else case chat_type c of Channel -> acc; _ -> chat_id c : acc
            )
            []
evalTgAct uid (AskForLogin target_id) cid = do
    env <- ask
    let tok = bot_token . tg_config $ env
    checkIfPrivate tok cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just private ->
            if not private
                then pure . Left $ ChatNotPrivate
                else
                    checkIfAdmin tok uid target_id >>= \case
                        Nothing -> pure . Left $ TelegramErr
                        Just ok ->
                            if not ok
                                then pure . Left $ UserNotAdmin
                                else do
                                    evalDb env (DbAskForLogin uid cid) >>= \case
                                        DbToken h -> pure . Right . mkReply . FromAdmin (base_url env) $ h
                                        _ ->
                                            pure . Right . mkReply . FromAdmin (base_url env) $
                                                "Unable to log you in. Are you sure you are an admin of this chat?"
evalTgAct uid (AboutChannel channel_id ref) _ = evalTgAct uid (About ref) channel_id
evalTgAct _ Changelog _ = pure . Right $ mkReply FromChangelog
evalTgAct uid (GetChannelItems channel_id ref) _ = evalTgAct uid (GetItems ref) channel_id
evalTgAct _ (GetItems ref) cid = do
    env <- ask
    let chats_mvar = subs_state env
    chats_hmap <- liftIO $ readMVar chats_mvar
    feeds_hmap <-
        getAllFeeds env >>= \case
            Left _ -> pure HMS.empty
            Right fs -> pure fs
    -- get items by url or id depending on whether the
    -- user user a number or a string referencing the target feed
    case ref of
        ByUrl url -> urlPath url feeds_hmap chats_hmap env
        ById _id -> case HMS.lookup cid chats_hmap of
            Nothing -> pure . Right . ServiceReply . renderUserError $ NotFoundChat
            Just c -> case maybeUserIdx (sort . S.toList $ sub_feeds_links c) _id of
                Nothing -> pure . Left . BadRef $ T.pack . show $ _id
                Just r -> urlPath r feeds_hmap chats_hmap env
  where
    urlPath url f_hmap c_hmap env = case HMS.lookup url f_hmap of
        Nothing -> pure . Left . NotFoundFeed $ url
        Just f ->
            if hasSubToFeed (f_link f) c_hmap
                then do
                    liftIO $ writeChan (postjobs env) (JobIncReadsJob [f_link f])
                    pure . Right $ mkReply (FromFeedItems f)
                else pure . Right . ServiceReply $ "It appears that you are not subscribed to this feed. Use /sub or talk to your chat administrator about it."
    hasSubToFeed flink chats_hmap = maybe False (\c -> flink `elem` sub_feeds_links c) (HMS.lookup cid chats_hmap)
evalTgAct _ (GetLastXDaysItems n) cid = do
    env <- ask
    chats_hmap <- liftIO . readMVar $ subs_state env
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
                        withCache (CacheXDays subscribed n) >>= \case
                            Right (CacheLinkDigest feeds) -> do
                                liftIO $ writeChan (postjobs env) (JobIncReadsJob $ map fst feeds)
                                pure . Right $ mkReply (FromFeedLinkItems feeds)
                            _ -> pure . Right . ServiceReply $ "Unable to find any feed for this chat."
evalTgAct uid (GetSubchannelSettings channel_id) _ = evalTgAct uid GetSubchatSettings channel_id
evalTgAct _ GetSubchatSettings cid =
    ask >>= liftIO . readMVar . subs_state >>= \hmap ->
        case HMS.lookup cid hmap of
            Nothing -> pure . Left $ NotFoundChat
            Just ch -> pure . Right . ServiceReply . render $ ch
evalTgAct _ ListSubs cid = do
    env <- ask
    chats_hmap <- liftIO . readMVar $ subs_state env
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
                        getAllFeeds env >>= \case
                            Left _ ->
                                pure . Left . NotFoundFeed $
                                    "Unable to find these feeds " `T.append` T.intercalate " " subs
                            Right fs -> case mapM (`HMS.lookup` fs) subs of
                                Nothing ->
                                    pure . Left . NotFoundFeed $
                                        "Unable to find these feeds " `T.append` T.intercalate " " subs
                                Just feeds -> pure . Right $ mkReply (FromChatFeeds c feeds)
evalTgAct uid (Link target_id) cid = do
    env <- ask
    verdict <- liftIO $ mapConcurrently (checkIfAdmin (bot_token . tg_config $ env) uid) [cid, target_id]
    case and <$> sequence verdict of
        Nothing -> pure . Left $ TelegramErr
        Just authorized ->
            if not authorized
                then exitNotAuth uid
                else
                    withChat (Link target_id) cid >>= \case
                        Left err -> pure . Right . ServiceReply . renderUserError $ err
                        Right _ -> pure . Right . ServiceReply $ succeeded
  where
    succeeded = "Successfully linked the two chats!"
evalTgAct uid (ListSubsChannel chan_id) _ = evalTgAct uid ListSubs chan_id
evalTgAct uid (Migrate to) cid = do
    env <- ask
    let tok = bot_token . tg_config $ env
    mb_ok <- liftIO $ do
        (one, two) <- concurrently (checkIfAdmin tok uid cid) (checkIfAdmin tok uid to)
        pure $ (&&) <$> one <*> two
    case mb_ok of
        Nothing -> pure . Left $ TelegramErr
        Just ok ->
            if not ok
                then
                    pure . Right . ServiceReply $
                        "Not authorized. Make sure you have admin permissions \
                        \ in both the origin and the destination."
                else
                    migrate >>= \case
                        Left err -> restore >> onErr err
                        Right _ -> onOk
  where
    migrate = sequence <$> sequence [withChat (Migrate to) cid, withChat Purge cid]
    restore = withChat (Migrate cid) to >> withChat Purge to
    onOk =
        pure . Right . ServiceReply $
            "Successfully migrated "
                `T.append` (T.pack . show $ cid)
                `T.append` " to "
                `T.append` (T.pack . show $ to)
    onErr err = pure . Right . ServiceReply $ renderUserError err
evalTgAct uid (MigrateChannel fr to) _ = evalTgAct uid (Migrate to) fr
evalTgAct uid (Pause pause_or_resume) cid =
    ask >>= \env ->
        let tok = bot_token . tg_config $ env
         in checkIfAdmin tok uid cid >>= \case
                Nothing -> pure . Left $ TelegramErr
                Just verdict ->
                    if not verdict
                        then exitNotAuth uid
                        else
                            withChat (Pause pause_or_resume) cid >>= \case
                                Left err -> pure . Right . ServiceReply . renderUserError $ err
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
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict
                then exitNotAuth uid
                else
                    withChat Purge cid >>= \case
                        Left err -> pure . Right . ServiceReply $ "Unable to purge this chat" `T.append` renderUserError err
                        Right _ -> pure . Right . ServiceReply $ "Successfully purged the chat from the database."
evalTgAct uid (PurgeChannel chan_id) _ = evalTgAct uid Purge chan_id
evalTgAct _ Start cid =
    ask >>= \env ->
        let existing_chat = liftIO $ readMVar (subs_state env) <&> HMS.lookup cid
         in existing_chat >>= \case Just c -> start_with_reload c; _ -> fresh_start
  where
    fresh_start = pure . Right . mkReply $ FromStart
    start_with_reload c =
        pure . Right $
            mkReply
                ( FromChat
                    c
                    "Welcome back! This chat was already using this bot in the past;\
                    \ find the last settings below. Use /help is to display the list of commands.\n---\n"
                )
evalTgAct uid (Sub feeds_urls) cid = do
    env <- ask
    chats <- liftIO . readMVar $ subs_state env
    let tok = bot_token . tg_config $ env
    -- fails if 50 feeds subscribed to already
    if tooManySubs 50 chats cid
        then exitTooMany
        else
            checkIfAdmin tok uid cid >>= \case
                Nothing -> pure . Left $ TelegramErr
                Just verdict ->
                    if not verdict
                        then exitNotAuth uid
                        else subFeed cid feeds_urls <&> Right
  where
    exitTooMany = pure . Left . MaxFeedsAlready $ "As of now, chats are not allowed to subscribe to more than 25 feeds."
evalTgAct _ RenderCmds _ = pure . Right $ mkReply FromCmds
evalTgAct uid Reset cid = do
    env <- ask
    let tok = bot_token . tg_config $ env
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict
                then exitNotAuth uid
                else
                    withChat Reset cid >>= \case
                        Left err -> pure . Right . ServiceReply $ renderUserError err
                        Right _ -> pure . Right . ServiceReply $ "Chat settings set to defaults."
evalTgAct uid (ResetChannel chan_id) _ = evalTgAct uid Reset chan_id
evalTgAct _ (Search keywords) cid =
    ask >>= \env ->
        let get_chats = liftIO . readMVar $ subs_state env
         in get_chats >>= \hmap -> case HMS.lookup cid hmap of
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
                                evalDb env (DbSearch (S.fromList keywords) (S.fromList scope) Nothing) >>= \case
                                    DbSearchRes keys sc -> pure . Right $ mkReply (FromSearchRes keys sc)
                                    _ -> pure . Left . BadInput $ "The database was not able to run your query."
evalTgAct uid (SetChannelSettings chan_id settings) _ = evalTgAct uid (SetChatSettings $ Parsed settings) chan_id
evalTgAct uid (SetChatSettings settings) cid =
    ask >>= \env ->
        let tok = bot_token . tg_config $ env
         in checkIfAdmin tok uid cid >>= \case
                Nothing -> pure . Left $ TelegramErr
                Just verdict ->
                    if not verdict
                        then exitNotAuth uid
                        else
                            withChat (SetChatSettings settings) cid >>= \case
                                Left err -> pure . Right . ServiceReply $ "Unable to udpate this chat settings" `T.append` renderUserError err
                                Right (ChatUpdated c) -> pure . Right $ mkReply (FromChat c "Ok. New settings below.\n---\n")
                                _ -> pure . Left . BadInput $ "Unable to update the settings for this chat. Please try again later."
evalTgAct uid (SubChannel chan_id urls) _ =
    ask >>= \env ->
        let tok = bot_token . tg_config $ env
            jobs = postjobs env
         in checkIfAdmin tok uid chan_id >>= \case
                Nothing -> pure . Left $ TelegramErr
                Just verdict ->
                    if not verdict
                        then pure . Left . NotAdmin $ "Only users with administrative rights in the target channel is allowed to link the bot to the channel."
                        else
                            testChannel tok chan_id jobs >>= \case
                                Left err -> pure . Left $ err
                                Right _ -> subFeed chan_id urls >>= \rep -> pure . Right $ rep
evalTgAct uid TestDigest cid =
    ask >>= \env ->
        checkIfAdmin (bot_token . tg_config $ env) uid cid >>= \case
            Nothing -> pure . Right . ServiceReply $ "Unable to test the digest for this chat, as Telegram didn't let us check if you were an admin."
            Just is_admin ->
                if not is_admin
                    then exitNotAuth uid
                    else
                        liftIO (readMVar $ subs_state env) >>= \chats -> case HMS.lookup cid chats of
                            Nothing -> pure . Left $ NotFoundChat
                            Just c -> do
                                let feeds = sub_feeds_links c
                                (now, either_rebuilt) <- liftIO $ do
                                    now <- getCurrentTime
                                    rebuilt <- mapConcurrently rebuildFeed (S.toList feeds)
                                    pure (now, rebuilt)
                                let (failed, succeeded) = partitionEither either_rebuilt
                                if null failed
                                    then
                                        pure . Right . mkReply $
                                            FromDigest (new_since_last_week now succeeded) Nothing (sub_settings c)
                                    else pure . Right . ServiceReply $ "Unable to construct these feeds: " `T.append` T.intercalate ", " failed
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
        checkIfAdmin (bot_token . tg_config $ env) uid cid >>= \case
            Nothing -> pure . Right . ServiceReply $ "Error occured when requesting Telegram. Try again."
            Just is_admin ->
                if not is_admin
                    then exitNotAuth uid
                    else
                        withChat (UnSub feeds) cid >>= \case
                            Left err -> pure . Left $ err
                            Right _ -> pure . Right . ServiceReply $ "Successfully unsubscribed from " `T.append` T.intercalate " " (unFeedRefs feeds)
evalTgAct uid (UnSubChannel chan_id feeds) _ = evalTgAct uid (UnSub feeds) chan_id

processCbq :: (MonadIO m, MonadReader AppConfig m, TgReqM m, HasCache m) => CallbackQuery -> m ()
processCbq cbq =
    ask >>= \env ->
        let send_result n (Right (CachePage p i mb_url)) =
                let rep = EditReply mid p True (mkKeyboard n i mb_url)
                 in reply (bot_token . tg_config $ env) cid rep (postjobs env)
            send_result _ (Left err) = report err
            send_result _ _ = pure ()
            report err = liftIO $ writeChan (postjobs env) $ JobTgAlert err
            send_answer =
                answer (bot_token . tg_config $ env) mkAnswer >>= \case
                    Left err -> report err
                    _ -> pure ()
            get_page n = withCache $ CacheGetPage cid mid n
         in case cbq_data cbq
                    >>= readMaybe
                        . T.unpack ::
                    Maybe Int of
                Nothing -> pure ()
                Just n ->
                    liftIO $
                        concurrently (runApp env $ get_page n) send_answer
                            >>= send_result n . fst
  where
    cid = chat_id . chat . fromJust . cbq_message $ cbq
    mid = message_id . fromJust . cbq_message $ cbq
    mkAnswer = AnswerCallbackQuery (cbq_id cbq) Nothing Nothing Nothing Nothing