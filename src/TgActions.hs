{-# LANGUAGE LambdaCase #-}

module TgActions where
import AppTypes
import Backend (evalFeeds, withChat)
import Control.Concurrent (Chan, readMVar, writeChan)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Text as T
import Database (Db (evalDb))
import Network.HTTP.Req (renderUrl, responseBody)
import Parsing (eitherUrlScheme, getFeedFromUrlScheme, parseSettings)
import Replies (render, toReply)
import Requests (reqSend, setWebhook)
import Text.Read (readMaybe)
import TgramInJson
import TgramOutJson
import Utils (maybeUserIdx, partitionEither, tooManySubs)

registerWebhook :: AppConfig -> IO ()
registerWebhook config =
    let tok = bot_token . tg_config $ config
        webhook = webhook_url . tg_config $ config
    in  putStrLn "Trying to set webhook" >> setWebhook tok webhook >>
            print ("Webhook successfully set at " `T.append` webhook)

checkIfAdmin :: MonadIO m => BotToken -> UserId -> ChatId -> m (Maybe Bool)
checkIfAdmin tok uid cid = liftIO $ getChatType >>= \case
    Left _ -> pure Nothing
    Right res_chat ->
        let chat_resp = responseBody res_chat :: TgGetChatResponse
            c = resp_result chat_resp :: Chat
        in  if chat_type c == Private then pure . Just $ True else getChatAdmins >>= \case
            Left _ -> pure Nothing
            Right res_chat_type ->
                let res_cms = responseBody res_chat_type :: TgGetChatMembersResponse
                    chat_members = resp_cm_result res_cms :: [ChatMember]
                in  pure . Just $ if_admin chat_members
    where
        getChatAdmins = reqSend tok "getChatAdministrators" $ GetChatAdministrators cid
        getChatType = reqSend tok "getChat" $ GetChatAdministrators cid
        is_admin member acc
            | uid /= (user_id . cm_user $ member) = acc
            | "administrator" == cm_status member || "creator" == cm_status member = True
            | otherwise = False
        if_admin chat_members = foldr is_admin False chat_members

exitNotAuth :: (Applicative f, Show a) => a -> f (Either UserError b)
exitNotAuth = pure . Left . NotAdmin . T.pack . show

interpretCmd :: T.Text -> Either UserError UserAction
interpretCmd contents
    | cmd == "/changelog" = Right Changelog
    | cmd == "/feed" || cmd == "/f" =
        if length args == 1 then case toFeedRef args of
            Left err -> Left err
            Right single_ref -> Right . About . head $ single_ref
        else if length args == 2 then case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /feed (with 2 arguments) could not be parsed into a valid chat_id."
            Just channel_id -> case toFeedRef . tail $ args of
                Left err -> Left err
                Right single_ref -> Right . AboutChannel channel_id . head $ single_ref
        else Left . BadInput $ "/feed must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
    | cmd == "/fresh" =
        if length args /= 1 then Left . BadInput $ "/fresh takes exactly 1 argument, standing for number of days."
        else case readMaybe . T.unpack . head $ args :: Maybe Int of
            Nothing -> Left . BadInput $ "/fresh's argument must be a valid integer."
            Just i -> Right $ GetLastXDaysItems i
    | cmd == "/items" || cmd == "/i" =
        if length args == 1 then case toFeedRef args of
            Left err -> Left err
            Right single_ref -> Right . GetItems . head $ single_ref
        else if length args == 2 then case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /items (with 2 arguments) could not be parsed into a valid chat_id."
            Just channel_id -> case toFeedRef . tail $ args of
                Left err -> Left err
                Right single_ref -> Right . GetChannelItems channel_id . head $ single_ref
        else Left . BadInput $ "/items must take at least 1 argument (feed url or #) and cannot take more than 2 (<chann_id> <feed url or #>)."
    | cmd == "/list" =
        if null args then Right ListSubs
        else if length args > 1 then Left . BadInput $ "/list cannot take more than one (optional) argument (channel_id)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /list could not be parsed into a valid chat_id."
            Just n -> Right . ListSubsChannel $ n
    | cmd == "/migrate" =
        if null args then Left . BadInput $ 
            "/migrate takes at least one argument, standing for the chat_id of the destination you want to migrate this chat's settings to. \
            \ If you call /migrate with 2 arguments, the first should be the chat_id of the origin and the second the chat_id of the destination."
        else
            let (x:xs) = args in case readMaybe . T.unpack $ x :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /migrate could not be parsed into a valid chat_id."
            Just n -> if null xs then Right $ Migrate n else case readMaybe . T.unpack . head $ xs :: Maybe ChatId of
                Nothing -> Left . BadInput $ "The second value passed to /migrate could not be parsed into a valid chat_id."
                Just m -> Right $ MigrateChannel n m
    | cmd == "/pause" || cmd == "/p" =
        if null args then Right . Pause $ True
        else if length args > 1 then Left . BadInput $ "/pause cannot take more than one (optional) argument (channel_id)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /pause could not be parsed into a valid chat_id."
            Just n -> Right . PauseChannel n $ True
    | cmd == "/purge" =
        if null args then Right Purge
        else if length args > 1 then Left . BadInput $ "/purge cannot take more than one (optional) argument (channel_id)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /purge could not be parsed into a valid chat_id."
            Just n -> Right . PurgeChannel $ n
    | cmd == "/resume" =
        if null args then Right . Pause $ False
        else if length args > 1 then Left . BadInput $ "/resume cannot take more than one (optional) argument (channel_id)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /resume could not be parsed into a valid chat_id."
            Just n -> Right . PauseChannel n $ False
    | cmd == "/reset" =
        if null args then Right Reset
        else if length args > 1 then Left . BadInput $ "/reset cannot take more than one (optional) argument (channel_id)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /reset could not be parsed into a valid chat_id."
            Just n -> Right . ResetChannel $ n
    | cmd == "/search" || cmd == "/se" =
        if null args then Left . BadInput $ "/search requires at least one keyword. Separate keywords with a space."
        else Right . Search $ args
    | cmd == "/set" =
        let body = tail . T.lines . T.toLower $ contents in
        if null args then Right GetSubchatSettings else
        case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> case parseSettings body of
                Left err -> Left . BadInput $ err
                Right settings -> Right $ SetChatSettings settings
            Just n -> if null $ tail args then Right $ GetSubchannelSettings n else
                case parseSettings body of
                Left err -> Left . BadInput $ err
                Right settings -> Right $ SetChannelSettings n settings
    | cmd == "/start" || cmd == "/help" = Right RenderCmds
    | cmd == "/sub" || cmd == "/s" =
        if null args then Left . BadInput $ "/sub <optional: channel id> <mandatory: list of url feeds>" else
        case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Right . Sub $ args
            Just n ->
                if null $ tail args then Left . BadInput $ "Cannot subscribe to an empty list urls..."
                else Right . SubChannel n $ tail args
    | cmd == "/unsub" =
        if null args then Left . BadInput $ "/unsub <optional: channel id> <mandatory: list of #s or url feeds>" else
        case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> case toFeedRef args of
                Left err -> Left err
                Right refs -> Right . UnSub $ refs
            Just n ->
                if abs n < 10000 then case toFeedRef args of
                    Left err -> Left err
                    Right refs -> Right . UnSub $ refs
                else case toFeedRef $ tail args of
                    Left err -> Left err
                    Right refs -> Right . UnSubChannel n $ refs
    | otherwise = Left $ Ignore contents
    where
        (cmd, args) =
            let (h':t) = T.words contents
                (h:_) = T.splitOn "@" h'
            in  (h, t)

testChannel :: MonadIO m => BotToken -> ChatId -> Chan Job -> m (Either UserError ())
testChannel tok chan_id jobs =
    -- tries sending a message to the given channel
    -- if the response rewards the test with a message_id, it's won.
    reqSend tok "sendMessage" (OutboundMessage chan_id "Channel linked successfully. This message will be removed in 10s." Nothing Nothing) >>= \case
    Left _ -> pure . Left . NotAdmin $ "Unable to post to " `T.append` (T.pack . show $ chan_id) `T.append` ". Make sure the bot has administrative rights in that channel."
    Right resp ->
        let res = responseBody resp :: TgGetMessageResponse
            mid = message_id . resp_msg_result $ res
        in  if not $ resp_msg_ok res then pure $ Left TelegramErr
            -- tries removing the test message
            else do
                liftIO $ writeChan jobs . JobRemoveMsg chan_id mid $ 30
                pure (Right ())

subFeed :: MonadIO m => AppConfig -> ChatId -> [T.Text] -> App m (Either UserError Reply)
subFeed env cid feeds_urls =
    liftIO (readMVar . feeds_state $ env) >>= \known_feeds ->
    -- check url scheme
    case traverse eitherUrlScheme feeds_urls of
    Left err -> pure . Left $ err
    Right valid_urls -> do
        -- sort out already existent feeds to minimize network call
        -- , and subscribe chat to them
        let urls = map renderUrl valid_urls
            old_keys = HMS.keys $ HMS.filter (\f -> f_link f `elem` urls) known_feeds
            new_url_schemes = filter (\u -> renderUrl u `notElem` old_keys) valid_urls
        unless (null old_keys) (void $ withChat (Sub old_keys) cid)
        if null new_url_schemes then pure . Right . ServiceReply $
            "Successfully subscribed to " `T.append` T.intercalate ", " old_keys
        -- fetches feeds at remaining urls
        else liftIO (mapConcurrently getFeedFromUrlScheme new_url_schemes) >>= \res ->
            -- add feeds
            let (failed, built_feeds) = partitionEither res
                all_links = old_keys ++ map f_link built_feeds
            in  evalFeeds (AddF built_feeds) >>= \case
                FeedsOk ->
                    -- subscribes chat to newly added feeds, returning result to caller
                    let to_sub_to = map f_link built_feeds
                    in  withChat (Sub to_sub_to) cid >>= \case
                    Left err -> pure . Right . ServiceReply $ renderUserError err
                    Right _ ->
                        let failed_text = ". Failed to subscribe to these feeds: " `T.append` T.intercalate ", " failed
                            ok_text = "Added and subscribed to these feeds: " `T.append` T.intercalate ", " all_links
                        in  pure . Right . ServiceReply $ (if null failed then ok_text else T.append ok_text failed_text)
                _ -> pure . Left $ UpdateError "Something bad occurred; unable to add and subscribe to these feeds."

evalTgAct :: MonadIO m => UserId -> UserAction -> ChatId -> App m (Either UserError Reply)
evalTgAct _ (About ref) cid = ask >>= \env -> do
    chats_hmap <- liftIO . readMVar $ subs_state env
    case HMS.lookup cid chats_hmap of
        Nothing -> pure . Left $ NotFoundChat
        Just c ->
            let subs = sort . S.toList . sub_feeds_links $ c
            in  if null subs then pure . Left $ NotSubscribed
                else liftIO (readMVar $ feeds_state env) >>= \feeds_hmap -> case ref of
                    ByUrl u -> case HMS.lookup u feeds_hmap of
                        Nothing -> pure . Left $ NotSubscribed
                        Just f -> pure . Right $ toReply (FromFeedDetails f) (Just $ sub_settings c)
                    ById i ->
                        let feed = (`HMS.lookup` feeds_hmap) =<< maybeUserIdx subs i
                        in  case feed of
                            Nothing -> pure . Left $ NotSubscribed
                            Just f -> pure . Right $ toReply (FromFeedDetails f) (Just $ sub_settings c)
evalTgAct uid (AboutChannel channel_id ref) _ = evalTgAct uid (About ref) channel_id
evalTgAct _ Changelog _ =  pure . Right $ toReply FromChangelog Nothing
evalTgAct uid (GetChannelItems channel_id ref) _ = evalTgAct uid (GetItems ref) channel_id
evalTgAct _ (GetItems ref) cid = do
    env <- ask
    let feeds_mvar = feeds_state env
        chats_mvar = subs_state env
    (feeds_hmap, chats_hmap) <- liftIO $ (,) <$>
        readMVar feeds_mvar <*>
        readMVar chats_mvar
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
                if hasSubToFeed (f_link f) c_hmap then do
                liftIO $ writeChan (postjobs env) (JobIncReadsJob [f_link f])
                pure . Right $ toReply (FromFeedItems f) Nothing
                else pure . Right . ServiceReply $ "It appears that you are not subscribed to this feed. Use /sub or talk to your chat administrator about it."
        hasSubToFeed flink chats_hmap = maybe False (\c -> flink `elem` sub_feeds_links c) (HMS.lookup cid chats_hmap)
evalTgAct _ (GetLastXDaysItems n) cid = do
    env <- ask
    chats_hmap <- liftIO . readMVar $ subs_state env
    case HMS.lookup cid chats_hmap of
        Nothing -> pure . Right . ServiceReply $ "Apparently this chat is not subscribed to any feed yet. Use /sub or talk to an admin!"
        Just c ->
            let subscribed = S.toList . sub_feeds_links $ c
            in  if null subscribed then pure . Right . ServiceReply $ "Apparently this chat is not subscribed to any feed yet. Use /sub or talk to an admin!"
            else evalFeeds (GetAllXDays subscribed n) >>= \case
                FeedLinkDigest feeds -> do
                    liftIO $ writeChan (postjobs env) (JobIncReadsJob $ map fst feeds)
                    pure . Right $ toReply (FromFeedLinkItems feeds) (Just $ sub_settings c)
                _ -> pure . Right . ServiceReply $ "Unable to find any feed for this chat."
evalTgAct uid (GetSubchannelSettings channel_id) _ = evalTgAct uid GetSubchatSettings channel_id
evalTgAct _ GetSubchatSettings cid = ask >>= liftIO . readMVar . subs_state >>= \hmap ->
    case HMS.lookup cid hmap of
        Nothing -> pure . Left $ NotFoundChat
        Just ch -> pure . Right . ServiceReply . render $ ch
evalTgAct _ ListSubs cid = do
    env <- ask
    chats_hmap <- liftIO . readMVar $ subs_state env
    case HMS.lookup cid chats_hmap of
        Nothing -> pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
        Just c ->
            let subs = sort . S.toList . sub_feeds_links $ c
            in  if null subs then pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
                else do
                feeds_hmap <- liftIO (readMVar $ feeds_state env)
                case traverse (`HMS.lookup` feeds_hmap) subs of
                    Nothing -> pure . Left . NotFoundFeed $
                        "Unable to find these feeds " `T.append` T.intercalate " " subs
                    Just feeds -> pure . Right $ toReply (FromChatFeeds c feeds) (Just $ sub_settings c)
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
            if not ok then pure . Right . ServiceReply $ 
                "Not authorized. Make sure you have admin permissions \
                \ in both the origin and the destination."
            else migrate >>= \case
                Left err -> restore >> onErr err
                Right _ -> onOk
    where
        migrate = sequence <$> sequence [withChat (Migrate to) cid, withChat Purge cid]
        restore = withChat (Migrate cid) to >> withChat Purge to
        onOk = pure . Right . ServiceReply $
            "Successfully migrated " `T.append`
            (T.pack . show $ cid) `T.append`
            " to " `T.append` (T.pack . show $ to)
        onErr err = pure . Right . ServiceReply $ renderUserError err
evalTgAct uid (MigrateChannel fr to) _ = evalTgAct uid (Migrate to) fr
evalTgAct uid (Pause pause_or_resume) cid = ask >>= \env ->
    let tok = bot_token . tg_config $ env in
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat (Pause pause_or_resume) cid >>= \case
                Left err -> pure . Right . ServiceReply . renderUserError $ err
                Right _ -> pure . Right . ServiceReply $ succeeded
    where
        succeeded =
            if pause_or_resume then "All notifications to this chat are now suspended. Use /resume to resume."
            else "Resuming notifications. This chat is receiving messages again."
evalTgAct uid (PauseChannel chan_id pause_or_resume) _ = evalTgAct uid (Pause pause_or_resume) chan_id
evalTgAct uid Purge cid = do
    env <- ask
    let tok = bot_token . tg_config $ env
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat Purge cid >>= \case
                Left err -> pure . Right . ServiceReply $ "Unable to purge this chat"  `T.append` renderUserError err
                Right _ -> pure . Right . ServiceReply $ "Successfully purged the chat from the database."
evalTgAct uid (PurgeChannel chan_id) _ = evalTgAct uid Purge chan_id
evalTgAct uid (Sub feeds_urls) cid = do
    env <- ask
    chats <- liftIO . readMVar $ subs_state env
    let tok = bot_token . tg_config $ env
    -- fails if 50 feeds subscribed to already
    if tooManySubs 50 chats cid then exitTooMany
    else checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else subFeed env cid feeds_urls
    where
        exitTooMany = pure . Left . MaxFeedsAlready $ "As of now, chats are not allowed to subscribe to more than 25 feeds."
evalTgAct _ RenderCmds _ = pure . Right $ toReply FromStart Nothing
evalTgAct uid Reset cid = do
    env <- ask
    let tok = bot_token . tg_config $ env
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat Reset cid >>= \case
                Left err -> pure . Right . ServiceReply $ renderUserError err
                Right _ -> pure . Right . ServiceReply $ "Chat settings set to defaults."
evalTgAct uid (ResetChannel chan_id) _ = evalTgAct uid Reset chan_id
evalTgAct _ (Search keywords) cid = ask >>= \env ->
    let get_chats = liftIO . readMVar $ subs_state env
    in  get_chats >>= \hmap -> case HMS.lookup cid hmap of
        Nothing -> pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
        Just c ->
            let scope = S.toList . sub_feeds_links $ c
            in  if null scope
                then pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet. Subscribe to a feed to be able to search its items."
                else evalDb env (DbSearch (S.fromList keywords) (S.fromList scope)) >>= \case
                    DbSearchRes keys sc -> pure . Right $ toReply (FromSearchRes keys sc) Nothing
                    _ -> pure . Left . BadInput $ "The database was not able to run your query."
evalTgAct uid (SetChannelSettings chan_id settings) _ = ask >>= \env ->
    let tok = bot_token . tg_config $ env in
    checkIfAdmin tok uid chan_id >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat (SetChatSettings settings) chan_id >>= \case
                Left err -> pure . Right . ServiceReply $ "Unable to udpate this chat settings" `T.append` renderUserError err
                Right (ChatUpdated c) -> pure . Right . toReply (FromChat c) $ Nothing
                _ -> pure . Left . BadInput $ "Unable to update the settings for this chat. Please try again later."
evalTgAct uid (SetChatSettings settings) cid = ask >>= \env ->
    let tok = bot_token . tg_config $ env in
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat (SetChatSettings settings) cid >>= \case
                Left err -> pure . Right . ServiceReply $ "Unable to udpate this chat settings" `T.append` renderUserError err
                Right _ -> pure . Right . ServiceReply $ "Settings applied successfully."
evalTgAct uid (SubChannel chan_id urls) _ = ask >>= \env ->
    let tok = bot_token . tg_config $ env
        jobs = postjobs env
    in  checkIfAdmin tok uid chan_id >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then pure . Left . NotAdmin $ "Only users with administrative rights in the target channel is allowed to link the bot to the channel."
            else testChannel tok chan_id jobs >>= \case
                Left err -> pure . Left $ err
                Right _ -> subFeed env chan_id urls
evalTgAct uid (UnSub feeds) cid = ask >>= \env ->
    checkIfAdmin (bot_token . tg_config $ env) uid cid >>= \case
        Nothing -> pure . Right . ServiceReply $ "Error occured when requesting Telegram. Try again."
        Just is_admin ->
            if not is_admin then exitNotAuth uid
            else withChat (UnSub feeds) cid >>= \case
            Left err -> pure . Left $ err
            Right _ -> pure . Right . ServiceReply $ "Successfully unsubscribed from " `T.append` T.intercalate " " (unFeedRefs feeds)
evalTgAct uid (UnSubChannel chan_id feeds) _ = evalTgAct uid (UnSub feeds) chan_id
