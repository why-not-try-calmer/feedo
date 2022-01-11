{-# LANGUAGE LambdaCase #-}

module TgActions where

import AppTypes
import Backend (evalFeedsAct, withChat)
import Control.Concurrent (readMVar, writeChan, Chan)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sort)
import qualified Data.Set as S
import qualified Data.Text as T
import Network.HTTP.Req (renderUrl, responseBody)
import Parser (eitherUrlScheme, getFeedFromHref)
import Replies (render, toReply)
import Requests (reqSend, setWebhook)
import Search (searchWith)
import Text.Read (readMaybe)
import TgramInJson
import TgramOutJson
import Utils (maybeUserIdx, parseSettings, partitionEither, tooManySubs)

registerWebhook :: AppConfig -> IO ()
registerWebhook config =
    let tok = bot_token . tg_config $ config
        webhook = webhook_url . tg_config $ config
    in  putStrLn "Trying to set webhook" >> setWebhook tok webhook >>
            print ("Webhook successfully set at " `T.append` webhook)

checkIfAdmin :: MonadIO m => BotToken -> UserId -> ChatId -> m (Maybe Bool)
checkIfAdmin tok uid cid = do
    resp_chat_admins <- getChatAdmins
    case resp_chat_admins of
        Left _ -> pure Nothing
        Right res ->
            let chat_resp = responseBody res :: TgGetChatResponse
                c = resp_result chat_resp :: Chat
            in  if chat_type c == Private then pure. Just $ True else do
                resp_chat_type <- getChatType
                case resp_chat_type of
                    Left _ -> pure Nothing
                    Right res_chat_admins ->
                        let res_cms = responseBody res_chat_admins :: TgGetChatMembersResponse
                            chat_members = resp_cm_result res_cms :: [ChatMember]
                        in  pure . Just . if_admin $ chat_members
    where
        getChatType = reqSend tok "getChatAdministrators" $ GetChatAdministrators cid
        getChatAdmins = reqSend tok "getChat" $ GetChatAdministrators cid
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
        if length args /= 1 then Left . BadInput $ "/about takes exactly 1 argument, standing for the url or # of the feed to get information about."
        else case toFeedRef args of
            Left err -> Left err
            Right ref -> Right . About $ head ref
    | cmd == "/fresh" =
        if length args /= 1 then Left . BadInput $ "/fresh takes exactly 1 argument, standing for number of days."
        else case readMaybe . T.unpack . head $ args :: Maybe Int of
            Nothing -> Left . BadInput $ "/fresh's argument must be a valid integer."
            Just i -> Right $ GetLastXDaysItems i
    | cmd == "/items" || cmd == "/i" =
        if length args /= 1 then Left . BadInput $ "/items needs exactly 1 argument, standing for the url or # of the feed to get items from."
        else case toFeedRef args of
            Left err -> Left err
            Right single_ref -> Right . GetItems . head $ single_ref
    | cmd == "/list" || cmd == "/l" =
        if not . null $ args then Left . BadInput $ "/list takes no argument."
        else Right ListSubs
    | cmd == "/listchan" || cmd == "/lchan" =
        if length args /= 1 then Left . BadInput $ "/listchan takes exactly one argument, standing for the chat_id of the target channel."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /listchan could not be parsed into a valid chat_id."
            Just chan_id -> Right . ListSubsChannel $ chan_id
    | cmd == "/pause" || cmd == "/p" = Right . Pause $ True
    | cmd == "/pausechan" =
        if length args /= 1 then Left . BadInput $ "/pausechan takes exactly one argument, standing for the chat_id of the target channel."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /pausechan could not be parsed into a valid chat_id."
            Just chan_id -> Right . PauseChannel chan_id $ True
    | cmd == "/purge" = if not . null $ args then Left . BadInput $ "/purge takes no argument." else Right Purge
    | cmd == "/purgechan" = 
        if length args /= 1 then Left . BadInput $ "/purgechan takes exactly one argument, standing for the target channel id."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /purgechan could not be parsed into a valid chat_id."
            Just chan_id -> Right . PurgeChannel $ chan_id 
    | cmd == "/resume" = Right . Pause $ False
    | cmd == "/resumechan" =
        if length args /= 1 then Left . BadInput $ "/resumechan takes exactly one argument, standing for the chat_id of the target channel."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /resumechan could not be parsed into a valid chat_id."
            Just chan_id -> Right . PauseChannel chan_id $ False 
    | cmd == "/reset" = Right Reset
    | cmd == "/resetchan" =
        if length args /= 1 then Left . BadInput $ "/resetchan takes exactly one argument, standing for the target channel id."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first value passed to /resetchan could not be parsed into a valid chat_id."
            Just chan_id -> Right . ResetChannel $ chan_id
    | cmd == "/search" || cmd == "/se" =
        if null args then Left . BadInput $ "/search requires at least one keyword. Separate keywords with a space."
        else Right . Search $ args
    | cmd == "/set" =
        if null args then Right GetSubFeedSettings else
        let body = tail . T.lines . T.toLower $ contents
        in  case parseSettings body of
            Left err -> Left . BadInput $ err
            Right settings -> Right $ SetChatSettings settings
    | cmd == "/setchan" =
        if length args < 2 then Left . BadInput $ "/settings_channel takes a string of parameters for connecting the bot to a channel where it was invited." else
        let body = tail . T.lines . T.toLower $ contents
        in  case readMaybe . T.unpack $ head args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "/settings needs at least 2 arguments, and the first one stands for the id of the target channel. I coulnd't find the first argument."
            Just channel_id -> case parseSettings body of
                Left err -> Left . BadInput $ err
                Right settings -> Right $ SetChannelSettings channel_id settings
    | cmd == "/start" || cmd == "/help" = Right RenderCmds
    | cmd == "/sub" || cmd == "/s" =
        if null args then Left . BadInput $ "/sub needs at least 1 argument, standing for the url of the feed to subscribe to."
        else Right . Sub $ args
    | cmd == "/subchan" =
        if length args < 2 then Left . BadInput $ "/subchan at least 2 arguments, the first standing for the id of the target channel, and another argument for the target web feed (url)."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "/subchan's first argument must be a valid integer, positive or negative."
            Just n -> Right $ SubChannel n (tail args)
    | cmd == "/unsub" =
        if null args then Left . BadInput $ "/unsub needs at least 1 argument, standing for the url or # of the feed to unsubscribe from."
        else case toFeedRef args of
            Left err -> Left err
            Right refs -> Right . UnSub $ refs
    | cmd == "/unsubchan" =
        if length args < 2 then Left . BadInput $ "/unsubchan takes at least two arguments, the chat_id of the target channel and either a # or full url of the target feed."
        else case readMaybe . T.unpack . head $ args :: Maybe ChatId of
            Nothing -> Left . BadInput $ "The first argument of /unsubchan must be a valid chat_id."
            Just n -> case toFeedRef . tail $ args of
                Left err -> Left err
                Right refs -> 
                    if abs n < 10000 then Left . BadInput $ "Are you sure you passed a valid chat_id for the target channel?"
                    else Right $ UnSubChannel n refs
    | otherwise = Left Ignore
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
        in  if not (resp_msg_ok res) then pure $ Left TelegramErr
            else do
                -- tries to remove the test message
                liftIO $ writeChan jobs . JobRemoveMsg chan_id mid $ Nothing
                pure $ Right ()

subFeed :: MonadIO m => AppConfig -> ChatId -> [T.Text] -> App m (Either UserError Reply)
subFeed env cid feeds_urls = liftIO (readMVar . feeds_state $ env) >>= \known_feeds ->
    -- check url scheme
    case traverse eitherUrlScheme feeds_urls of
    Left err -> pure . Left $ err
    Right valid_urls ->
        -- sort out already existent feeds to minimize network call
        -- and subscribe chat to them
        let urls = map renderUrl valid_urls
            old_keys = HMS.keys $ HMS.filter (\f -> f_link f `elem` urls) known_feeds
            new_keys = filter (`notElem` old_keys) urls
        in  unless (null old_keys) (void $ withChat (Sub old_keys) cid) >>
            if null new_keys
            then pure . Right . ServiceReply $ "Successfully subscribed to " `T.append` T.intercalate ", " old_keys
            -- fetches feeds at remaining urls
            else liftIO (mapConcurrently getFeedFromHref new_keys) >>= \res ->
                -- add feeds
                let (failed, built_feeds) = partitionEither res
                in  evalFeedsAct (AddF built_feeds) >>= \case
                    FeedsOk ->
                        -- subscribes chat to newly added feeds, returning result to caller
                        let to_sub_to = map f_link built_feeds
                        in  withChat (Sub to_sub_to) cid >>= \case
                        Left err -> pure . Right . ServiceReply $ renderUserError err
                        Right _ ->
                            let failed_text = ". Failed to subscribe to these feeds: " `T.append` T.intercalate " " failed
                                ok_text = "Added and subscribed to these feeds: " `T.append` T.intercalate " " (map f_link built_feeds)
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
evalTgAct _ Changelog _ =  pure . Right $ toReply FromChangelog Nothing
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
            else evalFeedsAct (GetAllXDays subscribed n) >>= \case
                FeedLinkBatch feeds -> do
                    liftIO $ writeChan (postjobs env) (JobIncReadsJob $ map fst feeds)
                    pure . Right $ toReply (FromFeedLinkItems feeds) (Just $ sub_settings c)
                _ -> pure . Right . ServiceReply $ "Unable to find any feed for this chat."
evalTgAct _ GetSubFeedSettings cid = ask >>= liftIO . readMVar . subs_state >>= \hmap ->
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
                Left _ -> pure . Right . ServiceReply $ "Unable to purge this chat. It seems like there's no trace of it in the database."
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
        search_from_subs subs =
            liftIO (readMVar $ search_engine env) >>= \(kitems, engine) ->
                let results = searchWith kitems keywords engine
                in  pure $ foldl' (\acc kitem ->
                        let i = item kitem
                        in  if i_feed_link i `elem` subs then acc ++ [i]
                            else acc) [] results
    in  get_chats >>= \hmap -> case HMS.lookup cid hmap of
        Nothing -> pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet!"
        Just c ->
            let subs = S.toList . sub_feeds_links $ c
            in  if null subs
                then pure . Right . ServiceReply $ "This chat is not subscribed to any feed yet. Search applies only to items of which to which you are subscribed."
                else search_from_subs subs >>= \res -> pure . Right $ toReply (FromSearchRes res) Nothing
evalTgAct uid (SetChannelSettings chan_id settings) _ = ask >>= \env ->
    let tok = bot_token . tg_config $ env in
    checkIfAdmin tok uid chan_id >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat (SetChatSettings settings) chan_id >>= \case
                Left _ -> pure . Right . ServiceReply $ "Unable to udpate this chat settings"
                Right _ -> pure . Right . ServiceReply $ "Settings applied successfully."
evalTgAct uid (SetChatSettings settings) cid = ask >>= \env ->
    let tok = bot_token . tg_config $ env in
    checkIfAdmin tok uid cid >>= \case
        Nothing -> pure . Left $ TelegramErr
        Just verdict ->
            if not verdict then exitNotAuth uid
            else withChat (SetChatSettings settings) cid >>= \case
                Left _ -> pure . Right . ServiceReply $ "Unable to udpate this chat settings"
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
