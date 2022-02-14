{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppServer (startApp, registerWebhook, makeConfig) where
import AppTypes
import Backend
import Control.Concurrent (newChan, newMVar)
import Control.Exception (throwIO)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database (checkDbMapper, initConnectionMongo)
import HtmlViews
import Jobs
import Network.Wai
import Network.Wai.Handler.Warp
import Requests (reply)
import Servant
import Servant.HTML.Blaze
import System.Environment (getEnvironment)
import Text.Blaze
import TgActions
import TgramInJson (Message (chat, from, reply_to_message, text), Update (message), User (user_id), chat_id)
import TgramOutJson (ChatId, UserId)

type BotAPI =
    Get '[HTML] Markup :<|>
    "webhook" :> Capture "secret" T.Text :> ReqBody '[JSON] Update :> Post '[JSON] () :<|>
    "digests" :> Capture "digest_id" T.Text :> Get '[HTML] Markup :<|>
    "view" :> QueryParam "flinks" T.Text :> QueryParam "from" T.Text :> QueryParam "to" T.Text :> Get '[HTML] Markup

botApi :: Proxy BotAPI
botApi = Proxy

server :: MonadIO m => ServerT BotAPI (App m)
server = home :<|> handleWebhook :<|> viewDigests :<|> viewSearchRes where

    handleWebhook :: MonadIO m => T.Text -> Update -> App m ()
    handleWebhook secret update = ask >>= \env ->
        if      EQ == compare secret (tok env)
        then    handle update env
        else    liftIO $ putStrLn "Secrets do not match."
        where
            tok = bot_token . tg_config
            finishWith env cid err = reply (tok env) cid (ServiceReply $ renderUserError err) (postjobs env)
            handle upd env = case message upd of
                Nothing -> liftIO $ putStrLn "Failed to parse message"
                Just msg ->
                    let cid = chat_id . chat $ msg :: ChatId
                        uid = user_id . fromJust . from $ msg :: UserId
                    in  case reply_to_message msg of
                        Just _ -> pure ()
                        Nothing -> case TgramInJson.text msg of
                            Nothing -> pure ()
                            Just conts -> case interpretCmd conts of
                                Left (Ignore _) -> pure ()
                                Left err -> finishWith env cid err
                                Right action -> evalTgAct uid action cid >>= \case
                                    Left err -> finishWith env cid err
                                    Right r -> reply (tok env) cid r (postjobs env)

initServer :: AppConfig -> Server BotAPI
initServer config = hoistServer botApi (runApp config) server

withServer :: AppConfig -> Application
withServer = serve botApi . initServer

makeConfig :: [(String, String)] -> IO (AppConfig, Int, Maybe [T.Text])
makeConfig env =
    let token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
        alert_chat_id = read . fromJust $ lookup "ALERT_CHATID" env
        webhook =
            let raw = T.pack . fromJust $ lookup "WEBHOOK_URL" env
            in  if T.last raw == T.last "/" then T.dropEnd 1 raw else raw
        connection_string = T.pack . fromJust $ lookup "MONGODB_CONNECTION_STRING" env
        [hn, db, un, pass] = T.splitOn ":" connection_string
        creds = MongoCredsReplicaSrv {
            host_name = T.unpack hn,
            database_name = db,
            user_name = un,
            password = pass
        }
        port = maybe 80 read $ lookup "PORT" env
        interval = maybe 60000000 read $ lookup "WORKER_INTERVAL" env
        starting_feeds = (Just . T.splitOn "," . T.pack) =<< lookup "STARTING_FEEDS" env
    in do
    mvar1 <- newMVar HMS.empty
    mvar2 <- newMVar HMS.empty
    chan <- newChan
    pipe_ref <- initConnectionMongo creds >>= \case
        Left err -> throwIO . userError $ T.unpack $ renderDbError err
        Right pipe -> newIORef pipe
    pure (AppConfig {
        tg_config = ServerConfig {bot_token = token, webhook_url = webhook, alert_chat = alert_chat_id},
        last_worker_run = Nothing,
        feeds_state = mvar1,
        subs_state = mvar2,
        postjobs = chan,
        worker_interval = interval,
        db_config = creds,
        db_connector = pipe_ref
        }, port, starting_feeds)

initStart :: AppConfig -> Maybe [T.Text] -> IO ()
initStart config mb_urls = case mb_urls of
    Nothing -> runApp config startup
    Just urls -> do
        putStrLn "Found urls. Trying to build feeds..."
        runApp config $ evalFeeds (InitF urls) >> startup
    where
        startup = evalFeeds LoadF >> loadChats >> procNotif >> postProcJobs

startApp :: IO ()
startApp = do
    checkDbMapper
    env <- getEnvironment
    (config, port, feeds_urls) <- makeConfig env
    registerWebhook config
    initStart config feeds_urls
    print $ "Server now istening to port " `T.append`(T.pack . show $ port)
    run port . withServer $ config