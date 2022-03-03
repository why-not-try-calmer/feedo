{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module AppServer (startApp, registerWebhook, makeConfig) where
import AppTypes
import Backend
import Control.Concurrent (newChan, newMVar)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Mongo (setupMongo)
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
import Web
import Redis (setupRedis)
import Broker (withCache)
import Control.Exception (throwIO)
import Data.IORef (newIORef)

type BotAPI =
    Get '[HTML] Markup :<|>
    "webhook" :> Capture "secret" T.Text :> ReqBody '[JSON] Update :> Post '[JSON] () :<|>
    "digests" :> Capture "digest_id" T.Text :> Get '[HTML] Markup :<|>
    "view" :> QueryParam "flinks" T.Text :> QueryParam "from" T.Text :> QueryParam "to" T.Text :> Get '[HTML] Markup :<|>
    "read_settings" :> ReqBody '[JSON] ReadReq :> Post '[JSON] ReadResp :<|>
    "write_settings" :> ReqBody '[JSON] WriteReq :> Post '[JSON] WriteResp :<|>
    "settings" :> Raw

botApi :: Proxy BotAPI
botApi = Proxy

server :: MonadIO m => ServerT BotAPI (App m)
server = 
    home :<|> 
    handleWebhook :<|>
    viewDigests :<|> 
    viewSearchRes :<|> 
    readSettings :<|>
    writeSettings :<|> 
    staticSettings where

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

    staticSettings :: MonadIO m => ServerT Raw m
    staticSettings = serveDirectoryWebApp "/var/www/feedfarer-webui"
    
initServer :: AppConfig -> Server BotAPI
initServer config = hoistServer botApi (runApp config) server

withServer :: AppConfig -> Application
withServer = serve botApi . initServer

makeConfig :: [(String, String)] -> IO (AppConfig, Int)
makeConfig env =
    let token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
        alert_chat_id = read . fromJust $ lookup "ALERT_CHATID" env
        base = T.pack . fromJust $ lookup "BASE_URL" env
        webhook =
            let raw = T.pack . fromJust $ lookup "WEBHOOK_URL" env
            in  if T.last raw == T.last "/" then T.dropEnd 1 raw else raw
        mongo_connection_string = fromJust $ lookup "MONGO_CONN_STRING" env
        port = maybe 80 read $ lookup "PORT" env
        interval = maybe 60000000 read $ lookup "WORKER_INTERVAL" env
    in do
    mvar <- newMVar HMS.empty
    chan <- newChan
    conn <- setupRedis
    (pipe, creds) <- setupMongo mongo_connection_string >>= \case
        Left _ -> throwIO . userError $ "Failed to produce a valid Mongo pipe."
        Right p -> pure p
    pipe_ioref <- newIORef pipe
    pure (AppConfig {
        tg_config = ServerConfig {bot_token = token, webhook_url = webhook, alert_chat = alert_chat_id},
        base_url = base,
        mongo_creds = creds,
        last_worker_run = Nothing,
        subs_state = mvar,
        postjobs = chan,
        worker_interval = interval,
        connectors = (conn, pipe_ioref)
        }, port)

initStart :: MonadIO m => App m ()
initStart = withCache CacheWarmup >>= \case
    Left msg -> report msg >> continue
    _ -> continue
    where 
        report msg = liftIO $ do
            print $ "Unable to warm up cache: " `T.append` msg
            putStrLn "Proceededing nonetheless"
        continue = loadChats >> procNotif >> postProcJobs

startApp :: IO ()
startApp = do
    env <- getEnvironment
    (config, port) <- makeConfig env
    registerWebhook config
    runApp config initStart
    print $ "Server now listening to port " `T.append`(T.pack . show $ port)
    run port . withServer $ config