{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppServer (startApp, registerWebhook, makeConfig) where
import AppTypes
import Backend
import Control.Concurrent (newChan, newEmptyMVar, newMVar)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Jobs
import Network.Wai
import Network.Wai.Handler.Warp
import Requests (reply)
import Responses
import Servant
import System.Environment (getEnvironment)
import TgActions
import TgramInJson (Message (chat, from, reply_to_message, text), Update (message), User (user_id), chat_id)
import TgramOutJson (ChatId, UserId)
import Database (initConnectionMongo)
import Control.Exception (throwIO)

type BotAPI =
    Get '[JSON] ServerResponse :<|>
    "webhook" :> Capture "secret" T.Text :> ReqBody '[JSON] Update :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy

server :: MonadIO m => ServerT BotAPI (App m)
server = root :<|> handleWebhook    where

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
                        Nothing -> case text msg of
                            Nothing -> pure ()
                            Just contents -> case interpretCmd contents of
                                Left (Ignore _) -> pure ()
                                Left err -> finishWith env cid err
                                Right action -> evalTgAct uid action cid >>= \case
                                    Left err -> finishWith env cid err
                                    Right r -> reply (tok env) cid r (postjobs env)

    root :: MonadIO m => App m ServerResponse
    root = pure $ RespOk "ok" "testing"

initServer :: AppConfig -> Server BotAPI
initServer config = hoistServer botApi (runApp config) server

withServer :: AppConfig -> Application
withServer = serve botApi . initServer

makeConfig :: [(String, String)] -> IO (AppConfig, Int, Maybe [T.Text])
makeConfig env = do
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
        interval = maybe 1200000000 read $ lookup "WORKER_INTERVAL" env
        starting_feeds = (Just . T.splitOn "," . T.pack) =<< lookup "STARTING_FEEDS" env
    mvar1 <- newMVar HMS.empty
    mvar2 <- newMVar HMS.empty
    mvar3 <- newEmptyMVar
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
        db_connector = pipe_ref,
        search_engine = mvar3
        }, port, starting_feeds)

initStart :: AppConfig -> Maybe [T.Text] -> IO ()
initStart config mb_urls = case mb_urls of
    Nothing -> runApp config startup
    Just urls -> do
        putStrLn "Found urls. Trying to build feeds..."
        runApp config $ evalFeedsAct (InitF urls) >> startup
    where 
        startup = evalFeedsAct LoadF >> loadChats >> refresher >> postProcJobs
        
startApp :: IO ()
startApp = do
    env <- getEnvironment
    (config, port, feeds_urls) <- makeConfig env
    registerWebhook config
    initStart config feeds_urls
    print $ "Server now istening to port " `T.append`(T.pack . show $ port)
    run port . withServer $ config