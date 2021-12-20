{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AppServer (startApp, registerWebhook, makeConfig) where

import AppTypes
import Backend
import Control.Concurrent (newChan, newMVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database (getValidCreds, initMongoCredsFrom)
import Jobs
import Network.Wai
import Network.Wai.Handler.Warp
import Replies (reply)
import Responses
import Servant
import System.Environment
import TgActions
import TgramInJson (Message (chat, from, text), Update (message), User (user_id), chat_id)
import TgramOutJson (ChatId, UserId)
import Search (initSearchWith)

type BotAPI =
    Get '[JSON] ServerResponse :<|>
    "webhook" :> Capture "secret" T.Text :> ReqBody '[JSON] Update :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy

server :: MonadIO m => ServerT BotAPI (App m)
server = root :<|> handleWebhook    where

    handleWebhook :: MonadIO m => T.Text -> Update -> App m ()
    handleWebhook secret update = ask >>= \env ->
        let tok = bot_token . tg_config $ env
        in  if EQ == compare secret tok
            then handle update tok
            else liftIO $ putStrLn "Secrets do not match."
        where
            handle upd tok = case message upd of
                Nothing -> liftIO $ putStrLn "Failed to parse message"
                Just msg ->
                    let cid = chat_id . chat $ msg :: ChatId
                        uid = user_id . fromJust . from $ msg :: UserId
                        finishWith err t c = reply t c . PlainReply $ renderUserError err
                    in  case text msg of
                        Nothing -> liftIO . putStrLn $ "Suppressed message:" ++ show msg
                        Just contents -> case interpretCmd contents of
                            Left err -> finishWith err tok cid
                            Right action -> evalTgAct uid action cid >>= \case
                                Left err -> finishWith err tok cid
                                Right r -> reply tok cid r

    root :: MonadIO m => App m ServerResponse
    root = pure $ RespOk "ok" "testing"

initServer :: AppConfig -> Server BotAPI
initServer config = hoistServer botApi (runApp config) server

withServer :: AppConfig -> Application
withServer = serve botApi . initServer

makeConfig :: [(String, String)] -> IO (Int, AppConfig, Maybe [T.Text])
makeConfig env = do
    let token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
        alert_chat_id = read . fromJust $ lookup "ALERT_CHATID" env
        webhook = 
            let raw = T.pack . fromJust $ lookup "WEBHOOK_URL" env
            in  if T.last raw == T.last "/" then T.dropEnd 1 raw else raw
        creds_line = T.pack . fromJust $ lookup "MONGODB_SHARDS" env
        (first_shard:_) = T.splitOn ";" creds_line
        creds = initMongoCredsFrom first_shard creds_line
        port = maybe 80 read $ lookup "PORT" env
        interval = maybe 1200000000 read $ lookup "WORKER_INTERVAL" env
        starting_feeds = (Just . T.splitOn "," . T.pack) =<< lookup "STARTING_FEEDS" env
    mvar1 <- newMVar HMS.empty
    mvar2 <- newMVar HMS.empty
    mvar3 <- newEmptyMVar
    chan <- newChan
    valid_creds <- getValidCreds creds 
    creds_ref <- newIORef valid_creds
    pure (port, AppConfig {
        tg_config = ServerConfig {bot_token = token, webhook_url = webhook, alert_chat = alert_chat_id},
        last_worker_run = Nothing,
        feeds_state = mvar1,
        subs_state = mvar2,
        tasks_queue = chan,
        worker_interval = interval,
        db_config = creds_ref,
        search_engine = mvar3
        }, starting_feeds)

initStart :: AppConfig -> Maybe [T.Text] -> IO ()
initStart config mb_urls = case mb_urls of
    Nothing -> runApp config startup
    Just urls -> do
        putStrLn "Found urls. Trying to build feeds..."
        runApp config $ evalFeedsAct (InitF urls) >> startup
    where 
        startup = evalFeedsAct LoadF >> initSearchEngine >> loadChats >> runRefresh >> runJobs
        initSearchEngine = 
            liftIO $ readMVar (feeds_state config) >>= \fs ->
            putMVar (search_engine config) (initSearchWith $ HMS.elems fs) >>
            putStrLn "Search engine initialized."
        
startApp :: IO ()
startApp = do
    env <- getEnvironment
    (port, config, feeds_urls) <- makeConfig env
    concurrently_ (registerWebhook config) (initStart config feeds_urls)
    print $ "Server now istening to port " `T.append`(T.pack . show $ port)
    run port . withServer $ config