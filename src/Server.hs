{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Server (startApp, makeConfig) where

import Control.Concurrent (newChan, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Jobs
import Mongo (setupDb)
import Network.Wai
import Network.Wai.Handler.Warp
import Redis (setUpKeyStore)
import Replies (render)
import Requests (alertAdmin, reply)
import Servant
import Servant.HTML.Blaze
import System.Environment (getEnvironment)
import Text.Blaze
import TgActions
import TgramInJson (Message (chat, from, reply_to_message, text), Update (callback_query, message), User (user_id), chat_id)
import Types
import Web

type BotAPI =
  Get '[HTML] Markup
    :<|> "webhook" :> Capture "secret" T.Text :> ReqBody '[JSON] Update :> Post '[JSON] ()
    :<|> "digests" :> Capture "digest_id" T.Text :> Get '[HTML] Markup
    :<|> "view" :> QueryParam "flinks" T.Text :> QueryParam "from" T.Text :> QueryParam "to" T.Text :> Get '[HTML] Markup
    :<|> "read_settings" :> ReqBody '[JSON] ReadReq :> Post '[JSON] ReadResp
    :<|> "write_settings" :> ReqBody '[JSON] WriteReq :> Post '[JSON] WriteResp
    :<|> "settings" :> Raw

botApi :: Proxy BotAPI
botApi = Proxy

server :: (MonadIO m) => ServerT BotAPI (App m)
server =
  home
    :<|> handleWebhook
    :<|> viewDigests
    :<|> viewSearchRes
    :<|> readSettings
    :<|> writeSettings
    :<|> staticSettings
 where
  handleWebhook :: (MonadIO m) => T.Text -> Update -> App m ()
  handleWebhook secret update = do
    env <- ask
    let tok = bot_token . tg_config $ env
    case compare secret tok of
      EQ -> accept env
      _ -> decline
   where
    decline = liftIO $ putStrLn "Secrets do not match."
    accept env = do
      res <- liftIO . try $ runApp env $ handle update
      case res of
        Left (SomeException err) ->
          let alert = "Exception thrown against handler: " `T.append` (T.pack . show $ err)
           in alertAdmin (postjobs env) alert
        Right _ -> pure ()
    handle upd = case callback_query upd of
      Just cbq -> processCbq cbq
      Nothing -> proc_msg (message upd)
    proc_msg msg = case msg of
      Nothing -> liftIO $ putStrLn "Failed to parse message"
      Just inboundMsg -> proc_rep inboundMsg
    proc_rep msg = case reply_to_message msg of
      Just _ -> pure ()
      Nothing -> proc_contents msg
    proc_contents msg = for_ (TgramInJson.text msg) (proc_cmd msg)
    proc_cmd msg txt = case interpretCmd txt of
      Left (UnknownCommand no_command _) ->
        let logline = "Not a command: " ++ T.unpack no_command
         in liftIO . putStrLn $ logline
      Left err -> sendErrorAsServiceReply err
      Right action ->
        evalTgAct uid action cid >>= \case
          Left err -> sendErrorAsServiceReply err
          Right outboundMsg -> reply cid outboundMsg
     where
      cid = chat_id . chat $ msg
      uid = user_id . fromJust . from $ msg
      sendErrorAsServiceReply err = reply cid (ServiceReply $ render err)

  staticSettings :: (MonadIO m) => ServerT Raw m
  staticSettings = serveDirectoryWebApp "/var/www/feedfarer-webui"

initServer :: AppConfig -> Server BotAPI
initServer config = hoistServer botApi (runApp config) server

withServer :: AppConfig -> Application
withServer = serve botApi . initServer

makeConfig :: [(String, String)] -> IO AppConfig
makeConfig env =
  let alert_chat_id = read . fromJust $ lookup "ALERT_CHATID" env
      base = T.pack . fromJust $ lookup "BASE_URL" env
      (dbName, dbServiceName) = case lookup "TEST" env of
        Just "1" -> ("feedfarer-test", "mongo_test")
        _ -> ("feedfarer", "mongo")
      creds =
        let [user, pwd] = T.pack . fromJust . flip lookup env <$> ["MONGO_INITDB_ROOT_USERNAME", "MONGO_INITDB_ROOT_PASSWORD"]
         in MongoCredsServer dbServiceName dbName user pwd
      token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
      interval = maybe 60000000 read $ lookup "WORKER_INTERVAL" env
      version = T.pack . fromMaybe "(unspecified)" $ lookup "APP_VERSION" env
   in do
        chan <- newChan
        conn <-
          setUpKeyStore >>= \case
            Left err -> throwIO . userError $ T.unpack err
            Right c -> putStrLn "Redis...OK" >> pure c
        (pipe, connected_creds) <-
          liftIO $
            setupDb creds
              >>= \case
                Left _ -> throwIO . userError $ "Failed to produce a valid Mongo pipe."
                Right p -> putStrLn "Mongo...OK" >> pure p
        pipe_ioref <- newIORef pipe
        pure
          AppConfig
            { app_version = version
            , tg_config = ServerConfig{bot_token = token, alert_chat = alert_chat_id}
            , base_url = base
            , mongo_creds = connected_creds
            , postjobs = chan
            , worker_interval = interval
            , connectors = (conn, pipe_ioref)
            }

initStart :: AppConfig -> IO ()
initStart env = runApp env $ do
  startJobs
  liftIO $ putStrLn "jobs queue started"
  startNotifs
  liftIO $ do
    putStrLn "digests / follows queue started"
    writeChan (postjobs env) $ JobTgAlertAdmin "Feedo just started."

startApp :: IO ()
startApp = do
  env <- getEnvironment
  config <- makeConfig env
  initStart config
  putStrLn $ "Startup completed for version " ++ T.unpack (app_version config)
  putStrLn $ "Running now using " ++ show port
  run port $ withServer config
 where
  port = 8000
