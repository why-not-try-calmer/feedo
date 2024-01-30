{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Server (startApp, registerWebhook, makeConfig) where

import Control.Concurrent (newChan, newMVar, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (newIORef)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Jobs
import Mem
import Mongo (HasMongo (evalDb), setupDb)
import Network.Wai
import Network.Wai.Handler.Warp
import Notifications (alertAdmin)
import Redis (setUpKeyStore)
import Requests (reply)
import Servant
import Servant.HTML.Blaze
import System.Environment (getEnvironment)
import Text.Blaze
import TgActions
import TgramInJson (Message (chat, from, reply_to_message, text), Update (callback_query, message), User (user_id), chat_id)
import Types
import Utils (renderUserError)
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
    let tok = bot_token . tg_config
        sendReply cid err = reply (tok env) cid (ServiceReply $ renderUserError err) (postjobs env)
        handle upd = case callback_query upd of
          Just cbq -> processCbq cbq
          Nothing -> case message upd of
            Nothing -> liftIO $ putStrLn "Failed to parse message"
            Just inboundMsg ->
              let cid = chat_id . chat $ inboundMsg
                  uid = user_id . fromJust . from $ inboundMsg
               in case reply_to_message inboundMsg of
                    Just _ -> pure () -- ignoring replies
                    Nothing -> case TgramInJson.text inboundMsg of
                      Nothing -> pure () -- ignoring empty contents
                      Just conts -> case interpretCmd conts of
                        Left err -> sendReply cid err
                        Right action ->
                          evalTgAct uid action cid >>= \case
                            Left err -> sendReply cid err
                            Right outboundMsg -> reply (tok env) cid outboundMsg (postjobs env)
    if EQ == compare secret (tok env)
      then
        liftIO $
          (try . runApp env . handle $ update)
            >>= \case
              -- catching all leftover exceptions if any
              Left (SomeException err) ->
                alertAdmin (postjobs env) $
                  "Exception thrown against handler: "
                    `T.append` (T.pack . show $ err)
              Right _ -> pure ()
      else liftIO $ putStrLn "Secrets do not match."

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
      dbName = case lookup "TEST" env of
        Just "1" -> "feedfarer-test"
        _ -> "feedfarer"
      creds =
        let [user, pwd] = T.pack . fromJust . flip lookup env <$> ["MONGO_INITDB_ROOT_USERNAME", "MONGO_INITDB_ROOT_PASSWORD"]
         in MongoCredsServer "mongo" dbName user pwd
      token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
      webhook =
        let raw = T.pack . fromJust $ lookup "WEBHOOK_URL" env
         in if T.last raw == T.last "/" then T.dropEnd 1 raw else raw
      interval = maybe 60000000 read $ lookup "WORKER_INTERVAL" env
      version = T.pack . fromMaybe "(unspecified)" $ lookup "APP_VERSION" env
   in do
        mvar1 <- newMVar HMS.empty
        mvar2 <- newMVar HMS.empty
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
        last_run_ioref <- newIORef Nothing
        pure
          AppConfig
            { app_version = version
            , blacklist = mvar1
            , tg_config = ServerConfig{bot_token = token, webhook_url = webhook, alert_chat = alert_chat_id}
            , base_url = base
            , mongo_creds = connected_creds
            , last_worker_run = last_run_ioref
            , subs_state = mvar2
            , postjobs = chan
            , worker_interval = interval
            , connectors = (conn, pipe_ioref)
            }

initStart :: AppConfig -> IO ()
initStart env = runApp env $ do
  startJobs -- must be first started in any it's sent any job in the steps below
  liftIO $ putStrLn "jobs queue started"
  loadChatsIntoMem
  liftIO $ putStrLn "chats loaded"
  feeds <- rebuildAllFeedsFromMem
  liftIO $ putStrLn "feeds built"
  void $ evalDb $ UpsertFeeds feeds
  liftIO $ putStrLn "feeds saved"
  startNotifs
  liftIO $ do
    putStrLn "digests / follows queue started"
    writeChan (postjobs env) $ JobTgAlertAdmin "Feedo just started."

startApp :: IO ()
startApp = do
  env <- getEnvironment
  config <- makeConfig env
  initStart config
  print $ "Startup completed for version " `T.append` app_version config
  print $ "Running now using " ++ show port
  run port $ withServer config
 where
  port = 8000
