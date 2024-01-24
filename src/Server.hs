{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Server (startApp, registerWebhook, makeConfig) where

import Backend
import Cache (HasCache)
import Control.Concurrent (newChan, newMVar, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (newIORef)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Jobs
import Mongo (setupDb)
import Network.Wai
import Network.Wai.Handler.Warp
import Redis (setupRedis)
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
  handleWebhook secret update =
    ask >>= \env ->
      let tok = bot_token . tg_config
          finishWith cid err = reply (tok env) cid (ServiceReply $ renderUserError err) (postjobs env)
          handle upd = case callback_query upd of
            Just dat -> processCbq dat
            Nothing -> case message upd of
              Nothing -> liftIO $ putStrLn "Failed to parse message"
              Just msg ->
                let cid = chat_id . chat $ msg
                    uid = user_id . fromJust . from $ msg
                 in case reply_to_message msg of
                      Just _ -> pure () -- ignoring replies
                      Nothing -> case TgramInJson.text msg of
                        Nothing -> pure () -- ignoring empty contents
                        Just conts -> case interpretCmd conts of
                          Left (Ignore _) -> pure () -- ignoring neither interpreted nor error
                          Left err -> finishWith cid err
                          Right action ->
                            evalTgAct uid action cid >>= \case
                              Left err -> finishWith cid err
                              Right r -> reply (tok env) cid r (postjobs env)
       in if EQ == compare secret (tok env)
            then
              liftIO (try . runApp env . handle $ update) >>= \case
                -- catching all leftover exceptions if any
                Left (SomeException err) ->
                  liftIO $
                    writeChan (postjobs env) $
                      JobTgAlertAdmin $
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

makeConfig :: [(String, String)] -> IO (AppConfig, Int)
makeConfig env =
  let alert_chat_id = read . fromJust $ lookup "ALERT_CHATID" env
      key = T.encodeUtf8 . T.pack . fromJust $ lookup "API_KEY" env
      base = T.pack . fromJust $ lookup "BASE_URL" env
      dbName = case lookup "TEST" env of
        Just "1" -> "feedfarer-test"
        _ -> "feedfarer"
      creds =
        let [user, pwd] = T.pack . fromJust . flip lookup env <$> ["MONGO_INITDB_ROOT_USERNAME", "MONGO_INITDB_ROOT_PASSWORD"]
         in MongoCredsServer "mongo" dbName user pwd
      port = maybe 80 read (lookup "PORT" env)
      token = T.append "bot" . T.pack . fromJust $ lookup "TELEGRAM_TOKEN" env
      webhook =
        let raw = T.pack . fromJust $ lookup "WEBHOOK_URL" env
         in if T.last raw == T.last "/" then T.dropEnd 1 raw else raw
      interval = maybe 60000000 read $ lookup "WORKER_INTERVAL" env
   in do
        mvar <- newMVar HMS.empty
        mvar2 <- newMVar HMS.empty
        chan <- newChan
        conn <-
          setupRedis >>= \case
            Left err -> throwIO . userError $ T.unpack err
            Right c -> putStrLn "Redis...OK" >> pure c
        (pipe, connected_creds) <-
          setupDb creds >>= \case
            Left _ -> throwIO . userError $ "Failed to produce a valid Mongo pipe."
            Right p -> putStrLn "Mongo...OK" >> pure p
        pipe_ioref <- newIORef pipe
        last_run_ioref <- newIORef Nothing
        pure
          ( AppConfig
              { api_key = key
              , blacklist = mvar2
              , tg_config = ServerConfig{bot_token = token, webhook_url = webhook, alert_chat = alert_chat_id}
              , base_url = base
              , mongo_creds = connected_creds
              , last_worker_run = last_run_ioref
              , subs_state = mvar
              , postjobs = chan
              , worker_interval = interval
              , connectors = (conn, pipe_ioref)
              }
          , port
          )

initStart :: (HasCache m, MonadIO m, MonadReader AppConfig m) => m ()
initStart = do
  loadChats
  liftIO . putStrLn $ "Chats loaded"
  feeds <- regenFeeds
  liftIO . putStrLn $ "Feeds regenerated"
  refreshCache feeds
  liftIO . putStrLn $ "Cache refreshed"
  postProcJobs >> procNotif
  env <- ask
  liftIO $
    writeChan (postjobs env) $
      JobTgAlertAdmin "Feedo just started."

startApp :: IO ()
startApp = do
  env <- getEnvironment
  (config, port) <- makeConfig env
  runApp config initStart
  run port $ withServer config
