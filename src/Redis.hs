module Redis where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Database.Redis (Connection, checkedConnect, defaultConnectInfo, runRedis, Redis, ConnectInfo (connectHost))
import AppTypes (AppConfig(connectors), App)

class Monad m => HasRedis m where
    withRedis :: AppConfig -> Redis a -> m a

instance MonadIO m => HasRedis (App m) where
    withRedis = withRedis'

singleK :: T.Text -> B.ByteString
singleK = B.append "feeds:" . T.encodeUtf8

withRedis' :: MonadIO m => AppConfig -> Redis a -> m a
withRedis' conf action = liftIO $ do
    let (conn, _) = connectors conf 
    runRedis conn action

setupRedis :: MonadIO m => m Connection
setupRedis = liftIO $ do
    putStrLn "Attempting to connect to 'redis-host' on default port:" 
    conn <- checkedConnect defaultConnectInfo { connectHost = "redis" }
    putStrLn "REDIS setup....OK"
    pure conn
