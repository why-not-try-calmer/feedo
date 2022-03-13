module Redis where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Database.Redis (Connection, checkedConnect, defaultConnectInfo, runRedis, Redis, ConnectInfo (connectHost))
import AppTypes (AppConfig(connectors), App)
import Data.Int (Int64)

class Monad m => HasRedis m where
    withRedis :: AppConfig -> Redis a -> m a

instance MonadIO m => HasRedis (App m) where
    withRedis = withRedis'

-- instance HasRedis IO where
--    withRedis = withRedis'

singleK :: T.Text -> B.ByteString
singleK = B.append "feeds:" . T.encodeUtf8

pageCidMidK :: Int64 -> Int -> B.ByteString
pageCidMidK cid mid = "page_cid_mid:" `B.append` f cid `B.append` f mid 
    where
        f :: Show a => a -> B.ByteString
        f = T.encodeUtf8 . T.pack . show

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
