module Redis where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Database.Redis (Connection, checkedConnect, defaultConnectInfo, runRedis, Redis, ConnectInfo (connectHost))
import AppTypes (AppConfig(connectors), App)
import Data.Int (Int64)
import Control.Exception

class Monad m => HasRedis m where
    withRedis :: AppConfig -> Redis a -> m a

instance MonadIO m => HasRedis (App m) where
    withRedis = withRedis'

singleK :: T.Text -> B.ByteString
singleK = B.append "feeds:" . T.encodeUtf8

pageKeys :: Int64 -> Int -> (B.ByteString, B.ByteString)
pageKeys cid mid = (lk, k)
    where
        lk = "pages_cid_mid:" `B.append` f cid `B.append` f mid
        k = "page_url_cid_mid:" `B.append` f cid `B.append` f mid
        f :: Show a => a -> B.ByteString
        f = T.encodeUtf8 . T.pack . show

withRedis' :: MonadIO m => AppConfig -> Redis a -> m a
withRedis' conf action = 
    let (conn, _) = connectors conf 
    in  liftIO $ runRedis conn action

setupRedis :: MonadIO m => m (Either String Connection)
setupRedis = 
    let redis_host = "redis"
    in  liftIO $ do
        putStrLn "Attempting to connect to default port..." 
        try (checkedConnect defaultConnectInfo { connectHost = redis_host }) >>= \case
            Left (SomeException _) -> do
                print $ "Failed to connect with " ++ redis_host
                try (checkedConnect defaultConnectInfo) >>= \case
                    Left (SomeException err) -> do
                        putStrLn "Failed to connect with default ('localhost')"
                        pure . Left $ show err
                    Right c -> pure $ Right c
            Right c -> pure $ Right c