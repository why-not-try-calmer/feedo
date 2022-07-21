module Redis where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Redis (ConnectInfo (connectHost), Connection, Redis, checkedConnect, defaultConnectInfo, runRedis)
import Types (App, AppConfig (connectors))

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
     in liftIO $ runRedis conn action

setupRedis :: MonadIO m => m (Either T.Text Connection)
setupRedis = liftIO $ do
    putStrLn "Attempting to connect to default port..."
    Right <$> try_over 1
  where
    try_over n =
        let hostname = switch_hostnames n
         in try (checkedConnect defaultConnectInfo{connectHost = hostname})
                >>= ( handleWith >=> \case
                        Left () ->
                            if n == 5
                                then throwIO $ userError "Giving up"
                                else threadDelay (1000000 * n * 2) >> try_over (n + 1)
                        Right c -> pure c
                    )
    switch_hostnames n = if even n then "redis" else "localhost"
    handleWith (Right connector) = pure $ Right connector
    handleWith (Left (SomeException e)) = do
        print $ "Failed to connect. Error: " `T.append` (T.pack . show $ e)
        putStrLn "Retrying now..."
        pure $ Left ()
