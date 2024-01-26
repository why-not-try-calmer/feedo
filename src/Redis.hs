{-# LANGUAGE RecordWildCards #-}

module Redis where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding as T
import Database.Redis (ConnectInfo (connectHost), Connection, Redis, RedisCtx, Status, TxResult, checkedConnect, defaultConnectInfo, del, get, multiExec, runRedis, set)
import Types (App, AppConfig (connectors), Digest (..))

class (Monad m) => HasRedis m where
  withKeyStore :: AppConfig -> Redis a -> m a

instance (MonadIO m) => HasRedis (App m) where
  withKeyStore = withRedis

singleFeedsK :: T.Text -> B.ByteString
singleFeedsK = B.append "feeds:" . T.encodeUtf8

pageKeys :: Int64 -> Int -> (B.ByteString, B.ByteString)
pageKeys cid mid = (lk, k)
 where
  lk = "pages_cid_mid:" `B.append` f cid `B.append` f mid
  k = "page_url_cid_mid:" `B.append` f cid `B.append` f mid
  f :: (Show a) => a -> B.ByteString
  f = T.encodeUtf8 . T.pack . show

withRedis :: (MonadIO m) => AppConfig -> Redis a -> m a
withRedis conf action =
  let (conn, _) = connectors conf
   in liftIO $ runRedis conn action

setupRedis :: (MonadIO m) => m (Either T.Text Connection)
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

{- Enqueue digests on Redis -}

writeDigest :: (RedisCtx m f) => Digest -> m (f Status)
writeDigest dig@Digest{..} =
  let key = B.append "digests:" (B.encodeUtf8 . fromJust $ digest_id)
      contents = B.concat . LB.toChunks . encode $ dig
   in set key contents

readDigest :: T.Text -> Redis (TxResult (Maybe B.ByteString, Integer))
readDigest digest_id =
  let key = B.append "digests:" (B.encodeUtf8 digest_id)
   in multiExec $ do
        q1 <- get key
        q2 <- del [key]
        return $ (,) <$> q1 <*> q2
