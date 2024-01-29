{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Redis where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding as T
import Database.Redis (ConnectInfo (connectHost), Connection, Redis, RedisCtx, Status, TxResult (TxSuccess), checkedConnect, defaultConnectInfo, del, expire, get, lindex, llen, lpush, multiExec, runRedis, set)
import Mongo (evalDb)
import Types (App, AppConfig, CacheAction (..), DbAction (..), DbResults (DbPages), Digest (..), FromCache (..), connectors)
import Utils (renderDbError)

class (Monad m) => HasRedis m where
  evalKeyStore :: Redis a -> m a
  withKeyStore :: (MonadReader AppConfig m) => CacheAction -> m (Either T.Text FromCache)

instance (MonadIO m) => HasRedis (App m) where
  evalKeyStore :: (MonadIO m) => Redis a -> App m a
  evalKeyStore action = ask >>= \env -> liftIO $ runRedis (fst . connectors $ env) action

  withKeyStore :: (MonadIO m) => CacheAction -> App m (Either T.Text FromCache)
  withKeyStore (CacheGetPage cid mid n) = do
    env <- ask
    evalKeyStore query >>= \case
      Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
      _ ->
        refresh env >>= \case
          Left err -> pure . Left $ err
          Right _ ->
            evalKeyStore query >>= \case
              Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
              _ ->
                pure
                  . Left
                  $ "TgActError while trying to refresh after pulling anew from database. Chat involved: "
                  `T.append` (T.pack . show $ cid)
   where
    (lk, k) = pageKeys cid mid
    query = do
      d <- lindex lk (toInteger $ n - 1)
      l <- llen lk
      url <- get k
      pure $ (,,) <$> d <*> l <*> url
    success p i mb_page =
      let (page', i') = (B.decodeUtf8 p, fromInteger i)
       in pure . Right $ CachePage page' i' mb_page
    refresh env =
      evalDb env (GetPages cid mid) >>= \case
        Right (DbPages pages mb_link) -> withKeyStore (CacheSetPages cid mid pages mb_link)
        Left err -> pure . Left $ renderDbError err
        _ -> pure $ Left "Unknown error while trying to refresh after pulling anew from database."
  withKeyStore (CacheSetPages _ _ [] _) = pure $ Left "No pages to set!"
  withKeyStore (CacheSetPages cid mid pages mb_link) =
    let (lk, k) = pageKeys cid mid
        action = case mb_link of
          Nothing ->
            evalKeyStore (lpush lk (map B.encodeUtf8 pages) >> expire lk 86400) >>= \case
              Right _ -> pure $ Right CacheOk
              Left _ -> pure $ Left "Nothing"
          Just l ->
            evalKeyStore
              ( multiExec $ do
                  q1 <- set k $ B.encodeUtf8 l
                  q1' <- expire k 86400
                  q2 <- lpush lk $ map B.encodeUtf8 pages
                  q2' <- expire lk 86400
                  pure $ (,,,) <$> q1 <*> q2 <*> q1' <*> q2'
              )
              >>= \case
                TxSuccess _ -> pure $ Right CacheOk
                _ -> pure $ Left "Nothing"
     in action

setUpKeyStore :: (MonadIO m) => m (Either T.Text Connection)
setUpKeyStore = liftIO $ do
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
    print $ "Failed to connect. TgActError: " `T.append` (T.pack . show $ e)
    putStrLn "Retrying now..."
    pure $ Left ()

singleFeedsK :: T.Text -> B.ByteString
singleFeedsK = B.append "feeds:" . T.encodeUtf8

pageKeys :: Int64 -> Int -> (B.ByteString, B.ByteString)
pageKeys cid mid = (lk, k)
 where
  lk = "pages_cid_mid:" `B.append` f cid `B.append` f mid
  k = "page_url_cid_mid:" `B.append` f cid `B.append` f mid
  f :: (Show a) => a -> B.ByteString
  f = T.encodeUtf8 . T.pack . show

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
