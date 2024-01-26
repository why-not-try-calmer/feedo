{-# LANGUAGE FlexibleContexts #-}

module Cache where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import Data.Time (getCurrentTime)
import Database.Redis
import Mongo (HasMongo (evalDb))
import Redis (HasRedis, getAllFeeds, pageKeys, withRedis)
import Types hiding (Reply)
import Utils (freshLastXDays, renderDbError)

type CacheRes = Either T.Text FromCache

class (MonadReader AppConfig m) => HasCache m where
  withCache :: CacheAction -> m CacheRes

instance (MonadIO m) => HasCache (App m) where
  withCache = withBroker

withBroker :: (MonadReader AppConfig m, MonadIO m, HasRedis m, HasMongo m) => CacheAction -> m CacheRes
{- Cache in-app or in-data frequently requested data -}
withBroker (CacheGetPage cid mid n) = do
  env <- ask
  withRedis env query >>= \case
    Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
    _ ->
      refresh env >>= \case
        Left err -> pure . Left $ err
        Right _ ->
          withRedis env query >>= \case
            Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
            _ ->
              pure
                . Left
                $ "Error while trying to refresh after pulling anew from database. Chat involved: "
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
      DbPages pages mb_link -> withBroker (CacheSetPages cid mid pages mb_link)
      DbErr err -> pure . Left $ renderDbError err
      _ -> pure $ Left "Unknown error while trying to refresh after pulling anew from database."
withBroker (CacheSetPages _ _ [] _) = pure $ Left "No pages to set!"
withBroker (CacheSetPages cid mid pages mb_link) =
  ask >>= \env ->
    let (lk, k) = pageKeys cid mid
        action = case mb_link of
          Nothing ->
            withRedis env (lpush lk (map B.encodeUtf8 pages) >> expire lk 86400) >>= \case
              Right _ -> pure $ Right CacheOk
              Left _ -> pure $ Left "Nothing"
          Just l ->
            withRedis
              env
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
withBroker (CacheXDays links days) =
  ask
    >>= ( getAllFeeds
            >=> ( \case
                    Left txt -> pure . Left $ txt
                    Right fs -> liftIO getCurrentTime <&> (Right . CacheLinkDigest . foldFeeds fs)
                )
        )
 where
  collect f acc now =
    let fresh = freshLastXDays days now $ f_items f
     in if null fresh then acc else (f_link f, fresh) : acc
  foldFeeds fs now = HMS.foldl' (\acc f -> if f_link f `notElem` links then acc else collect f acc now) [] fs
