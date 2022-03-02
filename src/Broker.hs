{-# LANGUAGE FlexibleContexts #-}

module Broker where

import AppTypes hiding (Reply)
import Backend (collectDue, markNotified)
import Control.Concurrent (readMVar, writeChan)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldl', traverse_)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import Data.Time (getCurrentTime)
import Database.Redis
import Mongo (HasMongo (evalDb))
import Parsing (rebuildFeed)
import Redis (singleK, withRedis, HasRedis)
import Utils (freshLastXDays, notifFrom, partitionEither)

type CacheRes = Either T.Text FromCache

class (MonadReader AppConfig m) => HasCache m where
    withCache :: CacheAction -> m CacheRes

instance MonadIO m => HasCache (App m) where
    withCache = withBroker

writeOneFeed :: Feed -> Redis (TxResult Integer)
writeOneFeed f = multiExec $ do
    _ <- set (singleK . f_link $ f) . B.concat . LB.toChunks . encode $ f
    sadd "feeds" [B.encodeUtf8 . f_link $ f]

writeManyFeeds :: [Feed] -> Redis (TxResult Integer)
writeManyFeeds fs =
    let write_to_keys f = set (singleK . f_link $ f) . B.concat . LB.toChunks . encode $ f
        write_to_sets = sadd "feeds" . map (B.encodeUtf8 . f_link) $ fs
    in  multiExec $ traverse_ write_to_keys fs >> write_to_sets

deleteManyFeeds :: [T.Text] -> Redis (TxResult Integer)
deleteManyFeeds fs = multiExec $ do
    _ <- del (map singleK fs)
    srem "feeds" $ map B.encodeUtf8 fs

getAllFeeds :: (MonadIO m, HasRedis m) => AppConfig -> m (Either T.Text FeedsMap)
getAllFeeds env =
    let action = withRedis env $ smembers "feeds" >>= \case
            Left _ -> pure $ Left "Unable to find keys"
            Right ks -> traverse (get . B.append "feeds:") ks >>= (\case
                Left _ -> pure $ Left "Unable to find feeds"
                Right xs -> pure $ Right xs) . sequence
    in  action >>= \case
        Left err -> pure $ Left err
        Right bs -> case sequence bs of
            Nothing -> pure $ Left "Unable to decode feeds"
            Just bs' -> case traverse decodeStrict' bs' :: Maybe [Feed] of
                Nothing -> pure $ Left "Unable to decode feeds"
                Just feeds -> pure . Right . feedsHmap $ feeds

feedsHmap :: [Feed] -> HMS.HashMap FeedLink Feed
feedsHmap = HMS.fromList . map (\f -> (f_link f, f))

withBroker :: (MonadReader AppConfig m, MonadIO m, HasRedis m, HasMongo m) => CacheAction -> m CacheRes
withBroker (CacheDeleteFeeds flinks) = ask >>= \env ->
    let action = withRedis env $ deleteManyFeeds flinks
    in  action >>= \case
        TxSuccess _ -> pure . Right $ CacheOk
        _ -> pure . Left $ 
                "Unable to delete these feeds: " 
                    `T.append`  T.intercalate ", " flinks
withBroker (CachePullFeed flink) = do
    env <- ask
    withRedis env (get . singleK $ flink) >>= \case
        Left _ -> pure $ Left "Redis ran into an error"
        Right Nothing -> pure $ Left "Cache miss"
        Right (Just doc) -> case decodeStrict' doc :: Maybe Feed of
            Nothing -> pure . Left $ "Unable to get this feed" `T.append` flink
            Just f -> pure . Right . CacheFeed $ f
withBroker (CachePullFeeds flinks) = do
    env <- ask
    res <- withRedis env $ red_get_all_s flinks
    let (_, successful) = partitionEither res
        feeds = foldl' (\acc v -> case v of
            Nothing -> acc
            Just b -> case decodeStrict' b :: Maybe Feed of
                Nothing -> acc
                Just f -> f:acc) [] successful
    if  length feeds == length flinks then pure . Right . CacheFeeds $ feeds
    else
        let misses = missing_from_cache feeds
        in  pure . Left $ "Missing these feeds: " `T.append` T.intercalate "," misses
    where
        red_get_one_s f = get . singleK $ f
        red_get_all_s fs = sequence . sequence <$> traverse red_get_one_s fs
        missing_from_cache feeds =
            if null feeds then flinks
            else filter (\fl -> fl `notElem` map f_link feeds) flinks
withBroker (CachePushFeed f) = do
    env <- ask
    withRedis env $ writeOneFeed f >>= \case
        TxSuccess _ -> pure $ Right CacheOk
        _ -> pure $ Left "Failed to save this feed."
withBroker (CachePushFeeds fs) = do
    env <- ask
    withRedis env $ writeManyFeeds fs >>= \case
        TxSuccess _ -> pure . Right $ CacheOk
        _ -> pure . Left $ "Unable to push these feeds: " `T.append` T.intercalate ", " flinks
    where
        flinks = map f_link fs
withBroker CacheRefresh = do
    env <- ask
    (chats, now) <- (,) <$> liftIO (readMVar $ subs_state env) <*> liftIO getCurrentTime
    let last_run = last_worker_run env
        due = collectDue chats last_run now
        flinks_to_rebuild = foldMap (readBatchRecipe . snd) due
    if  HMS.null due then pure $ Right CacheOk
    else rebuild_update flinks_to_rebuild now env >>= \case
        Left err -> pure $ Left err
        Right rebuilt -> do
            -- creating update notification payload
            let digests = notifFrom flinks_to_rebuild rebuilt due
                has_digest = HMS.keys digests
                no_digest = HMS.foldl' (\acc (!c, _) ->
                    let cid = sub_chatid  c in
                    if cid `notElem` has_digest then cid:acc else acc) [] due
            -- marking chats with no digest as notified
                -- returning payload to the ones to notify
            liftIO $ unless (null no_digest) (markNotified env no_digest now)
            pure . Right $ CacheDigests digests
    where
        rebuild_update flinks now env = do
            succeeded <- liftIO $ do
                -- rebuilding all feeds with any subscribers
                eitherUpdated <- mapConcurrently rebuildFeed flinks
                let (not_rebuilt, succeeded) = partitionEither eitherUpdated
                -- handling case of some feeds not rebuilding
                unless (null not_rebuilt) (writeChan (postjobs env) . JobTgAlert $
                    "Failed to update theses feeds: " `T.append` T.intercalate ", " not_rebuilt)
                -- archiving
                writeChan (postjobs env) $ JobArchive succeeded now
                pure succeeded
            -- updating cache
            getAllFeeds env >>= \case
                Left _ -> pure . Left $ "Unable to acquire old feeds. Refresh aborted."
                Right old_feeds ->
                    let fresh_feeds = HMS.fromList . map (\f -> (f_link f, f)) $ succeeded
                        to_cache = HMS.union fresh_feeds old_feeds
                    in  withBroker (CachePushFeeds $ HMS.elems to_cache) >> pure (Right to_cache)
withBroker CacheWarmup = do
    env <- ask
    evalDb env GetAllFeeds >>= \case
        DbFeeds fs -> withBroker $ CachePushFeeds fs
        _ -> pure . Left $ "Unable to warm the cache."
withBroker (CacheXDays links days) = ask >>= (getAllFeeds >=> (\case
    Left txt -> pure . Left $ txt
    Right fs -> liftIO getCurrentTime <&> (Right . CacheLinkDigest . foldFeeds fs)))
    where
        collect f acc now =
            let fresh = freshLastXDays days now $ f_items f
            in  if null fresh then acc else (f_link f, fresh):acc
        foldFeeds fs now = HMS.foldl' (\acc f -> if f_link f `notElem` links then acc else collect f acc now) [] fs