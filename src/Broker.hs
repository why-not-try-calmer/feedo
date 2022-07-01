{-# LANGUAGE FlexibleContexts #-}

module Broker where

import AppTypes hiding (Reply)
import Control.Concurrent (readMVar, writeChan)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as B
import Data.Time (UTCTime, getCurrentTime)
import Database.Redis
import Mongo (HasMongo (evalDb))
import Notifications (collectDue, collectNoDigest, feedlinksWithMissingPubdates, keepNew, markNotified, notifFrom)
import Parsing (rebuildFeed)
import Redis (HasRedis, pageKeys, singleK, withRedis)
import Utils (freshLastXDays, partitionEither, readBatchRecipe, sortItems)

type CacheRes = Either T.Text FromCache

class (MonadReader AppConfig m) => HasCache m where
    withCache :: CacheAction -> m CacheRes

instance MonadIO m => HasCache (App m) where
    withCache = withBroker

writeManyFeeds :: [Feed] -> Redis (TxResult ([Status], Integer))
writeManyFeeds fs =
    let write_to_keys f = set (singleK . f_link $ f) . B.concat . LB.toChunks . encode $ sortItems f
        write_to_sets = sadd "feeds" . map (B.encodeUtf8 . f_link) $ fs
        action = do
            q1 <- sequence <$> mapM write_to_keys fs
            q2 <- write_to_sets
            pure $ (,) <$> q1 <*> q2
     in multiExec action

deleteManyFeeds :: [T.Text] -> Redis (TxResult Integer)
deleteManyFeeds fs = multiExec $ do
    _ <- del (map singleK fs)
    srem "feeds" $ map B.encodeUtf8 fs

getAllFeeds :: HasRedis m => AppConfig -> m (Either T.Text FeedsMap)
getAllFeeds env =
    let action =
            withRedis env $
                smembers "feeds" >>= \case
                    Left _ -> pure $ Left "Unable to find keys"
                    Right ks ->
                        mapM (get . B.append "feeds:") ks
                            >>= ( \case
                                    Left _ -> pure $ Left "Unable to find feeds"
                                    Right xs -> pure $ Right xs
                                )
                                . sequence
     in action >>= \case
            Left err -> pure $ Left err
            Right bs -> case sequence bs of
                Nothing -> pure $ Left "Unable to decode feeds"
                Just bs' -> case mapM decodeStrict' bs' :: Maybe [Feed] of
                    Nothing -> pure $ Left "Unable to decode feeds"
                    Just feeds -> pure . Right . feedsHmap $ feeds

feedsHmap :: [Feed] -> HMS.HashMap FeedLink Feed
feedsHmap = HMS.fromList . map (\f -> (f_link f, f))

rebuildUpdate ::
    (MonadReader AppConfig m, HasRedis m, HasMongo m, MonadIO m) =>
    AppConfig ->
    [FeedLink] ->
    UTCTime ->
    m (Either T.Text (HMS.HashMap T.Text Feed))
rebuildUpdate env flinks now = do
    succeeded <- liftIO $ do
        -- rebuilding all feeds with any subscribers
        eitherUpdated <- mapConcurrently rebuildFeed flinks
        let (failed, succeeded) = partitionEither eitherUpdated
        -- handling case of some feeds not rebuilding
        unless
            (null failed)
            ( writeChan (postjobs env) . JobTgAlert $
                "Failed to update these feeds: " `T.append` T.intercalate ", " failed
            )
        -- archiving
        writeChan (postjobs env) $ JobArchive succeeded now
        pure succeeded
    getAllFeeds env >>= \case
        Left _ -> pure . Left $ "Unable to acquire old feeds. Refresh aborted."
        Right old_feeds ->
            -- updating cache
            let fresh_feeds = HMS.fromList . map (\f -> (f_link f, f)) $ succeeded
                to_cache = HMS.union fresh_feeds old_feeds
             in withBroker (CachePushFeeds $ HMS.elems to_cache) >> pure (Right to_cache)

withBroker :: (MonadReader AppConfig m, MonadIO m, HasRedis m, HasMongo m) => CacheAction -> m CacheRes
withBroker (CacheDeleteFeeds []) = pure $ Left "No feed to delete!"
withBroker (CacheDeleteFeeds flinks) =
    ask >>= \env ->
        let action = withRedis env $ deleteManyFeeds flinks
         in action >>= \case
                TxSuccess _ -> pure . Right $ CacheOk
                _ ->
                    pure . Left $
                        "Unable to delete these feeds: "
                            `T.append` T.intercalate ", " flinks
withBroker (CacheGetPage cid mid n) =
    ask >>= \env ->
        withRedis env query >>= \case
            Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
            _ ->
                refresh env >> withRedis env query >>= \case
                    Right (Just page, i, mb_digest_url) -> success page i (B.decodeUtf8 <$> mb_digest_url)
                    _ -> pure $ Left "Error while trying to refresh after pulling anew from database."
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
            _ -> pure $ Left "Error while trying to refresh after pulling anew from database."
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
withBroker (CachePullFeed flink) = do
    env <- ask
    withRedis env (get . singleK $ flink) >>= \case
        Left _ -> pure $ Left "Redis ran into an error"
        Right Nothing -> pure $ Left "Cache miss"
        Right (Just doc) -> case decodeStrict' doc :: Maybe Feed of
            Nothing -> pure . Left $ "Unable to get this feed" `T.append` flink
            Just f -> pure . Right . CacheFeed $ f
withBroker (CachePullFeeds []) = pure . Left $ "No feed to pull!"
withBroker (CachePullFeeds flinks) = do
    env <- ask
    res <- withRedis env $ red_get_all_s flinks
    let (_, successful) = partitionEither res
        feeds =
            foldl'
                ( \acc v -> case v of
                    Nothing -> acc
                    Just b -> case decodeStrict' b :: Maybe Feed of
                        Nothing -> acc
                        Just f -> f : acc
                )
                []
                successful
    if length feeds == length flinks
        then pure . Right . CacheFeeds $ feeds
        else
            let misses = missing_from_cache feeds
             in pure . Left $ "Missing these feeds: " `T.append` T.intercalate "," misses
  where
    red_get_one_s = get . singleK
    red_get_all_s fs = sequence . sequence <$> mapM red_get_one_s fs
    missing_from_cache feeds =
        if null feeds
            then flinks
            else filter (\fl -> fl `notElem` map f_link feeds) flinks
withBroker (CachePushFeeds []) = pure $ Left "No feed to push!"
withBroker (CachePushFeeds fs) =
    let flinks = map f_link fs
     in ask >>= \env ->
            withRedis env (writeManyFeeds fs) >>= \case
                TxSuccess _ -> pure . Right $ CacheOk
                _ -> pure . Left $ "Unable to push these feeds: " `T.append` T.intercalate ", " flinks
withBroker CacheRefresh = do
    env <- ask
    (chats, now, last_run) <- liftIO $ do
        chats <- readMVar $ subs_state env
        now <- getCurrentTime
        last_run <- readIORef $ last_worker_run env
        pure (chats, now, last_run)
    -- checking due chats
    let due = collectDue chats last_run now
        flinks_to_rebuild = foldMap (readBatchRecipe . snd) due
    -- handling due chats
    if HMS.null due
        then pure $ Right CacheOk
        else
            rebuildUpdate env flinks_to_rebuild now >>= \case
                Left err -> pure $ Left err
                Right rebuilt -> do
                    -- sometimes a digest would contain items with the same timestamps, but
                    -- we can filter them out through a simple comparison
                    rebuilt_replaced <-
                        withBroker (CachePullFeeds $ feedlinksWithMissingPubdates rebuilt) >>= \case
                            Left err -> liftIO $ do
                                writeChan (postjobs env) $ JobTgAlert err
                                pure rebuilt
                            Right (CacheFeeds fs) -> liftIO $ do
                                let (discarded_duplicates, rebuilt_replaced) = keepNew rebuilt fs
                                -- logging when that happens
                                unless (null discarded_duplicates) $
                                    writeChan (postjobs env) . JobLog $
                                        LogMissing discarded_duplicates (sum . map (length . snd) $ discarded_duplicates) now
                                pure rebuilt_replaced
                            Right _ -> pure rebuilt
                    -- creating update notification payload, with 'last_run' used only for 'follow notifications'
                    let digests = notifFrom last_run flinks_to_rebuild (HMS.union rebuilt_replaced rebuilt) due
                        has_digest = HMS.keys digests
                        no_digest = collectNoDigest has_digest due
                        (not_updated_feeds, updated_feeds) = partitionDigests digests
                    liftIO $ do
                        -- logging due chats with no digest
                        unless (null no_digest) $ do
                            markNotified env no_digest now
                            writeChan (postjobs env) . JobLog $ LogNoDigest no_digest now
                        -- logging feeds of due chats with updates / no update
                        unless (S.null $ not_updated_feeds `S.union` updated_feeds) $ do
                            writeChan (postjobs env) . JobLog $
                                LogDigest
                                    { log_updated_feeds = S.toList updated_feeds
                                    , log_not_updated = S.toList not_updated_feeds
                                    , log_at = now
                                    }
                        -- logging possibly too aggressive union:
                        unless (rebuilt == rebuilt_replaced) $ do
                            writeChan (postjobs env) . JobTgAlert $
                                "Replaced " `T.append` reportOverwritten rebuilt rebuilt_replaced
                    pure . Right $ CacheDigests digests
  where
    reportOverwritten reb repl =
        let get_items = foldMap (map i_link . f_items)
         in T.take 3950 . T.intercalate ", " $ filter (`notElem` get_items repl) (get_items reb)
    partitionDigests =
        foldl'
            ( \(!not_found, !found) (!c, !bat) ->
                let subs = sub_feeds_links c
                    found' = case bat of
                        Follows fs -> S.fromList $ map f_link fs
                        Digests fs -> S.fromList $ map f_link fs
                    not_found' = S.filter (`notElem` found') subs
                 in (not_found `S.union` not_found', found `S.union` found')
            )
            (mempty, mempty)
withBroker CacheWarmup =
    ask >>= \env ->
        evalDb env GetAllFeeds >>= \case
            DbFeeds fs -> withBroker $ CachePushFeeds fs
            _ -> pure . Left $ "Unable to warm the cache."
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