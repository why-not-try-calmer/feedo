{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import AppTypes
import Control.Concurrent (writeChan)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Database.MongoDB
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Transport.Tls as Tls
import GHC.IORef (atomicSwapIORef)

{- Interface -}

class Db m where
    openDbHandle :: AppConfig -> m ()
    evalDb :: AppConfig -> DbAction -> m DbRes

instance MonadIO m => Db (App m) where
    openDbHandle config = initConnectionMongo (db_config config) >>= \case
        Left err -> liftIO . print $ renderDbError err
        Right p -> installPipe p config
    evalDb = evalMongo

instance Db IO where
    openDbHandle config = initConnectionMongo (db_config config) >>= \case
        Left err -> print $ renderDbError err
        Right p -> installPipe p config
    evalDb = evalMongo

installPipe :: MonadIO m => DbConnector -> AppConfig -> m ()
installPipe (MongoPipe pipe) config = void . liftIO $ atomicSwapIORef
    (db_connector config)
    (MongoPipe pipe)
installPipe _ _ = undefined

{- Evaluation -}

runMongo :: MonadIO m => Pipe -> Action m a -> m a
runMongo pipe = access pipe master "feedfarer"

withMongo :: (Db m, MonadIO m) => AppConfig -> Action IO a -> m (Either () a)
withMongo config action = go >>= \case
    Left (ConnectionFailure err) -> do
        alert err >> openDbHandle config >> go >>= \case
            Left (e :: Failure) -> alertGiveUp e
            Right r -> pure $ Right r
    Left err -> alertGiveUp err
    Right r -> pure $ Right r
    where
        alertGiveUp err = do
            alert err
            pure $ Left ()
        alert err = liftIO $ writeChan (postjobs config) . JobTgAlert $
            "withMongo failed with " `T.append` (T.pack . show $ err) `T.append`
            " If the connector timed out, one retry will be carried out."
        go = liftIO $ do
            MongoPipe pipe <- readIORef $ db_connector config
            try $ runMongo pipe action

{- Connection -}

authWith :: MonadIO m => DbCreds -> Pipe -> m (Either DbError DbConnector)
authWith creds pipe = do
    liftIO $ putStrLn "Pipe created. Authenticating..."
    isAuth <- access pipe master "admin" $ auth (user_name creds) (password creds)
    if isAuth
        then liftIO (putStrLn "Authenticated now.") >> pure (Right $ MongoPipe pipe)
        else liftIO (putStrLn "Authentication failed.") >> pure (Left PipeNotAcquired)

initConnectionMongo :: MonadIO m => DbCreds -> m (Either DbError DbConnector)
initConnectionMongo creds@MongoCredsTls{..} = liftIO $ do
    pipe <- Tls.connect host_name db_port
    verdict <- isClosed pipe
    if verdict then pure . Left $ PipeNotAcquired else authWith creds pipe
initConnectionMongo creds@MongoCredsReplicaTls{..} = liftIO $ do
    repset <- openReplicaSetTLS (replicateset, hosts)
    pipe <- primary repset
    verdict <- isClosed pipe
    if verdict then pure . Left $ PipeNotAcquired else authWith creds pipe
initConnectionMongo creds@MongoCredsReplicaSrv{..} = liftIO $ do
    repset <- openReplicaSetSRV' host_name
    pipe <- primary repset
    verdict <- isClosed pipe
    if verdict then pure . Left $ PipeNotAcquired else authWith creds pipe

{- Actions -}

writeOrRetry :: (MonadIO m, Db m) => WriteResult -> AppConfig -> m (Either () WriteResult) -> m DbRes
writeOrRetry res env action =
    if not $ failed res then pure DbOk
    else openDbHandle env >> action >>= \case
        Left _ -> liftIO $ giveUp env res
        Right retried -> if failed retried then liftIO $ giveUp env retried else pure DbOk
    where
        giveUp config err = do
            writeChan (postjobs config) . JobTgAlert $
                "withMongo failed with " `T.append` (T.pack . show $ err) `T.append`
                " If the connector timed out, one retry will be carried out."
            pure . DbErr $ FailedToUpdate "writeOrRetry failed for this reason: " (T.pack . show $ res)

searchKeywords :: S.Set T.Text -> [Document]
searchKeywords ws =
    let search = [
            "$search" =: [
            "index" =: ("default" :: T.Text),
            "text" =: [
                "query" =: S.toList ws,
                "fuzzy" =: ([] :: Document),
                "path" =: (["i_desc", "i_title"] :: [T.Text])
            ]]]
        project = [
            "$project" =: [
                "_id" =: (0 :: Int),
                "i_title" =: (1 :: Int),
                "i_feed_link" =: (1 :: Int),
                "i_link"=: (1 :: Int),
                "score" =: [ "$meta" =: ("searchScore" :: T.Text)]
            ]]
    in [search, project]

evalMongo :: (Db m, MonadIO m) => AppConfig -> DbAction -> m DbRes
evalMongo env (ArchiveItems feeds) =
    let selector = foldMap (map (\i -> (["i_link" =: i_link i], itemToBson i, [Upsert])) . f_items) feeds
        action = withMongo env $ updateAll "items" selector
    in  action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate mempty "ArchiveItems failed"
        Right res -> writeOrRetry res env action
evalMongo env (DbSearch keywords scope) =
    let action = aggregate "items" $ searchKeywords keywords
    in  withMongo env action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate mempty "DbSearch failed"
        Right res  ->
            let mkSearchRes doc =
                    let title = M.lookup "i_title" doc
                        link = M.lookup "i_link" doc
                        f_link = M.lookup "i_feed_link" doc
                        score = M.lookup "score" doc
                    in  case sequence [title, link, f_link] :: Maybe [T.Text] of
                        Just [t, l, fl] -> case score :: Maybe Double of
                            Nothing -> Nothing
                            Just s -> Just $ SearchResult t l fl s
                        _ -> Nothing
                sort_limit = take 10 . sortOn (Down . sr_score)
                rescind = filter (\sr -> sr_feedlink sr `elem` scope)
                payload r =
                    if null scope then sort_limit r
                    else rescind . sort_limit $ r
            in  case traverse mkSearchRes res of
                Nothing -> pure $ DbSearchRes S.empty []
                Just r -> pure . DbSearchRes keywords . payload $ r
evalMongo env (DeleteChat cid) =
    let action = withMongo env $ deleteOne (select ["sub_chatid" =: cid] "chats")
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate mempty "DeleteChat failed"
        Right _ -> pure DbOk
evalMongo env Get100Feeds =
    let action = find (select [] "feeds") {sort = [ "f_reads" =: (-1 :: Int)], limit = 100}
    in  withMongo env (action >>= rest) >>= \case
        Left () -> pure $ DbErr $ FailedToUpdate mempty "Get100Feeds failed"
        Right docs ->
            if null docs then pure DbNoFeed
            else pure . DbFeeds $ map bsonToFeed docs
evalMongo env GetAllChats =
    let action = find (select [] "chats")
    in  withMongo env (action >>= rest) >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate mempty "GetAllChats failed"
        Right docs ->
            if null docs then pure DbNoChat
            else pure $ DbChats . map bsonToChat $ docs
evalMongo env (GetFeed link) =
    let action = withMongo env $ findOne (select ["f_link" =: link] "feeds")
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate mempty "GetFeed failed"
        Right (Just doc) -> pure $ DbFeeds [bsonToFeed doc]
        Right Nothing -> pure DbNoFeed
evalMongo env (IncReads links) =
    let action l = withMongo env $ modify (select ["f_link" =: l] "feeds") ["$inc" =: ["f_reads" =: (1 :: Int)]]
    in  traverse_ action links >> pure DbOk
evalMongo env PruneOneMonthBatches =
    let oneMonth = liftIO $ getCurrentTime <&> addUTCTime (-2592000)
        action t = deleteAll "batches" [(["batch_created" =: ["$lt" =: t]], [])]
    in  oneMonth >>= withMongo env . action >> pure DbOk
evalMongo env (PruneOldItems t) =
    let action = deleteAll "items" [(["i_pubdate" =: ["$lt" =: (t :: UTCTime)]], [])]
    in  withMongo env action >> pure DbOk
evalMongo env (ReadBatch _id) =
    let action = findOne (select ["batch_id" =: _id] "batches")
    in  withMongo env action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate "Batch" "ReadBatch refused to read from the database."
        Right doc -> maybe (pure DbNoBatch) (pure . DbBatch . bsonToBatch) doc
evalMongo env (UpsertChat chat) =
    let action = withMongo env $ upsert (select ["sub_chatid" =: sub_chatid chat] "chats") $ chatToBson chat
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.pack . show . sub_chatid $ chat) "UpsertChat failed"
        Right _ -> pure DbOk
evalMongo env (UpsertChats chatshmap) =
    let chats = HMS.elems chatshmap
        selector = map (\c -> (["sub_chatid" =: sub_chatid c], chatToBson c, [Upsert])) chats
        action = withMongo env $ updateAll "chats" selector
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.intercalate ", " (map (T.pack . show . sub_chatid) chats)) "UpsertChats failed"
        Right res -> writeOrRetry res env action
evalMongo env (UpsertFeeds feeds) =
    let selector = map (\f -> (["f_link" =: f_link f], feedToBson f, [Upsert])) feeds
        action = withMongo env $ updateAll "feeds" selector
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.intercalate ", " (map f_link feeds)) mempty
        Right res -> writeOrRetry res env action
evalMongo env (View flinks start end) =
    let query = find (select ["i_feed_link" =: ["$in" =: (flinks :: [T.Text])], "i_pubdate" =: ["$gt" =: (start :: UTCTime), "$lt" =: (end :: UTCTime)]] "items") >>= rest
    in  withMongo env query >>= \case
        Left _ -> pure $ DbErr FailedToLoadFeeds
        Right is -> pure $ DbView (map bsonToItem is) start end
evalMongo env (WriteBatch batch) =
    let action = insert "batches" $ batchToBson batch
    in  withMongo env action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate "Batch" "Db refused to insert batch items"
        Right _ -> pure DbOk

{- Items -}

itemToBson :: Item -> Document
itemToBson i = ["i_title" =: i_title i, "i_desc" =: i_desc i, "i_feed_link" =: i_feed_link i, "i_link" =: i_link i, "i_pubdate" =: i_pubdate i]

bsonToItem :: Document -> Item
bsonToItem doc = Item
    (fromJust $ M.lookup "i_title" doc)
    (fromJust $ M.lookup "i_desc" doc)
    (fromJust $ M.lookup "i_link" doc)
    (fromJust $ M.lookup "i_feed_link" doc)
    (fromJust $ M.lookup "i_pubdate" doc)

{- Feeds -}

feedToBson :: Feed -> Document
feedToBson Feed {..} =
    [ "f_type" =: show f_type,
        "f_desc" =: f_desc,
        "f_title" =: f_title,
        "f_link" =: f_link,
        "f_items" =: map itemToBson (take 30 . sortOn (Down . i_pubdate) $ f_items),
        "f_avg_interval" =: (realToFrac <$> f_avg_interval :: Maybe Double),
        "f_last_refresh" =: f_last_refresh,
        "f_reads" =: f_reads
    ]

bsonToFeed :: Document -> Feed
bsonToFeed doc =
    let raw_items = fromJust $ M.lookup "f_items" doc
        items = map bsonToItem raw_items
    in  Feed {
            f_type = if fromJust (M.lookup "f_type" doc) == (T.pack . show $ Rss) then Rss else Atom,
            f_desc = fromJust $ M.lookup "f_desc" doc,
            f_title = fromJust $ M.lookup "f_title" doc,
            f_link = fromJust $ M.lookup "f_link" doc,
            f_items = items,
            f_avg_interval = M.lookup "f_avgInterval" doc,
            f_last_refresh = M.lookup "f_desc" doc,
            f_reads = fromJust $ M.lookup "f_reads" doc
        }

{- Chats -}

bsonToChat :: Document -> SubChat
bsonToChat doc =
    let feeds_links = fromJust $ M.lookup "sub_feeds_links" doc :: [T.Text]
        settings_doc = fromJust $ M.lookup "sub_settings" doc :: Document
        feeds_settings_docs = Settings {
            settings_batch_size = fromJust $ M.lookup "settings_batch_size" settings_doc :: Int,
            settings_batch_interval =
                let every = M.lookup "settings_batch_every_secs" settings_doc :: Maybe NominalDiffTime
                    hm_docs = M.lookup "settings_batch_at" settings_doc :: Maybe [Document]
                    adjust n
                        | n < 6 && n > 0 = n * 10
                        | otherwise = n
                    extract mds = case mds of
                        Nothing -> []
                        Just docs -> foldr (\d acc ->
                            let h = M.lookup "hour" d :: Maybe Int
                                m = M.lookup "minute" d :: Maybe Int
                            in  case sequence [h, m] of
                                Nothing -> []
                                Just hm -> acc ++ [(head hm, adjust $ last hm)]) [] docs
                in  BatchInterval every (if null $ extract hm_docs then Nothing else Just $ extract hm_docs),
            settings_word_matches = WordMatches
                (maybe S.empty S.fromList $ M.lookup "settings_blacklist" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_searchset" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_only_search_results" settings_doc),
            settings_paused = fromMaybe False $ M.lookup "settings_paused" doc,
            settings_disable_web_view = fromMaybe False $ M.lookup "settings_disable_web_view" doc,
            settings_pin = fromMaybe False $ M.lookup "settings_pin" doc,
            settings_share_link = fromMaybe False $ M.lookup "settings_share_link" doc
            }
    in  SubChat {
            sub_chatid = fromJust $ M.lookup "sub_chatid" doc,
            sub_last_digest = M.lookup "sub_last_digest" doc,
            sub_next_digest = M.lookup "sub_next_digest" doc,
            sub_last_follow = M.lookup "sub_last_follow" doc,
            sub_feeds_links = S.fromList feeds_links,
            sub_settings = feeds_settings_docs
        }

chatToBson :: SubChat -> Document
chatToBson (SubChat chat_id last_digest next_digest last_follow flinks settings) =
    let blacklist = S.toList . match_blacklist . settings_word_matches $ settings
        searchset = S.toList . match_searchset . settings_word_matches $ settings
        only_search_results = S.toList . match_only_search_results . settings_word_matches $ settings
        settings' = [
            "settings_blacklist" =: blacklist,
            "settings_searchset" =: searchset,
            "settings_only_search_results" =: only_search_results,
            "settings_batch_size" =: settings_batch_size settings,
            "settings_paused" =: settings_paused settings,
            "settings_disable_web_view" =: settings_disable_web_view settings,
            "settings_pin" =: settings_pin settings
            ]
        with_secs = maybe []
            (\secs -> ["settings_batch_every_secs" =: secs])
            (batch_every_secs . settings_batch_interval $ settings)
        with_at = maybe []
            (\hm -> ["settings_batch_at" =: map (\(h, m) -> ["hour" =: h, "minute" =: m]) hm])
            (batch_at . settings_batch_interval $ settings)
    in  [
            "sub_chatid" =: chat_id,
            "sub_last_digest" =: last_digest,
            "sub_next_digest" =: next_digest,
            "sub_last_follow" =: last_follow,
            "sub_feeds_links" =: S.toList flinks,
            "sub_settings" =: settings' ++ with_secs ++ with_at
        ]

{- Batches -}

bsonToBatch :: Document -> Batch
bsonToBatch doc =
    let items = map bsonToItem . fromJust $ M.lookup "batch_items" doc
        created = fromJust $ M.lookup "batch_created" doc
        _id = fromJust $ M.lookup "batch_id" doc
        flinks = fromJust $ M.lookup "batch_flinks" doc
    in  Batch _id created items flinks

batchToBson :: Batch -> Document
batchToBson Batch{..} = [
    "batch_id" =: batch_id,
    "batch_created" =: (batch_created :: UTCTime),
    "batch_items" =: map itemToBson batch_items,
    "batch_flinks" =: (S.toList . S.fromList . map i_feed_link $ batch_items :: [T.Text])
    ]

{- Logs -}

bsonToLog :: Document -> LogItem
bsonToLog doc = LogPerf {
    log_message = fromMaybe mempty $ M.lookup "log_message" doc,
    log_at = fromMaybe undefined $ M.lookup "log_at" doc,
    log_refresh = fromMaybe 0 $ M.lookup "log_refresh" doc,
    log_sending_notif = fromMaybe 0 $ M.lookup "log_sending_notif" doc,
    log_updating = fromMaybe 0 $ M.lookup "log_update" doc,
    log_total = fromMaybe 0 $ M.lookup "log_total" doc
    }

saveToLog :: (Db m, MonadIO m) => AppConfig -> LogItem -> m ()
saveToLog env LogPerf{..} = withMongo env (insert "logs" doc) >> pure ()
    where
        doc = [
            "log_message" =: log_message,
            "log_at" =: log_at,
            "log_refresh" =: log_refresh,
            "log_sending_notif" =: log_sending_notif,
            "log_update" =: log_updating,
            "log_total" =: log_total
            ]

cleanLogs :: (Db m, MonadIO m) => AppConfig -> m (Either String ())
cleanLogs env = do
    now <- liftIO getCurrentTime
    res <- withMongo env $ do
        void $ deleteAll "logs" [(["log_at" =: ["$lt" =: (one_week_from now :: UTCTime)]], [])]
        res_delete <- deleteAll "logs" [(["log_at" =: ["$exists" =: False]], [])]
        res_found <- find (select ["log_at" =: ["$exists" =: False]] "logs") >>= rest
        pure (res_delete, res_found)
    case res of
        Left _ -> pure . Left $ "Failed to cleanLogs"
        Right (del, found) ->
            if failed del then pure . Left . show $ del else
            if not $ null found then pure . Left . show $ found else
            pure $ Right ()
    where
        one_week_from now = addUTCTime (-604800) now

collectLogStats :: (Db m, MonadIO m) => AppConfig -> m T.Text
collectLogStats env = do
    res <- withMongo env $ do
        logs <- rest =<< find (select ["log_total" =: ["$gte" =: (0.5 :: Double)]] "logs")
        counts <- rest =<< find (select [] "feeds") {sort = [ "f_reads" =: (-1 :: Int)], limit = 100}
        pure (logs, counts)
    case res of
        Left _ -> pure "Failed to collectLogStats"
        Right (docs_logs, feeds_docs) ->
            let logs = sortOn log_at . filter (not . T.null . log_message) . map bsonToLog $ docs_logs
                feeds_counts = map (\d -> let f = bsonToFeed d in (f_link f, T.pack . show $ f_reads f)) feeds_docs
            in  pure $ foldl' (\acc (k, v) -> acc `T.append` " " `T.append` k `T.append` ": " `T.append` v) T.empty feeds_counts `T.append` mkStats logs
    where
        mkStats logs =
            let values =
                    let (a,b,c,d) = foldl' (\(!r, !s, !u, !t) (LogPerf _ _ r' s' u' t') ->
                            (r'+r, s+s', u+u', t+t')) (0,0,0,0) logs
                    in  [a,b,c,d]
                keys = ["refreshing", "sending", "updating", "total"]
            in  showLength logs `T.append`
                    " logs. Averages for: " `T.append`
                    T.intercalate ", " (zipWith (\k v -> k `T.append` ": " `T.append`
                    showAvgLength v logs) keys values)
        showLength = T.pack . show . length
        showAvgLength a l = T.pack . show $ fromIntegral a `div` length l

{- Cleanup -}

purgeCollections :: (Db m, MonadIO m) => AppConfig -> [T.Text] -> m (Either String ())
purgeCollections _ [] = pure . Left $ "Empty list of collection names!"
purgeCollections env colls =
    let action = withMongo env $ traverse (`deleteAll` [([],[])]) colls
    in  action >>= \case
        Left _ -> pure . Left $ "Failed to purgeCollections."
        Right res ->
            if any failed res then pure . Left $ "Failed to purge collections " ++ show colls
            else pure $ Right ()