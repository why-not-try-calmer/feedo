{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import AppTypes
import Control.Concurrent (writeChan)
import Control.Exception
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime, UTCTime)
import Data.Time.Clock.System (getSystemTime)
import Database.MongoDB
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Transport.Tls as Tls
import GHC.IORef (atomicSwapIORef)
import Text.Read (readMaybe)
import Utils (defaultChatSettings)

{- Interface -}

class MongoDoc v where
    readDoc :: Document -> v
    writeDoc :: v -> Document
    checks :: v -> Bool

instance MongoDoc SubChat where
    readDoc = bsonToChat
    writeDoc = chatToBson
    checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Feed where
    readDoc = bsonToFeed
    writeDoc = feedToBson
    checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Item where
    readDoc = bsonToItem
    writeDoc = itemToBson
    checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Digest where
    readDoc = bsonToDigest
    writeDoc = digestToBson
    checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc LogItem where
    readDoc = bsonToLog
    writeDoc = logToBson
    checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc AdminUser where
    readDoc = bsonToAdmin
    writeDoc = adminToBson
    checks v = v == (readDoc . writeDoc $ v)

class Db m where
    openDbHandle :: AppConfig -> m ()
    evalDb :: AppConfig -> DbAction -> m DbRes

instance MonadIO m => Db (App m) where
    openDbHandle config = initConnectionMongo (db_config config) >>= \case
        Left err -> liftIO . print $ renderDbError err
        Right p -> installPipe p config
    evalDb = evalMongo

instance Db IO where
    openDbHandle = openDbHandle
    evalDb = evalMongo

{- Setup -}

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
        alertGiveUp err = alert err >> pure (Left ())
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
        Right retried -> if not $ failed retried then pure DbOk else liftIO $ giveUp env retried
    where
        giveUp config err = do
            writeChan (postjobs config) . JobTgAlert $
                "withMongo failed with " `T.append` (T.pack . show $ err) `T.append`
                " If the connector timed out, one retry will be carried out."
            pure . DbErr $ FailedToUpdate "writeOrRetry failed for this reason: " (T.pack . show $ res)

buildSearchQuery :: S.Set T.Text -> Maybe UTCTime -> [Document]
buildSearchQuery ws mb_last_time =
    let search = [
            "$search" =: [
                "index" =: ("default" :: T.Text),
                "text" =: [
                    "query" =: S.toList ws,
                    "fuzzy" =: ([] :: Document),
                    "path" =: (["i_desc", "i_title"] :: [T.Text])
                ]]]
        match t = ["$match" =: ["i_pubdate" =: ["$gt" =: (t :: UTCTime)]]]
        project = [
            "$project" =: [
                "_id" =: (0 :: Int),
                "i_title" =: (1 :: Int),
                "i_feed_link" =: (1 :: Int),
                "i_link"=: (1 :: Int),
                "i_pubdate" =: (1 :: Int),
                "score" =: [ "$meta" =: ("searchScore" :: T.Text)]
            ]]
    in  case mb_last_time of
            Nothing -> [search, project]
            Just t -> [search, match t, project]

evalMongo :: (Db m, MonadIO m) => AppConfig -> DbAction -> m DbRes
evalMongo env (DbAskForLogin uid cid) =
    let selector = ["admin_uid" =: uid, "admin_chatid" =: cid]
        get_doc = findOne $ select selector "admins"
        write_doc h n = insert_ "admins" . writeDoc $ AdminUser uid h cid n
        delete_doc = deleteOne $ select selector "admins"
    in  liftIO getCurrentTime >>= \now ->
        withMongo env get_doc >>= \case
            Left _ -> pure . DbErr $ FaultyToken
            Right Nothing -> do
                h <- mkSafeHash
                _ <- withMongo env (write_doc h now)
                pure $ DbToken h
            Right (Just doc) -> do
                when (diffUTCTime now (admin_created . readDoc $ doc) > 2592000) (withMongo env delete_doc >> pure ())
                pure $ DbToken . admin_token . readDoc $ doc
    where
        mkSafeHash = liftIO getSystemTime <&>
            T.pack . show . hashWith SHA256 . B.pack . show
evalMongo env (CheckLogin h) =
    let r = findOne (select ["admin_token" =: h] "admins")
        del = deleteOne (select ["admin_token" =: h] "admins")
    in  liftIO getCurrentTime >>= \now ->
        withMongo env r >>= \case
            Left _ -> nope
            Right Nothing -> nope
            Right (Just doc) ->
                if diffUTCTime now (admin_created . readDoc $ doc) < 2592000
                then pure $ DbLoggedIn (admin_chatid $ readDoc doc)
                else withMongo env del >> nope
    where
        nope = pure . DbErr $ FaultyToken
evalMongo env (ArchiveItems feeds) =
    let selector = foldMap (map (\i -> (["i_link" =: i_link i], writeDoc i, [Upsert])) . f_items) feeds
        action = withMongo env $ updateAll "items" selector
    in  action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate mempty "ArchiveItems failed"
        Right res -> writeOrRetry res env action
evalMongo env (DbSearch keywords scope last_time) =
    let action = aggregate "items" $ buildSearchQuery keywords last_time
    in  withMongo env action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate mempty "DbSearch failed"
        Right res  ->
            let mkSearchRes doc =
                    let title = M.lookup "i_title" doc
                        link = M.lookup "i_link" doc
                        pubdate = M.lookup "i_pubdate" doc :: Maybe UTCTime
                        f_link = M.lookup "i_feed_link" doc
                        score = M.lookup "score" doc :: Maybe Double
                        nothings = [isNothing title, isNothing link, isNothing pubdate, isNothing f_link, isNothing score]
                    in  if or nothings then Nothing else Just $ SearchResult {
                            sr_title = fromJust title,
                            sr_link = fromJust link,
                            sr_pubdate = fromJust pubdate,
                            sr_feedlink = fromJust f_link,
                            sr_score = fromJust score
                    }
                sort_limit = take 10 . sortOn (Down . sr_score)
                rescind = filter (\sr -> sr_feedlink sr `elem` scope)
                payload r =
                    if null scope then sort_limit r
                    else sort_limit . rescind $ r
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
            else pure . DbFeeds $ map readDoc docs
evalMongo env GetAllChats =
    let action = find (select [] "chats")
    in  withMongo env (action >>= rest) >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate mempty "GetAllChats failed"
        Right docs ->
            if null docs then pure DbNoChat
            else pure $ DbChats . map readDoc $ docs
evalMongo env (GetFeed link) =
    let action = withMongo env $ findOne (select ["f_link" =: link] "feeds")
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate mempty "GetFeed failed"
        Right (Just doc) -> pure $ DbFeeds [bsonToFeed doc]
        Right Nothing -> pure DbNoFeed
evalMongo env (IncReads links) =
    let action l = withMongo env $ modify (select ["f_link" =: l] "feeds") ["$inc" =: ["f_reads" =: (1 :: Int)]]
    in  traverse_ action links >> pure DbOk
evalMongo env (PruneOld t) =
    let del_items = deleteAll "items" [(["i_pubdate" =: ["$lt" =: (t :: UTCTime)]], [])]
        del_digests = deleteAll "digests" [(["digest_created" =: ["$lt" =: t]], [])]
    in  withMongo env (del_items >> del_digests) >> pure DbOk
evalMongo env (ReadDigest _id) =
    let mkSelector = case readMaybe . T.unpack $ _id :: Maybe ObjectId of
            Nothing -> case readMaybe . T.unpack $ _id :: Maybe Int of
                Nothing -> Left FailedToProduceValidId
                Just n -> Right ["digest_id" =: n]
            Just oid -> Right ["_id" =: oid ]
        action s = findOne $ select s "digests"
    in  case mkSelector of
        Left err -> pure $ DbErr err
        Right selector ->  withMongo env (action selector) >>= \case
            Left () -> pure . DbErr $ FailedToUpdate "digest" "Read digest refused to read from the database."
            Right doc -> maybe (pure DbNoDigest) (pure . DbDigest . readDoc) doc
evalMongo env (UpsertChat chat) =
    let action = withMongo env $ upsert (select ["sub_chatid" =: sub_chatid chat] "chats") $ writeDoc chat
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.pack . show . sub_chatid $ chat) "UpsertChat failed"
        Right _ -> pure DbOk
evalMongo env (UpsertChats chatshmap) =
    let chats = HMS.elems chatshmap
        selector = map (\c -> (["sub_chatid" =: sub_chatid c], writeDoc c, [Upsert])) chats
        action = withMongo env $ updateAll "chats" selector
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.intercalate ", " (map (T.pack . show . sub_chatid) chats)) "UpsertChats failed"
        Right res -> writeOrRetry res env action
evalMongo env (UpsertFeeds feeds) =
    let selector = map (\f -> (["f_link" =: f_link f], writeDoc f, [Upsert])) feeds
        action = withMongo env $ updateAll "feeds" selector
    in  action >>= \case
        Left _ -> pure $ DbErr $ FailedToUpdate (T.intercalate ", " (map f_link feeds)) mempty
        Right res -> writeOrRetry res env action
evalMongo env (View flinks start end) =
    let query = find (select ["i_feed_link" =: ["$in" =: (flinks :: [T.Text])], "i_pubdate" =: ["$gt" =: (start :: UTCTime), "$lt" =: (end :: UTCTime)]] "items") >>= rest
    in  withMongo env query >>= \case
        Left _ -> pure $ DbErr FailedToLoadFeeds
        Right is -> pure $ DbView (map readDoc is) start end
evalMongo env (WriteDigest digest) =
    let action = insert "digests" $ writeDoc digest
    in  withMongo env action >>= \case
        Left _ -> pure . DbErr $ FailedToUpdate "digest" "Db refused to insert digest items"
        Right res -> case res of
            ObjId _id -> pure $ DbDigestId . T.pack . show $ _id
            _ -> pure . DbErr $ FailedToProduceValidId

{- Items -}

itemToBson :: Item -> Document
itemToBson i = [
    "i_title" =: i_title i,
    "i_desc" =: i_desc i,
    "i_feed_link" =: i_feed_link i,
    "i_link" =: i_link i,
    "i_pubdate" =: i_pubdate i
    ]

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
    [
        "f_avg_interval" =: (realToFrac <$> f_avg_interval :: Maybe NominalDiffTime),
        "f_desc" =: f_desc,
        "f_items" =: map writeDoc (take 30 . sortOn (Down . i_pubdate) $ f_items),
        "f_last_refresh" =: f_last_refresh,
        "f_link" =: f_link,
        "f_reads" =: f_reads,
        "f_title" =: f_title,
        "f_type" =: (T.pack . show $ f_type)
    ]

bsonToFeed :: Document -> Feed
bsonToFeed doc =
    let raw_items = fromJust $ M.lookup "f_items" doc
        items = map readDoc raw_items
    in  Feed {
            f_avg_interval = M.lookup "f_avg_interval" doc,
            f_desc = fromJust $ M.lookup "f_desc" doc,
            f_items = items,
            f_last_refresh = M.lookup "f_last_refresh" doc,
            f_link = fromJust $ M.lookup "f_link" doc,
            f_reads = fromJust $ M.lookup "f_reads" doc,
            f_title = fromJust $ M.lookup "f_title" doc,
            f_type = if fromJust (M.lookup "f_type" doc) == (T.pack . show $ Rss) then Rss else Atom
        }

{- Chats, Settings -}

bsonToChat :: Document -> SubChat
bsonToChat doc =
    let feeds_links = fromJust $ M.lookup "sub_feeds_links" doc :: [T.Text]
        settings_doc = fromJust $ M.lookup "sub_settings" doc :: Document
        feeds_settings_docs = Settings {
            settings_word_matches = WordMatches
                (maybe S.empty S.fromList $ M.lookup "settings_blacklist" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_searchset" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_only_search_results" settings_doc),
            settings_digest_interval =
                let every = M.lookup "settings_digest_every_secs" settings_doc :: Maybe NominalDiffTime
                    hm_docs = M.lookup "settings_digest_at" settings_doc :: Maybe [Document]
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
                in  DigestInterval every (if null $ extract hm_docs then Nothing else Just $ extract hm_docs),
            settings_digest_collapse = fromMaybe (settings_digest_collapse defaultChatSettings) $ M.lookup "settings_digest_collapse" settings_doc,
            settings_digest_size = fromMaybe (settings_digest_size defaultChatSettings) $ M.lookup "settings_digest_size" settings_doc :: Int,
            settings_digest_title = fromMaybe (settings_digest_title defaultChatSettings) $ M.lookup "settings_digest_title" settings_doc,
            settings_digest_start = fromMaybe (settings_digest_start defaultChatSettings) $ M.lookup "settings_digest_start" settings_doc :: Maybe UTCTime,
            settings_paused = Just True == M.lookup "settings_paused" settings_doc,
            settings_disable_web_view = fromMaybe (settings_disable_web_view defaultChatSettings) $ M.lookup "settings_disable_web_view" settings_doc,
            settings_pin = fromMaybe (settings_pin defaultChatSettings) $ M.lookup "settings_pin" settings_doc,
            settings_share_link = fromMaybe (settings_share_link defaultChatSettings) $ M.lookup "settings_share_link" settings_doc,
            settings_follow = fromMaybe (settings_follow defaultChatSettings) $ M.lookup "settings_follow" settings_doc
            }
    in  SubChat {
            sub_chatid = fromJust $ M.lookup "sub_chatid" doc,
            sub_last_digest = M.lookup "sub_last_digest" doc,
            sub_next_digest = M.lookup "sub_next_digest" doc,
            sub_feeds_links = S.fromList feeds_links,
            sub_settings = feeds_settings_docs
        }

chatToBson :: SubChat -> Document
chatToBson (SubChat chat_id last_digest next_digest flinks settings) =
    let blacklist = S.toList . match_blacklist . settings_word_matches $ settings
        searchset = S.toList . match_searchset . settings_word_matches $ settings
        only_search_results = S.toList . match_only_search_results . settings_word_matches $ settings
        settings' = [
            "settings_blacklist" =: blacklist,
            "settings_digest_collapse" =: settings_digest_collapse settings,
            "settings_disable_web_view" =: settings_disable_web_view settings,
            "settings_digest_size" =: settings_digest_size settings,
            "settings_digest_title" =: settings_digest_title settings,
            "settings_digest_start" =: settings_digest_start settings,
            "settings_follow" =: settings_follow settings,
            "settings_only_search_results" =: only_search_results,
            "settings_paused" =: settings_paused settings,
            "settings_pin" =: settings_pin settings,
            "settings_searchset" =: searchset,
            "settings_share_link" =: settings_share_link settings
            ]
        with_secs = maybe []
            (\secs -> ["settings_digest_every_secs" =: secs])
            (digest_every_secs . settings_digest_interval $ settings)
        with_at = maybe []
            (\hm -> ["settings_digest_at" =: map (\(h, m) -> ["hour" =: h, "minute" =: m]) hm])
            (digest_at . settings_digest_interval $ settings)
    in  [
            "sub_chatid" =: chat_id,
            "sub_last_digest" =: last_digest,
            "sub_next_digest" =: next_digest,
            "sub_feeds_links" =: S.toList flinks,
            "sub_settings" =: settings' ++ with_secs ++ with_at
        ]

{- digests -}

bsonToDigest :: Document -> Digest
bsonToDigest doc =
    let items = map readDoc . fromJust $ M.lookup "digest_items" doc
        created = fromJust $ M.lookup "digest_created" doc
        _id = M.lookup "_id" doc :: Maybe ObjectId
        flinks = fromJust $ M.lookup "digest_flinks" doc
        ftitles = fromMaybe [] $ M.lookup "digest_ftitles" doc
    in  Digest _id created items flinks ftitles

digestToBson :: Digest -> Document
digestToBson Digest{..} = [
    "digest_created" =: (digest_created :: UTCTime),
    "digest_items" =: map writeDoc digest_items,
    "digest_flinks" =: digest_links,
    "digest_ftitles"=: digest_titles
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

logToBson :: LogItem -> Document
logToBson LogPerf{..} = [
    "log_message" =: log_message,
    "log_at" =: log_at,
    "log_refresh" =: log_refresh,
    "log_sending_notif" =: log_sending_notif,
    "log_update" =: log_updating,
    "log_total" =: log_total
    ]

saveToLog :: (Db m, MonadIO m) => AppConfig -> LogItem -> m ()
saveToLog env logitem = withMongo env (insert "logs" $ writeDoc logitem) >> pure ()

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
            let logs = sortOn log_at . filter (not . T.null . log_message) . map readDoc $ docs_logs
                feeds_counts = map (\d -> let f = readDoc d in (f_link f, T.pack . show $ f_reads f)) feeds_docs
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

{- Admins -}

bsonToAdmin :: Document -> AdminUser
bsonToAdmin doc =
    let uid = fromJust $ M.lookup "admin_uid" doc
        token = fromJust $ M.lookup "admin_token" doc
        cid = fromJust $ M.lookup "admin_chatid" doc
        created = fromJust $ M.lookup "admin_created" doc
    in  AdminUser uid token cid created

adminToBson :: AdminUser -> Document
adminToBson AdminUser{..} = [
        "admin_uid" =: admin_uid,
        "admin_chatid" =: admin_chatid,
        "admin_token" =: admin_token,
        "admin_created" =: admin_created
    ]

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

{- Mapping -}

remapChats :: (MonadIO m, Db m) => AppConfig -> SubChats -> m (Either String ())
remapChats env chats =
    let selector = map (\c -> (["sub_chatid" =: sub_chatid c], writeDoc c, [Upsert])) $ HMS.elems chats
        action = updateAll "chats" selector
    in  withMongo env action >>= \case Left _ -> pure $ Left "Failed"; Right _ -> pure $ Right ()

checkDbMapper :: MonadIO m => m ()
checkDbMapper = do
    now <- liftIO getCurrentTime
    let item = Item mempty mempty mempty mempty now
        digest_interval = DigestInterval (Just 0) (Just [(1,20)])
        word_matches = WordMatches S.empty S.empty (S.fromList ["1","2","3"])
        settings = Settings (Just 3) digest_interval 0 Nothing "title" True False False word_matches False False
        chat = SubChat 0 (Just now) (Just now) S.empty settings
        feed = Feed Rss "1" "2" "3" [item] (Just 0) (Just now) 0
        log' = LogPerf mempty now 0 0 0 0
        digest = Digest Nothing now [item] [mempty] [mempty]
        admin_user = AdminUser 123 "just user" 456 now
        equalities = [
            ("item", checks item),
            ("digest", checks digest),
            ("feed", checks feed),
            ("chat", checks chat),
            ("log", checks log'),
            ("admin", checks admin_user)] :: [(T.Text, Bool)]
    if all snd equalities
    then pure ()
    else liftIO $ do
        print digest
        print (readDoc . writeDoc $ digest :: Digest)
        throwIO . userError $ "Mapper failed. " ++ (show . filter (not . snd) $ equalities)
        