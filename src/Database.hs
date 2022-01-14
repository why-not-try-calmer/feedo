{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where
import AppTypes
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (readIORef)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Database.MongoDB
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Transport.Tls as Tls
import GHC.IORef (atomicSwapIORef)
import TgramOutJson (ChatId)

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

{- Evaluation -}

runMongo :: MonadIO m => Pipe -> Action IO a -> m a
runMongo pipe action = access pipe master "feedfarer" $ liftDB action

withMongo :: (Db m, MonadIO m) => AppConfig -> Action IO a -> m a
withMongo config action = liftIO $ do
    MongoPipe pipe <- getPipe config
    try (runMongo pipe action) >>= \case
        Left (SomeException _) -> openDbHandle config >> runMongo pipe action
        Right res -> pure res
    where
        getPipe = readIORef . db_connector

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

installPipe :: MonadIO m => DbConnector -> AppConfig -> m ()
installPipe (MongoPipe pipe) config = void . liftIO $ atomicSwapIORef
    (db_connector config)
    (MongoPipe pipe)
installPipe _ _ = undefined

{- Actions -}

evalMongo :: (Db m, MonadIO m) => AppConfig -> DbAction -> m DbRes
evalMongo env (UpsertFeeds feeds) =
    let selector = map (\f -> (["f_link" =: f_link f], feedToBson f, [Upsert])) feeds
    in  withMongo env $ updateAll "feeds" selector >>= \res ->
        let titles = T.intercalate ", " $ map f_link feeds
        in  if failed res then pure . DbErr . FailedToUpdate $ titles
            else pure DbOk
evalMongo env (GetFeed link) =
    withMongo env $ findOne (select ["f_link" =: link] "feeds") >>= \case
        Just doc -> pure $ DbFeeds [bsonToFeed doc]
        Nothing -> pure DbNoFeed
evalMongo env Get100Feeds =
    withMongo env $ find (select [] "feeds") {sort = [ "f_reads" =: (-1 :: Int)], limit = 100}
        >>= rest >>= \docs ->
            if null docs then pure DbNoFeed
            else pure . DbFeeds $ map bsonToFeed docs
evalMongo env (RemoveFeeds links) = do
    _ <- withMongo env $ deleteAll "feeds" $ map (\l -> (["f_link" =: l], [])) links
    pure DbOk
evalMongo env (GetChat cid) =
    withMongo env $ findOne (select ["sub_chatid" =: cid] "chats") >>= \case
        Just doc -> pure $ DbChats [bsonToChat doc]
        Nothing -> pure DbNoChat
evalMongo env GetAllChats =
    withMongo env $ find (select [] "chats") >>= rest >>= \docs ->
        if null docs then pure DbNoChat
        else pure $ DbChats . map bsonToChat $ docs
evalMongo env (UpsertChat chat) = do
    withMongo env $ upsert (select ["sub_chatid" =: sub_chatid chat] "chats") $ chatToBson chat
    pure DbOk
evalMongo env (UpsertChats chatshmap) =
    let chats = HMS.elems chatshmap
        selector = map (\c -> (["sub_chatid" =: sub_chatid c], chatToBson c, [Upsert])) chats
    in  withMongo env $ updateAll "chats" selector >>= \res ->
            let chatids = T.intercalate ", " $ map (T.pack . show . sub_chatid) chats
            in  if failed res then pure . DbErr . FailedToUpdate $ chatids
                else pure DbOk
evalMongo env (DeleteChat cid) = do
    withMongo env $ deleteOne (select ["sub_chatid" =: cid] "chats")
    pure DbOk
evalMongo env (IncReads links) =
    let action l = withMongo env $ modify (select ["f_link" =: l] "feeds") ["$inc" =: ["f_reads" =: (1 :: Int)]]
    in  traverse_ action links >> pure DbOk

{- Feeds -}

itemToBson :: Item -> Document
itemToBson i = ["i_title" =: i_title i, "i_desc" =: i_desc i, "i_link" =: i_link i, "i_pubdate" =: i_pubdate i]

feedToBson :: Feed -> Document
feedToBson Feed {..} =
    [ "f_type" =: show f_type,
        "f_desc" =: f_desc,
        "f_title" =: f_title,
        "f_link" =: f_link,
        "f_items" =: map itemToBson f_items,
        "f_avg_interval" =: (realToFrac <$> f_avg_interval :: Maybe Double),
        "f_last_refresh" =: f_last_refresh,
        "f_reads" =: f_reads
    ]

bsonToFeed :: Document -> Feed
bsonToFeed doc =
    let raw_items = fromJust $ M.lookup "f_items" doc
        items = map (\i -> Item (fromJust $ M.lookup "i_title" i) (fromJust $ M.lookup "i_desc" i) (fromJust $ M.lookup "i_link" i) (fromMaybe (fromJust $ M.lookup "f_link" doc) $ M.lookup "i_feed_link" i) (fromJust $ M.lookup "i_pubdate" i)) raw_items
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
            settings_pin = fromMaybe False $ M.lookup "settings_pin" doc
            }
    in  SubChat {
            sub_chatid = fromJust $ M.lookup "sub_chatid" doc,
            sub_last_notification = M.lookup "sub_last_notification" doc,
            sub_next_notification = M.lookup "sub_next_notification" doc,
            sub_feeds_links = S.fromList feeds_links,
            sub_settings = feeds_settings_docs
        }

chatToBson :: SubChat -> Document
chatToBson SubChat{..} =
    let blacklist = ["settings_blacklist" =: S.toList (match_blacklist (settings_word_matches sub_settings))]
        searchset = ["settings_searchset" =: S.toList (match_searchset (settings_word_matches sub_settings))]
        only_search_results = ["settings_blacklist" =: S.toList (match_only_search_results (settings_word_matches sub_settings))]
        settings = [
            "settings_blacklist" =: blacklist,
            "settings_searchset" =: searchset,
            "settings_only_search_results" =: only_search_results,
            "settings_batch_size" =: settings_batch_size sub_settings,
            "settings_paused" =: settings_paused sub_settings,
            "settings_disable_web_view" =: settings_disable_web_view sub_settings,
            "settings_pin" =: settings_pin sub_settings
            ]
        with_secs = maybe []
            (\secs -> ["settings_batch_every_secs" =: secs])
            (batch_every_secs . settings_batch_interval $ sub_settings)
        with_at = maybe []
            (\hm -> ["settings_batch_at" =: map (\(h, m) -> ["hour" =: h, "minute" =: m]) hm])
            (batch_at . settings_batch_interval $ sub_settings)
    in  [
            "sub_chatid" =: sub_chatid,
            "sub_last_notification" =: sub_last_notification,
            "sub_feeds_links" =: (S.toList sub_feeds_links :: [T.Text]),
            "sub_settings" =: settings ++ with_secs ++ with_at,
            "sub_next_notification" =: sub_next_notification
        ]

{- Batches -}

toBsonBatch :: UTCTime -> (ChatId, FeedLink, [Item]) -> Document
toBsonBatch now (cid, f, i) = ["created" =: now, "feed_link" =: f, "items" =: map itemToBson i, "chat_id" =: cid]

{- Logs -}

saveToLog :: (Db m, MonadIO m) => AppConfig -> LogItem -> m ()
saveToLog env item = withMongo env $ insert "logs" doc >> pure ()
    where
        doc = [
            "log_when" =: log_when item,
            "log_who" =: log_who item,
            "log_what" =: log_what item,
            "log_n" =: log_n item
            ]

{- Cleanup -}

purgeCollections :: (Db m, MonadIO m) => AppConfig -> [T.Text] -> m (Either String ())
purgeCollections _ [] = pure . Left $ "Empty list of collection names!" 
purgeCollections env colls = 
    withMongo env $ traverse (`deleteAll` [([],[])]) colls >>= \res ->
        if any failed res then pure . Left $ "Failed to purge collections " ++ show colls
        else pure $ Right ()