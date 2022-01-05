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
import qualified Database.MongoDB.Transport.Tls as DbTLS
import GHC.IORef (atomicSwapIORef)
import TgramOutJson (ChatId)

{- Interface -}

class Db m where
    openDbHandle :: AppConfig -> m ()
    evalDb :: AppConfig -> DbAction -> m (DbRes a)

instance MonadIO m => Db (App m) where
    openDbHandle config = getFreshPipe (db_config config) >>= flip installPipe config
    evalDb = evalMongo

instance Db IO where
    openDbHandle config = getFreshPipe (db_config config) >>= flip installPipe config
    evalDb = evalMongo

{- Evaluation -}

runMongo :: MonadIO m => Pipe -> Action IO a -> m a
runMongo pipe action = access pipe master "feedfarer" $ liftDB action

withMongo :: (Db m, MonadIO m) => AppConfig -> Action IO a -> m a
withMongo config action = liftIO $ do
    pipe <- getPipe config
    try (runMongo pipe action) >>= \case
        Left (SomeException _) -> openDbHandle config >> runMongo pipe action
        Right res -> pure res
    where   getPipe = readIORef . db_connector

{- Connection -}

initMongoCredsFrom :: T.Text -> T.Text -> DbCreds
initMongoCredsFrom h whole_line =
    let [host_name, db_name, password] = T.splitOn ":" h
    in MongoCreds host_name whole_line db_name password

getFreshPipe :: MonadIO m => DbCreds -> m Pipe
getFreshPipe creds = liftIO $ folded >>= \(Just p) -> pure p
    where
        folded = foldr check (pure Nothing) (T.splitOn ";" $ all_shards creds)
        check v acc =
            let new_creds = initMongoCredsFrom v $ all_shards creds
            in  connectPipe new_creds >>= \case
                Left _ -> acc
                Right pipe -> checkPipe pipe >>= \case
                    Left _ -> close pipe >> acc
                    Right _ -> pure $ Just pipe
        connectPipe new_creds = liftIO $ try (DbTLS.connect (T.unpack $ shard new_creds) (PortNumber 27017)) >>= \case
            Left (SomeException e) -> do
                putStrLn ("Error while trying to connect: " ++ show e)
                pure . Left $ PipeNotAcquired
            Right pipe -> do
                authorized <- access pipe UnconfirmedWrites "admin" $ auth (user new_creds) (pwd new_creds)
                if authorized then pure . Right $ pipe
                else pure . Left $ DbLoginFailed
        checkPipe pipe =
            let ref = "123" :: T.Text
                sample = ["ref" =: ref]
                go = runMongo pipe $ do
                    insert_ "test" sample
                    findOne (select sample "test") >>= \case
                        Nothing -> pure . Left $ DbChangedMaster
                        Just _ -> do
                            _ <- deleteAll "test" [(sample, [])]
                            pure . Right $ ()
            in liftIO $ go `catch` \(SomeException _) -> pure . Left $ DbChangedMaster

installPipe :: MonadIO m => Pipe -> AppConfig -> m ()
installPipe pipe config = void . liftIO $ atomicSwapIORef (db_connector config) pipe

{- Actions -}

evalMongo :: (Db m, MonadIO m) => AppConfig -> DbAction -> m (DbRes a)
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
        feeds_settings_docs = ChatSettings {
            settings_batch_size = fromJust $ M.lookup "settings_batch_size" settings_doc :: Int,
            settings_batch_interval =
                let raw_value = M.lookup "settings_batch_every" settings_doc :: Maybe NominalDiffTime
                    hm_docs = M.lookup "settings_batch_at" settings_doc :: Maybe [Document]
                    dflt = Secs 9000
                    adjust n
                        |   n < 6 && n > 0 = n * 10
                        |   otherwise = n
                in  case raw_value of
                    Nothing -> case hm_docs of
                        Nothing -> Secs 9000
                        Just docs ->
                            let collected = foldr (\d acc ->
                                    let h = M.lookup "hour" d :: Maybe Int
                                        m = M.lookup "minute" d :: Maybe Int
                                    in  case sequence [h, m] of
                                        Nothing -> []
                                        Just hm -> acc ++ [(head hm, adjust $ last hm)]) [] docs
                            in  if null collected then dflt
                                else HM collected
                    Just xs -> Secs xs,
            settings_filters = Filters
                (fromMaybe [] $ M.lookup "settings_blacklist" settings_doc)
                (fromMaybe [] $ M.lookup "settings_whitelist" settings_doc),
            settings_is_paused = fromMaybe False $ M.lookup "settings_is_paused" doc
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
    let settings' = [
            "settings_blacklist" =: (filters_blacklist . settings_filters $ sub_settings),
            "settings_whitelist" =: (filters_whitelist . settings_filters $ sub_settings),
            "settings_batch_size" =: settings_batch_size sub_settings,
            "settings_is_paused" =: settings_is_paused sub_settings
            ]
        settings = case settings_batch_interval sub_settings of
            Secs xs -> settings' ++ ["settings_batch_every" =: xs]
            HM hm -> settings' ++ ["settings_batch_at" =: map (\(h, m) -> ["hour" =: h, "minute" =: m]) hm]
    in  [
            "sub_chatid" =: sub_chatid,
            "sub_last_notification" =: sub_last_notification,
            "sub_feeds_links" =: (S.toList sub_feeds_links :: [T.Text]),
            "sub_settings" =: settings,
            "sub_next_notification" =: sub_next_notification
        ]

{- Batches -}

toBsonBatch :: UTCTime -> (ChatId, FeedLink, [Item]) -> Document
toBsonBatch now (cid, f, i) = ["created" =: now, "feed_link" =: f, "items" =: map itemToBson i, "chat_id" =: cid]

{- Logs -}

saveToLog :: (Db m, MonadIO m) => AppConfig -> LogItem -> m ()
saveToLog env item = withMongo env $ insert "logs" doc >> pure ()
    where
        doc = ["log_when" =: log_when item, "log_who" =: log_who item, "log_what" =: log_what item]

{- Cleanup -}

purgeCollections :: (Db m, MonadIO m) => AppConfig -> m (Either String ())
purgeCollections env =
    let collections = ["feeds", "chats", "logs", "test"]
    in  withMongo env $ traverse (`deleteAll` [([],[])]) collections >>= \res ->
        if any failed res then pure . Left $ "Failed to purge collections " ++ show collections
        else pure $ Right ()