{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Database where

import AppTypes (DbAction (..), DbError (..), DbRes (..), Feed (..), FeedSettings(..), FeedType (..), Item (..), MongoCreds (..), SubChat (..), Filters (Filters, filters_blacklist), FeedLink, LogItem (log_type, log_when, log_who, log_what), renderDbError)
import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.MongoDB
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Transport.Tls as DbTLS
import Data.Time (NominalDiffTime, UTCTime)
import TgramOutJson (ChatId)

{- Connection -}

initMongoCredsFrom :: T.Text -> T.Text -> MongoCreds
-- FIX ME: MongoAtlas tends to shuffle around the role of 'primary' versus 'secondary' shard
-- Make sure to call selectOK to avoid failing to authenticate
initMongoCredsFrom h whole_line =
    let [host_name, db_name, password] = T.splitOn ":" h
    in MongoCreds host_name whole_line db_name password

createPipe :: MonadIO m => MongoCreds -> m (Either DbError Pipe)
createPipe creds = liftIO $ try (DbTLS.connect (T.unpack $ shard creds) (PortNumber 27017)) >>= \case
    Left (SomeException e) -> do
        putStrLn ("Error while trying to connect: " ++ show e)
        pure . Left $ PipeNotAcquired
    Right pipe -> do
        authorized <- access pipe UnconfirmedWrites "admin" $ auth (user creds) (pwd creds)
        if authorized
        then pure . Right $ pipe
        else pure . Left $ DbLoginFailed

runMongo :: MonadIO m => Pipe -> Action m a -> m a
runMongo pipe = access pipe master "feedfarer"

withMongo :: MonadIO m => MongoCreds -> Action m a -> m a
withMongo config action = createPipe config >>= \case
    Left _ -> liftIO $ throwIO . userError $ "No Pipe!"
    Right pipe -> runMongo pipe action

evalMongoAct :: MonadIO m => MongoCreds -> DbAction -> m (DbRes a)
evalMongoAct creds (UpsertFeeds feeds) =
    let selector = map (\f -> (["f_link" =: f_link f], feedToBson f, [Upsert])) feeds
    in  withMongo creds $ updateAll "feeds" selector >>= \res ->
        let titles = T.intercalate ", " $ map f_link feeds
        in  if failed res then pure . DbErr . FailedToUpdateFeeds $ titles
            else pure DbOk
evalMongoAct creds (GetFeed link) =
    withMongo creds $ findOne (select ["f_link" =: link] "feeds") >>= \case
        Just doc -> pure $ DbFeeds [bsonToFeed doc]
        Nothing -> pure DbNoFeed
evalMongoAct creds Get100Feeds =
    withMongo creds $ find (select [] "feeds") {sort = [ "f_reads" =: (-1 :: Int)], limit = 100}
        >>= rest >>= \docs ->
            if null docs then pure DbNoFeed
            else pure . DbFeeds $ map bsonToFeed docs
evalMongoAct creds (RemoveFeeds links) = do
    _ <- withMongo creds $ deleteAll "feeds" $ map (\l -> (["f_link" =: l], [])) links
    pure DbOk
evalMongoAct creds (GetChat cid) =
    withMongo creds $ findOne (select ["sub_chatid" =: cid] "chats") >>= \case
        Just doc -> pure $ DbChats [bsonToChat doc]
        Nothing -> pure DbNoChat
evalMongoAct creds GetAllChats =
    withMongo creds $ find (select [] "chats") >>= rest >>= \docs ->
        if null docs then pure DbNoChat
        else pure $ DbChats . map bsonToChat $ docs
evalMongoAct creds (UpsertChat chat) = do
        withMongo creds $ upsert (select ["sub_chatid" =: sub_chatid chat] "chats") $ chatToBson chat
        pure DbOk
evalMongoAct creds (DeleteChat cid) = do
    withMongo creds $ deleteOne (select ["sub_chatid" =: cid] "chats")
    pure DbOk
evalMongoAct creds (IncReads links) =
    let action l = withMongo creds $ modify (select ["f_link" =: l] "feeds") ["$inc" =: ["f_reads" =: (1 :: Int)]]
    in  traverse_ action links >> pure DbOk

-- evalMongoAct creds _ = pure DbOk

getValidCreds :: MongoCreds -> IO MongoCreds
getValidCreds creds = folded >>= \(Just new_creds) -> pure new_creds
    where
        folded = foldr check (pure Nothing) (T.splitOn ";" $ all_shards creds)
        check v acc =
            let new_creds = initMongoCredsFrom v $ all_shards creds
            in  createPipe new_creds >>= \case
            Left _ -> acc
            Right pipe -> checkDbHealth pipe >>= \case
                Left _ -> close pipe >> acc
                Right _ -> do
                    close pipe
                    print $ "This one worked: " `T.append` v
                    pure $ Just new_creds
        checkDbHealth :: MonadIO m => Pipe -> m (Either DbError ())
        checkDbHealth pipe =
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
        "f_avgInterval" =: (realToFrac <$> f_avgInterval :: Maybe Double),
        "f_created" =: f_created,
        "f_lastRefresh" =: f_lastRefresh,
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
            f_avgInterval = M.lookup "f_avgInterval" doc,
            f_created = fromJust $ M.lookup "f_created" doc,
            f_lastRefresh = M.lookup "f_desc" doc,
            f_reads = fromJust $ M.lookup "f_reads" doc
        }

{- Chats -}

bsonToChat :: Document -> SubChat
bsonToChat doc =
    let raw_items = fromJust $ M.lookup "sub_last_notified" doc
        items = map (\i -> Item (fromJust $ M.lookup "i_title" i) (fromJust $ M.lookup "i_desc" i) (fromJust $ M.lookup "i_link" i) (fromJust $ M.lookup "i_feed_link" i) (fromJust $ M.lookup "i_pubdate" i)) raw_items
        feeds_links = fromJust $ M.lookup "sub_feeds_links" doc :: [T.Text]
        settings_doc = fromJust $ M.lookup "sub_settings" doc :: Document
        feeds_settings_docs = FeedSettings {
            settings_batch = fromJust $ M.lookup "settings_batch" settings_doc :: Bool,
            settings_batch_size = fromJust $ M.lookup "settings_batch_size" settings_doc :: Int,
            settings_batch_interval = M.lookup "settings_batch_interval" settings_doc :: Maybe NominalDiffTime,
            settings_filters = Filters $ fromJust $ M.lookup "settings_blacklist" settings_doc
            }
    in  SubChat {
            sub_chatid = fromJust $ M.lookup "sub_chatid" doc,
            sub_last_notified = items,
            sub_last_notification = fromJust $ M.lookup "sub_last_notification" doc,
            sub_feeds_links = S.fromList feeds_links,
            sub_settings = feeds_settings_docs,
            sub_is_paused = fromMaybe False $ M.lookup "sub_is_paused" doc
        }

chatToBson :: SubChat -> Document
chatToBson SubChat{..} =
    let last_notif_items = map (\i -> ["i_title" =: i_title i, "i_desc" =: i_desc i, "i_link" =: i_link i, "i_pubdate" =: i_pubdate i]) sub_last_notified
        settings = [
            "settings_blacklist" =: (filters_blacklist . settings_filters $ sub_settings),
            "settings_batch" =: settings_batch sub_settings,
            "settings_batch_size" =: settings_batch_size sub_settings,
            "settings_batch_interval" =: (realToFrac <$> settings_batch_interval sub_settings :: Maybe Double)
            ]
    in  [
            "sub_chatid" =: sub_chatid,
            "sub_last_notified" =: last_notif_items,
            "sub_last_notification" =: sub_last_notification,
            "sub_feeds_links" =: (S.toList sub_feeds_links :: [T.Text]),
            "sub_settings" =: settings,
            "sub_is_paused" =: sub_is_paused
        ]

{- Batches -}

toBsonBatch :: UTCTime -> (ChatId, FeedLink, [Item]) -> Document
toBsonBatch now (cid, f, i) = ["created" =: now, "feed_link" =: f, "items" =: map itemToBson i, "chat_id" =: cid]

{- Logs -}

saveToLog :: MonadIO m => MongoCreds -> LogItem -> m ()
saveToLog creds item = liftIO (try . withMongo creds $ insert "logs" doc :: IO (Either SomeException Value)) >>= \case
    Left _ -> liftIO $ print . renderDbError $ FailedToLog
    Right _ -> pure ()
    where   doc = ["log_type" =: log_type item, "log_when" =: log_when item, "log_who" =: log_who item, "log_what" =: log_what item]

{- Cleanup -}

purgeCollections :: MonadIO m => MongoCreds -> m (Either String ())
purgeCollections creds = 
    let collections = ["feeds", "chats", "logs", "test"]
    in  withMongo creds $ traverse (`deleteAll` [([],[])]) collections >>= \res ->
        if any failed res then pure . Left $ "Failed to purge collections " ++ show collections
        else pure $ Right ()