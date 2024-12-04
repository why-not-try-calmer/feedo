{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Mongo where

import Control.Exception
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask))
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteString.Char8 as B
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock.System (getSystemTime)
import Database.MongoDB
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Transport.Tls as Tls
import GHC.IORef (atomicModifyIORef', readIORef)
import Replies (render)
import Text.Read (readMaybe)
import TgramOutJson (ChatId, UserId)
import Types
import Utils (defaultChatSettings, findNextTime, freshLastXDays)

{- Interface -}

class MongoDoc v where
  readDoc :: Document -> v
  writeDoc :: v -> Document
  checks :: v -> Bool

instance MongoDoc SubChat where
  readDoc :: Document -> SubChat
  readDoc = bsonToChat
  writeDoc :: SubChat -> Document
  writeDoc = chatToBson
  checks :: SubChat -> Bool
  checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Feed where
  readDoc :: Document -> Feed
  readDoc = bsonToFeed
  writeDoc :: Feed -> Document
  writeDoc = feedToBson
  checks :: Feed -> Bool
  checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Item where
  readDoc :: Document -> Item
  readDoc = bsonToItem
  writeDoc :: Item -> Document
  writeDoc = itemToBson
  checks :: Item -> Bool
  checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc Digest where
  readDoc :: Document -> Digest
  readDoc = bsonToDigest
  writeDoc :: Digest -> Document
  writeDoc = digestToBson
  checks :: Digest -> Bool
  checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc LogItem where
  readDoc :: Document -> LogItem
  readDoc = undefined
  writeDoc :: LogItem -> Document
  writeDoc = logToBson
  checks :: LogItem -> Bool
  checks v = v == (readDoc . writeDoc $ v)

instance MongoDoc AdminUser where
  readDoc :: Document -> AdminUser
  readDoc = bsonToAdmin
  writeDoc :: AdminUser -> Document
  writeDoc = adminToBson
  checks :: AdminUser -> Bool
  checks v = v == (readDoc . writeDoc $ v)

{- Database -}

class (Monad m) => HasMongo m where
  evalDb :: DbAction -> m DbRes
  withDb :: Action IO a -> m (Either T.Text a)

setupDb :: (MonadIO m) => MongoCreds -> m (Either DbError (Pipe, MongoCreds))
setupDb credentials = do
  initConnectionMongo credentials >>= \case
    Left err -> pure $ Left err
    Right pipe -> pure $ Right (pipe, credentials)
 where
  initConnectionMongo creds@MongoCredsServer{..} = liftIO $ do
    pipe <- connect $ host (T.unpack host_name)
    verdict <- isClosed pipe
    if verdict then pure . Left $ PipeNotAcquired else loginDb creds pipe
  initConnectionMongo creds@MongoCredsTls{..} = liftIO $ do
    pipe <- Tls.connect (T.unpack host_name) db_port
    verdict <- isClosed pipe
    if verdict then pure . Left $ PipeNotAcquired else loginDb creds pipe
  initConnectionMongo creds@MongoCredsReplicaTls{..} = liftIO $ do
    repset <- openReplicaSetTLS (replicateset, hosts)
    mb_pipe <- primaryOrSecondary repset
    maybe
      (pure . Left $ PipeNotAcquired)
      (\p -> isClosed p >>= \v -> if v then pure . Left $ PipeNotAcquired else loginDb creds p)
      mb_pipe
  initConnectionMongo creds@MongoCredsReplicaSrv{..} = liftIO $ do
    repset <- openReplicaSetSRV' (T.unpack host_name)
    mb_pipe <- primaryOrSecondary repset
    maybe
      (pure . Left $ PipeNotAcquired)
      (\p -> isClosed p >>= \v -> if v then pure . Left $ PipeNotAcquired else loginDb creds p)
      mb_pipe
  loginDb creds pipe = do
    isAuth <- access pipe master admin $ auth (user_name creds) (password creds)
    if isAuth
      then liftIO (putStrLn "Authenticated now.") >> pure (Right pipe)
      else liftIO (putStrLn "Authentication failed.") >> pure (Left PipeNotAcquired)

instance (MonadIO m) => HasMongo (App m) where
  withDb :: (MonadIO m) => Action IO a -> App m (Either T.Text a)
  withDb action =
    ask >>= \env -> liftIO $ do
      pipe <- readIORef (snd . connectors $ env)
      let tryOnce = try (runMongo (database_name . mongo_creds $ env) pipe action)
      tryOnce >>= \case
        Left (SomeException err) -> do
          let err' = T.pack $ show err
              msg = "DB choked on: " `T.append` err'
          print msg
          -- alertAdmin (postjobs env) msg
          when (T.toCaseFold "pipe" `T.isInfixOf` err' || T.toCaseFold "connection" `T.isInfixOf` err') $ do
            closed <- isClosed pipe
            unless closed $ do
              let pipe_open_msg = "Pipe found open. Closing..."
              -- alertAdmin (postjobs env) pipe_open_msg
              putStrLn pipe_open_msg
              close pipe
            setupDb (mongo_creds env) >>= \case
              Left _ -> let choked = "Unable to recreate pipe." in putStrLn choked -- >> alertAdmin (postjobs env) choked
              Right (new_pipe, _) -> do
                let replacing_msg = "Pipe created. Replacing old."
                putStrLn replacing_msg
                -- alertAdmin (postjobs env) replacing_msg
                atomicModifyIORef' (snd $ connectors env) $ const (new_pipe, ())
          pure $ Left msg
        Right ok -> pure $ Right ok

  evalDb :: (MonadIO m) => DbAction -> App m DbRes
  evalDb (DbAskForLogin uid cid) = do
    let selector = ["admin_uid" =: uid, "admin_chatid" =: cid]
        get_doc = findOne $ select selector "admins"
        write_doc h n = insert_ "admins" . writeDoc $ AdminUser uid h cid n
        delete_doc = deleteOne $ select selector "admins"
    liftIO getCurrentTime >>= \now ->
      withDb get_doc >>= \case
        Left _ -> pure . Left $ FaultyToken
        Right Nothing -> do
          h <- mkSafeHash
          _ <- withDb (write_doc h now)
          pure . Right . DbToken $ h
        Right (Just doc) -> do
          when (diffUTCTime now (admin_created . readDoc $ doc) > 2592000) (void $ withDb delete_doc)
          pure . Right . DbToken . admin_token . readDoc $ doc
   where
    mkSafeHash =
      liftIO getSystemTime
        <&> T.pack
          . show
          . hashWith SHA256
          . B.pack
          . show
  evalDb (CheckLogin h) =
    let r = findOne (select ["admin_token" =: h] "admins")
        del = deleteOne (select ["admin_token" =: h] "admins")
     in liftIO getCurrentTime >>= \now ->
          withDb r >>= \case
            Left _ -> nope
            Right Nothing -> nope
            Right (Just doc) ->
              if diffUTCTime now (admin_created . readDoc $ doc) < 2592000
                then pure . Right . DbLoggedIn $ admin_chatid (readDoc doc)
                else withDb del >> nope
   where
    nope = pure . Left $ FaultyToken
  evalDb (ArchiveItems feeds) =
    let selector = foldMap (map (\i -> (["i_link" =: i_link i], writeDoc i, [Upsert])) . f_items) feeds
        action = withDb $ updateAll "items" selector
     in action >>= \case
          Left _ -> pure . Left $ FailedToUpdate "ArchiveItems" "Action failed during connection."
          Right res ->
            if failed res
              then pure . Left $ FailedToUpdate "ArchiveItems failed to write feeds" (T.pack . show $ res)
              else pure $ Right DbDone
  evalDb (BumpNotified cids now) =
    let failure = FailedToUpdate (T.pack . show $ cids) "BumpNotified failed"
        action = withDb $ do
          docs <- find (select ["sub_chatid" =: ["$in" =: cids]] "chats") >>= rest
          let chats =
                map
                  ( \doc ->
                      let
                        chat = readDoc doc :: SubChat
                        set_interval = settings_digest_interval . sub_settings $ chat
                        next_time = findNextTime now set_interval
                       in
                        chat{sub_last_digest = Just now, sub_next_digest = Just next_time}
                  )
                  docs
              selector = map (\c -> (["sub_chatid" =: sub_chatid c], writeDoc c, [Upsert])) chats
          updateAll "chats" selector
     in action >>= \case
          Left _ -> pure $ Left failure
          Right _ -> pure $ Right DbDone
  evalDb (DbSearch keywords scope mb_last_time) =
    let action = aggregate "items" $ buildSearchQuery keywords
     in withDb action >>= \case
          Left err -> pure . Left $ FailedToUpdate mempty ("DbSearch failed on :" `T.append` err)
          Right docs ->
            let toSearchRes doc =
                  SearchResult
                    <$> M.lookup "i_title" doc
                    <*> M.lookup "i_link" doc
                    <*> M.lookup "i_pubdate" doc
                    <*> M.lookup "i_feed_link" doc
                results = mapM toSearchRes docs
                f sr =
                  sr_feedlink sr
                    `S.member` scope
                    && maybe True (\t -> sr_pubdate sr >= t) mb_last_time
             in pure
                  . Right
                  $ DbSearchRes keywords scope
                  $ maybe mempty (filter f) results
  evalDb (DeleteChat cid) =
    let action = withDb $ deleteOne (select ["sub_chatid" =: cid] "chats")
     in action >>= \case
          Left _ -> pure $ Left $ FailedToUpdate mempty "DeleteChat failed"
          Right _ -> pure $ Right DbDone
  evalDb (GetChat cid) =
    let action = findOne (select ["sub_chatid" =: cid] "chats")
     in withDb action >>= \case
          Left _ -> pure . Left $ NotFound $ T.pack . show $ cid
          Right m_doc -> case m_doc of
            Nothing -> pure . Right $ DbNoChat
            Just doc -> pure . Right . DbChat . readDoc $ doc
  evalDb GetAllChats =
    let action = find (select [] "chats")
     in withDb (action >>= rest) >>= \case
          Left _ -> pure $ Left $ FailedToUpdate mempty "GetAllChats failed"
          Right docs ->
            if null docs
              then pure . Right $ DbNoChat
              else pure . Right . DbChats . map readDoc $ docs
  evalDb (GetSomeFeeds flinks) =
    let action = find (select ["f_link" =: ["$in" =: flinks]] "feeds")
     in withDb (action >>= rest) >>= \case
          Left _ -> pure $ Left $ FailedToUpdate mempty "GetAllFeeds failed"
          Right docs ->
            if null docs
              then pure $ Right DbNoFeed
              else pure . Right . DbFeeds $ map readDoc docs
  evalDb GetAllFeeds =
    let action = find (select [] "feeds")
     in withDb (action >>= rest) >>= \case
          Left _ -> pure $ Left $ FailedToUpdate mempty "GetAllFeeds failed"
          Right docs ->
            if null docs
              then pure . Right $ DbNoFeed
              else pure . Right . DbFeeds $ map readDoc docs
  evalDb (GetPages cid mid) =
    let action = withDb $ findOne (select ["chat_id" =: cid, "message_id" =: mid] "pages")
     in action >>= \case
          Left err -> pure $ Left . BadQuery $ err
          Right Nothing -> pure . Right $ DbNoPage cid mid
          Right (Just doc) -> case M.lookup "pages" doc of
            Just pages -> pure $ Right $ DbPages pages (M.lookup "url" doc)
            Nothing -> pure . Right $ DbNoPage cid mid
  evalDb (InsertPages cid mid pages mb_link) =
    let base_payload = ["chat_id" =: cid, "message_id" =: mid, "pages" =: pages]
        payload = case mb_link of
          Nothing -> base_payload
          Just l -> base_payload ++ ["url" =: l]
        action = withDb $ insert "pages" payload
     in action >>= \case
          Left _ -> pure . Left $ FailedToInsertPage
          _ -> pure . Right $ DbDone
  evalDb (PruneOld t) = do
    let del_items = deleteAll "items" [(["i_pubdate" =: ["$lt" =: (t :: UTCTime)]], [])]
        del_digests = deleteAll "digests" [(["digest_created" =: ["$lt" =: t]], [])]
    _ <- withDb (del_items >> del_digests)
    pure . Right $ DbDone
  evalDb (ReadDigest _id) =
    let mkSelector = case readMaybe . T.unpack $ _id :: Maybe ObjectId of
          Nothing -> case readMaybe . T.unpack $ _id :: Maybe Int of
            Nothing -> Left FailedToProduceValidId
            Just n -> Right ["digest_id" =: n]
          Just oid -> Right ["_id" =: oid]
        action s = findOne $ select s "digests"
     in case mkSelector of
          Left err -> pure $ Left err
          Right selector ->
            withDb (action selector) >>= \case
              Left _ -> pure . Left $ FailedToUpdate "digest" "Read digest refused to read from the database."
              Right doc -> maybe (pure . Right $ DbNoDigest) (pure . Right . DbDigest . readDoc) doc
  evalDb (UpsertChat chat) =
    let action = withDb $ upsert (select ["sub_chatid" =: sub_chatid chat] "chats") $ writeDoc chat
     in action >>= \case
          Left _ -> pure $ Left $ FailedToUpdate (T.pack . show . sub_chatid $ chat) "UpsertChat failed"
          Right _ -> pure . Right $ DbDone
  evalDb (UpsertChats chatshmap) =
    let chats = HMS.elems chatshmap
        selector = map (\c -> (["sub_chatid" =: sub_chatid c], writeDoc c, [Upsert])) chats
        action = withDb $ updateAll "chats" selector
     in action >>= \case
          Left _ -> pure $ Left $ FailedToUpdate (T.intercalate ", " (map (T.pack . show . sub_chatid) chats)) "UpsertChats failed"
          Right res ->
            if failed res
              then pure . Left $ FailedToUpdate "Failed to write feeds" (T.pack . show $ res)
              else pure . Right $ DbDone
  evalDb (UpsertFeeds feeds) =
    let selector = map (\f -> (["f_link" =: f_link f], writeDoc f, [Upsert])) feeds
        action = withDb $ updateAll "feeds" selector
     in action >>= \case
          Left _ -> pure $ Left $ FailedToUpdate (T.intercalate ", " (map f_link feeds)) mempty
          Right res ->
            if failed res
              then pure . Left $ FailedToUpdate "Failed to write feeds" (T.pack . show $ res)
              else pure . Right $ DbDone
  evalDb (View flinks start end) =
    let query = find (select ["i_feed_link" =: ["$in" =: (flinks :: [T.Text])], "i_pubdate" =: ["$gt" =: (start :: UTCTime), "$lt" =: (end :: UTCTime)]] "items") >>= rest
     in withDb query >>= \case
          Left _ -> pure $ Left FailedToLoadFeeds
          Right is -> pure . Right $ DbView (map readDoc is) start end
  evalDb (WriteDigest digest) =
    let action = insert "digests" $ writeDoc digest
     in withDb action >>= \case
          Left _ -> pure . Left $ FailedToUpdate "digest" "HasMongo refused to insert digest items"
          Right res -> case res of
            ObjId _id -> pure . Right . DbDigestId . T.pack . show $ _id
            _ -> pure . Left $ FailedToProduceValidId
  evalDb (GetXDays links days) = do
    now <- liftIO getCurrentTime
    evalDb GetAllFeeds
      >>= \case
        Left err -> pure . Left . NotFound $ render err
        Right (DbFeeds fs) ->
          let listed = HMS.fromList . map (\f -> (f_link f, f)) $ fs
              collected = foldFeeds listed now
           in pure . Right . DbLinkDigest $ collected
        _ -> pure . Left . NotFound $ "Unknown error from CacheXDays"
   where
    collect f acc now =
      let fresh = freshLastXDays days now $ f_items f
       in if null fresh then acc else (f_link f, fresh) : acc
    foldFeeds fs now = HMS.foldl' (\acc f -> if f_link f `notElem` links then acc else collect f acc now) [] fs

{- Connection -}

primaryOrSecondary :: ReplicaSet -> IO (Maybe Pipe)
primaryOrSecondary rep =
  try (primary rep) >>= \case
    Left (SomeException err) -> do
      putStrLn $
        "Failed to acquire primary replica, reason:"
          ++ show err
          ++ ". Moving to second."
      try (secondaryOk rep) >>= \case
        Left (SomeException _) -> pure Nothing
        Right pipe -> pure $ Just pipe
    Right pipe -> pure $ Just pipe

{- Evaluation -}

runMongo :: (MonadIO m) => T.Text -> Pipe -> Action m a -> m a
runMongo dbName pipe = access pipe master dbName

{- Search and indices -}

buildSearchQuery :: Keywords -> [Document]
{-
  Searches for the 10 best full-text matches for the given keywords, filtering in
  items from subscribed-to feeds, filtering out items saved before the last digest.
-}
buildSearchQuery keys =
  let searchExpr = ["$search" =: T.intercalate " " (S.toList keys)]
      matchStage =
        [ "$match"
            =: ["$text" =: searchExpr]
        ]
      sortStage = ["$sort" =: ["score" =: ["$meta" =: ("textScore" :: T.Text)]]]
      limitStage = ["$limit" =: (10 :: Int)]
      projectStage =
        [ "$project"
            =: [ "_id" =: (0 :: Int)
               , "i_title" =: (1 :: Int)
               , "i_feed_link" =: (1 :: Int)
               , "i_link" =: (1 :: Int)
               , "i_pubdate" =: (1 :: Int)
               , "score" =: ["$meta" =: ("searchScore" :: T.Text)]
               ]
        ]
   in [matchStage, sortStage, limitStage, projectStage]

itemsIndex :: Index
itemsIndex =
  let fields = ["i_desc" =: ("text" :: T.Text)]
   in Index "items" fields "items__i_desc__idx" True False Nothing

{- Items -}

itemToBson :: Item -> Document
itemToBson i =
  [ "i_title" =: i_title i
  , "i_feed_link" =: i_feed_link i
  , "i_link" =: i_link i
  , "i_pubdate" =: i_pubdate i
  ]

bsonToItem :: Document -> Item
bsonToItem doc =
  Item
    (fromJust $ M.lookup "i_title" doc)
    (fromJust $ M.lookup "i_link" doc)
    (fromJust $ M.lookup "i_feed_link" doc)
    (fromJust $ M.lookup "i_pubdate" doc)

{- Feeds -}

feedToBson :: Feed -> Document
feedToBson Feed{..} =
  [ "f_avg_interval" =: (realToFrac <$> f_avg_interval :: Maybe NominalDiffTime)
  , "f_desc" =: f_desc
  , "f_items" =: map writeDoc (take 30 . List.sortOn (Down . i_pubdate) $ f_items)
  , "f_last_refresh" =: f_last_refresh
  , "f_link" =: f_link
  , "f_title" =: f_title
  , "f_type" =: (T.pack . show $ f_type)
  ]

bsonToFeed :: Document -> Feed
bsonToFeed doc =
  let raw_items = fromJust $ M.lookup "f_items" doc
      items = map readDoc raw_items
   in Feed
        { f_avg_interval = M.lookup "f_avg_interval" doc
        , f_desc = fromJust $ M.lookup "f_desc" doc
        , f_items = items
        , f_last_refresh = M.lookup "f_last_refresh" doc
        , f_link = fromJust $ M.lookup "f_link" doc
        , f_title = fromJust $ M.lookup "f_title" doc
        , f_type = if fromJust (M.lookup "f_type" doc) == (T.pack . show $ Rss) then Rss else Atom
        }

{- Chats, Settings -}

bsonToChat :: Document -> SubChat
bsonToChat doc =
  let feeds_links = fromJust $ M.lookup "sub_feeds_links" doc :: [T.Text]
      linked_to_chats = M.lookup "sub_linked_to_chats" doc :: Maybe ChatId
      settings_doc = fromJust $ M.lookup "sub_settings" doc :: Document
      active_admins_doc = M.lookup "sub_active_admins" doc :: Maybe [Document]
      active_admins =
        let f d = (,) <$> (M.lookup "sub_active_userid" d :: Maybe UserId) <*> (M.lookup "sub_active_time" d :: Maybe UTCTime)
         in maybe mempty (mapM f) active_admins_doc
      feeds_settings_docs =
        Settings
          { settings_word_matches =
              WordMatches
                (maybe S.empty S.fromList $ M.lookup "settings_blacklist" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_searchset" settings_doc)
                (maybe S.empty S.fromList $ M.lookup "settings_only_search_results" settings_doc)
          , settings_digest_interval =
              let every = M.lookup "settings_digest_every_secs" settings_doc :: Maybe NominalDiffTime
                  hm_docs = M.lookup "settings_digest_at" settings_doc :: Maybe [Document]
                  adjust n
                    | n < 6 && n > 0 = n * 10
                    | otherwise = n
                  extract mds = case mds of
                    Nothing -> []
                    Just docs ->
                      foldr
                        ( \d acc ->
                            let h = M.lookup "hour" d :: Maybe Int
                                m = M.lookup "minute" d :: Maybe Int
                             in case sequence [h, m] of
                                  Nothing -> []
                                  Just hm -> acc ++ [(head hm, adjust $ last hm)]
                        )
                        []
                        docs
               in DigestInterval every (if null $ extract hm_docs then Nothing else Just $ extract hm_docs)
          , settings_digest_collapse = fromMaybe (settings_digest_collapse defaultChatSettings) $ M.lookup "settings_digest_collapse" settings_doc
          , settings_digest_size = fromMaybe (settings_digest_size defaultChatSettings) $ M.lookup "settings_digest_size" settings_doc :: Int
          , settings_digest_title = fromMaybe (settings_digest_title defaultChatSettings) $ M.lookup "settings_digest_title" settings_doc
          , settings_digest_start = fromMaybe (settings_digest_start defaultChatSettings) $ M.lookup "settings_digest_start" settings_doc :: Maybe UTCTime
          , settings_paused = Just True == M.lookup "settings_paused" settings_doc
          , settings_disable_web_view = fromMaybe (settings_disable_web_view defaultChatSettings) $ M.lookup "settings_disable_web_view" settings_doc
          , settings_pin = fromMaybe (settings_pin defaultChatSettings) $ M.lookup "settings_pin" settings_doc
          , settings_share_link = fromMaybe (settings_share_link defaultChatSettings) $ M.lookup "settings_share_link" settings_doc
          , settings_forward_to_admins = fromMaybe (settings_forward_to_admins defaultChatSettings) $ M.lookup "settings_forward_to_admins" settings_doc
          , settings_pagination = fromMaybe (settings_pagination defaultChatSettings) $ M.lookup "settings_pagination" settings_doc
          , settings_digest_no_collapse = maybe S.empty S.fromList $ M.lookup "settings_digest_no_collapse" settings_doc
          }
   in SubChat
        { sub_chatid = fromJust $ M.lookup "sub_chatid" doc
        , sub_last_digest = M.lookup "sub_last_digest" doc
        , sub_next_digest = M.lookup "sub_next_digest" doc
        , sub_feeds_links = S.fromList feeds_links
        , sub_linked_to = linked_to_chats
        , sub_settings = feeds_settings_docs
        , sub_active_admins = maybe mempty HMS.fromList active_admins
        }

chatToBson :: SubChat -> Document
chatToBson (SubChat chat_id last_digest next_digest flinks linked_to settings active_admins) =
  let blacklist = S.toList . match_blacklist . settings_word_matches $ settings
      searchset = S.toList . match_searchset . settings_word_matches $ settings
      only_search_results = S.toList . match_only_search_results . settings_word_matches $ settings
      active_admins' = map (\(uid, d) -> ["user_id" =: uid, "last_activity" =: d]) $ HMS.toList active_admins
      settings' =
        [ "settings_blacklist" =: blacklist
        , "settings_digest_collapse" =: settings_digest_collapse settings
        , "settings_disable_web_view" =: settings_disable_web_view settings
        , "settings_digest_size" =: settings_digest_size settings
        , "settings_digest_title" =: settings_digest_title settings
        , "settings_digest_start" =: settings_digest_start settings
        , "settings_forward_to_admins" =: settings_forward_to_admins settings
        , "settings_only_search_results" =: only_search_results
        , "settings_paused" =: settings_paused settings
        , "settings_pin" =: settings_pin settings
        , "settings_searchset" =: searchset
        , "settings_share_link" =: settings_share_link settings
        , "settings_pagination" =: settings_pagination settings
        , "settings_digest_no_collapse" =: S.toList (settings_digest_no_collapse settings)
        ]
      with_secs =
        maybe
          []
          (\secs -> ["settings_digest_every_secs" =: secs])
          (digest_every_secs . settings_digest_interval $ settings)
      with_at =
        maybe
          []
          (\hm -> ["settings_digest_at" =: map (\(h, m) -> ["hour" =: h, "minute" =: m]) hm])
          (digest_at . settings_digest_interval $ settings)
   in [ "sub_chatid" =: chat_id
      , "sub_last_digest" =: last_digest
      , "sub_next_digest" =: next_digest
      , "sub_feeds_links" =: S.toList flinks
      , "sub_linked_to_chats" =: linked_to
      , "sub_settings" =: settings' ++ with_secs ++ with_at
      , "sub_active_admins" =: active_admins'
      ]

{- Digests -}

bsonToDigest :: Document -> Digest
bsonToDigest doc =
  let items = map readDoc . fromJust $ M.lookup "digest_items" doc
      created = fromJust $ M.lookup "digest_created" doc
      _id = M.lookup "_id" doc :: Maybe T.Text
      flinks = fromJust $ M.lookup "digest_flinks" doc
      ftitles = fromMaybe [] $ M.lookup "digest_ftitles" doc
   in Digest _id created items flinks ftitles

digestToBson :: Digest -> Document
digestToBson Digest{..} =
  [ "digest_created" =: (digest_created :: UTCTime)
  , "digest_items" =: map writeDoc digest_items
  , "digest_flinks" =: digest_links
  , "digest_ftitles" =: digest_titles
  ]

{- Admins -}

bsonToAdmin :: Document -> AdminUser
bsonToAdmin doc =
  let uid = fromJust $ M.lookup "admin_uid" doc
      token = fromJust $ M.lookup "admin_token" doc
      cid = fromJust $ M.lookup "admin_chatid" doc
      created = fromJust $ M.lookup "admin_created" doc
   in AdminUser uid token cid created

adminToBson :: AdminUser -> Document
adminToBson AdminUser{..} =
  [ "admin_uid" =: admin_uid
  , "admin_chatid" =: admin_chatid
  , "admin_token" =: admin_token
  , "admin_created" =: admin_created
  ]

{- Logs -}

logToBson :: LogItem -> Document
logToBson (LogFailed (FeedError url status err _ attempt)) =
  [ "log_url" =: url
  , "log_status" =: status
  , "log_err" =: err
  , "log_last_attempt" =: attempt
  ]
logToBson (LogMissing missing total t) =
  [ "log_discarded_duplicates" =: missing
  , "log_total_discarded" =: total
  , "log_at" =: t
  , "log_type" =: ("discarded_duplicates" :: T.Text)
  ]
logToBson (LogDiscardedToRefreshRecipes to_refresh discarded recipes t) =
  [ "log_refresh" =: to_refresh
  , "log_discarded" =: discarded
  , "log_at" =: t
  , "log_recipes" =: recipes
  ]

saveToLog :: (HasMongo m, MonadIO m) => LogItem -> m ()
saveToLog logitem =
  let collection = case logitem of
        LogDiscardedToRefreshRecipes{} -> "logs_discarded"
        LogMissing{} -> "logs_missing"
        LogFailed _ -> "logs_failed"
      getDelta = do
        -- 30 days ago, in seconds
        now <- getCurrentTime
        pure $ posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - fromIntegral (30 * 86400 :: Integer)
      pruneLogsOn x_days_ago =
        let selector = ["log_at" =: ["$lt" =: (x_days_ago :: UTCTime)]]
            opts = []
         in deleteAll collection [(selector, opts)]
      thenInsert = insert collection $ writeDoc logitem
   in liftIO getDelta >>= \delta -> void $ withDb $ pruneLogsOn delta >> thenInsert

{- Tests -}

checkDbMapper :: (MonadIO m) => m ()
checkDbMapper = do
  now <- liftIO getCurrentTime
  let item = Item mempty mempty mempty now
      digest_interval = DigestInterval (Just 0) (Just [(1, 20)])
      word_matches = WordMatches S.empty S.empty (S.fromList ["1", "2", "3"])
      settings = Settings (Just 3) digest_interval 0 Nothing "title" True False True True False False word_matches mempty
      chat = SubChat 0 (Just now) (Just now) S.empty Nothing settings HMS.empty
      feed = Feed Rss "1" "2" "3" [item] (Just 0) (Just now)
      digest = Digest Nothing now [item] [mempty] [mempty]
      equalities =
        [ ("item", checks item)
        , ("digest", checks digest)
        , ("feed", checks feed)
        , ("chat", checks chat)
        ] ::
          [(T.Text, Bool)]
  unless (all snd equalities) . liftIO $ do
    print digest
    print (readDoc . writeDoc $ digest :: Digest)
    throwIO . userError $ "Mapper failed. " ++ (show . filter (not . snd) $ equalities)
