{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Concurrent (Chan, MVar)
import Control.Monad.List (foldM)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Data.Aeson
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM, rfc822DateFormat)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601
import Database.MongoDB (Host, ObjectId, Pipe, PortID)
import Database.Redis (Connection)
import Text.Read (readMaybe)
import TgramOutJson (ChatId, InlineKeyboardMarkup, UserId)

{- Replies -}

data Reply
    = ChatReply
        { reply_contents :: T.Text
        , reply_markdown :: Bool
        , reply_disable_webview :: Bool
        , reply_pin_on_send :: Bool
        , reply_pagination :: Bool
        , reply_permalink :: Maybe T.Text
        }
    | ServiceReply T.Text
    | EditReply
        { edit_message_id :: Int
        , edit_text :: T.Text
        , edit_markdown :: Bool
        , edit_pagination_keyboard :: Maybe InlineKeyboardMarkup
        }
    deriving (Show)

{- URLs -}

newtype Path = Path T.Text

{- Items, feeds, digests -}

data Item = Item
    { i_title :: T.Text
    , i_desc :: T.Text
    , i_link :: T.Text
    , i_feed_link :: T.Text
    , i_pubdate :: UTCTime
    }
    deriving (Eq, Show, Ord)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Item)

data FeedType = Rss | Atom deriving (Eq, Show)

$(deriveJSON defaultOptions ''FeedType)

data Feed = Feed
    { f_type :: FeedType
    , f_desc :: T.Text
    , f_title :: T.Text
    , f_link :: T.Text
    , f_items :: [Item]
    , f_avg_interval :: Maybe NominalDiffTime
    , f_last_refresh :: Maybe UTCTime
    , f_reads :: Int
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Feed)

data APIFeed = APIFeed
    { api_f_type :: FeedType
    , api_f_desc :: T.Text
    , api_f_title :: T.Text
    , api_f_link :: T.Text
    , api_f_items :: [Item]
    , api_f_avg_interval :: Maybe NominalDiffTime
    , api_f_last_refresh :: Maybe UTCTime
    , api_f_reads :: Int
    }
    deriving (Eq, Show)

data Digest = Digest
    { digest_id :: Maybe T.Text
    , digest_created :: UTCTime
    , digest_items :: [Item]
    , digest_links :: [T.Text]
    , digest_titles :: [T.Text]
    }
    deriving (Show, Eq)

instance FromJSON Digest where
    parseJSON = withObject "APIDigest" $ \o ->
        Digest
            <$> o .:? "_id"
            <*> o .: "digest_created"
            <*> o .: "digest_items"
            <*> o .: "digest_flinks"
            <*> o .: "digest_ftitles"

$(deriveToJSON defaultOptions{omitNothingFields = True} ''Digest)

{- Searches -}

type Keywords = S.Set T.Text

type FeedLink = T.Text

type Scope = S.Set FeedLink

data SearchResult = SearchResult
    { sr_title :: T.Text
    , sr_link :: T.Text
    , sr_pubdate :: UTCTime
    , sr_feedlink :: T.Text
    , sr_score :: Double
    }
    deriving (Show, Eq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''SearchResult)

{- Settings -}

data DigestInterval = DigestInterval
    { digest_every_secs :: Maybe NominalDiffTime
    , digest_at :: Maybe [(Int, Int)]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''DigestInterval)

data WordMatches = WordMatches
    { match_blacklist :: Keywords
    , match_searchset :: Keywords
    , match_only_search_results :: Scope
    }
    deriving (Show, Eq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''WordMatches)

data Settings = Settings
    { settings_digest_collapse :: Maybe Int
    , settings_digest_interval :: DigestInterval
    , settings_digest_size :: Int
    , settings_digest_start :: Maybe UTCTime
    , settings_digest_title :: T.Text
    , settings_disable_web_view :: Bool
    , settings_follow :: Bool
    , settings_pagination :: Bool
    , settings_paused :: Bool
    , settings_pin :: Bool
    , settings_share_link :: Bool
    , settings_word_matches :: WordMatches
    }
    deriving (Show, Eq)

$(deriveToJSON defaultOptions{omitNothingFields = True} ''Settings)

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \o ->
        let blacklisted = o .:? "settings_blacklist" .!= S.empty
            search_search_keywords = o .:? "settings_searchset" .!= S.empty
            search_only_results_flinks = o .:? "settings_only_search_results" .!= S.empty
            digest_every =
                (o .:? "settings_digest_every_secs") >>= \p ->
                    case mbNom <$> p of
                        Nothing -> pure $ Just 86400
                        Just t -> pure t
            _digest_at =
                (o .:? "settings_digest_at") >>= \case
                    Just (Array arr) ->
                        Just
                            <$> foldM
                                ( \acc v -> case v of
                                    Object kmap ->
                                        let hour = parseJSON $ fromMaybe "0" $ A.lookup "hour" kmap :: Parser Int
                                            minute = parseJSON $ fromMaybe "0" $ A.lookup "minute" kmap :: Parser Int
                                            res = (,) <$> hour <*> minute
                                         in (:) <$> res <*> pure acc
                                    _ -> pure acc
                                )
                                []
                                arr
                    _ -> pure Nothing
         in Settings
                <$> o .:? "settings_digest_collapse"
                <*> (DigestInterval <$> digest_every <*> _digest_at)
                <*> o .:? "settings_digest_size" .!= 10
                <*> o .:? "settings_digest_start"
                <*> o .:? "settings_digest_title" .!= mempty
                <*> o .:? "settings_disable_web_view" .!= False
                <*> o .:? "settings_follow" .!= False
                <*> o .:? "settings_pagination" .!= True
                <*> o .:? "settings_paused" .!= False
                <*> o .:? "settings_pin" .!= False
                <*> o .:? "settings_share_link" .!= True
                <*> (WordMatches <$> blacklisted <*> search_search_keywords <*> search_only_results_flinks)
      where
        mbNom :: String -> Maybe NominalDiffTime
        mbNom s =
            maybe second_pass pure $
                iso8601ParseM s >>= \t ->
                    pure . diffUTCTime t $ posixSecondsToUTCTime 0
          where
            second_pass = foldr step Nothing formats
            step f acc = maybe acc pure $ parseTimeM True defaultTimeLocale f s
            formats = [rfc822DateFormat, "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%d"]

data SubChat = SubChat
    { sub_chatid :: ChatId
    , sub_last_digest :: Maybe UTCTime
    , sub_next_digest :: Maybe UTCTime
    , sub_feeds_links :: S.Set FeedLink
    , sub_linked_to :: Maybe ChatId
    , sub_settings :: Settings
    }
    deriving (Show, Eq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''SubChat)

type SubChats = (HMS.HashMap ChatId SubChat)

{- Feed references -}

data FeedRef = ByUrl T.Text | ById Int deriving (Eq, Show)

{- Admins -}

data AdminUser = AdminUser
    { admin_uid :: UserId
    , admin_token :: T.Text
    , admin_chatid :: ChatId
    , admin_created :: UTCTime
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''AdminUser)

{- User actions, errors -}

data ParsingSettings
    = PDigestAt [(Int, Int)]
    | PDigestEvery NominalDiffTime
    | PDigestSize Int
    | PDigestStart UTCTime
    | PDigestTitle T.Text
    | PBlacklist (S.Set T.Text)
    | PDisableWebview Bool
    | PPagination Bool
    | PPaused Bool
    | PPin Bool
    | PDigestCollapse Int
    | PSearchKws (S.Set T.Text)
    | PSearchLinks (S.Set T.Text)
    | PShareLink Bool
    | PFollow Bool
    deriving (Show, Eq)

data SettingsUpdater = Parsed [ParsingSettings] | Immediate Settings deriving (Eq, Show)

data UserAction
    = About FeedRef
    | Announce T.Text
    | AskForLogin ChatId
    | AboutChannel ChatId FeedRef
    | Changelog
    | GetChannelItems ChatId FeedRef
    | GetItems FeedRef
    | GetLastXDaysItems Int
    | GetSubchannelSettings ChatId
    | GetSubchatSettings
    | Link ChatId
    | ListSubs
    | ListSubsChannel ChatId
    | Migrate ChatId
    | MigrateChannel ChatId ChatId
    | Pause Bool
    | PauseChannel ChatId Bool
    | Purge
    | PurgeChannel ChatId
    | RenderCmds
    | Reset
    | ResetChannel ChatId
    | Search [T.Text]
    | SetChannelSettings ChatId [ParsingSettings]
    | SetChatSettings SettingsUpdater
    | Start
    | Sub [T.Text]
    | SubChannel ChatId [T.Text]
    | TestDigest
    | TestDigestChannel ChatId
    | UnSub [FeedRef]
    | UnSubChannel ChatId [FeedRef]
    deriving (Eq, Show)

data UserError
    = BadFeedUrl T.Text
    | BadInput T.Text
    | BadRef T.Text
    | NotAdmin T.Text
    | NotFoundChat
    | NotFoundFeed T.Text
    | NotSubscribed
    | MaxFeedsAlready T.Text
    | ParseError T.Text
    | UpdateError T.Text
    | TelegramErr
    | Ignore T.Text
    | ChatNotPrivate
    | UserNotAdmin
    deriving (Eq, Show)

data ChatRes
    = ChatUpdated SubChat
    | ChatOk

{- Replies -}

data Replies
    = FromAdmin T.Text T.Text
    | FromAnnounce T.Text
    | FromChangelog
    | FromChatFeeds SubChat [Feed]
    | FromChat SubChat T.Text
    | FromCmds
    | FromFeedDetails Feed
    | FromFeedItems Feed
    | FromFeedLinkItems [(FeedLink, [Item])]
    | FromDigest [Feed] (Maybe T.Text) Settings
    | FromFollow [Feed] Settings
    | FromSearchRes Keywords [SearchResult]
    | FromStart
    deriving (Eq, Show)

data BatchRecipe
    = FollowFeedLinks [FeedLink]
    | DigestFeedLinks [FeedLink]
    deriving (Show, Eq)

data Batch
    = Follows [Feed]
    | Digests [Feed]
    deriving (Show, Eq)

{- Database actions, errors -}

data MongoCreds
    = MongoCredsTls
        { host_name :: String
        , database_name :: T.Text
        , user_name :: T.Text
        , password :: T.Text
        , db_port :: PortID
        }
    | MongoCredsReplicaTls
        { replicateset :: T.Text
        , hosts :: [Host]
        , user_name :: T.Text
        , password :: T.Text
        }
    | MongoCredsReplicaSrv
        { host_name :: String
        , database_name :: T.Text
        , user_name :: T.Text
        , password :: T.Text
        }
    deriving (Eq, Show)

type AdminToken = T.Text

data DbAction
    = DbAskForLogin UserId ChatId
    | CheckLogin AdminToken
    | ArchiveItems [Feed]
    | DeleteChat ChatId
    | GetAllFeeds
    | GetAllChats
    | GetPages ChatId Int
    | IncReads [FeedLink]
    | InsertPages ChatId Int [T.Text] (Maybe T.Text)
    | DbSearch Keywords Scope (Maybe UTCTime)
    | PruneOld UTCTime
    | ReadDigest T.Text
    | UpsertChat SubChat
    | UpsertChats SubChats
    | UpsertFeeds [Feed]
    | View [FeedLink] UTCTime UTCTime
    | WriteDigest Digest
    deriving (Show, Eq)

data DbRes
    = DbFeeds [Feed]
    | DbChats [SubChat]
    | DbNoChat
    | DbBadOID
    | DbNoDigest
    | DbErr DbError
    | DbOk
    | DbLoggedIn ChatId
    | DbToken T.Text
    | DbSearchRes Keywords [SearchResult]
    | DbView [Item] UTCTime UTCTime
    | DbDigest Digest
    | DbDigestId T.Text
    | DbNoPage ChatId Int
    | DbPages [T.Text] (Maybe T.Text)
    deriving (Eq, Show)

data DbError
    = PipeNotAcquired
    | FaultyToken
    | NotFound T.Text
    | FailedToUpdate T.Text T.Text
    | FailedToLog
    | FailedToLoadFeeds
    | BadQuery T.Text
    | FailedToSaveDigest
    | FailedToProduceValidId
    | FailedToInsertPage
    | FailedToGetAllPages
    deriving (Show, Eq)

data PageOne = PageOne
    { page_one :: T.Text
    , page_cid :: ChatId
    , page_mid :: Int
    , page_n :: Int
    , page_url :: Maybe T.Text
    }
    deriving (Show, Eq)

{- Cache -}

data CacheAction
    = CacheDeleteFeeds [T.Text]
    | CachePullFeed T.Text
    | CachePullFeeds [T.Text]
    | CachePushFeeds [Feed]
    | CacheRefresh
    | CacheXDays [FeedLink] Int
    | CacheGetPage ChatId Int Int
    | CacheSetPages ChatId Int [T.Text] (Maybe T.Text)

data FromCache
    = CacheOk
    | CacheNothing
    | CacheFeed Feed
    | CacheFeeds [Feed]
    | CacheMissed [FeedLink]
    | CacheDigests (HMS.HashMap ChatId (SubChat, Batch))
    | CacheLinkDigest [(FeedLink, [Item])]
    | CachePage T.Text Int (Maybe T.Text)
    deriving (Show, Eq)

type FeedItems = [(Feed, [Item])]

data Notifier
    = Pre {feeds_to_refresh :: [FeedLink], batch_recipes :: HMS.HashMap ChatId (SubChat, BatchRecipe), n_last_run :: Maybe UTCTime}
    | Post {discarded_items_links :: [T.Text], batches :: HMS.HashMap ChatId (SubChat, Batch)}
    deriving (Show, Eq)

{- Logs -}

data LogItem
    = LogPerf
        { log_message :: T.Text
        , log_at :: UTCTime
        , log_refresh :: Int64
        , log_sending_notif :: Int64
        , log_updating :: Int64
        , log_total :: Int64
        }
    | LogMissing
        { log_feeds_with_missing :: [T.Text]
        , log_total_missed :: Int
        , log_at :: UTCTime
        }
    | LogDigest
        { log_updated_feeds :: [FeedLink]
        , log_not_updated :: [FeedLink]
        , log_at :: UTCTime
        }
    | LogNoDigest
        { log_due_chats_with_no_digest :: [ChatId]
        , log_at :: UTCTime
        }
    | LogCouldNotArchive
        {log_no_achive :: [Feed], log_at :: UTCTime, log_error :: T.Text}
    deriving (Eq, Show)

$(deriveFromJSON defaultOptions{omitNothingFields = True} ''LogItem)

{- Background tasks -}

data Job
    = JobArchive [Feed] UTCTime
    | JobIncReadsJob [FeedLink]
    | JobLog LogItem
    | JobPin ChatId Int
    | JobPurge ChatId
    | JobRemoveMsg ChatId Int Int
    | JobSetPagination ChatId Int [T.Text] (Maybe T.Text)
    | JobTgAlert T.Text
    deriving (Eq, Show)

{- Mong API Requests -}

type APIKey = ByteString

data APIFilter = APIFilter
    { filter_chat_id :: Maybe ChatId
    , filter_message_id :: Maybe Int
    , filter__id :: Maybe ObjectId
    }

data Collection = CDigests | CPages | CFeeds | CChats

renderCollection :: Collection -> T.Text
renderCollection CChats = "chats"
renderCollection CFeeds = "feeds"
renderCollection CDigests = "digests"
renderCollection CPages = "pages"

data APIReq = APIReq
    { api_collection :: Collection
    , api_filter :: Maybe APIFilter
    }

data Pages = Pages
    { pages_chat_id :: ChatId
    , pages_message_id :: Int
    , pages_pages :: [T.Text]
    , pages_url :: Maybe T.Text
    }

{- Web responses -}

newtype ReadReq = ReadReq {read_req_hash :: T.Text}

data ReadResp = ReadResp
    { read_resp_settings :: Maybe Settings
    , read_resp_cid :: Maybe ChatId
    , read_resp_error :: Maybe T.Text
    }

data WriteReq = WriteReq
    { write_req_hash :: T.Text
    , write_req_settings :: Settings
    , write_req_confirm :: Maybe Bool
    }

data WriteResp = WriteResp
    { write_resp_status :: Int
    , write_resp_checkout :: Maybe T.Text
    , write_resp_error :: Maybe T.Text
    }

{- Instances -}

instance ToJSON APIFilter where
    toJSON (APIFilter cid mid _id) =
        let chat_id = ("chat_id" .=) <$> cid
            message_id = ("message_id" .=) <$> mid
            oid = (\v -> "_id" .= object [("$oid" :: Key, toJSON $ show v)]) <$> _id
         in object $ catMaybes [chat_id, message_id, oid]

instance ToJSON APIReq where
    toJSON (APIReq co fi) =
        let ba = Just $ "database" .= ("feedfarer" :: T.Text)
            so = Just $ "dataSource" .= ("Cluster0" :: T.Text)
            collection = Just $ "collection" .= renderCollection co
            filt = ("filter" .=) <$> fi
         in object $ catMaybes [ba, so, collection, filt]

instance FromJSON Pages where
    parseJSON = withObject "Pages" $ \o ->
        Pages
            <$> o .: "chat_id"
            <*> o .: "message_id"
            <*> o .: "pages"
            <*> o .:? "url"

$(deriveToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop 6} ''Pages)

{- Mongo API Responses -}

newtype APIChats = APIDoc {chats_documents :: [SubChat]}

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 6} ''APIChats)

newtype APIFeeds = APIFeeds {feeds_documents :: [APIFeed]}

instance FromJSON APIFeed where
    parseJSON = withObject "APIFeed" $ \o ->
        let parsed_interval =
                o .:? "f_avg_interval" >>= \case
                    Nothing -> pure Nothing
                    Just s -> pure $ readMaybe s :: Parser (Maybe NominalDiffTime)
         in APIFeed
                <$> o .: "f_type"
                <*> o .: "f_desc"
                <*> o .: "f_title"
                <*> o .: "f_link"
                <*> o .: "f_items"
                <*> parsed_interval
                <*> o .: "f_last_refresh"
                <*> o .: "f_reads"

fromAPIFeed :: APIFeed -> Feed
fromAPIFeed (APIFeed a b c d e f g h) = Feed a b c d e f g h

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 6} ''APIFeeds)

newtype APIDigest = APIDigest {digest_document :: Digest}

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 7} ''APIDigest)

newtype APIPages = APIPages {pages_documents :: [Pages]}

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 6} ''APIPages)

$(deriveJSON defaultOptions ''ReadReq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''ReadResp)

$(deriveJSON defaultOptions{omitNothingFields = True} ''WriteReq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''WriteResp)

{- Application, settings -}

type Connectors = (Connection, IORef Pipe)

type BotToken = T.Text

data ServerConfig = ServerConfig
    { bot_token :: BotToken
    , webhook_url :: T.Text
    , alert_chat :: ChatId
    }
    deriving (Show, Eq)

type FeedsMap = HMS.HashMap T.Text Feed

data AppConfig = AppConfig
    { api_key :: APIKey
    , last_worker_run :: IORef (Maybe UTCTime)
    , mongo_creds :: MongoCreds
    , connectors :: Connectors
    , tg_config :: ServerConfig
    , base_url :: T.Text
    , subs_state :: MVar SubChats
    , postjobs :: Chan Job
    , worker_interval :: Int
    }

printConfig :: AppConfig -> IO ()
printConfig AppConfig{..} =
    let zipped =
            zip
                ["api_key", "mongo_creds", "tg_config", "base_url", "worker_interval"]
                [ T.decodeUtf8 api_key
                , T.pack $ show mongo_creds
                , T.pack $ show tg_config
                , base_url
                , T.pack $ show worker_interval
                ]
     in print $ T.intercalate "\n" $ map (\(k, v) -> k `T.append` ": " `T.append` v) zipped

{- Application -}

newtype App m a = App {getApp :: ReaderT AppConfig m a}
    deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env action = runReaderT (getApp action) env
