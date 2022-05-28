{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Concurrent (Chan, MVar)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Data.Aeson.TH (Options (fieldLabelModifier, omitNothingFields), defaultOptions, deriveJSON)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Int (Int64)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Database.MongoDB (Host, ObjectId, Pipe, PortID)
import Database.Redis (Connection)
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

$(deriveJSON defaultOptions ''Item)

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

$(deriveJSON defaultOptions ''Feed)

data Digest = Digest
    { digest_id :: Maybe ObjectId
    , digest_created :: UTCTime
    , digest_items :: [Item]
    , digest_links :: [T.Text]
    , digest_titles :: [T.Text]
    }
    deriving (Show, Eq)

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

{- Settings -}

data DigestInterval = DigestInterval
    { digest_every_secs :: Maybe NominalDiffTime
    , digest_at :: Maybe [(Int, Int)]
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions ''DigestInterval)

data WordMatches = WordMatches
    { match_blacklist :: Keywords
    , match_searchset :: Keywords
    , match_only_search_results :: Scope
    }
    deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''WordMatches)

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

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop 9} ''Settings)

data SubChat = SubChat
    { sub_chatid :: ChatId
    , sub_last_digest :: Maybe UTCTime
    , sub_next_digest :: Maybe UTCTime
    , sub_feeds_links :: S.Set FeedLink
    , sub_linked_to :: Maybe ChatId
    , sub_settings :: Settings
    }
    deriving (Show, Eq)

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
    | DbNoFeed
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
    | NoFeedFound T.Text
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
    | CacheWarmup
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
        { log_feeds_with_missing :: [(T.Text, [T.Text])]
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
    deriving (Eq, Show)

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

{- Web responses -}

newtype ReadReq = ReadReq {read_req_hash :: T.Text}

$(deriveJSON defaultOptions ''ReadReq)

data ReadResp = ReadResp
    { read_resp_settings :: Maybe Settings
    , read_resp_cid :: Maybe ChatId
    , read_resp_error :: Maybe T.Text
    }

$(deriveJSON defaultOptions{omitNothingFields = True} ''ReadResp)

data WriteReq = WriteReq
    { write_req_hash :: T.Text
    , write_req_settings :: Settings
    , write_req_confirm :: Maybe Bool
    }

$(deriveJSON defaultOptions{omitNothingFields = True} ''WriteReq)

data WriteResp = WriteResp
    { write_resp_status :: Int
    , write_resp_checkout :: Maybe T.Text
    , write_resp_error :: Maybe T.Text
    }

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
    { last_worker_run :: IORef (Maybe UTCTime)
    , mongo_creds :: MongoCreds
    , connectors :: Connectors
    , tg_config :: ServerConfig
    , base_url :: T.Text
    , subs_state :: MVar SubChats
    , postjobs :: Chan Job
    , worker_interval :: Int
    }

{- Application -}

newtype App m a = App {getApp :: ReaderT AppConfig m a}
    deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env action = runReaderT (getApp action) env