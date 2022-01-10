{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppTypes where

import Control.Concurrent (Chan, MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import Data.SearchEngine (SearchEngine, NoFeatures)
import Data.Ix (Ix)
import Database.MongoDB (Pipe)

{- Replies -}

data Reply = 
    ChatReply {
        reply_contents :: T.Text,
        reply_markdown :: Bool,
        reply_disable_webview :: Bool,
        reply_pin_on_send :: Bool,
        reply_clean_behind :: Bool
    } | ServiceReply T.Text
    deriving Show

{- URLs -}

type Host = T.Text

newtype Rest = Rest [T.Text]

newtype Path = Path T.Text

{- Items, feeds -}

data Item = Item
  { i_title :: T.Text,
    i_desc :: T.Text,
    i_link :: T.Text,
    i_feed_link :: T.Text,
    i_pubdate :: UTCTime
  }
  deriving (Eq, Show)

data Feed = Feed
  { f_type :: FeedType,
    f_desc :: T.Text,
    f_title :: T.Text,
    f_link :: T.Text,
    f_items :: [Item],
    f_avg_interval :: Maybe NominalDiffTime,
    f_last_refresh :: Maybe UTCTime,
    f_reads :: Int
  }
  deriving (Eq, Show)

data FeedType = Rss | Atom deriving (Eq, Show)

{- Chat -}

type BlackList = [T.Text]

type WhiteList = [T.Text]

type FeedLink = T.Text

data Filters = Filters { 
    filters_blacklist :: [T.Text],
    filters_whitelist :: [T.Text] 
} deriving (Show, Eq)

data BatchInterval = BatchInterval {
    batch_every_secs :: Maybe NominalDiffTime,
    batch_at :: Maybe [(Int, Int)]
} deriving (Eq, Show)

data Settings = Settings {
    settings_paused :: Bool,
    settings_batch_size :: Int,
    settings_batch_interval :: BatchInterval,
    settings_filters :: Filters,
    settings_disable_web_view :: Bool,
    settings_pin :: Bool,
    settings_clean :: Bool
} deriving (Show, Eq)

data SubChat = SubChat
  { sub_chatid :: ChatId,
    sub_last_notification :: Maybe UTCTime,
    sub_next_notification :: Maybe UTCTime,
    sub_feeds_links :: S.Set FeedLink,
    sub_settings :: Settings
} deriving (Show, Eq)

type SubChats = (HMS.HashMap ChatId SubChat)

{- Feed references -}

data FeedRef = ByUrl T.Text | ById Int deriving (Eq, Show)

unFeedRef :: FeedRef -> T.Text
unFeedRef (ByUrl s) = s
unFeedRef (ById s) = T.pack $ show s

unFeedRefs :: [FeedRef] -> [T.Text]
unFeedRefs = map unFeedRef

toFeedRef :: [T.Text] -> Either UserError [FeedRef]
toFeedRef ss
  | all_valid_urls = Right intoUrls
  | all_ints = Right intoIds
  | otherwise = Left . BadRef . T.concat $ ss
  where
    all_valid_urls = all (== "https://") (first8 ss)
    first8 = map (T.take 8)
    all_ints = isJust maybeInts
    maybeInts = traverse (readMaybe . T.unpack) ss :: Maybe [Int]
    intoUrls = map ByUrl ss
    intoIds = maybe [] (map ById) (traverse (readMaybe . T.unpack) ss)

-- -- Business logic -- --

{- User actions, errors -}

type KeysParsedSettings = ([T.Text], Settings)

data UserAction
  = About FeedRef
  | Changelog
  | GetItems FeedRef
  | GetLastXDaysItems Int
  | GetSubFeedSettings
  | ListSubs
  | ListSubsChannel ChatId
  | Pause Bool
  | PauseChannel ChatId Bool
  | Purge
  | PurgeChannel ChatId
  | RenderCmds
  | Reset
  | ResetChannel ChatId
  | Search [T.Text]
  | SetChannelSettings ChatId KeysParsedSettings
  | SetChatSettings KeysParsedSettings
  | Sub [T.Text]
  | SubChannel ChatId [T.Text]
  | UnSub [FeedRef]
  | UnSubChannel ChatId [FeedRef]
  deriving (Eq, Show)

data UserError
  = BadFeedUrl T.Text
  | BadFilter
  | BadInput T.Text
  | BadRef T.Text
  | FeedFailToBuild T.Text
  | NoSettings
  | NotAdmin T.Text
  | NotFoundChat
  | NotFoundFeed T.Text
  | NotImplemented T.Text
  | NotSubscribed
  | MaxFeedsAlready T.Text
  | ParseError T.Text
  | UpdateError T.Text
  | TelegramErr
  deriving (Eq, Show)

renderUserError :: UserError -> T.Text
renderUserError (BadInput t) = T.append "I don't know what to do with this input: " t
renderUserError (BadFeedUrl t) = T.append "No feed could be found at this address: " t
renderUserError (FeedFailToBuild t) = T.append "We found a feed at this address, but we couldn't build any feed from it: " t
renderUserError (NotImplemented _) = "This feature is not implemented yet. Bear with us."
renderUserError (NotAdmin _) = "Unable to perform this action, as it's reserved to admins in this chat."
renderUserError (MaxFeedsAlready _) = "This chat has reached the limit of subscriptions (10)"
renderUserError (ParseError input) = T.append "Parsing this input failed: " input
renderUserError (UpdateError err) = T.append "Unable to update, because of this error: " err
renderUserError (NotFoundFeed feed) = T.append "The feed you were looking for does not exist: " feed
renderUserError NotFoundChat = "The chat you called from is not subscribed to any feed yet."
renderUserError (BadRef contents) = T.append "References to web feeds must be either single digits or full-blown urls starting with 'https://', but you sent this: " contents
renderUserError BadFilter = "Filters should be at least 4-character long."
renderUserError NotSubscribed = "The feed your were looking for could not be found. Make sure you are subscribed to it."
renderUserError NoSettings = "No settings found for the supscription to this feed."
renderUserError TelegramErr = "An error occurred while requesting Telegram's services. Please try again"

{- Replies -}

data ToReply = FromChangelog
    | FromChatFeeds SubChat [Feed]
    | FromFeedDetails Feed
    | FromFeedItems Feed
    | FromFeedLinkItems [(FeedLink, [Item])]
    | FromFeedsItems [(Feed, [Item])]
    | FromSearchRes [Item]
    | FromStart
    deriving (Eq, Show)

{- Database actions, errors -}

data DbAction
  = UpsertFeeds [Feed]
  | Get100Feeds
  | GetFeed FeedLink
  | RemoveFeeds [FeedLink]
  | GetAllChats
  | UpsertChat SubChat
  | UpsertChats SubChats
  | GetChat ChatId
  | DeleteChat ChatId
  | IncReads [FeedLink]
  deriving (Show, Eq)

data DbRes = DbFeeds [Feed]
  | DbChats [SubChat]
  | DbNoChat
  | DbNoFeed
  | DbErr DbError
  | DbOk

data DbError
  = PipeNotAcquired
  | DbLoginFailed
  | DbChangedMaster
  | NoFeedFound T.Text
  | FailedToUpdate T.Text
  | FailedToDeleteAll
  | FailedToStoreAll
  | FailedToLog
  | FailedToLoadFeeds
  deriving (Show, Eq)

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError DbChangedMaster = "You need to make sure you are authenticating with the latest master instance."
renderDbError DbLoginFailed = "Pipe acquired, but login failed."
renderDbError FailedToDeleteAll = "Unable to delete these items."
renderDbError (FailedToUpdate txt) = "Unable to update these items for this reason: " `T.append` txt
renderDbError (NoFeedFound url) = "This feed could not be retrieved from the database: " `T.append` url
renderDbError FailedToStoreAll = "Unable to store all these items."
renderDbError FailedToLog = "Failed to log."
renderDbError FailedToLoadFeeds = "Failed to load feeds!"

{- Feeds -}

data FeedsAction
  = InitF [FeedLink]
  | AddF [Feed]
  | RemoveF [FeedLink]
  | GetAllXDays [FeedLink] Int
  | RefreshNotifyF
  | IncReadsF [FeedLink]
  | LoadF
  deriving (Eq, Show)

type FeedItems = [(Feed, [Item])]

data FeedsRes = FeedsOk
    | FeedsError DbError
    | FeedBatches (HMS.HashMap ChatId (Settings, FeedItems))
    | FeedLinkBatch [(FeedLink, [Item])]

{- Logs -}

data LogItem = LogItem
  { log_when :: UTCTime,
    log_who :: T.Text,
    log_what :: T.Text,
    log_n :: Double
  }
  deriving (Eq, Show)

{- Search engine -}

type FeedsSearch = SearchEngine KeyedItem Int Field NoFeatures

data KeyedItem = KeyedItem {
    key ::Int,
    item :: Item
} deriving (Eq, Show)

data Field = FieldTitle | FieldDescription deriving (Eq, Ord, Enum, Bounded, Ix, Show)

{- Application, settings -}

type BotToken = T.Text

data ServerConfig = ServerConfig
  { bot_token :: BotToken,
    webhook_url :: T.Text,
    alert_chat :: ChatId
  }
  deriving (Show, Eq)

data DbCreds = MongoCreds
  { shard :: T.Text,
    all_shards :: T.Text,
    user :: T.Text,
    pwd :: T.Text
  }

type KnownFeeds = HMS.HashMap T.Text Feed

data Job = 
    JobIncReadsJob [FeedLink] |
    JobRemoveMsg ChatId Int (Maybe Int) |
    JobLog LogItem |
    JobPin ChatId Int |
    JobTgAlert T.Text |
    JobUpdateSchedules [ChatId]
    deriving (Eq, Show)

data AppConfig = AppConfig
  { last_worker_run :: Maybe UTCTime,
    db_config :: DbCreds,
    db_connector :: IORef Pipe,
    tg_config :: ServerConfig,
    feeds_state :: MVar KnownFeeds,
    subs_state :: MVar SubChats,
    search_engine :: MVar ([KeyedItem], FeedsSearch),
    postjobs :: Chan Job,
    worker_interval :: Int
  }

newtype App m a = App {getApp :: ReaderT AppConfig m a}
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env = flip runReaderT env . getApp