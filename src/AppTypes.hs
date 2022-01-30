{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppTypes where

import Control.Concurrent (Chan, MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Int (Int64)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Database.MongoDB (Host, Pipe, PortID)
import Text.Read (readMaybe)
import TgramOutJson (ChatId)

{- Replies -}

data Reply =
    ChatReply {
        reply_contents :: T.Text,
        reply_markdown :: Bool,
        reply_disable_webview :: Bool,
        reply_pin_on_send :: Bool
    } | ServiceReply T.Text
    deriving Show

{- URLs -}

newtype Path = Path T.Text

{- Items, feeds -}

data Item = Item
  { i_title :: T.Text,
    i_desc :: T.Text,
    i_link :: T.Text,
    i_feed_link :: T.Text,
    i_pubdate :: UTCTime
  }
  deriving (Eq, Show, Ord)

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

{- Searches -}

type Scope = S.Set T.Text

type Keywords = S.Set T.Text

data SearchResult = SearchResult {
    sr_title :: T.Text,
    sr_link :: T.Text,
    sr_feedlink :: T.Text,
    sr_score :: Double
} deriving (Show, Eq)

{- Settings -}

data BatchInterval = BatchInterval {
    batch_every_secs :: Maybe NominalDiffTime,
    batch_at :: Maybe [(Int, Int)]
} deriving (Eq, Show)

type FeedLink = T.Text

type BlackList = S.Set T.Text

data WordMatches = WordMatches {
    match_blacklist :: BlackList,
    match_searchset :: Scope,
    match_only_search_results :: S.Set FeedLink
} deriving (Show, Eq)

data Settings = Settings {
    settings_batch_interval :: BatchInterval,
    settings_batch_size :: Int,
    settings_disable_web_view :: Bool,
    settings_paused :: Bool,
    settings_pin :: Bool,
    settings_word_matches :: WordMatches
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
    all_ints = maybe False (all (\n -> n >= 1 && n < 100)) maybeInts
    maybeInts = traverse (readMaybe . T.unpack) ss :: Maybe [Int]
    intoUrls = map ByUrl ss
    intoIds = maybe [] (map ById) (traverse (readMaybe . T.unpack) ss)

{- User actions, errors -}

data ParsingSettings =
    PBatchAt [(Int, Int)] |
    PBatchEvery NominalDiffTime |
    PBatchSize Int |
    PBlacklist (S.Set T.Text) |
    PDisableWebview Bool |
    PPaused Bool |
    PPin Bool |
    PSearchKws (S.Set T.Text) |
    PSearchLinks (S.Set T.Text)
    deriving (Show, Eq)

data UserAction
  = About FeedRef
  | AboutChannel ChatId FeedRef
  | Changelog
  | GetChannelItems ChatId FeedRef
  | GetItems FeedRef
  | GetLastXDaysItems Int
  | GetSubchannelSettings ChatId
  | GetSubchatSettings
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
  | SetChatSettings [ParsingSettings]
  | Sub [T.Text]
  | SubChannel ChatId [T.Text]
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
  deriving (Eq, Show)

renderUserError :: UserError -> T.Text
renderUserError (BadInput t) = T.append "I don't know what to do with this input: " t
renderUserError (BadFeedUrl t) = T.append "No feed could be found at this address: " t
renderUserError (NotAdmin _) = "Unable to perform this action, as it's reserved to admins in this chat."
renderUserError (MaxFeedsAlready _) = "This chat has reached the limit of subscriptions (10)"
renderUserError (ParseError input) = T.append "Parsing this input failed: " input
renderUserError (UpdateError err) = T.append "Unable to update, because of this error: " err
renderUserError (NotFoundFeed feed) = T.append "The feed you were looking for does not exist: " feed
renderUserError NotFoundChat = "The chat you called from is not subscribed to any feed yet."
renderUserError (BadRef contents) = T.append "References to web feeds must be either single digits or full-blown urls starting with 'https://', but you sent this: " contents
renderUserError NotSubscribed = "The feed your were looking for could not be found. Make sure you are subscribed to it."
renderUserError TelegramErr = "An error occurred while requesting Telegram's services. Please try again"
renderUserError (Ignore input) = "Ignoring " `T.append` input

{- Replies -}

data ToReply = FromChangelog
    | FromChatFeeds SubChat [Feed]
    | FromFeedDetails Feed
    | FromFeedItems Feed
    | FromFeedLinkItems [(FeedLink, [Item])]
    | FromFeedsItems [(Feed, [Item])]
    | FromSearchRes Keywords [SearchResult]
    | FromStart
    deriving (Eq, Show)

{- Database actions, errors -}

data DbCreds
  = MongoCredsTls
      { host_name :: String,
        database_name :: T.Text,
        user_name :: T.Text,
        password :: T.Text,
        db_port :: PortID
      }
  | MongoCredsReplicaTls
      { replicateset :: T.Text,
        hosts :: [Host],
        user_name :: T.Text,
        password :: T.Text
      }
  | MongoCredsReplicaSrv
      { host_name :: String,
        database_name :: T.Text,
        user_name :: T.Text,
        password :: T.Text
      }
  deriving (Eq, Show)

data DbConnector = MongoPipe Pipe | SomethingElse

data DbAction
  = ArchiveItems [Feed]
  | DeleteChat ChatId
  | Get100Feeds
  | GetAllChats
  | GetFeed FeedLink
  | IncReads [FeedLink]
  | DbSearch Keywords Scope
  | PruneOldItems UTCTime
  | UpsertChat SubChat
  | UpsertChats SubChats
  | UpsertFeeds [Feed]
  | View [FeedLink] UTCTime (Maybe UTCTime)
  deriving (Show, Eq)

data DbRes = DbFeeds [Feed]
  | DbChats [SubChat]
  | DbNoChat
  | DbNoFeed
  | DbErr DbError
  | DbOk
  | DbSearchRes Keywords [SearchResult]
  | DbView [Item]

data DbError
  = PipeNotAcquired
  | DbLoginFailed
  | NoFeedFound T.Text
  | FailedToUpdate T.Text T.Text
  | FailedToLog
  | FailedToLoadFeeds
  | BadQuery T.Text
  deriving (Show, Eq)

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError DbLoginFailed = "Pipe acquired, but login failed."
renderDbError (FailedToUpdate items reason) = "Unable to update the following items :" `T.append` items `T.append` ". Reason: " `T.append` reason
renderDbError (NoFeedFound url) = "This feed could not be retrieved from the database: " `T.append` url
renderDbError FailedToLog = "Failed to log."
renderDbError FailedToLoadFeeds = "Failed to load feeds!"
renderDbError (BadQuery txt) = T.append "Bad query parameters: " txt

{- Feeds -}

data FeedsAction
  = InitF [FeedLink]
  | AddF [Feed]
  | RemoveF [FeedLink]
  | GetAllXDays [FeedLink] Int
  | Refresh
  | IncReadsF [FeedLink]
  | LoadF
  deriving (Eq, Show)

type FeedItems = [(Feed, [Item])]

data FeedsRes = FeedsOk
    | FeedsError DbError
    | FeedBatches (HMS.HashMap ChatId (Settings, FeedItems)) (HMS.HashMap ChatId DbRes)
    | FeedLinkBatch [(FeedLink, [Item])]

{- Logs -}

data LogItem = LogPerf 
  { log_message :: T.Text,
    log_at :: UTCTime,
    log_refresh :: Int64,
    log_sending_notif :: Int64,
    log_updating :: Int64,
    log_total :: Int64
  }
  deriving (Eq, Show)

{- Application, settings -}

type BotToken = T.Text

data ServerConfig = ServerConfig
  { bot_token :: BotToken,
    webhook_url :: T.Text,
    alert_chat :: ChatId
  }
  deriving (Show, Eq)

type KnownFeeds = HMS.HashMap T.Text Feed

data Job =
    JobIncReadsJob [FeedLink] |
    JobRemoveMsg ChatId Int Int |
    JobLog LogItem |
    JobPin ChatId Int |
    JobTgAlert T.Text |
    JobArchive [Feed] UTCTime
    deriving (Eq, Show)

data AppConfig = AppConfig
  { last_worker_run :: Maybe UTCTime,
    db_config :: DbCreds,
    db_connector :: IORef DbConnector,
    tg_config :: ServerConfig,
    feeds_state :: MVar KnownFeeds,
    subs_state :: MVar SubChats,
    postjobs :: Chan Job,
    worker_interval :: Int
  }

newtype App m a = App {getApp :: ReaderT AppConfig m a}
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env = flip runReaderT env . getApp