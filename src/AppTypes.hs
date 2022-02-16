{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import Control.Concurrent (Chan, MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Aeson.TH (Options (fieldLabelModifier, omitNothingFields), defaultOptions, deriveJSON)
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Int (Int64)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Database.MongoDB (Host, ObjectId, Pipe, PortID)
import Text.Read (readMaybe)
import TgramOutJson (ChatId, UserId)

{- Replies -}

data Reply =
    ChatReply {
        reply_contents :: T.Text,
        reply_markdown :: Bool,
        reply_disable_webview :: Bool,
        reply_pin_on_send :: Bool,
        reply_share_link :: Bool
    } | ServiceReply T.Text
    deriving Show

{- URLs -}

newtype Path = Path T.Text

{- Items, feeds, digests -}

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

data Digest = Digest {
    digest_id :: Maybe ObjectId,
    digest_created :: UTCTime,
    digest_items :: [Item],
    digest_links :: [T.Text],
    digest_titles :: [T.Text]
} deriving (Show, Eq)

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

data DigestInterval = DigestInterval {
    digest_every_secs :: Maybe NominalDiffTime,
    digest_at :: Maybe [(Int, Int)]
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''DigestInterval)

type FeedLink = T.Text

type BlackList = S.Set T.Text

data WordMatches = WordMatches {
    match_blacklist :: BlackList,
    match_searchset :: Scope,
    match_only_search_results :: S.Set FeedLink
} deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''WordMatches)

data Settings = Settings {
    settings_digest_collapse :: Maybe Int,
    settings_digest_interval :: DigestInterval,
    settings_digest_size :: Int,
    settings_digest_start :: Maybe UTCTime,
    settings_digest_title :: T.Text,
    settings_disable_web_view :: Bool,
    settings_paused :: Bool,
    settings_pin :: Bool,
    settings_word_matches :: WordMatches,
    settings_share_link :: Bool,
    settings_follow :: Bool
} deriving (Show, Eq)

$(deriveJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 9 } ''Settings)

data SubChat = SubChat
  { sub_chatid :: ChatId,
    sub_last_digest :: Maybe UTCTime,
    sub_next_digest :: Maybe UTCTime,
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

{- Admins -}

data AdminUser = AdminUser {
    admin_uid :: UserId,
    admin_token :: T.Text,
    admin_chatid :: ChatId,
    admin_created :: UTCTime
} deriving (Eq, Show)

{- User actions, errors -}

data ParsingSettings =
    PDigestAt [(Int, Int)] |
    PDigestEvery NominalDiffTime |
    PDigestSize Int |
    PDigestStart UTCTime |
    PDigestTitle T.Text |
    PBlacklist (S.Set T.Text) |
    PDisableWebview Bool |
    PPaused Bool |
    PPin Bool |
    PDigestCollapse Int |
    PSearchKws (S.Set T.Text) |
    PSearchLinks (S.Set T.Text) |
    PShareLink Bool |
    PFollow Bool
    deriving (Show, Eq)

data UserAction
  = About FeedRef
  | AskForLogin ChatId
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
  | ChatNotPrivate
  | UserNotAdmin
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
renderUserError TelegramErr = "Telegram responded with an error. Are you sure you're using the right chat_id?"
renderUserError (Ignore input) = "Ignoring " `T.append` input
renderUserError ChatNotPrivate = "Unable to send personal credentials to non-private chat. Please message the post."
renderUserError UserNotAdmin = "Only admins can change settings."

data ChatRes = 
    ChatUpdated SubChat |
    ChatOk

{- Replies -}

data Replies = FromAdmin T.Text T.Text
    | FromChangelog
    | FromChatFeeds SubChat [Feed]
    | FromChat SubChat T.Text
    | FromFeedDetails Feed
    | FromFeedItems Feed
    | FromFeedLinkItems [(FeedLink, [Item])]
    | FromDigest [Feed] (Maybe T.Text) Settings
    | FromFollow [Feed] Settings
    | FromSearchRes Keywords [SearchResult]
    | FromStart
    deriving (Eq, Show)

data BatchRecipe = 
    FollowFeedLinks [FeedLink] |
    DigestFeedLinks [FeedLink]
    deriving (Show, Eq)

data Batch =
    Follows [Feed] |
    Digests [Feed]
    deriving (Show, Eq)

readBatchRecipe :: BatchRecipe -> [FeedLink]
readBatchRecipe (FollowFeedLinks ls) = ls
readBatchRecipe (DigestFeedLinks ls) = ls

mkBatch :: BatchRecipe -> [Feed] -> Batch
mkBatch (FollowFeedLinks _) ls = Follows ls 
mkBatch (DigestFeedLinks _) ls = Digests ls 

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

type AdminToken = T.Text

data DbAction
  = DbAskForLogin UserId AdminToken ChatId
  | CheckLogin AdminToken
  | ArchiveItems [Feed]
  | DeleteChat ChatId
  | Get100Feeds
  | GetAllChats
  | GetFeed FeedLink
  | IncReads [FeedLink]
  | DbSearch Keywords Scope (Maybe UTCTime)
  | PruneOld UTCTime
  | ReadDigest T.Text
  | UpsertChat SubChat
  | UpsertChats SubChats
  | UpsertFeeds [Feed]
  | View [FeedLink] UTCTime UTCTime
  | WriteDigest Digest
  deriving (Show, Eq)

data DbRes = DbFeeds [Feed]
  | DbChats [SubChat]
  | DbNoChat
  | DbNoFeed
  | DbNoDigest
  | DbErr DbError
  | DbOk
  | DbLoggedIn ChatId
  | DbSearchRes Keywords [SearchResult]
  | DbView [Item] UTCTime UTCTime 
  | DbDigest Digest
  | DbDigestId T.Text

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
  deriving (Show, Eq)

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError FaultyToken = "Login failed. This token is not valid, and perhaps never was."
renderDbError (FailedToUpdate items reason) = "Unable to update the following items :" `T.append` items `T.append` ". Reason: " `T.append` reason
renderDbError (NoFeedFound url) = "This feed could not be retrieved from the database: " `T.append` url
renderDbError FailedToLog = "Failed to log."
renderDbError FailedToLoadFeeds = "Failed to load feeds!"
renderDbError (BadQuery txt) = T.append "Bad query parameters: " txt
renderDbError FailedToSaveDigest = "Unable to save this digest. The database didn't return a valid identifier."
renderDbError FailedToProduceValidId = "Db was unable to return a valid identifier"

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
    | FeedDigests 
        (HMS.HashMap ChatId (SubChat, Batch))
        (HMS.HashMap ChatId DbRes)
    | FeedLinkDigest [(FeedLink, [Item])]
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
    base_url :: T.Text,
    feeds_state :: MVar KnownFeeds,
    subs_state :: MVar SubChats,
    postjobs :: Chan Job,
    worker_interval :: Int
  }

{- Web responses -}

newtype ReadReq = ReadReq { read_req_hash :: T.Text }

$(deriveJSON defaultOptions ''ReadReq)

data ReadResp = ReadResp {
    read_resp_settings :: Maybe Settings,
    read_resp_cid :: Maybe ChatId,
    read_resp_error :: Maybe T.Text
}

$(deriveJSON defaultOptions { omitNothingFields = True } ''ReadResp)

data WriteReq = WriteReq {
    write_req_hash :: T.Text,
    write_req_settings :: Settings,
    write_req_confirm :: Maybe Bool
}

$(deriveJSON defaultOptions { omitNothingFields = True } ''WriteReq)

data WriteResp = WriteResp {
    write_resp_status :: Int,
    write_resp_checkout :: Maybe T.Text,
    write_resp_error :: Maybe T.Text
}

$(deriveJSON defaultOptions { omitNothingFields = True } ''WriteResp)

{- Application -}

newtype App m a = App {getApp :: ReaderT AppConfig m a}
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env = flip runReaderT env . getApp