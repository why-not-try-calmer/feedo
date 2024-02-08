{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types where

import Control.Concurrent (Chan, MVar)
import Control.Monad.List (foldM)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Data.Aeson
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HMS
import Data.IORef (IORef)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM, rfc822DateFormat)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601
import Database.MongoDB (Host, Pipe, PortID)
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
  | EditReply
      { edit_message_id :: Int
      , edit_text :: T.Text
      , edit_markdown :: Bool
      , edit_pagination_keyboard :: Maybe InlineKeyboardMarkup
      }
  | ServiceReply T.Text
  | ForwardServiceReply ChatId T.Text
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
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Feed)

data Digest = Digest
  { digest_id :: Maybe T.Text
  , digest_created :: UTCTime
  , digest_items :: [Item]
  , digest_links :: [T.Text]
  , digest_titles :: [T.Text]
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Digest)

{- Searches -}

type Keywords = S.Set T.Text

type FeedLink = T.Text

type Scope = S.Set FeedLink

data SearchResult = SearchResult
  { sr_title :: T.Text
  , sr_link :: T.Text
  , sr_pubdate :: UTCTime
  , sr_feedlink :: T.Text
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
  , settings_forward_to_admins :: Bool
  , settings_pagination :: Bool
  , settings_paused :: Bool
  , settings_pin :: Bool
  , settings_share_link :: Bool
  , settings_word_matches :: WordMatches
  , settings_digest_no_collapse :: Scope
  }
  deriving (Show, Eq)

$(deriveToJSON defaultOptions{omitNothingFields = True} ''Settings)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o ->
    let blacklisted = o .:? "settings_blacklist" >>= memptyOrList
        search_search_keywords = o .:? "settings_searchset" >>= memptyOrList
        search_only_results_flinks = o .:? "settings_only_search_results" >>= memptyOrList
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
          <$> o
            .:? "settings_digest_collapse"
          <*> (DigestInterval <$> digest_every <*> _digest_at)
          <*> o
            .:? "settings_digest_size"
            .!= 10
          <*> o
            .:? "settings_digest_start"
          <*> o
            .:? "settings_digest_title"
            .!= mempty
          <*> o
            .:? "settings_disable_web_view"
            .!= False
          <*> o
            .:? "settings_follow"
            .!= False
          <*> o
            .:? "settings_forward_to_admins"
            .!= False
          <*> o
            .:? "settings_pagination"
            .!= True
          <*> o
            .:? "settings_paused"
            .!= False
          <*> o
            .:? "settings_pin"
            .!= False
          <*> o
            .:? "settings_share_link"
            .!= True
          <*> (WordMatches <$> blacklisted <*> search_search_keywords <*> search_only_results_flinks)
          <*> o
            .:? "settings_digest_no_collapse"
            .!= mempty
   where
    memptyOrList o = case o of
      Nothing -> pure mempty
      Just l -> pure $ S.fromList l
    mbNom :: String -> Maybe NominalDiffTime
    mbNom s =
      maybe second_pass pure $
        iso8601ParseM s
          >>= \t ->
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
  , sub_active_admins :: HMS.HashMap UserId UTCTime
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
  | PForwardToAdmins Bool
  | PPaused Bool
  | PPin Bool
  | PDigestCollapse Int
  | PSearchKws (S.Set T.Text)
  | PSearchLinks (S.Set T.Text)
  | PShareLink Bool
  | PFollow Bool
  | PNoCollapse (S.Set T.Text)
  deriving (Show, Eq)

data SettingsUpdater = Parsed [ParsingSettings] | Immediate Settings deriving (Eq, Show)

data UserAction
  = About
  | Announce T.Text
  | AskForLogin ChatId
  | AboutChannel ChatId FeedRef
  | Changelog
  | FeedInfo FeedRef
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

data FeedError = FeedError
  { r_url :: T.Text
  , r_status_code :: Maybe Int
  , r_error_message :: T.Text
  , r_user_message :: T.Text
  }
  deriving (Show, Eq)

data InterpreterErr
  = InterpreterErr T.Text
  | UnknownCommand T.Text [T.Text]
  deriving (Show, Eq)

data TgEvalError
  = BadFeedUrl T.Text
  | BadFeed FeedError
  | BadRef T.Text
  | DbQueryError DbError
  | NotAdmin T.Text
  | NotFoundChat
  | NotFoundFeed T.Text
  | NotSubscribed
  | MaxFeedsAlready T.Text
  | ParseError T.Text
  | UpdateError T.Text
  | TelegramErr T.Text
  | ChatNotPrivate
  | UserNotAdmin
  deriving (Eq, Show)

data ChatRes
  = ChatUpdated SubChat
  | ChatOk
  deriving (Show, Eq)

{- Replies -}

data Replies
  = FromAbout T.Text
  | FromAdmin T.Text T.Text
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
      { host_name :: T.Text
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
      { host_name :: T.Text
      , database_name :: T.Text
      , user_name :: T.Text
      , password :: T.Text
      }
  | MongoCredsServer
      { host_name :: T.Text
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
  | GetSomeFeeds [FeedLink]
  | GetAllChats
  | GetPages ChatId Int
  | GetXDays [FeedLink] Int
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

data DbResults
  = DbFeeds [Feed]
  | DbChats [SubChat]
  | DbNoChat
  | DbBadOID
  | DbNoDigest
  | DbLinkDigest [(FeedLink, [Item])]
  | DbXDays [FeedLink] Int
  | DbNoFeed
  | DbLoggedIn ChatId
  | DbToken T.Text
  | DbDone
  | DbSearchRes Keywords Scope [SearchResult]
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

type DbRes = Either DbError DbResults

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
  = CacheGetPage ChatId Int Int
  | CacheSetPages ChatId Int [T.Text] (Maybe T.Text)

data FromCache
  = CacheOk
  | CacheNothing
  | CacheDigests (HMS.HashMap ChatId (SubChat, Batch))
  | CachePage T.Text Int (Maybe T.Text)
  deriving (Show, Eq)

type FeedItems = [(Feed, [Item])]

data Notifier
  = Pre {feeds_to_refresh :: [FeedLink], batch_recipes :: HMS.HashMap ChatId (SubChat, BatchRecipe), n_last_run :: Maybe UTCTime}
  | Post {discarded_items_links :: [T.Text], batches :: HMS.HashMap ChatId (SubChat, Batch)}
  deriving (Show, Eq)

{- Logs -}

data LogItem
  = LogMissing
      { log_feeds_with_missing :: [T.Text]
      , log_total_missed :: Int
      , log_at :: UTCTime
      }
  | LogDiscardedToRefreshRecipes
      { discarded :: [T.Text]
      , to_refresh :: [T.Text]
      , recipes :: [T.Text]
      }
  | LogNotifiers
      { pre_notifier_subchats :: HMS.HashMap ChatId SubChat
      , post_notifier_subchats :: HMS.HashMap ChatId SubChat
      }
  deriving (Eq, Show)

$(deriveFromJSON defaultOptions{omitNothingFields = True} ''LogItem)

{- Background tasks -}

data Job
  = JobArchive [Feed] UTCTime
  | JobLog LogItem
  | JobPin ChatId Int
  | JobPurge ChatId
  | JobRemoveMsg ChatId Int Int
  | JobSetPagination ChatId Int [T.Text] (Maybe T.Text)
  | JobTgAlertAdmin T.Text
  | JobTgAlertChats [ChatId] T.Text
  deriving (Eq, Show)

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

instance FromJSON Pages where
  parseJSON = withObject "Pages" $ \o ->
    Pages
      <$> o
        .: "chat_id"
      <*> o
        .: "message_id"
      <*> o
        .: "pages"
      <*> o
        .:? "url"

$(deriveToJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop 6} ''Pages)

{- Application API -}

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

data BlackListedUrl = BlackListedUrl
  { last_attempt :: UTCTime
  , error_message :: T.Text
  , status_code :: Maybe Int
  , offenses :: Int
  }

type BlacklistMap = MVar (HMS.HashMap FeedLink BlackListedUrl)

data AppConfig = AppConfig
  { app_version :: T.Text
  , base_url :: T.Text
  , blacklist :: BlacklistMap
  , connectors :: Connectors
  , last_worker_run :: IORef (Maybe UTCTime)
  , mongo_creds :: MongoCreds
  , postjobs :: Chan Job
  , subs_state :: MVar SubChats
  , tg_config :: ServerConfig
  , worker_interval :: Int
  }

{- Application -}

newtype App m a = App {getApp :: ReaderT AppConfig m a}
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

runApp :: AppConfig -> App m a -> m a
runApp env action = runReaderT (getApp action) env
