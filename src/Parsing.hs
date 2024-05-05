{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Parsing (eitherUrlScheme, rebuildFeed, getFeedFromUrlScheme, parseSettings) where

import Control.Monad.IO.Class
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Req
import Replies (mkdDoubles, render)
import Feeds (buildFeed)
import Text.Read (readMaybe)
import Types (
  Feed (..),
  FeedError (..),
  FeedType (..),
  ParsingSettings (..),
  Settings (settings_digest_title),
  TgEvalError (BadFeed, BadFeedUrl),
 )
import Utils (defaultChatSettings, mbTime, sortTimePairs)

{- Feeds, Items -}

eitherUrlScheme :: T.Text -> Either TgEvalError (Url 'Https)
{-# INLINEABLE eitherUrlScheme #-}
-- tries to make a valid Url Scheme from the given string
eitherUrlScheme s
  | T.null s = Left . BadFeedUrl $ s
  | head split /= "https:" = Left . BadFeedUrl $ s
  | length body >= 2 = Right toScheme
  | otherwise = Left . BadFeedUrl $ "Unable to parse this input." `T.append` s
 where
  s' = if T.last s == T.last "/" then T.dropEnd 1 s else s
  split = T.splitOn "/" s'
  body = drop 2 split
  (host : rest) = body
  toScheme = foldl' (/:) (https host) rest

getFeedFromUrlScheme :: (MonadIO m) => Url scheme -> m (Either T.Text (Feed, Maybe T.Text))
getFeedFromUrlScheme scheme =
  buildFeed Rss scheme >>= \case
    Left err@(BadFeed _) ->
      pure . Left $ "Exception when trying to fetch feed: " `T.append` render err
    Left _ ->
      buildFeed Atom scheme >>= \case
        Left err -> pure . Left . render $ err
        Right (feed, warning) -> finish_successfully (feed, warning)
    Right (feed, warning) -> finish_successfully (feed, warning)
 where
  finish_successfully = pure . Right

rebuildFeed :: (MonadIO m) => T.Text -> m (Either FeedError Feed)
-- updates a single feed
rebuildFeed key = case eitherUrlScheme key of
  Left err -> liftIO getCurrentTime >>= \now -> pure . Left $ FeedError key Nothing mempty (render err) now
  Right url -> liftIO $ build url
 where
  build url = do
    now <- getCurrentTime
    buildFeed Rss url >>= \case
      Left err@(BadFeed _) ->
        pure
          . Left
          $ FeedError
            { r_url = renderUrl url
            , r_error_message = "Exception when trying to fetch feed: " `T.append` render err
            , r_status_code = Nothing
            , r_user_message = mempty
            , r_last_attempt = now
            }
      Left _ ->
        buildFeed Atom url >>= \case
          Left build_error -> pure . Left $ FeedError (renderUrl url) Nothing mempty (render build_error) now
          Right (feed, _) -> done feed
      Right (feed, _) -> done feed
  done feed = liftIO getCurrentTime >>= \now -> pure . Right $ feed{f_last_refresh = Just now}

{- Settings -}

parseSettings :: [T.Text] -> Either T.Text [ParsingSettings]
parseSettings [] = Left "Unable to parse from an empty list of strings."
parseSettings lns = case foldr mkPairs Nothing lns of
  Nothing ->
    Left
      "Unable to parse the settings you've sent, please respect the format: \
      \ /set <optional: chat_id>\nkey: val"
  Just pairs ->
    let (not_parsed, parsed) = foldl' intoParsing ([], []) pairs
     in if null not_parsed
          then Right parsed
          else
            Left $
              T.intercalate ", " not_parsed
                `T.append` ". Make sure to use only valid key-value pairs: "
                `T.append` T.intercalate
                  "\n"
                  [ "blacklist <keyword keyword ...>"
                  , "digest_at <HH:MM HH:MM ...>"
                  , "digest_collapse <n>"
                  , "digest_every <n> <m|h|d>"
                  , "digest_size <n>"
                  , "digest_start <YYYY-mm-dd>"
                  , "digest_title <title>"
                  , "disable_webview <false|true>"
                  , "follow <false|true>"
                  , "forward_to_admins <true|false>"
                  , "only_search_notif <url1 url2 ...>"
                  , "pagination <false|true>"
                  , "pin <false|true>"
                  , "search_notif <keyword keyword ...>"
                  , "share_link <false|true>"
                  ]
 where
  intoParsing (!not_parsed, !parsed) (!k, !txt)
    | k == "digest_at" =
        let failure l = (l : not_parsed, parsed)
            success r = (not_parsed, PDigestAt r : parsed)
            collected = sortTimePairs . foldr into_hm [] . T.words $ txt
         in if "reset" `T.isInfixOf` T.toCaseFold txt
              then success []
              else
                if null collected
                  then
                    failure
                      "Unable to parse 'digest_at' values. \
                      \ Make sure every time is written as a 5-character string, i.e. '00:00' for midnight and '12:00' for noon. \
                      \ Use ':' to separate hours from minutes and whitespaces to separate many time values: '00:00 12:00' for midgnight and noon."
                  else success collected
    | k == "digest_every" =
        let failure l = (l : not_parsed, parsed)
            success r = (not_parsed, PDigestEvery r : parsed)
            int = T.init txt
            t_tag = T.singleton . T.last $ txt
            triage n t
              | 1 > n = Left "n must be bigger than 0."
              | "m" == t = Right $ n * 60
              | "h" == t = Right $ n * 3600
              | "d" == t = Right $ n * 86400
              | otherwise = Left "'digest_every' needs a number of minutes, hours or days. Example: digest_at: 1d, digest_at: 6h, digest_at: 40m."
         in if "reset" `T.isInfixOf` T.toCaseFold txt
              then success 0
              else
                if T.length txt < 2
                  then failure "The first character(s) must represent a valid integer, as in digest_every: 1d (one day)"
                  else case readMaybe . T.unpack $ int :: Maybe Int of
                    Nothing -> failure "The first character(s) must represent a valid integer, as in digest_every: 1d (one day)"
                    Just n -> case triage n t_tag of
                      Left t -> failure t
                      Right s -> success $ realToFrac s
    | k == "digest_size" =
        if "reset" `T.isInfixOf` T.toCaseFold txt
          then (not_parsed, PDigestSize 10 : parsed)
          else case readMaybe . T.unpack $ txt :: Maybe Int of
            Nothing -> (k : not_parsed, parsed)
            Just n -> (not_parsed, PDigestSize n : parsed)
    | k == "digest_collapse" =
        if "reset" `T.isInfixOf` T.toCaseFold txt
          then (not_parsed, PDigestCollapse 0 : parsed)
          else case readMaybe . T.unpack $ txt :: Maybe Int of
            Nothing -> (k : not_parsed, parsed)
            Just n -> (not_parsed, PDigestCollapse n : parsed)
    | k == "digest_start" =
        case mbTime . T.unpack $ txt :: Maybe UTCTime of
          Nothing -> (k : not_parsed, parsed)
          Just t -> (not_parsed, PDigestStart t : parsed)
    | k == "blacklist" =
        if T.length txt < 3
          then ("'blacklist' cannot be shorter than 3 characters." : not_parsed, parsed)
          else
            let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
             in (not_parsed, PBlacklist v : parsed)
    | k == "disable_webview" =
        if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDisableWebview True : parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PDisableWebview False : parsed) else ("'disable_webview' takes only 'true' or 'false' as values." : not_parsed, parsed)
    | k == "digest_title" =
        if "reset" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDigestTitle (settings_digest_title defaultChatSettings) : parsed) else (not_parsed, PDigestTitle txt : parsed)
    | k == "forward_to_admins" =
        let res = "true" `T.isInfixOf` T.toCaseFold txt
         in (not_parsed, PForwardToAdmins res : parsed)
    | k == "search_notif" =
        if T.length txt < 3
          then ("'search_notif' cannot be shorter than 3 characters." : not_parsed, parsed)
          else
            if any (`T.isPrefixOf` txt) mkdDoubles
              then ("'search_notif' accepts only alphanumeric symbols" : not_parsed, parsed)
              else
                let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
                 in (not_parsed, PSearchKws v : parsed)
    | k == "only_search_notif" =
        if T.length txt < 3
          then ("'only_search_notif' cannot be shorter than 3 characters." : not_parsed, parsed)
          else
            let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
             in (not_parsed, PSearchLinks v : parsed)
    | k == "paused" =
        if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPaused True : parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPaused False : parsed) else ("'paused' takes only 'true' or 'false' as values." : not_parsed, parsed)
    | k == "pin" =
        if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPin True : parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPin False : parsed) else ("'pin' takes only 'true' or 'false' as values." : not_parsed, parsed)
    | k == "share_link" =
        if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PShareLink True : parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PShareLink False : parsed) else ("'share_link' takes only 'true' or 'false' as values." : not_parsed, parsed)
    | k == "pagination" =
        if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPagination True : parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPagination False : parsed) else ("'pagination' takes only 'true' or 'false' as values." : not_parsed, parsed)
    | k == "no_collapse" =
        if T.length txt < 3
          then ("'no_collapse' cannot be shorter than 3 characters." : not_parsed, parsed)
          else
            let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
             in (not_parsed, PNoCollapse v : parsed)
    | otherwise = (k : not_parsed, parsed)
  into_hm val acc =
    let [hh, mm] = T.splitOn ":" val
        (m1, m2) = T.splitAt 1 mm
        mm' =
          if m1 == "0"
            then readMaybe . T.unpack $ m2 :: Maybe Int
            else readMaybe . T.unpack $ mm :: Maybe Int
        hh' = readMaybe . T.unpack $ hh :: Maybe Int
     in if T.length val /= 5 || not (":" `T.isInfixOf` val)
          then []
          else case sequence [hh', mm'] of
            Nothing -> []
            Just parsed ->
              if length parsed /= 2
                then []
                else
                  let (h, m) = (head parsed, last parsed)
                   in groupPairs h m acc
  groupPairs h m acc
    | h < 0 || h > 24 = []
    | m < 0 || m > 60 = []
    | otherwise = acc ++ [(h, m)]
  mkPairs l acc =
    let (k, rest) = let (h, r) = T.breakOn ":" l in (T.toLower h, T.drop 1 r)
     in if T.null rest
          then Nothing
          else case acc of
            Nothing -> Just [(k, rest)]
            Just p -> Just $ (k, rest) : p
