{-# LANGUAGE DataKinds #-}

module Parsing where

import AppTypes
  ( Feed (..),
    FeedType (..),
    Item (Item, i_pubdate),
    ParsingSettings (..),
    UserError (BadFeedUrl, BadInput, ParseError),
    renderUserError, Settings (settings_digest_title)
  )
import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Req
import Replies
import Requests (fetchFeed)
import Text.Read (readMaybe)
import Text.XML
import Text.XML.Cursor
import Utils (averageInterval, defaultChatSettings, mbTime, sortTimePairs)

{- Feeds, Items -}

buildFeed :: MonadIO m => FeedType -> Url scheme -> m (Either UserError Feed)
-- tries parsing bytes into a Feed
-- tries as Atom if Rss fails
buildFeed ty url = do
    now <- liftIO getCurrentTime
    fetchFeed url >>= \case
        Nothing -> pure. Left . BadFeedUrl $ renderUrl url
        Just feed -> case parseLBS def feed of
            Left (SomeException _) -> pure . Left . ParseError $ "Unable to parse feed at " `T.append` (T.pack . show $ url)
            Right doc ->
                let root = fromDocument doc
                in case ty of
                    Rss ->
                        let desc = T.concat $ child root >>= child >>= element "description" >>= child >>= content
                            title = T.concat $ child root >>= child >>= element "title" >>= child >>= content
                            -- link = T.concat $ child root >>= child >>= element "link" >>= child >>= content
                            get_item_data el =
                                Item
                                (T.concat $ child el >>= element "title" >>= child >>= content)
                                (T.concat $ child el >>= element "description" >>= child >>= content)
                                (T.concat $ child el >>= element "link" >>= child >>= content)
                                (renderUrl url)
                                (fromMaybe now . mbTime $ T.unpack (T.concat $ child el >>= element "pubDate" >>= child >>= content))
                            items = map get_item_data $ descendant root >>= element "item"
                            interval = averageInterval $ map i_pubdate items
                            res = Feed
                                {
                                    f_type = Rss,
                                    f_desc = desc,
                                    f_title = title,
                                    f_link = renderUrl url,
                                    f_items = items,
                                    f_avg_interval = interval,
                                    f_last_refresh = Just now,
                                    f_reads = 0
                                }
                        in  faultyOrValid url res
                    Atom ->
                        let desc = T.concat $ child root >>= laxElement "subtitle" >>= child >>= content
                            title = T.concat $ child root >>= laxElement "title" >>= child >>= content
                            -- link = T.concat . attribute "href" . head $ child root >>= laxElement "link"
                            get_item_data el =
                                Item
                                (T.concat $ child el >>= laxElement "title" >>= child >>= content)
                                (T.concat $ child el >>= laxElement "content" >>= child >>= content)
                                (T.concat . attribute "href" . head $ child el >>= laxElement "link")
                                (renderUrl url)
                                (fromMaybe now . mbTime $ T.unpack (T.concat $ child el >>= laxElement "updated" >>= child >>= content))
                            items = map get_item_data $ descendant root >>= laxElement "entry"
                            interval = averageInterval $ map i_pubdate items
                            res = Feed
                                {
                                    f_type = Atom,
                                    f_desc = desc,
                                    f_title = title,
                                    f_link = renderUrl url,
                                    f_items = items,
                                    f_avg_interval = interval,
                                    f_last_refresh = Just now,
                                    f_reads = 0
                                }
                        in  faultyOrValid url res
    where
        faultyOrValid u res =
            if faultyFeed res
            then pure . Left . ParseError $ T.pack . show . renderUrl $ u
            else pure . Right $ res
        faultyFeed f =
            let predicates = [any T.null [f_desc f, f_title f, f_link f], null . f_items $ f]
            in  or predicates

eitherUrlScheme :: T.Text -> Either UserError (Url 'Https)
-- tries to make a valid Url Scheme from the given string
eitherUrlScheme s
    | T.null s = Left . BadInput $ s
    | head split /= "https:" = Left . BadInput $ s
    | length body >= 2 = Right toScheme
    | otherwise = Left . BadInput $ "Unable to parse this input." `T.append` s
    where
        s' = if T.last s == T.last "/" then T.dropEnd 1 s else s
        split = T.splitOn "/" s'
        body = drop 2 split
        (host:rest) = body
        toScheme = foldl' (/:) (https host) rest

getFeedFromUrlScheme :: MonadIO m => Url scheme -> m (Either T.Text Feed)
getFeedFromUrlScheme scheme = buildFeed Rss scheme >>= \case
    Left _ -> buildFeed Atom scheme >>= \case
        Left err -> pure . Left .  renderUserError $ err
        Right feed -> finish_successfully feed
    Right feed -> finish_successfully feed
    where
        finish_successfully feed = pure . Right $ feed

getFeedFromHref :: MonadIO m => T.Text -> m (Either T.Text Feed)
-- parses url and tries to fetch & build  a new feed to the 'store'
getFeedFromHref url = case eitherUrlScheme url of
    Left err -> pure . Left .  renderUserError $ err
    Right urlScheme -> getFeedFromUrlScheme urlScheme

rebuildFeed :: MonadIO m => T.Text -> m (Either T.Text Feed)
-- updates one singular feed and returns the result to the caller
rebuildFeed key = case eitherUrlScheme key of
    Left _ -> pure . Left $ key
    Right url -> buildFeed Rss url >>= \case
        Left _ -> buildFeed Atom url >>= \case
            Left _ ->
                let faulty_key = T.pack . show $ url
                in pure $ Left faulty_key
            Right feed -> done feed
        Right feed -> done feed
  where done feed = liftIO getCurrentTime >>= \now -> pure . Right $ feed {f_last_refresh = Just now}

{- Settings -}

parseSettings :: [T.Text] -> Either T.Text [ParsingSettings]
parseSettings [] = Left "Unable to parse from an empty list of strings."
parseSettings lns = case foldr mkPairs Nothing lns of
    Nothing -> Left "Unable to parse the settings you've sent, please respect the format: \
        \ /set <optional: chat_id>\nkey: val"
    Just pairs ->
        let (not_parsed, parsed) = foldl' intoParsing ([],[]) pairs in
        if null not_parsed then Right parsed
        else Left $ T.intercalate ", " not_parsed `T.append`
            ". Make sure to use only valid key-value pairs: " `T.append`
                T.intercalate "\n" [
                    "blacklist <keyword keyword ...>",
                    "digest_at <HH:MM HH:MM ...>",
                    "digest_collapse <n>",
                    "digest_every <n> <m|h|d>",
                    "digest_size <n>",
                    "digest_start <YYYY-mm-dd>",
                    "digest_title <title>",
                    "disable_webview <false|true>",
                    "follow <false|true>",
                    "only_search_notif <url1 url2 ...>",
                    "pin <false|true>",
                    "search_notif <keyword keyword ...>",
                    "share_link <false|true>"
                    ]
    where
        intoParsing (!not_parsed, !parsed) (!k, !txt)
            | k == "digest_at" =
                let failure l = (l:not_parsed, parsed)
                    success r = (not_parsed, PDigestAt r:parsed)
                    collected = sortTimePairs . foldr into_hm [] . T.words $ txt
                in  if "reset" `T.isInfixOf` T.toCaseFold txt then success [] else
                    if null collected
                    then failure "Unable to parse 'digest_at' values. \
                        \ Make sure every time is written as a 5-character string, i.e. '00:00' for midnight and '12:00' for noon. \
                        \ Use ':' to separate hours from minutes and whitespaces to separate many time values: '00:00 12:00' for midgnight and noon."
                    else success collected
            | k == "digest_every" =
                let failure l = (l:not_parsed, parsed)
                    success r = (not_parsed, PDigestEvery r:parsed)
                    int = T.init txt
                    t_tag = T.singleton . T.last $ txt
                    triage n t
                        | 1 > n = Left "n must be bigger than 0."
                        | "m" == t = Right $ n * 60
                        | "h" == t = Right $ n * 3600
                        | "d" == t = Right $ n * 86400
                        | otherwise = Left "'digest_every' needs a number of minutes, hours or days. Example: digest_at: 1d, digest_at: 6h, digest_at: 40m."
                in  if "reset" `T.isInfixOf` T.toCaseFold txt then success 0 else
                    if T.length txt < 2 then failure "The first character(s) must represent a valid integer, as in digest_every: 1d (one day)" else
                    case readMaybe . T.unpack $ int :: Maybe Int of
                        Nothing -> failure "The first character(s) must represent a valid integer, as in digest_every: 1d (one day)"
                        Just n -> case triage n t_tag of
                            Left t -> failure t
                            Right s -> success $ realToFrac s
            | k == "digest_size" =
                if "reset" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDigestSize 10:parsed)
                else case readMaybe . T.unpack $ txt :: Maybe Int of
                    Nothing -> (k:not_parsed, parsed)
                    Just n -> (not_parsed, PDigestSize n:parsed)
            | k == "digest_collapse" =
                if "reset" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDigestCollapse 0:parsed)
                else case readMaybe . T.unpack $ txt :: Maybe Int of
                    Nothing -> (k:not_parsed, parsed)
                    Just n -> (not_parsed, PDigestCollapse n:parsed)
            | k == "digest_start" =
                case mbTime . T.unpack $ txt :: Maybe UTCTime of
                    Nothing -> (k: not_parsed, parsed)
                    Just t -> (not_parsed, PDigestStart t:parsed)
            | k == "blacklist" =
                if T.length txt < 3 then ("'blacklist' cannot be shorter than 3 characters.":not_parsed, parsed)
                else
                    let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
                    in (not_parsed, PBlacklist v:parsed)
            | k == "disable_webview" =
                if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDisableWebview True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PDisableWebview False:parsed) else ("'disable_webview' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "digest_title" =
                if "reset" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PDigestTitle (settings_digest_title defaultChatSettings):parsed) else (not_parsed, PDigestTitle txt:parsed)
            | k == "search_notif" =
                if T.length txt < 3 then ("'search_notif' cannot be shorter than 3 characters.":not_parsed, parsed)
                else if any (`T.isPrefixOf` txt) mkdV2 then ("'search_notif' accepts only alphanumeric symbols":not_parsed, parsed) else
                    let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
                    in (not_parsed, PSearchKws v:parsed)
            | k == "only_search_notif" =
                if T.length txt < 3 then ("'only_search_notif' cannot be shorter than 3 characters.":not_parsed, parsed)
                else
                    let v = if "reset" `T.isInfixOf` T.toCaseFold txt then S.empty else S.fromList . T.words . T.toLower $ txt
                    in (not_parsed, PSearchLinks v:parsed)
            | k == "paused" =
                if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPaused True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPaused False:parsed) else ("'paused' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "pin" =
                if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPin True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPin False:parsed) else ("'pin' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "share_link" =
                if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PPaused True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PShareLink False:parsed) else ("'share_link' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "follow" =
                if "true" `T.isInfixOf` T.toCaseFold txt then (not_parsed, PFollow True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PFollow False:parsed) else ("'follow' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | otherwise = (k:not_parsed, parsed)
        into_hm val acc =
            let [hh, mm] = T.splitOn ":" val
                (m1, m2) = T.splitAt 1 mm
                mm' =
                    if m1 == "0" then readMaybe . T.unpack $ m2 :: Maybe Int
                    else readMaybe . T.unpack $ mm :: Maybe Int
                hh' = readMaybe . T.unpack $ hh :: Maybe Int
            in  if T.length val /= 5 || not (":" `T.isInfixOf` val) then []
                else case sequence [hh', mm'] of
                    Nothing -> []
                    Just parsed -> if length parsed /= 2 then [] else
                        let (h, m) = (head parsed, last parsed)
                        in  groupPairs h m acc
        groupPairs h m acc
            | h < 0 || h > 24 = []
            | m < 0 || m > 60 = []
            | otherwise = acc ++ [(h, m)]
        mkPairs l acc =
            let (k, rest) = let (h, r) = T.breakOn ":" l in (T.toLower h, T.drop 1 r)
            in  if T.null rest then Nothing else case acc of
                Nothing -> Just [(k, rest)]
                Just p -> Just $ (k, rest):p