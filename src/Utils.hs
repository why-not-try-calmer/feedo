{-# LANGUAGE RecordWildCards #-}
module Utils where

import AppTypes
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime (utctDayTime), addUTCTime)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Text.Read (readMaybe)
import TgramOutJson (ChatId)

{- Data -}

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldl' step ([],[])
    where
        step (!ls, !rs) val =
            case val of
            Left l -> (ls++[l], rs)
            Right r -> (ls, rs++[r])

maybeUserIdx :: [a] -> Int -> Maybe a
maybeUserIdx [] _ = Nothing
maybeUserIdx ls i
    |   i < 1 = Nothing
    |   i > length ls = Nothing
    |   i == 1 = Just $ head ls
    |   i == length ls = Just $ last ls
    |   otherwise = Just $ ls !! (i-1)

removeByUserIdx :: [a] -> [Int] -> Maybe [a]
removeByUserIdx [] _ = Nothing
removeByUserIdx _ [] = Nothing
removeByUserIdx ls is =
    if any (\i -> i >= length ls || i < 0) adjusted
    then Nothing
    else rm [] 0 ls adjusted
    where
        adjusted = sort . map (\i -> i-1) $ is
        rm acc _ [] _ = Just acc
        rm acc _ rest [] = Just (acc++rest)
        rm !acc !n (l:ls') (i:is') =
            if n == i then rm acc (n+1) ls' is'
            else rm (acc++[l]) (n+1) ls' (i:is')

{- Time -}

freshLastXDays :: Int -> UTCTime -> [Item] -> [Item]
freshLastXDays days now items =
    let x = fromIntegral $ days * 86400
        x_days_ago = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - x
    in  filter (\i -> i_pubdate i > x_days_ago) items

findNextTime :: UTCTime -> BatchInterval -> UTCTime
findNextTime now (Secs xs) = addUTCTime xs now
findNextTime now (HM ts) =
    let from_midnight = realToFrac $ utctDayTime now
        times = foldl' (\acc (!h, !m) ->
            let t = toNominalDifftime h m
            in  if from_midnight < t then (t - from_midnight):acc else acc
            ) [] ts
    in  if null times then
            let (early_h, early_m) = foldl' (\(!h, !m) (!h', !m') ->
                    if h' < h || (h == h' && m' < m) then (h', m') else (h, m)) (23, 59) ts
                to_midnight = realToFrac $ 86401 - utctDayTime now
                until_midnight = addUTCTime to_midnight now
            in  addUTCTime (toNominalDifftime early_h early_m) until_midnight
        else addUTCTime (minimum times) now
    where
        toNominalDifftime h m = realToFrac $ h * 3600 + m * 60

tooManySubs :: Int -> SubChats -> ChatId -> Bool
tooManySubs upper_bound chats cid = case HMS.lookup cid chats of
    Nothing -> False
    Just chat ->
        let diff = upper_bound - length (sub_feeds_links chat)
        in  diff < 0

{- Update settings -}

parseSettings :: [T.Text] -> Maybe ParsedSettings
parseSettings [] = Nothing
parseSettings lns = Map.fromList <$> foldr step Nothing lns
    where
    step l acc =
        let (k:ss) =
                T.splitOn ":" .
                T.filter (not . isSpace) $ l
        in  if null ss then Nothing else case acc of
            Nothing -> Just [(k, last ss)]
            Just p -> Just $ (k, last ss):p

defaultChatSettings :: Settings
defaultChatSettings = Settings {
        settings_filters = Filters [] [],
        settings_batch_size = 15,
        settings_batch_interval = Secs 9000,
        settings_is_paused = False,
        settings_webview = False,
        settings_pin = False,
        settings_clean = False
    }

mergeSettings :: ParsedSettings -> Settings -> Settings
{-
Expected fields:
- blacklist: term, term, ...
- whitelist: term, term, ...
- batch_at: hhmm, hhmm, ...
- batch_every: n
- paused: true | false,
- webview: true | false,
- pin: true | false,
- clean_behind: true | false
-}
mergeSettings keyvals orig =
    let updater = Settings {
            settings_filters =
                let blacklist = maybe [] (T.splitOn ",") $ Map.lookup "blacklist" keyvals
                    whitelist = maybe [] (T.splitOn ",") $ Map.lookup "whitelist" keyvals
                in  Filters blacklist whitelist,
            settings_batch_size =
                let mbread = readMaybe . T.unpack =<<
                        Map.lookup "batch_size" keyvals
                in  fromMaybe 10 mbread,
            settings_batch_interval =
                case Map.lookup "batch_every" keyvals of
                    Nothing -> case Map.lookup "batch_at" keyvals of
                        Nothing -> dflt
                        Just txt ->
                            let datetimes = T.splitOn "," txt
                                collected = foldl' step [] datetimes
                            in if null collected then dflt else HM collected
                    Just mbint -> maybe dflt Secs (readMaybe . T.unpack $ mbint),
            settings_is_paused = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "paused" keyvals,
            settings_webview = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "webview" keyvals,
            settings_pin = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "pin" keyvals,
            settings_clean = maybe False (\t -> "true" `T.isInfixOf` t) $
                Map.lookup "clean_behind" keyvals
        }
    in  orig {
            settings_filters = if "blacklist" `elem` keys then settings_filters updater else settings_filters orig,
            settings_batch_size = if "batch_size" `elem` keys then settings_batch_size updater else settings_batch_size orig,
            settings_batch_interval =
                if any (`elem` keys) ["batch_at", "batch_every"] then settings_batch_interval updater
                else settings_batch_interval orig,
            settings_is_paused = if "paused" `elem` keys then settings_is_paused updater else settings_is_paused orig,
            settings_webview = if "webview" `elem` keys then settings_webview updater else settings_webview orig,
            settings_pin = if "pin" `elem` keys then settings_webview updater else settings_webview orig,
            settings_clean = if "clean_behind" `elem` keys then settings_clean updater else settings_clean orig
        }
    where
        keys = Map.keys keyvals
        dflt = Secs 9000
        step acc val =
            let (hh, mm) = T.splitAt 2 val
                (m1, m2) = T.splitAt 1 mm
                mm' =
                    if m1 == "0" then readMaybe . T.unpack $ m2 :: Maybe Int
                    else readMaybe . T.unpack $ mm :: Maybe Int
                hh' = readMaybe . T.unpack $ hh :: Maybe Int
            in  case sequence [hh', mm'] of
                Nothing -> []
                Just parsed -> if length parsed /= 2 then [] else
                    let (h, m) = (head parsed, last parsed)
                    in  toAcc h m acc
        toAcc h m acc
            |   h < 0 || h > 24 = []
            |   m < 0 || m > 60 = []
            |   otherwise = acc ++ [(h, m)]

notifFor :: KnownFeeds -> SubChats -> HMS.HashMap ChatId (Settings, FeedItems)
notifFor feeds subs = HMS.foldl' (\acc f -> HMS.union (layer acc f) acc) HMS.empty feeds
    where
        layer acc f = HMS.foldl' (\hmapping c ->
            if f_link f `notElem` sub_feeds_links c || null (fresh_filtered c f) then hmapping else
            let v = (f, fresh_filtered c f)
            in  HMS.alter (\case
                Nothing -> Just (sub_settings c, [v])
                Just (s, vs) -> Just (s, v:vs)) (sub_chatid c) hmapping) acc subs
        fresh_filtered c f = filterItemsWith (sub_settings c) (sub_last_notification c) $ f_items f
        with_filters fs i = all ($ i) fs
        blacklist filters i = not . any
            (\bw -> T.toCaseFold bw `T.isInfixOf` i_link i || T.toCaseFold bw `T.isInfixOf` i_desc i) $ filters
        filterItemsWith Settings{..} Nothing items =
            take settings_batch_size .
            filter (with_filters [blacklist (filters_blacklist settings_filters)]) $ items
        filterItemsWith Settings{..} (Just last_time) items =
            take settings_batch_size .
            filter (with_filters [blacklist (filters_blacklist settings_filters), fresh]) $ items
            where
                fresh i = last_time < i_pubdate i