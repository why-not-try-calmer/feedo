{-# LANGUAGE RecordWildCards #-}
module Utils where

import AppTypes
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime (utctDayTime), addUTCTime, timeToDaysAndTimeOfDay)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import qualified Data.Set as S

{- Data -}

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldl' step ([],[])
    where
        step (!ls, !rs) val = case val of
            Left l -> (ls++[l], rs)
            Right r -> (ls, rs++[r])

maybeUserIdx :: [a] -> Int -> Maybe a
maybeUserIdx [] _ = Nothing
maybeUserIdx ls i
    | i < 1 = Nothing
    | i > length ls = Nothing
    | i == 1 = Just $ head ls
    | i == length ls = Just $ last ls
    | otherwise = Just $ ls !! (i-1)

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

tooManySubs :: Int -> SubChats -> ChatId -> Bool
tooManySubs upper_bound chats cid = case HMS.lookup cid chats of
    Nothing -> False
    Just chat ->
        let diff = upper_bound - length (sub_feeds_links chat)
        in  diff < 0

{- Time -}

freshLastXDays :: Int -> UTCTime -> [Item] -> [Item]
freshLastXDays days now items =
    let x = fromIntegral $ days * 86400
        x_days_ago = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - x
    in  filter (\i -> i_pubdate i > x_days_ago) items

findNextTime :: UTCTime -> BatchInterval -> UTCTime
findNextTime now (BatchInterval Nothing Nothing) = addUTCTime 9000 now
findNextTime now (BatchInterval (Just xs) Nothing) = addUTCTime xs now
findNextTime now (BatchInterval mbxs (Just ts)) =
    let from_midnight = realToFrac $ utctDayTime now
        times = foldl' (\acc (!h, !m) ->
            let t = toNominalDifftime h m
            in  if from_midnight < t then (t - from_midnight):acc else acc
            ) [] ts
        (early_h, early_m) =
            foldl' (\(!h, !m) (!h', !m') ->
                if h' < h || (h == h' && m' < m) then (h', m')
                else (h, m)) (23, 59) ts
        to_midnight = realToFrac $ 86401 - utctDayTime now
        until_midnight = addUTCTime to_midnight now
        next_day = addUTCTime (toNominalDifftime early_h early_m) until_midnight
        still_today = addUTCTime (minimum times) now
    in  if null times then maybe next_day
            (\xs -> if xs >= 86401 then xs `addUTCTime` next_day else next_day) mbxs
        else still_today
    where
        toNominalDifftime h m = realToFrac $ h * 3600 + m * 60

secsToReadable :: NominalDiffTime -> T.Text
secsToReadable t =
    let (days, time) = timeToDaysAndTimeOfDay t
    in  (T.pack . show $ days) `T.append` " day(s), " `T.append` (T.pack . show $ time) `T.append` " hour(s)"

sortTimePairs :: [(Int, Int)] -> [(Int, Int)]
sortTimePairs = go [] 
    where
        go sorter [] = sorter
        go [] (p:ps) = go [p] ps
        go (s:ss) (p:ps) = 
            let (h, m) = s
                (h', m') = p
            in  if h' < h || (h == h' && m' < m)
                then go (p:s:ss) ps
                else go (s:p:ss) ps

{- Update settings -}

validSettingsKeys :: [T.Text]
validSettingsKeys = [
    "batch_size",
    "batch_at",
    "batch_every",
    "blacklist",
    "disable_webview",
    "search",
    "only_search_results",
    "paused",
    "pin"
    ]

parseSettings :: [T.Text] -> Either T.Text KeysParsedSettings
parseSettings [] = Left "Unable to parse from an empty string."
parseSettings lns = case foldr mkPairs Nothing lns of
    Nothing -> Left "Unable to parse one or more fields/values. \
            \ Did you forget to use linebreaks after /set or /setchan? \ 
            \ Correct format is: /set\nkey1:val1\nkey2:val2\n..."
    Just l_keyvals ->
        let keys = map fst l_keyvals
            invalid_keys = filter (`notElem` validSettingsKeys) keys
            settings = settings_from . Map.fromList $ l_keyvals
        in  if null invalid_keys then case settings of
                Left err -> Left err
                Right ss -> Right (keys, ss)
            else Left $ "Found invalid keys: " `T.append` T.intercalate ", " invalid_keys
    where
        left_typical_error = Left "'batch_every' needs a number of minutes, hours or days. Example: batch_at: 1d, batch_at: 6h, batch_at: 40m."
        reset setter resetter ws
            | "reset" `elem` ws = resetter
            | otherwise = setter ws
        settings_from kvals = case p_interval kvals of
            Left err -> Left err
            Right interval -> Right $ Settings {
                settings_batch_interval = interval,
                settings_batch_size = p_batch_size kvals,
                settings_word_matches = p_word_matches kvals,
                settings_disable_web_view = p_disable_web_view kvals,
                settings_paused = p_paused kvals,
                settings_pin = p_pin kvals
            }
        p_interval ks = case p_batch_at ks of
            Left err -> Left err
            Right at -> case p_batch_every ks of
                Left err -> Left err
                Right every -> Right $ BatchInterval
                    (if every == 0 then Nothing else Just . realToFrac $ every)
                    (if null at then Nothing else Just at)
        p_word_matches ks =
            let blacklist = maybe S.empty (reset S.fromList S.empty . T.words) $ Map.lookup "blacklist" ks
                searches = maybe S.empty (reset S.fromList S.empty . T.words) $ Map.lookup "search" ks
                only_results = maybe S.empty (reset S.fromList S.empty . T.words) $ Map.lookup "only_search_results" ks
            in  WordMatches blacklist searches only_results
        p_batch_size ks = maybe 10 (\v -> reset (fromMaybe 10 . readMaybe . T.unpack . T.concat) 10 [v]) $ Map.lookup "batch_size" ks
        p_batch_at ks = case Map.lookup "batch_at" ks of
            Nothing -> Right []
            Just datetime_strs ->
                let collected = sortTimePairs . foldr into_hm [] . T.words $ datetime_strs
                in  if "reset" `T.isInfixOf` datetime_strs then Right [] else
                    if null collected
                    then Left "Unable to parse 'batch_at' values. \
                        \ Make sure every time is written as a 5-character string, i.e. '00:00' for midnight and '12:00' for noon. \
                        \ Use ':' to separate hours from minutes and whitespaces to separate many time values: '00:00 12:00' for midgnight and noon."
                    else Right collected
        p_batch_every ks = case Map.lookup "batch_every" ks of
            Nothing -> Right 0
            Just every_s ->
                let int = T.init every_s
                    t_tag = T.singleton . T.last $ every_s
                    triage n t
                        | 1 > n = Left "n must be bigger than 0."
                        | "m" == t = Right $ n * 60
                        | "h" == t = Right $ n * 3600
                        | "d" == t = Right $ n * 86400
                        | otherwise = left_typical_error
                in  if "reset" `T.isInfixOf` every_s then Right 0 else
                        if T.length every_s < 2
                        then left_typical_error
                        else case readMaybe . T.unpack $ int :: Maybe Int of
                            Nothing -> Left "The first character(s) must represent a valid integer, as in batch_every: 1d (one day)"
                            Just n -> triage n t_tag
        p_paused ks = maybe False (\t -> "true" `T.isInfixOf` t) $
            Map.lookup "paused" ks
        p_disable_web_view ks = maybe False (\t -> "true" `T.isInfixOf` t) $
            Map.lookup "disable_webview" ks
        p_pin ks = maybe False (\t -> "true" `T.isInfixOf` t) $
            Map.lookup "pin" ks
        mkPairs l acc =
            let (k, rest) = let (h, r) = T.breakOn ":" l in (h, T.drop 1 r)
            in  if T.null rest then Nothing else case acc of
                Nothing -> Just [(k, rest)]
                Just p -> Just $ (k, rest):p
        into_hm val acc =
            let [hh, mm] = T.splitOn ":" val
                (m1, m2) = T.splitAt 1 mm
                mm' =
                    if m1 == "0" then readMaybe . T.unpack $ m2 :: Maybe Int
                    else readMaybe . T.unpack $ mm :: Maybe Int
                hh' = readMaybe . T.unpack $ hh :: Maybe Int
            in  if T.length val /= 5 || not (":" `T.isInfixOf` val) then [] else case sequence [hh', mm'] of
                Nothing -> []
                Just parsed -> if length parsed /= 2 then [] else
                    let (h, m) = (head parsed, last parsed)
                    in  groupPairs h m acc
        groupPairs h m acc
            | h < 0 || h > 24 = []
            | m < 0 || m > 60 = []
            | otherwise = acc ++ [(h, m)]

defaultChatSettings :: Settings
defaultChatSettings = Settings {
        settings_word_matches = WordMatches S.empty S.empty S.empty,
        settings_batch_size = 10,
        settings_batch_interval = BatchInterval (Just 86400) Nothing,
        settings_paused = False,
        settings_disable_web_view = False,
        settings_pin = False
    }

mergeSettings :: KeysParsedSettings -> Settings -> Settings
mergeSettings (keys, updater) orig = orig {
    settings_word_matches =
        let blacklist =
                if "blacklist" `elem` keys then match_blacklist $ settings_word_matches updater
                else match_blacklist $ settings_word_matches orig
            searches =
                if "search" `elem` keys then match_searchset $ settings_word_matches updater
                else match_searchset $ settings_word_matches orig
            only_search_results =
                if "only_search_results" `elem` keys then match_only_search_results $ settings_word_matches updater
                else match_searchset $ settings_word_matches orig
        in  WordMatches blacklist searches only_search_results,
    settings_batch_size =
        if "batch_size" `elem` keys then settings_batch_size updater
        else settings_batch_size orig,
    settings_batch_interval =
        let at =
                if "batch_at" `elem` keys then batch_at . settings_batch_interval $ updater
                else batch_at . settings_batch_interval $ orig
            every =
                if "batch_every" `elem` keys then batch_every_secs . settings_batch_interval $ updater
                else batch_every_secs . settings_batch_interval $ orig
        in  BatchInterval every at,
    settings_paused =
        if "paused" `elem` keys then settings_paused updater
        else settings_paused orig,
    settings_disable_web_view =
        if "disable_webview" `elem` keys then settings_disable_web_view updater
        else settings_disable_web_view orig,
    settings_pin =
        if "pin" `elem` keys then settings_pin updater
        else settings_pin orig
    }

notifFor :: KnownFeeds -> SubChats -> HMS.HashMap ChatId (Settings, FeedItems)
notifFor feeds subs = HMS.foldl' (\acc f -> HMS.union (layer acc f) acc) HMS.empty feeds
    where
        layer acc f = HMS.foldl' (\hmapping c -> if
                f_link f `notElem` sub_feeds_links c ||
                null (fresh_filtered c f) ||
                f_link f `elem` only_on_search c
            then hmapping else let feed_items = (f, fresh_filtered c f) in
                HMS.alter (\case
                    Nothing -> Just (sub_settings c, [feed_items])
                    Just (s, fits) -> Just (s, feed_items:fits)) (sub_chatid c) hmapping) acc subs
        only_on_search = match_only_search_results . settings_word_matches . sub_settings
        fresh_filtered c f = filterItemsWith (sub_settings c) (sub_last_notification c) $ f_items f
        with_filters fs i = all ($ i) fs
        blacklist filters i = not . any
            (\bw -> T.toCaseFold bw `T.isInfixOf` i_link i || T.toCaseFold bw `T.isInfixOf` i_desc i) $ filters
        filterItemsWith Settings{..} Nothing items =
            take settings_batch_size .
            filter (with_filters [blacklist (match_blacklist settings_word_matches)]) $ items
        filterItemsWith Settings{..} (Just last_time) items =
            take settings_batch_size .
            filter (with_filters [blacklist (match_blacklist settings_word_matches), fresh]) $ items
            where
                fresh i = last_time < i_pubdate i