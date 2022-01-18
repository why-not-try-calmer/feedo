{-# LANGUAGE RecordWildCards #-}

module Utils where
import AppTypes
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sort)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime (utctDayTime), addUTCTime, timeToDaysAndTimeOfDay)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import Data.Int (Int64)

{- Data -}

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldl' step ([],[])
    where
        step (!ls, !rs) val = case val of
            Left l -> (ls++[l], rs)
            Right r -> (ls, rs++[r])

fromEither :: b -> Either a b -> b
fromEither def (Left _) = def
fromEither _ (Right v) = v

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
                if h' < h || h == h' && m' < m then (h', m')
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
            in  if h' < h || h == h' && m' < m
                then go (p:s:ss) ps
                else go (s:p:ss) ps

scanTimeSlices :: [Int64] -> [Int64]
scanTimeSlices = go []
    where
        go done [] = done
        go !acc [x] = go (acc ++ [x - last acc]) []
        go !acc [x, y] = go (acc ++ [y-x]) []
        go !acc (x:y:vs) = go (acc ++ [y-x]) (y:vs)

{- Settings -}

validSettingsKeys :: [T.Text]
validSettingsKeys = [
    "batch_at",
    "batch_every",
    "batch_size",
    "blacklist",
    "disable_webview",
    "search_then_update",
    "only_search_results",
    "paused",
    "pin"
    ]

parseSettings :: [T.Text] -> Either T.Text [ParsingSettings]
parseSettings [] = Left "Unable to parse from an empty list of strings."
parseSettings lns = case foldr mkPairs Nothing lns of
    Nothing -> Left "Unable to parse the settings you've sent, please respect the format: \
        \ /set <optional: chat_id>\nkey: val val\nkey:val val"
    Just pairs ->
        let (not_parsed, parsed) = foldl' intoParsing ([],[]) pairs in
        if null not_parsed then Right parsed
        else Left $ T.intercalate ", " not_parsed
    where
        intoParsing (!not_parsed, !parsed) (!k, !txt)
            | k == "batch_at" =
                let failure l = (l:not_parsed, parsed)
                    success r = (not_parsed, PBatchAt r:parsed)
                    collected = sortTimePairs . foldr into_hm [] . T.words $ txt
                in  if "reset" `T.isInfixOf` txt then success [] else
                    if null collected
                    then failure "Unable to parse 'batch_at' values. \
                        \ Make sure every time is written as a 5-character string, i.e. '00:00' for midnight and '12:00' for noon. \
                        \ Use ':' to separate hours from minutes and whitespaces to separate many time values: '00:00 12:00' for midgnight and noon."
                    else success collected
            | k == "batch_every" =
                let failure l = (l:not_parsed, parsed)
                    success r = (not_parsed, PBatchEvery r:parsed)
                    int = T.init txt
                    t_tag = T.singleton . T.last $ txt
                    triage n t
                        | 1 > n = Left "n must be bigger than 0."
                        | "m" == t = Right $ n * 60
                        | "h" == t = Right $ n * 3600
                        | "d" == t = Right $ n * 86400
                        | otherwise = Left "'batch_every' needs a number of minutes, hours or days. Example: batch_at: 1d, batch_at: 6h, batch_at: 40m."
                in  if "reset" `T.isInfixOf` txt then success 0 else
                    if T.length txt < 2 then failure "The first character(s) must represent a valid integer, as in batch_every: 1d (one day)" else
                    case readMaybe . T.unpack $ int :: Maybe Int of
                        Nothing -> failure "The first character(s) must represent a valid integer, as in batch_every: 1d (one day)"
                        Just n -> case triage n t_tag of
                            Left t -> failure t
                            Right s -> success $ realToFrac s
            | k == "batch_size" =
                if "reset" `T.isInfixOf` txt then (not_parsed, PBatchSize 10:parsed)
                else case readMaybe . T.unpack $ txt :: Maybe Int of
                    Nothing -> (k:not_parsed, parsed)
                    Just n -> (not_parsed, PBatchSize n:parsed)
            | k == "blacklist" =
                if T.length txt < 3 then ("'blacklist' cannot be shorter than 3 characters.":not_parsed, parsed)
                else
                    let v = if "reset" `T.isInfixOf` txt then S.empty else S.fromList . T.words $ txt
                    in (not_parsed, PBlacklist v:parsed)
            | k == "disable_webview" =
                if "true" `T.isInfixOf` txt then (not_parsed, PDisableWebview True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PDisableWebview False:parsed) else ("'disable_webview' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "search_then_update" =
                if T.length txt < 3 then ("'search_then_update' cannot be shorter than 3 characters.":not_parsed, parsed)
                else
                    let v = if "reset" `T.isInfixOf` txt then S.empty else S.fromList . T.words $ txt
                    in (not_parsed, PSearchKws v:parsed)
            | k == "only_search_results" =
                if T.length txt < 3 then ("'only_search_results' cannot be shorter than 3 characters.":not_parsed, parsed)
                else
                    let v = if "reset" `T.isInfixOf` txt then S.empty else S.fromList . T.words $ txt
                    in (not_parsed, PSearchLinks v:parsed)
            | k == "paused" =
                if "true" `T.isInfixOf` txt then (not_parsed, PPaused True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPaused False:parsed) else ("'paused' takes only 'true' or 'false' as values.":not_parsed, parsed)
            | k == "pin" =
                if "true" `T.isInfixOf` txt then (not_parsed, PPin True:parsed) else if "false" `T.isInfixOf` txt then (not_parsed, PPin False:parsed) else ("'pin' takes only 'true' or 'false' as values.":not_parsed, parsed)
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
            let (k, rest) = let (h, r) = T.breakOn ":" l in (h, T.drop 1 r)
            in  if T.null rest then Nothing else case acc of
                Nothing -> Just [(k, rest)]
                Just p -> Just $ (k, rest):p

defaultChatSettings :: Settings
defaultChatSettings = Settings {
        settings_word_matches = WordMatches S.empty S.empty S.empty,
        settings_batch_size = 10,
        settings_batch_interval = BatchInterval (Just 86400) Nothing,
        settings_paused = False,
        settings_disable_web_view = False,
        settings_pin = False
    }

updateSettings :: [ParsingSettings] -> Settings -> Settings
updateSettings [] orig = orig
updateSettings parsed orig = foldl' (flip inject) orig parsed
    where
        inject p o = case p of
            PBatchAt v ->
                let bi = settings_batch_interval o
                    bi' = bi { batch_at = Just v }
                in  o { settings_batch_interval = bi' }
            PBatchEvery v ->
                let bi = settings_batch_interval o
                    bi' = bi { batch_every_secs = Just v}
                in  o { settings_batch_interval = bi' }
            PBatchSize v -> o { settings_batch_size = v }
            PBlacklist v ->
                let wm = settings_word_matches o
                    wm' = wm { match_blacklist = v}
                in  o { settings_word_matches = wm' }
            PDisableWebview v -> o { settings_disable_web_view = v }
            PPaused v -> o { settings_paused = v }
            PPin v -> o { settings_pin = v }
            PSearchKws v ->
                let wm = settings_word_matches o
                    wm' = wm { match_searchset = v}
                in  o { settings_word_matches = wm' }
            PSearchLinks v ->
                let wm = settings_word_matches o
                    wm' = wm { match_only_search_results = v}
                in  o { settings_word_matches = wm' }

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