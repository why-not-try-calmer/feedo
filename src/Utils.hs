{-# LANGUAGE RecordWildCards #-}

module Utils where
import AppTypes
import Data.Bits (Bits (xor))
import qualified Data.HashMap.Strict as HMS
import Data.Int (Int64)
import Data.List (foldl', sort)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime (utctDayTime), addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, parseTimeM, rfc822DateFormat, timeToDaysAndTimeOfDay)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Data.Time.Format.ISO8601
import TgramOutJson (ChatId)
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

hash :: String -> Int
hash = abs . foldl' (\h c -> 33*h `xor` fromEnum c) 5381

{- Time -}

mbTime :: String -> Maybe UTCTime
mbTime s = if isNothing first_pass then iso8601ParseM s else first_pass
    where
        first_pass = foldr step Nothing formats
        step f acc = maybe acc pure $ parseTimeM True defaultTimeLocale f s
        formats = [rfc822DateFormat, "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%d"]

averageInterval :: [UTCTime] -> Maybe NominalDiffTime
averageInterval [] = Nothing
averageInterval (x:xs) = go [] x (sort xs)
    where
        {-
        nroot :: (Integral a, Floating b) => a -> b -> b 
        n `nroot` x = x ** (1 / fromIntegral n)
        geo_avg acc = realToFrac $ nroot (length acc) (realToFrac . product . map abs $ acc) :: NominalDiffTime
        -}
        avg acc = realToFrac $ floor (sum $ map abs acc) `div` length acc :: NominalDiffTime
        go !acc _ [] = Just $ avg acc
        go !acc tn (tm:ts) =
            let diffed = diffUTCTime tn tm
            in  go (diffed:acc) tm ts

renderAvgInterval :: Maybe NominalDiffTime -> T.Text
renderAvgInterval Nothing = "No average interval was computed."
renderAvgInterval (Just i) =
    let a = if i < 0 then -i else i
        hours = floor a `div` 3600 :: Integer
        (d, h) = go hours 0
    in  (T.pack . show $ d) `T.append` " days, " `T.append` (T.pack . show $ h) `T.append` " hours."
    where
        go !hs !d = if hs < 24 then (d, hs) else go (hs-24 :: Integer) (d+1 :: Integer)

freshLastXDays :: Int -> UTCTime -> [Item] -> [Item]
freshLastXDays days now items =
    let x = fromIntegral $ days * 86400
        x_days_ago = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - x
    in  filter (\i -> i_pubdate i > x_days_ago) items

findNextTime :: UTCTime -> DigestInterval -> UTCTime
findNextTime now (DigestInterval Nothing Nothing) = addUTCTime 9000 now
findNextTime now (DigestInterval (Just xs) Nothing) = addUTCTime xs now
findNextTime now (DigestInterval mbxs (Just ts)) = case mbxs of
    Nothing ->
        if null times then next_day
        else still_today
    Just xs ->
        if xs >= 86400 then addUTCTime xs next_day
        else addUTCTime xs now
    where
        toNominalDifftime h m = realToFrac $ h * 3600 + m * 60
        from_midnight = realToFrac $ utctDayTime now
        times = foldl' (\acc (!h, !m) ->
            let t = toNominalDifftime h m
            in  if from_midnight < t then (t - from_midnight):acc else acc
            ) [] ts
        (early_h, early_m) =
            foldl' (\(!h, !m) (!h', !m') ->
                if h' < h || h == h' && m' < m then (h', m')
                else (h, m)) (23, 59) ts
        to_midnight = realToFrac $ 86400 - utctDayTime now
        until_midnight = addUTCTime to_midnight now
        next_day = addUTCTime (toNominalDifftime early_h early_m) until_midnight
        still_today = addUTCTime (minimum times) now

nomDiffToReadable :: NominalDiffTime -> T.Text
nomDiffToReadable t =
    let (days, time) = timeToDaysAndTimeOfDay t
    in  (T.pack . show $ days) `T.append` " day(s), " `T.append` (T.pack . show $ time) `T.append` " hour(s)"

utcToYmd :: UTCTime -> T.Text
utcToYmd = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

utcToYmdHMS :: UTCTime -> T.Text
utcToYmdHMS = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %T"

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
scanTimeSlices [] = []
scanTimeSlices (x:xs) = fst $ foldl' step ([], x) xs
    where
        step (!acc, !x') y = (acc ++ [y-x'], y)

{- Settings -}

defaultChatSettings :: Settings
defaultChatSettings = Settings {
        settings_word_matches = WordMatches S.empty S.empty S.empty,
        settings_digest_size = 10,
        settings_digest_interval = DigestInterval (Just 86400) Nothing,
        settings_digest_title = "A new digest is available",
        settings_paused = False,
        settings_disable_web_view = False,
        settings_pin = False,
        settings_share_link = True,
        settings_follow = False,
        settings_digest_collapse = Nothing,
        settings_digest_start = Nothing
    }

updateSettings :: [ParsingSettings] -> Settings -> Settings
updateSettings [] orig = orig
updateSettings parsed orig = foldl' (flip inject) orig parsed
    where
        inject p o = case p of
            PDigestCollapse v -> o { settings_digest_collapse = if v == 0 then Nothing else Just v }
            PDigestAt v ->
                let bi = settings_digest_interval o
                    bi' = bi { digest_at = if null v then Nothing else Just v }
                in  o { settings_digest_interval = bi' }
            PDigestEvery v ->
                let bi = settings_digest_interval o
                    bi' = bi { digest_every_secs = if v == 0 then Nothing else Just v}
                in  o { settings_digest_interval = bi' }
            PDigestSize v -> o { settings_digest_size = v }
            PDigestStart v -> o { settings_digest_start = Just v }
            PDigestTitle v -> o { settings_digest_title = v }
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
            PShareLink v -> o { settings_share_link = v }
            PFollow v -> o { settings_follow = v }

notifFrom ::
    [FeedLink] ->
    KnownFeeds ->
    HMS.HashMap ChatId (SubChat, [FeedLink], [FeedLink]) ->
    HMS.HashMap ChatId (SubChat, FeedItems, FeedItems)
notifFrom flinks feeds_map = foldl' (\hmap (!c, !ds, !fs) ->
    let (dig, fol) = foldl' (\(!digests, !follows) f ->
            let is = f_items f
                l = f_link f
                digests' =
                    if l `elem` ds && l `notElem` only_on_search c then (f, fresh_filtered c is):digests
                    else digests
                follows' =
                    if l `elem` fs && l `notElem` only_on_search c then (f, fresh_filtered c is):follows
                    else follows
                follows'' = filter (`notElem` digests') follows'
            in  if f_link f `notElem` flinks then (digests, follows)
                else (digests', follows'')) ([],[]) feeds_map
    in  if null (dig ++ fol) then hmap else HMS.insert (sub_chatid c) (c, dig, fol) hmap) HMS.empty
    where
        fresh_filtered c is = filterItemsWith (sub_settings c) (sub_last_digest c) is
        only_on_search = match_only_search_results . settings_word_matches . sub_settings
        with_filters fs i = all ($ i) fs
        blacklist filters i = not . any
            (\bw -> any (\t -> bw `T.isInfixOf` t) [i_desc i, i_link i, i_title i]) $ filters
        filterItemsWith Settings{..} Nothing items =
            take settings_digest_size .
            filter (with_filters [blacklist (match_blacklist settings_word_matches)]) $ items
        filterItemsWith Settings{..} (Just last_time) items =
            take settings_digest_size .
            filter (with_filters [blacklist (match_blacklist settings_word_matches), fresh]) $ items
            where
                fresh i = last_time < i_pubdate i