module Utils where

import qualified Data.HashMap.Strict as HMS
import Data.Int (Int64)
import Data.List (foldl', sort, sortOn)
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime (..), defaultTimeLocale, diffUTCTime, formatTime, parseTimeM, rfc822DateFormat, timeToDaysAndTimeOfDay)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (
  posixSecondsToUTCTime,
  utcTimeToPOSIXSeconds,
 )
import Data.Time.Format.ISO8601
import Text.Read (readMaybe)
import TgramOutJson (ChatId)
import Types

{- Data -}

partitionEither :: [Either a b] -> ([a], [b])
{-# INLINE partitionEither #-}
partitionEither = foldl' step ([], [])
 where
  step (!ls, !rs) val = case val of
    Left l -> (l : ls, rs)
    Right r -> (ls, r : rs)

reduceMaybeWith :: (a -> Maybe b) -> [a] -> [b]
{-# INLINE reduceMaybeWith #-}
reduceMaybeWith f = foldl' (\acc val -> case f val of Just x -> x : acc; _ -> acc) []

fromEither :: b -> Either a b -> b
{-# INLINE fromEither #-}
fromEither def (Left _) = def
fromEither _ (Right v) = v

maybeUserIdx :: [a] -> Int -> Maybe a
{-# INLINE maybeUserIdx #-}
maybeUserIdx [] _ = Nothing
maybeUserIdx ls i
  | i < 1 = Nothing
  | i > length ls = Nothing
  | i == 1 = Just $ head ls
  | i == length ls = Just $ last ls
  | otherwise = Just $ ls !! (i - 1)

removeByUserIdx :: [a] -> [Int] -> Maybe [a]
{-# INLINE removeByUserIdx #-}
removeByUserIdx [] _ = Nothing
removeByUserIdx _ [] = Nothing
removeByUserIdx ls is =
  if any (\i -> i >= length ls || i < 0) adjusted
    then Nothing
    else rm [] 0 ls adjusted
 where
  adjusted = sort . map (\i -> i - 1) $ is
  rm acc _ [] _ = Just acc
  rm acc _ rest [] = Just (acc ++ rest)
  rm !acc !n (l : ls') (i : is') =
    if n == i
      then rm acc (n + 1) ls' is'
      else rm (acc ++ [l]) (n + 1) ls' (i : is')

{- Subs -}

tooManySubs :: Int -> SubChats -> ChatId -> Bool
tooManySubs upper_bound chats cid = case HMS.lookup cid chats of
  Nothing -> False
  Just chat ->
    let diff = upper_bound - length (sub_feeds_links chat)
     in diff < 0

{- Batches, feeds -}

feedsFromList :: [Feed] -> HMS.HashMap FeedLink Feed
feedsFromList = HMS.fromList . map (\f -> (f_link f, f))

unFeedRef :: FeedRef -> T.Text
unFeedRef (ByUrl s) = s
unFeedRef (ById s) = T.pack $ show s

unFeedRefs :: [FeedRef] -> [T.Text]
unFeedRefs = map unFeedRef

toFeedRef :: [T.Text] -> Either InterpreterErr [FeedRef]
{-# INLINE toFeedRef #-}
toFeedRef ss
  | all_valid_urls = Right intoUrls
  | all_ints = Right intoIds
  | otherwise = Left . InterpreterErr . T.concat $ ss
 where
  all_valid_urls = all (== "https://") (first8 ss)
  first8 = map (T.take 8)
  all_ints = maybe False (all (\n -> n >= 1 && n < 100)) maybeInts
  maybeInts = mapM (readMaybe . T.unpack) ss :: Maybe [Int]
  intoUrls = map ByUrl ss
  intoIds = maybe [] (map ById) (mapM (readMaybe . T.unpack) ss)

{- Time -}

findNextTime :: UTCTime -> DigestInterval -> UTCTime
{-# INLINEABLE findNextTime #-}
--  No 'digest_at' or 'digest_every' set? In 1.5 hour
--  No 'digest_at' but 'digest_every' is set? In the <number of seconds> to which
--      'digest_every' is set.
--  Both set? The closest instant in the future among the two instants determined
--      from both, respectively.
findNextTime now (DigestInterval Nothing Nothing) = addUTCTime 9000 now
findNextTime now (DigestInterval (Just xs) Nothing) = addUTCTime xs now
findNextTime now (DigestInterval mbxs (Just ts)) = case mbxs of
  Nothing ->
    if null times
      then next_day
      else still_today
  Just xs ->
    if xs > 86400
      then addUTCTime (xs - 86400) next_day
      else
        if null times
          then min (addUTCTime xs now) next_day
          else min (addUTCTime xs now) still_today
 where
  toNominalDifftime h m = realToFrac $ h * 3600 + m * 60
  from_midnight = realToFrac $ utctDayTime now
  times =
    foldl'
      ( \acc (!h, !m) ->
          let t = toNominalDifftime h m
           in if from_midnight < t then (t - from_midnight) : acc else acc
      )
      []
      ts
  (early_h, early_m) =
    foldl'
      ( \(!h, !m) (!h', !m') ->
          if h' < h || h == h' && m' < m
            then (h', m')
            else (h, m)
      )
      (23, 59)
      ts
  to_midnight = realToFrac $ 86400 - utctDayTime now
  until_midnight = addUTCTime to_midnight now
  next_day = addUTCTime (toNominalDifftime early_h early_m) until_midnight
  still_today = addUTCTime (minimum times) now

mbTime :: String -> Maybe UTCTime
{-# INLINE mbTime #-}
mbTime s = maybe second_pass pure $ iso8601ParseM s
 where
  second_pass = foldr step Nothing formats
  step f acc = maybe acc pure $ parseTimeM True defaultTimeLocale f s
  formats = [rfc822DateFormat, "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%d"]

averageInterval :: [UTCTime] -> Maybe NominalDiffTime
averageInterval [] = Nothing
averageInterval (x : xs) = go [] x (sort xs)
 where
  avg acc = realToFrac $ floor (sum $ map abs acc) `div` length acc :: NominalDiffTime
  go !acc _ [] = if null acc then Nothing else Just $ avg acc
  go !acc tn (tm : ts) =
    let diffed = diffUTCTime tn tm
     in go (diffed : acc) tm ts

renderAvgInterval :: Maybe NominalDiffTime -> T.Text
renderAvgInterval Nothing = "No average interval was computed."
renderAvgInterval (Just i) =
  let a = if i < 0 then -i else i
      hours = floor a `div` 3600 :: Integer
      (d, h) = go hours 0
   in (T.pack . show $ d) `T.append` " days, " `T.append` (T.pack . show $ h) `T.append` " hours."
 where
  go !hs !d = if hs < 24 then (d, hs) else go (hs - 24 :: Integer) (d + 1 :: Integer)

freshLastXDays :: Int -> UTCTime -> [Item] -> [Item]
freshLastXDays days now items =
  let x = fromIntegral $ days * 86400
      x_days_ago = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds now - x
   in filter (\i -> i_pubdate i > x_days_ago) items

nomDiffToReadable :: NominalDiffTime -> T.Text
nomDiffToReadable t =
  let (days, time) = timeToDaysAndTimeOfDay t
   in (T.pack . show $ days) `T.append` " day(s), " `T.append` (T.pack . show $ time) `T.append` " hour(s)"

utcToYmd :: UTCTime -> T.Text
utcToYmd = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

utcToYmdHMS :: UTCTime -> T.Text
utcToYmdHMS = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %T"

sortTimePairs :: [(Int, Int)] -> [(Int, Int)]
{-# INLINE sortTimePairs #-}
sortTimePairs = go []
 where
  go sorter [] = sorter
  go [] (p : ps) = go [p] ps
  go (s : ss) (p : ps) =
    let (h, m) = s
        (h', m') = p
     in if h' < h || h == h' && m' < m
          then go (p : s : ss) ps
          else go (s : p : ss) ps

scanTimeSlices :: [Int64] -> [Int64]
scanTimeSlices [] = []
scanTimeSlices (x : xs) = fst . foldl' step ([], x) $ xs
 where
  step (!acc, !x') y = (acc ++ [y - x'], y)

{- Settings -}

defaultChatSettings :: Settings
defaultChatSettings =
  Settings
    { settings_word_matches = WordMatches S.empty S.empty S.empty
    , settings_digest_size = 10
    , settings_digest_interval = DigestInterval (Just 86400) Nothing
    , settings_digest_title = "A new digest is available"
    , settings_paused = False
    , settings_disable_web_view = False
    , settings_pagination = False
    , settings_pin = False
    , settings_share_link = True
    , settings_forward_to_admins = False
    , settings_digest_collapse = Nothing
    , settings_digest_start = Nothing
    , settings_digest_no_collapse = mempty
    }

updateSettings :: [ParsingSettings] -> Settings -> Settings
{-# INLINE updateSettings #-}
updateSettings [] orig = orig
updateSettings parsed orig = foldl' (flip inject) orig parsed
 where
  inject p o = case p of
    PDigestCollapse v -> o{settings_digest_collapse = if v == 0 then Nothing else Just v}
    PDigestAt v ->
      let bi = settings_digest_interval o
          bi' = bi{digest_at = if null v then Nothing else Just v}
       in o{settings_digest_interval = bi'}
    PDigestEvery v ->
      let bi = settings_digest_interval o
          bi' = bi{digest_every_secs = if v == 0 then Nothing else Just v}
       in o{settings_digest_interval = bi'}
    PDigestSize v -> o{settings_digest_size = v}
    PDigestStart v -> o{settings_digest_start = Just v}
    PDigestTitle v -> o{settings_digest_title = v}
    PBlacklist v ->
      let wm = settings_word_matches o
          wm' = wm{match_blacklist = v}
       in o{settings_word_matches = wm'}
    PDisableWebview v -> o{settings_disable_web_view = v}
    PPaused v -> o{settings_paused = v}
    PPin v -> o{settings_pin = v}
    PSearchKws v ->
      let wm = settings_word_matches o
          wm' = wm{match_searchset = v}
       in o{settings_word_matches = wm'}
    PForwardToAdmins v -> o{settings_forward_to_admins = v}
    PSearchLinks v ->
      let wm = settings_word_matches o
          wm' = wm{match_only_search_results = v}
       in o{settings_word_matches = wm'}
    PShareLink v -> o{settings_share_link = v}
    PPagination v -> o{settings_pagination = v}
    PNoCollapse v -> o{settings_digest_no_collapse = v}

{- Items -}

sortItems :: Feed -> Feed
{-# INLINE sortItems #-}
sortItems f = f{f_items = take 30 . sortOn (Down . i_pubdate) $ f_items f}

{- Replies -}

sliceOnN :: T.Text -> Int -> [T.Text]
sliceOnN t n =
  let (body, rest) = foldl' step ([], mempty) $ T.lines t
   in body ++ [rest]
 where
  step (acc, l') l =
    let (done, rest) = try_unlining l' l
     in if rest == mempty
          then (acc, done)
          else (acc ++ [done], rest)
  try_unlining l_acc l_v =
    let unlined = T.unlines [l_acc, l_v]
     in if T.length unlined > n
          then (l_acc, l_v)
          else (unlined, mempty)

removeAllEmptyLines :: T.Text -> T.Text
removeAllEmptyLines = fst . T.foldl' step tuple_acc
 where
  tuple_acc = (mempty, 0) :: (T.Text, Int)
  step (acc, counter) val
    | val == '\n' && counter == 0 = (acc `T.append` T.singleton val, 1)
    | val == '\n' && counter == 1 = (acc, 0)
    | otherwise = (acc `T.append` T.singleton val, counter)

sliceIfAboveTelegramMax :: T.Text -> [T.Text]
sliceIfAboveTelegramMax msg
  | T.length msg <= 4096 = pure msg
  | otherwise = removeAllEmptyLines <$> sliceOnN msg 4096
