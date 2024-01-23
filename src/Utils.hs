module Utils where

import qualified Data.HashMap.Strict as HMS
import Data.Int (Int64)
import Data.List (foldl', sort, sortOn)
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, parseTimeM, rfc822DateFormat, timeToDaysAndTimeOfDay)
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

readBatchRecipe :: BatchRecipe -> [FeedLink]
readBatchRecipe (FollowFeedLinks ls) = ls
readBatchRecipe (DigestFeedLinks ls) = ls

mkBatch :: BatchRecipe -> [Feed] -> Batch
mkBatch (FollowFeedLinks _) ls = Follows ls
mkBatch (DigestFeedLinks _) ls = Digests ls

unFeedRef :: FeedRef -> T.Text
unFeedRef (ByUrl s) = s
unFeedRef (ById s) = T.pack $ show s

unFeedRefs :: [FeedRef] -> [T.Text]
unFeedRefs = map unFeedRef

toFeedRef :: [T.Text] -> Either Error [FeedRef]
{-# INLINE toFeedRef #-}
toFeedRef ss
  | all_valid_urls = Right intoUrls
  | all_ints = Right intoIds
  | otherwise = Left . BadRef . T.concat $ ss
 where
  all_valid_urls = all (== "https://") (first8 ss)
  first8 = map (T.take 8)
  all_ints = maybe False (all (\n -> n >= 1 && n < 100)) maybeInts
  maybeInts = mapM (readMaybe . T.unpack) ss :: Maybe [Int]
  intoUrls = map ByUrl ss
  intoIds = maybe [] (map ById) (mapM (readMaybe . T.unpack) ss)

{- Errors -}

renderUserError :: Error -> T.Text
renderUserError (BadInput t) = T.append "I don't know what to do with this input: " t
renderUserError (BadFeed feederror) = T.append "Unable to fetch this feed: " (T.pack . show $ feederror)
renderUserError (BadFeedUrl t) = T.append "No feed could be found at this address: " t
renderUserError (NotAdmin _) = "Unable to perform this action, as it's reserved to admins in this chat."
renderUserError (MaxFeedsAlready _) = "This chat has reached the limit of subscriptions (10)"
renderUserError (ParseError input) = T.append "Parsing this input failed: " input
renderUserError (UpdateError err) = T.append "Unable to update, because of this error: " err
renderUserError (NotFoundFeed feed) = T.append "The feed you were looking for does not exist: " feed
renderUserError NotFoundChat = "The chat you called from is not subscribed to any feed yet."
renderUserError (BadRef contents) = T.append "References to web feeds must be either single digits or full-blown urls starting with 'https://', but you sent this: " contents
renderUserError NotSubscribed = "The feed your were looking for could not be found. Make sure you are subscribed to it."
renderUserError (TelegramErr err) = "Telegram responded with an error: " `T.append` err
renderUserError (Ignore input) = "Ignoring " `T.append` input
renderUserError ChatNotPrivate = "Unwilling to share authentication credentials in a non-private chat. Please use this command in a private conversation with to the bot."
renderUserError UserNotAdmin = "Only admins can change settings."

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError FaultyToken = "Login failed. This token is not valid, and perhaps never was."
renderDbError (FailedToUpdate items reason) = "Unable to update the following items: " `T.append` items `T.append` ". Reason: " `T.append` reason
renderDbError (NotFound item) = "Resource could not be found from the network: " `T.append` item
renderDbError FailedToLog = "Failed to log."
renderDbError FailedToLoadFeeds = "Failed to load feeds!"
renderDbError (BadQuery txt) = T.append "Bad query parameters: " txt
renderDbError FailedToSaveDigest = "Unable to save this digest. The database didn't return a valid identifier."
renderDbError FailedToProduceValidId = "Db was unable to return a valid identifier"
renderDbError FailedToInsertPage = "Db was unable to insert these pages."
renderDbError FailedToGetAllPages = "Db was unable to retrieve all pages."

{- Time -}

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
    , settings_follow = False
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
    PSearchLinks v ->
      let wm = settings_word_matches o
          wm' = wm{match_only_search_results = v}
       in o{settings_word_matches = wm'}
    PShareLink v -> o{settings_share_link = v}
    PFollow v -> o{settings_follow = v}
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
