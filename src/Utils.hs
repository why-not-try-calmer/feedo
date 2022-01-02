module Utils where

import AppTypes
import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime (utctDayTime), addUTCTime)
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
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

findNextTime :: BatchInterval -> UTCTime -> Maybe UTCTime 
findNextTime (Secs xs) now = Just $ addUTCTime xs now
findNextTime (HM ts) now =
    let from_midnight = realToFrac $ utctDayTime now
        times = foldl' (\acc (!h, !m) ->
            let t = realToFrac $ h * 3600 + m * 60
            in  if from_midnight < t then (t - from_midnight):acc else acc
            ) [] ts
    in  if null times then Nothing
        else Just $ addUTCTime (minimum times) now

tooManySubs :: Int -> SubChats -> ChatId -> Bool
tooManySubs upper_bound chats cid = case HMS.lookup cid chats of
    Nothing -> False
    Just chat ->
        let diff = upper_bound - length (sub_feeds_links chat)
        in  diff < 0

{- Update settings -}

parseUpdateSettings :: [T.Text] -> Maybe ParsedChatSettings
parseUpdateSettings [] = Nothing
parseUpdateSettings lns = Map.fromList <$> foldr step Nothing lns
    where
    step l acc =
        let (k:ss) = 
                T.splitOn ":" . 
                T.filter (not . isSpace) $ l
        in  if null ss then Nothing else case acc of
            Nothing -> Just [(k, last ss)]
            Just p -> Just $ (k, last ss):p