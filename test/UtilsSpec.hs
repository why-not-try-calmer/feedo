{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

import qualified Data.HashMap.Strict as HMS
import Data.Int (Int64)
import qualified Data.IntMap as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Digests
import Test.Hspec
import TgramOutJson (ChatId)
import Types (DigestInterval (DigestInterval), Feed (Feed, f_items, f_link), FeedType (Rss), Item (Item, i_feed_link, i_link, i_title), Settings (Settings, settings_word_matches), SubChat (SubChat), WordMatches (WordMatches), i_desc)
import Utils (
  defaultChatSettings,
  findNextTime,
  fromEither,
  maybeUserIdx,
  mbTime,
  partitionEither,
  scanTimeSlices,
  sortFeedsOnSettings,
 )

mockFeedsChats :: UTCTime -> ([T.Text], HMS.HashMap T.Text Feed, HMS.HashMap ChatId SubChat)
mockFeedsChats now =
  let i1 = Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now
      i2 = Item "Python item" "Until dawn we won't know what happened" "https://pyton.org/item" "https://python.org" now
      fl = map i_feed_link [i1, i2]
      f1 = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" [i1] Nothing Nothing
      f2 = Feed Rss "Python.org Description" "Python.org Title" "https://python.org" [i2] Nothing Nothing
      feeds = HMS.fromList $ map (\f -> (f_link f, f)) [f1, f2]
      word_b = WordMatches S.empty (S.fromList ["Target"]) (S.fromList ["https://hnrss.org/frontpage"])
      c =
        SubChat
          0
          Nothing
          Nothing
          Nothing
          (S.fromList fl)
          Nothing
          defaultChatSettings
            { settings_word_matches = word_b
            }
          HMS.empty
      chats = HMS.fromList [(0, c)] :: HMS.HashMap ChatId SubChat
   in (fl, feeds, chats)

spec :: Spec
spec = sequence_ [go, go1, go2, go3, go4, go5, go6, go7, go8]
 where
  go =
    let desc = describe "partitionEither"
        as = it "partitions a structure of Either's into a pair, not necessarily preserving the order"
        target = partitionEither [Left 1, Right 2, Left 3] `shouldBe` ([3, 1], [2])
     in desc $ as target
  go1 =
    let desc = describe "fromEither"
        as = it "extracts the wrapped value from an Either, or returns a default value"
        target = fromEither "default" (Left mempty :: Either T.Text T.Text) `shouldBe` "default"
     in desc $ as target
  go2 =
    let desc = describe "maybeUserIdx"
        as = it "extract the value from a foldable using a 'human friendly' index"
        target = maybeUserIdx [1, 2, 3, 4] 4 `shouldBe` (Just 4 :: Maybe Int)
     in desc $ as target
  go3 =
    let desc = describe "scanTimeSlice"
        as = it "find the distance between every pair of values in a list"
        target = scanTimeSlices [1, 3, 6, 10] `shouldBe` ([2, 3, 4] :: [Int64])
     in desc $ as target
  go4 =
    let desc = describe "mbTime"
        as = it "parses a datestring"
        target = do
          now <- getCurrentTime
          let times = ["Thu, 07 Apr 2022 11:23:32 +0000", "2022-02-17T00:01:00Z"]
          case mapM mbTime times of
            Just ts -> ts `shouldSatisfy` (\ts' -> all (< now) ts)
            Nothing -> undefined
     in desc $ as target
  go5 =
    let desc = describe "findNextTime"
        as = it "find the next time point from now given a set DigestInterval value"
        target = do
          let dig = DigestInterval (Just 21600) (Just [(2, 0), (23, 0)])
              d1 = fromJust $ mbTime "2022-02-17T00:01:00Z"
              d2 = fromJust $ mbTime "2022-02-17T18:00:00Z"
              t1 = findNextTime d1 dig
              t2 = findNextTime d2 dig
          t1 `shouldSatisfy` (\x -> diffUTCTime t1 x <= 7200)
          t2 `shouldSatisfy` (\x -> let v = diffUTCTime t2 x in v <= 18000)
     in desc $ as target
  go6 =
    let desc = describe "filter items"
        as = it "filters items in or out depending on textual occrrences"
        target = do
          now <- getCurrentTime
          let i1 = Item "HackerNews item" "Here is my target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now
              i2 = Item "Python item" "Until dawn we won't know what happened" "https://pyton.org/item" "https://python.org" now
              i3 = Item "my title" "something" "https://itmsmycountry.com/okok" "https://itmsmycountry.com" now
              onlyD = filter (\i -> i `has_keywords` ["is"]) [i1, i2, i3]
              exceptN = filter (\i -> i `lacks_keywords` ["my"]) [i1, i2, i3]
          onlyD `shouldBe` [i1]
          exceptN `shouldBe` [i2]
     in desc $ as target
   where
    has_keywords i = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` t) [i_desc i, i_link i, i_title i])
    lacks_keywords i kws = not $ has_keywords i kws
  go7 =
    let desc = describe "findNext time #2"
        as = it "map the given date to the next determined by the given time interval"
        target = do
          let datestring = "2022-06-04T00:01:00Z" :: String
              t0 = fromJust $ mbTime datestring
              interval = DigestInterval (Just 172800) (Just [(0, 1)])
              t1 = findNextTime t0 interval
          t1 `shouldSatisfy` (\t -> diffUTCTime t t0 >= 172800)
     in desc $ as target
  go8 =
    let desc = describe "Validates reply from settings x notification payload with preferred order"
        as = it "makes sure that preferred order is respected"
        target = do
          now <- getCurrentTime
          let url1 = "https://specially1.protected.org"
              url2 = "https://notspecially2.andunprotected.org"
              url3 = "https://specially3.protected.org"
              url4 = "https://notspecially4.andunprotected.org"
              matches = WordMatches S.empty S.empty S.empty
              scope = S.empty
              preferred_order = Just $ M.fromList [(1, url2), (4, url3), (3, url4), (2, url1)]
              settings = Settings (Just 3) (DigestInterval Nothing Nothing) 10 Nothing "title" False False preferred_order False False False False matches scope
              items1 = map (\i -> let n = show i in Item ("protected_title" `T.append` T.pack n) "protected_desc" "protected_link" url1 now) [1 .. 10]
              items2 = map (\i -> let n = show i in Item ("unprotected_title" `T.append` T.pack n) "unprotected_desc" "unprotected_link" url2 now) [1 .. 10]
              items3 = map (\i -> let n = show i in Item ("protected_title" `T.append` T.pack n) "protected_desc" "protected_link" url3 now) [1 .. 10]
              items4 = map (\i -> let n = show i in Item ("unprotected_title" `T.append` T.pack n) "unprotected_desc" "unprotected_link" url4 now) [1 .. 10]
              feeds =
                let one = Feed Rss "desc" "title" url1 items1 Nothing Nothing
                    two = Feed Rss "desc" "title" url2 items2 Nothing Nothing
                    three = Feed Rss "desc" "title" url3 items3 Nothing Nothing
                    four = Feed Rss "desc" "title" url4 items4 Nothing Nothing
                 in [one, two, three, four]
              reordered_feeds = sortFeedsOnSettings settings feeds
          reordered_feeds `shouldSatisfy` (\fs -> (f_link . head $ fs) == url2 && (f_link . last $ fs) == url3)
     in desc $ as target
