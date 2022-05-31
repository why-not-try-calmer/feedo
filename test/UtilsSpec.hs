{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

import AppTypes (Batch (Digests, Follows), BatchRecipe (DigestFeedLinks), DigestInterval (DigestInterval), Feed (Feed, f_items, f_link), FeedType (Rss), Item (Item, i_feed_link, i_link, i_title), Settings (settings_word_matches), SubChat (SubChat), WordMatches (WordMatches), i_desc)
import qualified Data.HashMap.Strict as HMS
import Data.Int (Int64)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, readTime)
import Notifications
import Test.Hspec
import TgramOutJson (ChatId)
import Utils
  ( defaultChatSettings,
    fromEither,
    maybeUserIdx,
    mbTime,
    partitionEither,
    scanTimeSlices,
  )

mockFeedsChats :: UTCTime -> ([T.Text], HMS.HashMap T.Text Feed, HMS.HashMap ChatId (SubChat, BatchRecipe))
mockFeedsChats now =
    let i1 = Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now
        i2 = Item "Python item" "Until dawn we won't know what happened" "https://pyton.org/item" "https://python.org" now
        fl = map i_feed_link [i1, i2]
        f1 = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" [i1] Nothing Nothing 0
        f2 = Feed Rss "Python.org Description" "Python.org Title" "https://python.org" [i2] Nothing Nothing 0
        feeds = HMS.fromList $ map (\f -> (f_link f, f)) [f1, f2]
        dg = DigestFeedLinks fl
        word_b = WordMatches S.empty (S.fromList ["Target"]) (S.fromList ["https://hnrss.org/frontpage"])
        c = SubChat 0 Nothing Nothing (S.fromList fl) Nothing defaultChatSettings {
                settings_word_matches = word_b
            }
        chats = HMS.fromList [(0, (c, dg))] :: HMS.HashMap ChatId (SubChat, BatchRecipe)
    in  (fl, feeds, chats)

spec :: Spec
spec = sequence_ [go, go1, go2, go3, go4, go5, go6, go7, go8]
    where
        go =
            let desc = describe "partitionEither"
                as = it "partitions a structure of Either's into a pair, not necessarily preserving the order"
                target = partitionEither [Left 1, Right 2, Left 3] `shouldBe` ([3,1], [2])
            in  desc $ as target
        go1 =
            let desc = describe "fromEither"
                as = it "extracts the wrapped value from an Either, or returns a default value"
                target = fromEither "default" (Left mempty :: Either T.Text T.Text) `shouldBe` "default"
            in  desc $ as target
        go2 =
            let desc = describe "maybeUserIdx"
                as = it "extract the value from a foldable using a 'human friendly' index"
                target = maybeUserIdx [1,2,3,4] 4 `shouldBe` (Just 4 :: Maybe Int)
            in  desc $ as target
        go3 =
            let desc = describe "scanTimeSlice"
                as = it "find the distance between every pair of values in a list"
                target = scanTimeSlices [1,3,6,10] `shouldBe` ([2,3,4] :: [Int64])
            in  desc $ as target
        go4 =
            let desc = describe "mbTime"
                as = it "parses a datestring"
                target = do
                    now <- getCurrentTime
                    let times = ["Thu, 07 Apr 2022 11:23:32 +0000", "2022-02-17T00:01:00Z"]
                    case mapM mbTime times of
                        Just ts -> ts `shouldSatisfy` (\ts' -> all (< now) ts)
                        Nothing -> undefined
            in  desc $ as target
        go5 =
            let desc = describe "findNextTime"
                as = it "find the next time point from now given a set DigestInterval value"
                target = do
                    let dig = DigestInterval (Just 21600) (Just [(2,0), (23,0)])
                        d1 = fromJust $ mbTime "2022-02-17T00:01:00Z"
                        d2 = fromJust $ mbTime "2022-02-17T18:00:00Z"
                        t1 = findNextTime d1 dig
                        t2 = findNextTime d2 dig
                    t1 `shouldSatisfy` (\x -> diffUTCTime t1 x <= 7200)
                    t2 `shouldSatisfy` (\x -> let v = diffUTCTime t2 x in v <= 18000)
            in  desc $ as target
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
            in  desc $ as target
            where
                has_keywords i = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` t) [i_desc i, i_link i, i_title i])
                lacks_keywords i kws = not $ has_keywords i kws
        go7 =
            let desc = describe "notifFrom"
                as = it "filters items according to chat settings, and prepares notifications accordingly"
                target = do
                    now <- getCurrentTime
                    let (fl, known_feeds, hmap) = mockFeedsChats now
                    (label, val) <- case HMS.lookup 0 (notifFrom (Just now) fl known_feeds hmap) of
                        Nothing -> undefined
                        Just (_, b) -> case b of
                            Digests fs -> pure ("Digests", fs)
                            Follows fs' -> pure ("Follow", fs')
                    (label, val) `shouldSatisfy` (\(l, v) -> l == "Digests" && length v == 2)
            in  desc $ as target
        go8 =
            let desc = describe "keepNew"
                as = it "ensures that only genuinely new items are posted even when i_pubdate could not be set"
                target = do
                    now <- getCurrentTime
                    let (_, new_feeds, chats) = mockFeedsChats now
                        duplicate_i = Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now
                        new_i = Item "Python item" "Until dawn we won't know what happened" "new url" "new url" now
                        feed_with_duplicate = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" [duplicate_i] Nothing Nothing 0
                        updated_new_feeds = HMS.update (\f -> Just $ f { f_items = f_items f ++ [new_i]}) "https://python.org" new_feeds
                        (_, compared) = keepNew updated_new_feeds [feed_with_duplicate]
                        expected_compared = HMS.delete "https://hnrss.org/frontpage" updated_new_feeds
                    compared `shouldBe` expected_compared
            in  desc $ as target