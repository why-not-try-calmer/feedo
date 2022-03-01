
module UtilsSpec where

import Test.Hspec
import Utils (partitionEither, fromEither, maybeUserIdx, scanTimeSlices, findNextTime, mbTime, defaultChatSettings, notifFrom)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (getCurrentTime, readTime, diffUTCTime)
import AppTypes (DigestInterval(DigestInterval), Item (Item, i_link, i_title, i_feed_link), i_desc, Feed (f_link, Feed), BatchRecipe (DigestFeedLinks), FeedType (Rss), SubChat (SubChat), Batch (Digests, Follows), Settings (settings_word_matches), WordMatches (WordMatches))
import Data.Maybe
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import TgramOutJson (ChatId)

spec :: Spec
spec = sequence_ [go, go1, go2, go3, go4, go5, go6, go7]
    where
        go =
            let desc as = describe "partitionEither" as
                as func = it "partitions a structure of Either's into a pair, preserving the order" func
                target = partitionEither [Left 1, Right 2, Left 3] `shouldBe` ([1,3], [2])
            in  desc $ as target
        go1 =
            let desc as = describe "fromEither" as
                as func = it "extracts the wrapped value from an Either, or returns a default value" func
                target = fromEither "default" (Left mempty :: Either T.Text T.Text) `shouldBe` "default"
            in  desc $ as target
        go2 =
            let desc as = describe "maybeUserIdx" as
                as func = it "extract the value from a foldable using a 'human friendly' index" func
                target = maybeUserIdx [1,2,3,4] 4 `shouldBe` (Just 4 :: Maybe Int)
            in  desc $ as target
        go3 =
            let desc as = describe "scanTimeSlice" as
                as func = it "find the distance between every pair of values in a list" func
                target = scanTimeSlices [1,3,6,10] `shouldBe` ([2,3,4] :: [Int64])
            in  desc $ as target
        go4 =
            let desc as = describe "mbTime" as
                as func = it "parses a datestring" func
                target = do
                    now <- getCurrentTime
                    let t = fromJust $ mbTime "2022-02-17T00:01:00Z"
                    t `shouldSatisfy` (< now)
            in  desc $ as target
        go5 =
            let desc as = describe "findNextTime" as
                as func = it "find the next time point from now given a set DigestInterval value" func
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
            let desc as = describe "filter items" as
                as func = it "filters items in or out depending on textual occrrences" func
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
                has_keywords i kws = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` t) [i_desc i, i_link i, i_title i]) kws
                lacks_keywords i kws = not $ has_keywords i kws
        go7 = 
            let desc as = describe "notifFrom" as
                as func = it "filter items according to chat settings, and prepares notifications accordingly" func
                target = do
                    now <- getCurrentTime
                    let i1 = Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now
                        i2 = Item "Python item" "Until dawn we won't know what happened" "https://pyton.org/item" "https://python.org" now
                        fl = map i_feed_link [i1, i2]
                        f1 = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" [i1] Nothing Nothing 0
                        f2 = Feed Rss "Python.org Description" "Python.org Title" "https://python.org" [i2] Nothing Nothing 0
                        known_feeds = HMS.fromList $ map (\f -> (f_link f, f)) [f1, f2]
                        dg = DigestFeedLinks fl
                        word_b = WordMatches S.empty (S.fromList ["Target"]) (S.fromList ["https://hnrss.org/frontpage"])
                        c = SubChat 0 Nothing Nothing (S.fromList fl) defaultChatSettings { 
                                settings_word_matches = word_b
                            }
                        hmap = HMS.fromList [(0, (c, dg))] :: HMS.HashMap ChatId (SubChat, BatchRecipe)
                    (label, val) <- case HMS.lookup 0 (notifFrom fl known_feeds hmap) of 
                        Nothing -> undefined
                        Just (_, b) -> case b of 
                            Digests fs -> pure ("Digests", fs)
                            Follows fs' -> pure ("Follow", fs')
                    (label, val) `shouldSatisfy` (\(l, v) -> l == "Digests" && length v == 2)
            in  desc $ as target