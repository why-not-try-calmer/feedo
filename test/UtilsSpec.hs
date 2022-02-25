
module UtilsSpec where

import Test.Hspec
import Utils (partitionEither, fromEither, maybeUserIdx, scanTimeSlices, findNextTime, mbTime)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (getCurrentTime, readTime, diffUTCTime)
import AppTypes (DigestInterval(DigestInterval), Item (Item, i_link, i_title), i_desc)
import Data.Maybe

spec :: Spec
spec = sequence_ [go, go1, go2, go3, go4, go5, go6]
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
                    let i1 = Item "my title" "A new way of Russian nationalism is lashing out all over Western Europe" "https://itmsmycountry.com/okok" "https://itmsmycountry.com" now
                        i2 = Item "my title" "Until dawn we won't know what happened" "https://itmsmycountry.com/okok" "https://itmsmycountry.com" now
                        i3 = Item "my title" "something" "https://itmsmycountry.com/okok" "https://itmsmycountry.com" now
                        onlyD = filter (\i -> i `has_keywords` ["Dawn"]) [i1, i2, i3]
                        exceptN = filter (\i -> i `lacks_keywords` ["nationalisM"]) [i1, i2, i3]
                    onlyD `shouldBe` [i2]
                    exceptN `shouldBe` [i2, i3] 
            in  desc $ as target
            where
                has_keywords i kws = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` t) [i_desc i, i_link i, i_title i]) kws
                lacks_keywords i kws = not $ has_keywords i kws