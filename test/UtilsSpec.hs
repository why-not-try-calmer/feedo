
module UtilsSpec where

import Test.Hspec
import Utils (partitionEither, fromEither, maybeUserIdx, scanTimeSlices, findNextTime, mbTime)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (getCurrentTime, readTime, diffUTCTime)
import AppTypes (DigestInterval(DigestInterval))
import Data.Maybe

spec :: Spec
spec = sequence_ [go, go1, go2, go3, go4, go5]
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