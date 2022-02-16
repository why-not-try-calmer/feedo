
module UtilsSpec where

import Test.Hspec
import Utils (partitionEither, fromEither, maybeUserIdx, scanTimeSlices)
import qualified Data.Text as T
import Data.Int (Int64)

spec :: Spec
spec = go >> go1 >> go2 >> go3
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
        