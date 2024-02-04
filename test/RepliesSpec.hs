{-# LANGUAGE OverloadedStrings #-}

module RepliesSpec where

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Replies (mkReply)
import Test.Hspec
import Types

spec :: Spec
spec = go
 where
  go =
    let desc = describe "Validates reply from settings x notification payload"
        as = it "makes sure that no_collapse is respected"
        target = do
          now <- getCurrentTime
          let protected = "https://specially.protected.org"
              unprotected = "https://notspecially.andunprotected.org"
              matches = WordMatches S.empty S.empty S.empty
              settings = Settings (Just 3) (DigestInterval Nothing Nothing) 10 Nothing "title" False False False False False False False matches (S.singleton protected)
              protected_items = map (\i -> let n = show i in Item ("protected_title" `T.append` T.pack n) "protected_desc" "protected_link" protected now) [1 .. 10]
              unprotected_items = map (\i -> let n = show i in Item ("unprotected_title" `T.append` T.pack n) "unprotected_desc" "unprotected_link" unprotected now) [1 .. 10]
              feeds =
                let one = Feed Rss "desc" "title" protected protected_items Nothing Nothing
                    two = Feed Rss "desc" "title" unprotected unprotected_items Nothing Nothing
                 in [one, two]
              digest = FromDigest feeds Nothing settings
              reply = mkReply digest
          reply `shouldSatisfy` (\(ChatReply contents _ _ _ _ _) -> T.length contents > 0)
     in desc $ as target
