{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import Test.Hspec
import Parsing (parseSettings, eitherUrlScheme, rebuildFeed)
import AppTypes (DigestInterval(DigestInterval), ParsingSettings (PDigestAt))
import qualified Data.Text as T
import Network.HTTP.Req (renderUrl)
import Utils (partitionEither)
import Control.Concurrent.Async (mapConcurrently)

spec :: Spec
spec = go >> go1 >> go2
    where
        go =
            let desc as = describe "parseSettings" as
                as func = it "parse settings sent over Telegram in view of updating a Settings value" func
                target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8,0)]]
            in  desc $ as target
        go1 =
            let desc as = describe "eitherUrlScheme" as
                as func = it "parse url sent over Telegram in view of fetching a web feed" func
                target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
                    Left _ -> undefined
                    Right res -> 
                        let s = renderUrl res
                        in  s `shouldSatisfy` (not . T.null)
            in  desc $ as target
        go2 =
            let desc as = describe "rebuildFeed" as
                as func = it "fetches and parses the given web feeds" func
                target = do
                    let feeds = ["https://hnrss.org/frontpage", "https://planetpython.org/rss20.xml", "https://talkpython.fm/episodes/rss", "https://www.blog.pythonlibrary.org/feed"]
                    res <- mapConcurrently rebuildFeed feeds
                    let (failed, done) = partitionEither res
                    print failed >> print done
                    length done `shouldSatisfy` (> length failed)
            in  desc $ as target