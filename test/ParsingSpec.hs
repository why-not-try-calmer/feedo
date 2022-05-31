{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import AppTypes (BatchRecipe (DigestFeedLinks), DigestInterval (DigestInterval), Feed (f_link, f_items, f_title), ParsingSettings (PDigestAt), Settings (Settings), SubChat (SubChat, sub_chatid), WordMatches (WordMatches), Batch (Digests))
import Control.Concurrent.Async (mapConcurrently)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Network.HTTP.Req (renderUrl)
import Notifications (notifFrom)
import Parsing (eitherUrlScheme, parseSettings, rebuildFeed)
import Test.Hspec
import Utils (partitionEither)
import Data.Time (getCurrentTime)

spec :: Spec
spec = go >> go1 >> go2 >> go3
    where
        go =
            let desc = describe "parseSettings"
                as = it "parse settings sent over Telegram in view of updating a Settings value"
                target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8,0)]]
            in  desc $ as target
        go1 =
            let desc = describe "eitherUrlScheme"
                as = it "parse url sent over Telegram in view of fetching a web feed"
                target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
                    Left _ -> undefined
                    Right res ->
                        let s = renderUrl res
                        in  s `shouldSatisfy` (not . T.null)
            in  desc $ as target
        go2 =
            let desc = describe "rebuildFeed"
                as = it "fetches and parses the given web feeds"
                target = do
                    let feeds = ["https://hnrss.org/frontpage", "https://planetpython.org/rss20.xml", "https://talkpython.fm/episodes/rss", "https://www.blog.pythonlibrary.org/feed"]
                    res <- mapConcurrently rebuildFeed feeds
                    let (failed, done) = partitionEither res
                    length done `shouldSatisfy` (> length failed)
            in  desc $ as target
        go3 =
            let desc = describe "test digests"
                as = it "creates a digest"
                target = do
                    let feedlinks= ["https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
                        settings = Settings Nothing (DigestInterval Nothing Nothing) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty)
                        chat = SubChat 123 Nothing Nothing (S.fromList feedlinks) Nothing settings
                        chats_recipes = HMS.singleton 123 (chat, DigestFeedLinks feedlinks)
                    (failed, done) <- mapConcurrently rebuildFeed feedlinks <&> partitionEither
                    now <- getCurrentTime
                    let feedsmap = HMS.fromList . map (\f -> (f_link f, f)) $ done
                        notifs = notifFrom (Just now) feedlinks feedsmap chats_recipes
                        only_feeds = HMS.map (\(_, Digests ds) -> foldMap (\f -> [
                            f_title f,
                            f_link f, 
                            T.pack . show . length $ f_items f
                            ]) ds) notifs
                    print only_feeds
                    notifs `shouldSatisfy` (not . null . HMS.toList)
            in  desc $ as target