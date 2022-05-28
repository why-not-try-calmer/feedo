{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import AppTypes (Batch (Digests), BatchRecipe (DigestFeedLinks), DigestInterval (DigestInterval), Feed (f_items, f_link, f_title, f_type, f_desc), ParsingSettings (PDigestAt), Settings (Settings, settings_digest_interval), SubChat (SubChat, sub_chatid), WordMatches (WordMatches), Item (i_pubdate, i_desc), i_title)
import Control.Concurrent.Async (mapConcurrently)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.HTTP.Req (renderUrl)
import Notifications (notifFrom)
import Parsing (eitherUrlScheme, parseSettings, rebuildFeed)
import Test.Hspec
import Utils (partitionEither, defaultChatSettings, mbTime)

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
                        -- every day
                        settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty)
                        -- last 31th of May
                        chat = SubChat 123 (mbTime "2022-05-31") Nothing (S.fromList feedlinks) Nothing settings
                        chats_recipes = HMS.singleton 123 (chat, DigestFeedLinks feedlinks)
                    (failed, done) <- mapConcurrently rebuildFeed feedlinks <&> partitionEither
                    now <- getCurrentTime
                    let feedsmap = HMS.fromList . map (\f -> (f_link f, f)) $ done
                        notifs = notifFrom (Just now) feedlinks feedsmap chats_recipes
                        only_feeds = HMS.map (\(_, Digests ds) -> foldMap (\f -> [
                            f_title f,
                            f_link f,
                            T.pack . show . f_type $ f,
                            T.pack . show . length $ f_items f,
                            "Faulty (default) pubdates: " `T.append` (faulty . f_items $ f),
                            "Proof: " `T.append` T.intercalate "," (map (T.pack . show . i_pubdate) $ f_items f),
                            T.intercalate "," $ map i_title $ f_items f
                            ]) ds) notifs
                    print only_feeds
                    notifs `shouldSatisfy` (not . null . HMS.toList)
            in  desc $ as target
            where
                faulty [] = "No item found!"
                faulty [i] = "Only one item found, cannot check if faulty."
                faulty (i:is) = 
                    let initial = i_pubdate i
                        final = i_pubdate . last $ is 
                    in  if length is == 1 && initial == final then "2"
                        else T.pack . show . length $ filter (\i -> i_pubdate i == initial) is