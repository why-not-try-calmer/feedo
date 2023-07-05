{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.HTTP.Req (renderUrl)
import Notifications (collectDue, postNotifier, preNotifier)
import Parsing (eitherUrlScheme, parseSettings, rebuildFeed)
import Test.Hspec
import Types (Batch (Digests, Follows), BatchRecipe (DigestFeedLinks), DigestInterval (DigestInterval), Feed (f_desc, f_items, f_link, f_title, f_type), Item (i_desc, i_feed_link, i_link, i_pubdate), Notifier (Post, Pre, batch_recipes, batches, discarded_items_links), ParsingSettings (PDigestAt), Settings (Settings, settings_digest_interval), SubChat (SubChat, sub_chatid), WordMatches (WordMatches), i_title)
import Utils (defaultChatSettings, mbTime, partitionEither)

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go >> go1 >> runIO getConns >>= \env -> go2 env
  where
    go =
        let desc = describe "parseSettings"
            as = it "parse settings sent over Telegram in view of updating a Settings value"
            target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8, 0)]]
         in desc $ as target
    go1 =
        let desc = describe "eitherUrlScheme"
            as = it "parse url sent over Telegram in view of fetching a web feed"
            target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
                Left _ -> undefined
                Right res ->
                    let s = renderUrl res
                     in s `shouldSatisfy` (not . T.null)
         in desc $ as target
    go2 env =
        let desc = describe "rebuildFeed"
            as = it "fetches and parses the given web feeds"
            target = do
                let feeds = ["https://hnrss.org/frontpage", "https://planetpython.org/rss20.xml", "https://talkpython.fm/episodes/rss", "https://www.blog.pythonlibrary.org/feed"]
                res <- mapConcurrently (rebuildFeed feeds env)
                let (failed, done) = partitionEither res
                length done `shouldSatisfy` (> length failed)
         in desc $ as target