{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Hooks (withHooks)
import Network.HTTP.Req (renderUrl)
import Parsing (eitherUrlScheme, parseSettings, rebuildFeed)
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types
import Utils (defaultChatSettings, mbTime, partitionEither)

spec :: Spec
spec = withHooks [go, go1, go2, go3]
 where
  go _ =
    let desc = describe "parseSettings (1/2)"
        as = it "parse settings sent over Telegram in view of updating a Settings value (1/2)"
        target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8, 0)]]
     in desc $ as target
  go1 _ =
    let desc = describe "parseSettings (2/2)"
        as = it "parse settings sent over Telegram in view of updating a Settings value (2/2)"
        target = do
          parseSettings ["forward_to_admins:"] `shouldSatisfy` (\case (Left _) -> True; _ -> False)
          parseSettings ["forward_to_admins: true"] `shouldSatisfy` (\case (Right [PForwardToAdmins True]) -> True; _ -> False)
          parseSettings ["forward_to_admins: false"] `shouldSatisfy` (\case (Right [PForwardToAdmins False]) -> True; _ -> False)
     in desc $ as target
  go2 _ =
    let desc = describe "eitherUrlScheme"
        as = it "parse url sent over Telegram in view of fetching a web feed"
        target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
          Left _ -> undefined
          Right res ->
            let s = renderUrl res
             in s `shouldSatisfy` (not . T.null)
     in desc $ as target
  go3 env =
    let desc = describe "rebuildFeed"
        as = it "fetches and parses the given web feeds"
        target = do
          let feeds = ["https://hnrss.org/frontpage", "https://planetpython.org/rss20.xml", "https://talkpython.fm/episodes/rss", "https://www.blog.pythonlibrary.org/feed"]
          res <- mapConcurrently rebuildFeed feeds
          let (failed, done) = partitionEither res
          length done `shouldSatisfy` (> length failed)
     in desc $ as target
