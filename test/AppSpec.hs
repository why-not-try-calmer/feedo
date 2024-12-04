{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Control.Concurrent (threadDelay, writeChan)
import Control.Exception (bracket, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Time
import Database.MongoDB
import Database.Redis (quit, runRedis)
import Hooks
import Jobs
import Mongo (HasMongo, withDb)
import Server
import System.Environment
import Test.Hspec
import Types (AppConfig (..), Feed (Feed), FeedType (Rss), Item (Item), Job (JobArchive), postjobs, runApp)

spec :: Spec
spec = withHooks [go]
 where
  go env =
    let desc = describe "startup"
        as = it "makes sure that the application completes startup as expected"
        target = do
          runApp env startJobs
          now <- getCurrentTime
          let jobs = postjobs env
              items = [Item "Nice Item" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now]
              feed = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" items Nothing Nothing
          writeChan jobs $ JobArchive [feed] now
          threadDelay 5000000
          docs <- runApp env $ withDb $ find (select ["i_title" =: ("Nice Item" :: T.Text)] "items") >>= rest
          docs `shouldSatisfy` (\(Right docs) -> not . null $ docs)
     in desc $ as target
