{-# LANGUAGE FlexibleContexts #-}

module BrokerSpec where

import AppServer (makeConfig)
import AppTypes
import Broker
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Functor ((<&>))
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Time
import Database.MongoDB
import Database.Redis (keys, runRedis)
import Mongo
import Redis
import System.Environment (getEnvironment)
import Test.Hspec

spec :: Spec
spec = pre >>= \(env, feeds) -> go env >> go1 env feeds
  where
    pre = runIO $ do
        env <- getEnvironment
        (config, _) <- makeConfig env
        res <- evalDb config GetAllFeeds
        case res of
            DbFeeds feeds -> pure (config, map f_link feeds)
            DbNoFeed -> putStrLn "No feed found!" >> pure (config, [])
            DbErr e -> print e >> pure (config, [])
            _ -> undefined
    go env =
        let desc = describe "withCache: Warmup"
            as = it "returns from the cache an item if found there, or updates the cache with the db and returns it afterwards"
            target = do
                res <- runApp env $ withCache CacheWarmup
                print res
                res `shouldSatisfy` (\case Right res -> res == CacheOk; _ -> undefined)
         in desc $ as target
    go1 env feeds =
        let desc = describe "withCache: GetFeeds"
            as = it "tries to obtain the feeds from the cache, otherwise turns to the db"
            target = do
                res <- runApp env $ withCache $ CachePullFeeds [head feeds]
                print res
                case res of
                    Right (CacheFeeds fs) -> length fs `shouldBe` 1
                    res -> print res >> undefined
         in desc $ as target