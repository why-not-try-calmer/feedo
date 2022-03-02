{-# LANGUAGE FlexibleContexts #-}
module BrokerSpec where

import AppTypes
import Broker
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import Data.Time
import Database.MongoDB
import Mongo
import System.Environment (getEnvironment)
import Test.Hspec
import Data.Functor ((<&>))
import qualified Data.Text as T
import Database.Redis (runRedis, keys)
import AppServer (makeConfig)
import Control.Monad.Reader (MonadReader)
import qualified Data.HashMap.Internal.Strict as HMS
import Redis

{-
fillMongo :: (MonadIO m, HasMongo m) => AppConfig -> [Feed] -> m ()
fillMongo env feeds =
    let selector = map (\f -> (["f_link" =: f_link f], writeDoc f, [Upsert])) feeds
        action = withMongo env $ updateAll "feeds" selector
    in  action >>= \case
        Left err -> liftIO $ print "Failed to fillMongo."
        Right res -> liftIO $ do
            if failed res then print "Failed to fillMongo with" >> print feeds
            else print "Successfully filled Mongo."

mockFeeds :: IO [Feed]
mockFeeds = do
    now <- getCurrentTime
    let item = Item "A" "Very" "Nice" "Title" now
        f1 = Feed Rss "url1" "We" "Love" [item] Nothing (Just now) 0
        f2 = Feed Rss "url2" "Nothing" "Beats" [item] Nothing (Just now) 0
    pure [f1, f2]
-}

-- to make the test preparation work
instance HasRedis IO where
    withRedis = withRedis'

spec :: Spec
spec = pre >>= \(env, feeds) -> go env >> go1 env feeds where
    pre = runIO $ do
        env <- getEnvironment
        (config, _) <- makeConfig env
        res <- evalDb config GetAllFeeds 
        case res of
            DbFeeds feeds -> pure (config, map f_link feeds)
            _ ->  undefined
    go env =
        let desc as = describe "withCache: Warmup" as
            as func = it "returns from the cache an item if found there, or updates the cache with the db and returns it afterwards" func
            target = do
                res <- runApp env $ withBroker CacheWarmup
                print res
                res `shouldSatisfy` (\case Right res -> res == CacheOk; _ -> undefined)
        in  desc $ as target
    go1 env feeds =
        let desc as = describe "withCache: GetFeeds" as
            as func = it "tries to obtain the feeds from the cache, otherwise turns to the db" func
            target = do
                res <- runApp env $ withBroker $ CachePullFeeds [head feeds]
                print res
                case res of
                    Right (CacheFeeds fs) -> do
                        length fs `shouldBe` 1
                    res -> print res >> undefined
        in desc $ as target