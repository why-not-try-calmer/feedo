module BrokerSpec where

import Backend (loadChats, refreshCache, regenFeeds)
import Broker
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (All (getAll))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as HMS
import Data.Functor ((<&>))
import qualified Data.HashMap.Internal.Strict as HMS
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Time
import Database.MongoDB
import Database.Redis (keys, runRedis)
import Mongo
import Redis
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types
import Utils (renderDbError)

spec :: Spec
spec = pre >>= \(config, mb_feeds) -> go config >> go1 config mb_feeds
  where
    pre = runIO $ do
        putStrLn "Running test suite. Creating environment..."
        env <- getEnvironment
        (config, _) <- makeConfig env
        putStrLn "Environment done. Loading chats..."
        runApp config $ do
            loadChats
            liftIO $ putStrLn "Chats loaded"
            mb_feeds <- regenFeeds
            liftIO $ putStrLn "Feeds regenerated"
            refreshCache mb_feeds
            liftIO $ putStrLn "Cache refreshed"
            pure (config, mb_feeds)
    go config =
        let desc = describe "withCache: get all feeds"
            as = it "returns from the cache an item if found there, or updates the cache with the db and returns it afterwards"
            target = do
                res <- runApp config $ getAllFeeds config
                print res
                res `shouldSatisfy` (\case Right hmap -> not $ null hmap; _ -> undefined)
         in desc $ as target
    go1 env mb_feeds =
        let desc = describe "withCache: pull feeds "
            as = it "tries to obtain the feeds from the cache, otherwise turns to the db"
            target = do
                case mb_feeds of
                    Just feeds -> do
                        let flinks = map f_link feeds
                        res <- runApp env . withCache $ CachePullFeeds flinks
                        print res
                        case res of
                            Right (CacheFeeds fs) -> length fs `shouldSatisfy` (> 0)
                            _ -> undefined
                    Nothing -> print "No feed. Aborted"
         in desc $ as target