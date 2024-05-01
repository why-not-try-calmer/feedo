module Hooks where

import Control.Concurrent (readMVar)
import Data.Foldable (sequenceA_)
import Data.IORef (readIORef)
import Database.MongoDB (close)
import Database.Redis
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec (afterAll_, runIO)
import Types

beforeTest :: IO AppConfig
beforeTest = getEnvironment >>= makeConfig

afterTest :: AppConfig -> IO ()
afterTest config = do
  let connectors_mvar = connectors config
  (redis_conn, mongo_conn) <- readMVar connectors_mvar
  runRedis redis_conn quit
  close mongo_conn

withHooks tests = do
  conf <- runIO beforeTest
  mapM_ (\t -> t conf) tests

-- afterTest conf `afterAll_` mapM_ (\t -> t conf) tests
