{-# LANGUAGE OverloadedStrings #-}

module RedisSpec where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (decodeStrict')
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime)
import Database.Redis (TxResult (TxSuccess))
import Debug.Trace (trace)
import Redis (readDigest, withRedis, writeDigest)
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types

getConns :: IO AppConfig
getConns = do
  env <- getEnvironment
  (config, _) <- makeConfig env
  pure config

spec :: Spec
spec = runIO getConns >>= \env -> go1 env >> go2 env
 where
  go1 env =
    let desc = describe "Validates enqueuing"
        as = it "makes sure that enqueuing can write to Redis"
        target = do
          now <- getCurrentTime
          let digest = Digest (Just "123") now [] [] []
          res <- liftIO $ withRedis env $ writeDigest digest
          res `shouldSatisfy` (\case Right _ -> True; _ -> False)
     in desc $ as target
  go2 env =
    let desc = describe "Validate dequeuing"
        as = it "makes sure that dequeuing reads & delete key transactionally"
        target = do
          now <- getCurrentTime
          let digest = Digest (Just "123") now [] [] []
              getDoc (TxSuccess (Just doc, 1)) = decodeStrict' doc :: Maybe Digest
              validateResults (Just doc) =
                let updated = doc{digest_titles = ["titles"]}
                 in trace (show $ digest_titles updated) $ digest_titles updated == ["titles"]
              validateResults Nothing = trace "failed to decode" False
              condition1 = validateResults . getDoc
              condition2 (TxSuccess (Nothing, 0)) = True
              condition2 _ = False
          res1 <- liftIO $ withRedis env $ readDigest "123"
          print res1
          res1 `shouldSatisfy` condition1
          res2 <- liftIO $ withRedis env $ readDigest "123"
          print res2
          res2 `shouldSatisfy` condition2
     in desc $ as target
