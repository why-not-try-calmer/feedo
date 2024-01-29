module MongoSpec where

import Control.Concurrent (newChan, newEmptyMVar)
import Control.Exception
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Data.Foldable (sequenceA_)
import Data.IORef (readIORef)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Database.MongoDB (aggregate, ensureIndex, getIndexes)
import Mongo
import Redis
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types

getConns :: IO AppConfig
getConns = do
  env <- getEnvironment
  config <- makeConfig env
  pure config

spec :: Spec
spec = do
  env <- runIO getConns
  sequenceA_ [go, go1 env, go2 env, go3 env, go4 env]
 where
  go =
    let desc = describe "checkDbMapper"
        as = it "make sure the ORM matches the application values"
        target = do
          has <- checkDbMapper >> pure "ok"
          let wants = "ok" :: T.Text
          has `shouldBe` wants
     in desc $ as target
  go1 env =
    let desc = describe "read a digest"
        as = it "ensure that db retrieves digests correctly"
        target = do
          now <- getCurrentTime
          let dig = Digest Nothing now [] [] []
          object_id <-
            runApp env $
              evalDb (WriteDigest dig) >>= \case
                Right (DbDigestId digest_id) -> pure . Just $ digest_id
                _ -> pure Nothing
          object_id `shouldSatisfy` (\case Just _ -> True; Nothing -> False)
          res <- runApp env $ evalDb (ReadDigest . fromJust $ object_id)
          res `shouldSatisfy` (\case Left _ -> False; _ -> True)
     in desc $ as target
  go2 env =
    let desc = describe "search indexes: creating"
        as = it "test search indexes creation"
        target = do
          now <- getCurrentTime
          let items = [Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now]
              feed = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" items Nothing Nothing
          res <- runApp env $ evalDb (ArchiveItems [feed])
          res `shouldSatisfy` (\(Right _) -> True)
          pipe <- readIORef (snd . connectors $ env)
          res <- try $ runMongo (database_name . mongo_creds $ env) pipe (ensureIndex itemsIndex)
          res `shouldSatisfy` (\case Right _ -> True; Left (SomeException _) -> False)
     in desc $ as target
  go3 env =
    let desc = describe "items and search"
        as = it "test full-text search"
        target = do
          now <- getCurrentTime
          let keywords = S.singleton "target"
              items = [Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now]
              feed = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" items Nothing Nothing
              search_query = DbSearch keywords S.empty Nothing
          res <- runApp env $ evalDb (ArchiveItems [feed])
          res `shouldSatisfy` (\(Right _) -> True)
          res <- runApp env $ evalDb search_query
          res `shouldSatisfy` (\case Right (DbSearchRes keywords' results) -> not $ null results && keywords == keywords'; Left _ -> False)
          print res
     in desc $ as target
  go4 env =
    let desc = describe "upsert and get feeds"
        as = it "test writing feeds to db as upserts"
        target = do
          now <- getCurrentTime
          let items = [Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now]
              feed = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" items Nothing Nothing
          res1 <- runApp env $ evalDb (UpsertFeeds [feed])
          res1 `shouldSatisfy` (\(Right _) -> True)
          res2 <- runApp env $ evalDb GetAllFeeds
          res2 `shouldSatisfy` (\(Right (DbFeeds feeds)) -> not $ null feeds)
          print res2
     in desc $ as target