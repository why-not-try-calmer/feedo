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
  (config, _) <- makeConfig env
  pure config

spec :: Spec
spec = do
  env <- runIO getConns
  sequenceA_ [go, go1 env, go2 env, go3 env]
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
            evalDb env (WriteDigest dig) >>= \case
              DbDigestId digest_id -> pure . Just $ digest_id
              _ -> pure Nothing
          object_id `shouldSatisfy` (\case Just _ -> True; Nothing -> False)
          res <- evalDb env (ReadDigest . fromJust $ object_id)
          res `shouldSatisfy` (\case DbErr _ -> False; _ -> True)
     in desc $ as target
  go2 env =
    let desc = describe "search indexes: creating"
        as = it "test search indexes creation"
        target = do
          now <- getCurrentTime
          let items = [Item "HackerNews item" "Target" "https://hnrss.org/frontpage/item" "https://hnrss.org/frontpage" now]
              feed = Feed Rss "HackerNews is coming for love (desc)" "HackerNews is back to business" "https://hnrss.org/frontpage" items Nothing Nothing
          res <- evalDb env (ArchiveItems [feed])
          res `shouldSatisfy` (\DbOk -> True)
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
          res <- evalDb env (ArchiveItems [feed])
          res `shouldSatisfy` (\DbOk -> True)
          res <- evalDb env search_query
          res `shouldSatisfy` (\case DbSearchRes keywords' results -> not $ null results && keywords == keywords'; DbErr _ -> False)
          print res
     in desc $ as target
