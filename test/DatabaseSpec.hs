module DatabaseSpec where

import Control.Concurrent (newChan, newEmptyMVar)
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Mongo
import Redis
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types

evalDb' action =
    liftIO getCurrentTime >>= \now -> case action of
        DbAskForLogin _ _ -> pure $ DbToken mempty
        CheckLogin _ -> pure $ DbLoggedIn 0
        ArchiveItems _ -> pure DbOk
        DeleteChat _ -> pure DbOk
        GetAllFeeds -> pure $ DbFeeds []
        GetAllChats -> pure $ DbChats []
        IncReads _ -> pure DbOk
        DbSearch key _ _ -> pure $ DbSearchRes key []
        PruneOld _ -> pure DbOk
        ReadDigest _ -> pure $ DbDigest $ Digest Nothing now [] [] []
        UpsertChat _ -> pure DbOk
        UpsertChats _ -> pure DbOk
        UpsertFeeds _ -> pure DbOk
        View _ t1 t2 -> pure $ DbView [] t1 t2
        WriteDigest dig -> pure $ DbDigest dig
        InsertPages cid _ _ _ -> pure DbOk
        GetPages _ _ -> pure $ DbPages [] Nothing

spec :: Spec
spec = go >> go1
  where
    go =
        let desc = describe "checkDbMapper"
            as = it "make sure the ORM matches the application values"
            target = do
                has <- checkDbMapper >> pure "ok"
                let wants = "ok" :: T.Text
                has `shouldBe` wants
         in desc $ as target
    go1 =
        let desc = describe "evalDb"
            as = it "evaluate database actions"
            target = do
                now <- getCurrentTime
                let dig = DbDigest (Digest Nothing now [] [] [])
                res <- evalDb' (ReadDigest mempty)
                res `shouldSatisfy` (\(DbDigest d) -> digest_created d >= now)
         in desc $ as target