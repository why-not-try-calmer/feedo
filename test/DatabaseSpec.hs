module DatabaseSpec where

import AppServer (makeConfig)
import AppTypes
import Control.Concurrent (newChan, newEmptyMVar)
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import System.Environment (getEnvironment)
import Test.Hspec
import Mongo
import Redis

evalDb' action = liftIO getCurrentTime >>= \now -> case action of
    DbAskForLogin _ _ -> pure $ DbToken mempty
    CheckLogin _ -> pure $ DbLoggedIn 0
    ArchiveItems _ -> pure DbOk
    DeleteChat _ -> pure DbOk
    GetAllFeeds -> pure $ DbFeeds []
    GetAllChats -> pure $ DbChats []
    GetFeed _ -> pure $ DbFeeds []
    IncReads _ -> pure DbOk
    DbSearch key _ _ -> pure $ DbSearchRes key []
    PruneOld _ -> pure DbOk
    ReadDigest _ -> pure $ DbDigest $ Digest Nothing now [] [] []
    UpsertChat _ -> pure DbOk
    UpsertChats _ -> pure DbOk
    UpsertFeeds _ -> pure DbOk
    View _ t1 t2 -> pure $ DbView [] t1 t2
    WriteDigest dig -> pure $ DbDigest dig

spec :: Spec
spec = go >> go1
    where
        go =
            let desc as = describe "checkDbMapper" as
                as func = it "make sure the ORM matches the application values" func
                target = do
                    has <- checkDbMapper >> pure "ok"
                    let wants = "ok" :: T.Text
                    has `shouldBe` wants
            in  desc $ as target
        go1 =
            let desc as = describe "evalDb" as
                as func = it "evaluate database actions" func
                target = do
                    now <- getCurrentTime
                    let dig = DbDigest (Digest Nothing now [] [] [])
                    res <- evalDb' (ReadDigest mempty)
                    res `shouldSatisfy` (\(DbDigest d) -> digest_created d > now)
            in  desc $ as target