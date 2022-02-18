module TgActionsSpec where

import AppServer (makeConfig)
import AppTypes
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import GHC.IO (evaluate)
import System.Environment (getEnvironment)
import Test.Hspec
import TgActions (evalTgAct, interpretCmd)

class FakeIO m where
    fakeIt :: m a -> m T.Text

instance MonadIO m => FakeIO (App m) where
    fakeIt action = pure "ok"

testConfig :: IO AppConfig
testConfig = 
    getEnvironment >>= 
    makeConfig >>= \(config, _, _) -> 
    pure config

spec :: Spec
spec = go where
    go =
        let desc as = describe "interpretCmd" as
            as func = it "interpret commands issued from Telegram" func
            t1 = interpretCmd "/items 1" `shouldBe` Right (GetItems $ ById 1)
            t2 = interpretCmd "/set\ndigest_size: 1\nfollow: true" `shouldBe` (Right . SetChatSettings . Parsed $ [PFollow True, PDigestSize 1])
            t3 = interpretCmd "/unsub 1234 oh_shoot" `shouldBe` Left (BadRef "1234oh_shoot")
        in  traverse_ (desc . as) [t1, t2, t3]
    go1 =
        let desc as = describe "evalTgAct" as
            as func = it "evaluate a monadic action issued from Telegram" func
            target = do
                config <- testConfig
                res <- runApp config $ fakeIt . evalTgAct 0 (GetLastXDaysItems 1) $ 0
                res `shouldBe` "ok"
        in  desc $ as target