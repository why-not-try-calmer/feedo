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
import TgramOutJson (UserId, ChatId)

evalTgtTest :: Monad m => UserId -> UserAction -> ChatId -> m (Either UserError Reply)
evalTgtTest _ action _ = pure . Right . ServiceReply $ T.pack . show $ action

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
                let action = GetLastXDaysItems 1
                res <- runTestM $ evalTgtTest 0 action 0
                res `shouldSatisfy` (\(Right (ServiceReply reply)) -> reply == "GetLastXDaysItems 1")
        in  desc $ as target