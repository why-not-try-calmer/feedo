module TgActionsSpec where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (mapM_)
import qualified Data.Text as T
import GHC.IO (evaluate)
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import TgActions (evalTgAct, interpretCmd)
import TgramOutJson (ChatId, UserId)
import Types

getConns :: IO AppConfig
getConns = do
  env <- getEnvironment
  config <- makeConfig env
  pure config

spec :: Spec
spec = go >> runIO getConns >>= go1
 where
  go =
    let desc = describe "interpretCmd"
        as = it "interpret commands issued from Telegram"
        t1 = interpretCmd "/items 1" `shouldBe` Right (GetItems $ ById 1)
        t2 = interpretCmd "/set\ndigest_size: 1\nfollow: true" `shouldBe` (Right . SetChatSettings . Parsed $ [PFollow True, PDigestSize 1])
        t3 = interpretCmd "/unsub 1234 oh_shoot" `shouldBe` Left (BadRef "1234oh_shoot")
     in mapM_ (desc . as) [t1, t2, t3]
  go1 env =
    let desc = describe "evalTgAct"
        as = it "evaluate a monadic action issued from Telegram"
        target = do
          let action = GetLastXDaysItems 1
          res <- runApp env $ evalTgAct 0 action 0
          res `shouldSatisfy` (\(Right (ServiceReply reply)) -> not $ T.null reply)
     in desc $ as target
