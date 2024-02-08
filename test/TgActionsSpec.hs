module TgActionsSpec where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (mapM_)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.IO (evaluate)
import Hooks (withHooks)
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import TgActions (evalTgAct, interpretCmd, subFeed)
import TgramOutJson (ChatId, UserId)
import Types

spec :: Spec
spec = withHooks [go, go1, go2]
 where
  go _ =
    let desc = describe "interpretCmd"
        as = it "interpret commands issued from Telegram"
        t1 = interpretCmd "/items 1" `shouldBe` Right (GetItems $ ById 1)
        t2 = interpretCmd "/set\ndigest_size: 1\nfollow: true" `shouldBe` (Right . SetChatSettings . Parsed $ [PFollow True, PDigestSize 1])
        t3 = interpretCmd "/unsub 1234 oh_shoot" `shouldSatisfy` (\case Left (InterpreterErr _) -> True; _ -> False)
     in mapM_ (desc . as) [t1, t2, t3]
  go1 env =
    let desc = describe "evalTgAct"
        as = it "evaluate a monadic action issued from Telegram"
        target = do
          let action = GetLastXDaysItems 1
          res <- runApp env $ evalTgAct 0 action 0
          res `shouldSatisfy` (\(Right (ServiceReply reply)) -> not $ T.null reply)
     in desc $ as target
  go2 env =
    let desc = describe "subscribe"
        as = it "subscribe a chat or channel to a feed"
        target = do
          let url = "https://reddit.com/r/haskell.rss"
              cid = 123
              action = subFeed cid [url]
          res <- runApp env action
          res `shouldSatisfy` (\(ServiceReply reply) -> T.isInfixOf "Added and subscribed" reply)
          all_subs <- readMVar (subs_state env)
          let res = HMS.lookup cid all_subs
          res `shouldSatisfy` (\case Just chat -> url `S.member` sub_feeds_links chat; Nothing -> False)
     in desc $ as target
