module TgActionsSpec where

import Control.Concurrent (readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (mapM_)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Database.MongoDB (Select (select), findOne, (=:))
import GHC.IO (evaluate)
import Hooks (withHooks)
import Mongo (HasMongo (evalDb, withDb), MongoDoc (readDoc))
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
          let url = "https://www.reddit.com/r/NixOS/.rss"
              cid = 123
              mem_action = subFeed cid [url]
              db_action = withDb $ findOne (select ["sub_chat_id" =: cid] "subchats")
          mem_res <- runApp env mem_action
          mem_res `shouldSatisfy` (\(ServiceReply reply) -> T.isInfixOf "Added and subscribed" reply)
          all_subs <- readMVar (subs_state env)
          let mem_lookup = HMS.lookup cid all_subs
          mem_lookup `shouldSatisfy` (\case Just chat -> url `S.member` sub_feeds_links chat; Nothing -> False)
          db_res <- runApp env db_action
          db_res `shouldSatisfy` (\case Right (Just doc) -> let chat = readDoc doc :: SubChat in url `S.member` sub_feeds_links chat)
     in desc $ as target
