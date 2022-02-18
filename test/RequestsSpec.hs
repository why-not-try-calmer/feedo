module RequestsSpec where

import Test.Hspec
import Control.Concurrent (newChan)
import AppTypes
import Control.Monad.IO.Class (liftIO)
import TgramOutJson (Outbound(OutboundMessage))
import Requests (TgReqM(fakeRunSend))

spec :: Spec
spec = go where
    go =
        let desc as = describe "reply" as
            as func = it "replies Telegram users with a JSON-encoded message" func
            target = do
                chan <- newChan
                let tok = "0"
                    cid = 0
                    out = OutboundMessage 0 mempty Nothing Nothing
                res <- getTestM $ fakeRunSend tok mempty out
                res `shouldBe` OutboundMessage 0 mempty Nothing Nothing
        in  desc $ as target