module RequestsSpec where

import Test.Hspec
import Control.Concurrent (newChan)
import AppTypes
import Control.Monad.IO.Class (liftIO, MonadIO)
import TgramOutJson (Outbound(OutboundMessage))
import Requests (TgReqM(fakeRunSend, runSend, runSend_))

instance MonadIO m => TgReqM (TestApp m) where
    runSend = undefined
    runSend_ = undefined
    fakeRunSend _ _ out = return out

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
                res <- runTestM $ fakeRunSend tok mempty out
                res `shouldBe` OutboundMessage 0 mempty Nothing Nothing
        in  desc $ as target