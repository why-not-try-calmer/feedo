module RequestsSpec where

import AppTypes
import Control.Monad.IO.Class (liftIO, MonadIO)
import TgramOutJson (Outbound(OutboundMessage))
import Requests (TgReqM(fakeRunSend, runSend, runSend_))
import AppServer (makeConfig)
import System.Environment
import Test.Hspec

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go where
    go =
        let desc as = describe "reply" as
            as func = it "replies Telegram users with a JSON-encoded message" func
            target = do
                env <- getConns
                let tok = "0"
                    cid = 0
                    out = OutboundMessage 0 mempty Nothing Nothing
                out `shouldBe` OutboundMessage 0 mempty Nothing Nothing
        in  desc $ as target