module RequestsSpec where

import AppServer (makeConfig)
import AppTypes
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Network.HTTP.Req (JsonResponse, responseBody)
import Replies
import Requests (TgReqM (fakeRunSend, runSend, runSend_), reqSend)
import System.Environment
import Test.Hspec
import TgramInJson (TgGetMessageResponse (TgGetMessageResponse, resp_msg_ok))
import TgramOutJson (Outbound (OutboundMessage))

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = runIO getConns >>= \config -> go >> go1 config  where
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
    go1 config =
        let desc as = describe "*" as
            as func = it "sends message using MarkdownV2" func
            target = do
                let line_deco = "\\| "
                    emboldened w = "*" `T.append` w `T.append` "* "
                    title = "Pop!_OS blog"
                    link_txt = "How to cook Pop!_OS without bacon?"
                    link_href = "https://www.how-to-cook-without.org/feed_1-1!!_ok"
                    body = line_deco `T.append` emboldened (escapeWhere title mkdV2) `T.append` toMkdV2Link Nothing link_txt link_href
                    out = OutboundMessage (alert_chat . tg_config $ config) body (Just "MarkdownV2") Nothing
                res <- reqSend "bot2103664842:AAG7FkTA8XtdAZhIIb401_kCctwxrHzJsu0" "sendMessage" out :: IO (Either T.Text (JsonResponse TgGetMessageResponse))
                case res of
                    Left err -> print err >> undefined
                    Right m -> m `shouldSatisfy` (\v -> let resp = responseBody v :: TgGetMessageResponse in resp_msg_ok resp)
        in  desc $ as target