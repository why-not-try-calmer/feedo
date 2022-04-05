{-# LANGUAGE ScopedTypeVariables #-}

module MarkdownCheckerSpec where

import AppServer (makeConfig)
import AppTypes
import qualified Data.Text as T
import MarkdownChecker (parse, render)
import System.Environment (getEnvironment)
import Test.Hspec
import Requests (reply, TgReqM (runSend))
import TgramOutJson (Outbound(..))
import Network.HTTP.Req (JsonResponse, responseBody)
import TgramInJson (TgGetMessageResponse (resp_msg_ok))
import Control.Monad.Reader (ask, MonadIO (liftIO))

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go >> runIO getConns >>= go1 where
    tester = "Hello _bea||utif||ully *nested* and_ my [.my-.humble.world](https://beautiful.ugly.world)"
    go =
        let desc as = describe "MarkdownChecker" as
            as func = it "check MarkdownV2 validity" func
            target = case parse tester of
                Left err -> print err >> undefined
                Right (parsed, _) -> parsed `shouldSatisfy` (not . T.null . render)
        in  desc $ as target
    go1 env =
        let desc as = describe "tries to send a message validated with MarkdownChecker as a Telegram Message" as
            as func = it "ensures Telegram agrees with the validation for this message" func
            target = do
                case parse tester of
                    Left err -> print err >> undefined
                    Right (parsed, _) -> do
                        let tok = bot_token . tg_config $ env
                            cid = alert_chat . tg_config $ env
                        let payload = render parsed
                        res <- runApp env $ action tok cid payload
                        case res of
                            Left err -> print err >> undefined
                            Right (resp :: JsonResponse TgGetMessageResponse) -> do
                                resp `shouldSatisfy` (resp_msg_ok . responseBody)
                                print resp
        in  desc $ as target
        where
            action tok cid contents = runSend tok "sendMessage" $ OutboundMessage {
                out_chat_id = cid,
                out_text = contents,
                out_parse_mode = Just "MarkdownV2",
                out_disable_web_page_preview = Nothing,
                out_reply_markup = Nothing
                }