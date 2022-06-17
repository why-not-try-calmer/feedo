{-# LANGUAGE ScopedTypeVariables #-}

module MarkdownCheckerSpec where

import AppServer (makeConfig)
import AppTypes
import Control.Monad.Reader (MonadIO (liftIO), ask)
import qualified Data.Text as T
import MarkdownChecker (parse, render)
import Network.HTTP.Req (JsonResponse, responseBody)
import Requests (TgReqM (runSend), reply)
import System.Environment (getEnvironment)
import Test.Hspec
import TgramInJson (TgGetMessageResponse (resp_msg_ok))
import TgramOutJson (Outbound (..))

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go >> runIO getConns >>= go1 where
    tester = "Hello _bea||utif||ully *nested* and_ my [.my.humble.world](https://beautiful.ugly.world)"
    go =
        let desc = describe "MarkdownChecker"
            as = it "check MarkdownV2 validity"
            target = case parse tester of
                Left err -> print err >> undefined
                Right (parsed, _) -> parsed `shouldSatisfy` not . T.null . render
        in  desc $ as target
    go1 env =
        let desc = describe "tries to send a message validated with MarkdownChecker as a Telegram Message"
            as = it "ensures Telegram agrees with the validation for this message"
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
                                resp `shouldSatisfy` resp_msg_ok . responseBody
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