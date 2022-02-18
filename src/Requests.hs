{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Requests where

import AppTypes (BotToken, Job (JobPin), Reply (reply_contents, reply_markdown, reply_disable_webview, reply_pin_on_send, ChatReply, ServiceReply), TestApp, App)
import Control.Concurrent (Chan, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.HTTP.Req
import TgramInJson (Message (message_id), TgGetMessageResponse (resp_msg_result))
import TgramOutJson (ChatId, Outbound (OutboundMessage, out_chat_id, out_text, out_parse_mode, out_disable_web_page_preview))

{- Interface -}

class MonadIO m => TgReqM m where
    runSend :: (FromJSON a) => BotToken -> T.Text -> Outbound -> m (Either T.Text (JsonResponse a))
    runSend_ :: BotToken -> T.Text -> Outbound -> m (Either T.Text ())
    fakeRunSend :: BotToken -> T.Text -> Outbound -> m Outbound

instance MonadIO m => TgReqM (App m) where
    runSend = reqSend
    runSend_ = reqSend_
    fakeRunSend = undefined

instance TgReqM IO where
    runSend = reqSend
    runSend_ = reqSend_
    fakeRunSend = undefined

instance MonadIO m => TgReqM (TestApp m) where
    runSend = undefined
    runSend_ = undefined
    fakeRunSend _ _ out = return out

{- Actions -}

setWebhook :: MonadIO m => BotToken -> T.Text -> m ()
setWebhook tok webhook = do
    resp <- withReqManager $ runReq defaultHttpConfig . pure request
    let code = responseStatusCode (resp :: JsonResponse Value) :: Int
        message = responseStatusMessage resp
    if code /= 200 then liftIO . throwIO . userError $ "Failed to set webhook, error message reads: " ++ show message
    else pure ()
    where
        request = req GET (https "api.telegram.org" /: tok /: "setWebhook") NoReqBody jsonResponse $
            "url" =: (webhook `T.append` "/webhook/" `T.append` tok)

fetchFeed :: MonadIO m => Url scheme -> m (Maybe LB.ByteString)
fetchFeed url = liftIO (try action :: IO (Either SomeException LbsResponse)) >>= \case
    Left _ -> pure Nothing
    Right resp ->
        let contents = responseBody resp
            code = responseStatusCode resp :: Int
        in  if code /= 200 then pure Nothing else pure . Just $ contents
    where
        action = withReqManager $ runReq defaultHttpConfig . pure request
        request = req GET url NoReqBody lbsResponse mempty

reqSend :: (TgReqM m, FromJSON a) => BotToken -> T.Text -> Outbound -> m (Either T.Text (JsonResponse a))
reqSend tok postMeth encodedMsg = liftIO (try action) >>= \case
    Left (SomeException err) ->
        let msg = "Tried to send a request, but failed for this reason: " ++ show err ++ " Please try again the very action you were doing"
        in  pure . Left . T.pack $ msg
    Right resp -> pure . Right $ resp
    where
        action = withReqManager $ runReq defaultHttpConfig . pure request
        request =
            let reqUrl = https "api.telegram.org" /: tok /: postMeth
            in  req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) jsonResponse mempty

reqSend_ :: TgReqM m => BotToken -> T.Text -> Outbound -> m (Either T.Text ())
reqSend_ a b c = runSend a b c >>= \case
    Left txt -> pure $ Left txt
    Right (_ :: JsonResponse Value) -> pure $ Right ()

reply :: TgReqM m => BotToken -> ChatId -> Reply -> Chan Job -> m ()
reply tok cid rep chan =
    let mid_from resp =
            let b = responseBody resp :: TgGetMessageResponse
            in  message_id . resp_msg_result $ b
        fromReply ChatReply{..} = OutboundMessage {
                out_chat_id = cid,
                out_text = non_empty reply_contents,
                out_parse_mode = if reply_markdown then Just "Markdown" else Nothing,
                out_disable_web_page_preview = if reply_disable_webview then Just True else Nothing
            }
        fromReply (ServiceReply contents) = OutboundMessage cid (non_empty contents) Nothing (Just True)
        redirect err = void $ runSend_ tok "sendMessage" $ OutboundMessage cid err Nothing Nothing
        non_empty txt = if T.null txt then "No result for this command." else txt
        triage_replies msg@ChatReply{..}
            | reply_pin_on_send = runSend tok "sendMessage" (fromReply msg) >>= \case
                Left err -> redirect err
                Right resp -> liftIO . writeChan chan $ JobPin cid (mid_from resp)
            | otherwise = runSend_ tok "sendMessage" (fromReply msg) >>= \case
                Left err -> redirect err
                Right _ -> pure ()
        triage_replies serv_rep = runSend_ tok "sendMessage" (fromReply serv_rep) >>= \case
            Left err -> redirect err
            Right _ ->  pure ()
    in  triage_replies rep