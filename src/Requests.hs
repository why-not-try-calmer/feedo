{-# LANGUAGE GADTs #-}

module Requests where

import AppTypes (BotToken, Job (JobRemoveMsg, JobPin), Reply (reply_contents, reply_markdown, reply_webview, reply_pin_on_send, reply_clean_behind))
import Control.Concurrent (Chan, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.HTTP.Req
import TgramInJson (Message (message_id), TgGetMessageResponse (resp_msg_result))
import TgramOutJson (ChatId, Outbound (OutboundMessage))

reqSend_ :: MonadIO m => BotToken -> T.Text -> Outbound -> m (Either T.Text ())
-- sends HTTP requests to Telegram service, ignoring the response
reqSend_ tok postMeth encodedMsg = liftIO (try action) >>= \case
    Left (SomeException err) -> 
        let msg = "Tried to send a request, but failed for this reason: " ++ show err ++ " Please try again the very action you were doing"
        in  pure . Left . T.pack $ msg
    Right _ -> pure . Right $ ()
    where
        action = withReqManager $ runReq defaultHttpConfig . pure request 
        request = 
            let reqUrl = https "api.telegram.org" /: tok /: postMeth
            in  req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) ignoreResponse mempty

reqSend :: (MonadIO m, FromJSON a) => BotToken -> T.Text -> Outbound -> m (Either T.Text (JsonResponse a))
-- sends HTTP requests to Telegram service, capturing the response
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

reply :: MonadIO m => BotToken -> ChatId -> Reply -> Chan Job -> m ()
reply tok cid rep chan
    |   reply_pin_on_send rep = reqSend tok "sendMessage" msg >>= \case
            Left err -> redirect err
            Right resp -> liftIO . writeChan chan $ JobPin cid (mid_from resp)
    |   reply_clean_behind rep = reqSend tok "sendMessage" msg >>= \case
            Left err -> redirect err
            Right resp -> liftIO . writeChan chan $ JobRemoveMsg cid (mid_from resp) Nothing
    |   otherwise = reqSend_ tok "sendMessage" msg >>= \case
            Left err -> redirect err
            Right _ -> pure ()
    where
        mid_from resp = 
            let res = responseBody resp :: TgGetMessageResponse
            in  message_id . resp_msg_result $ res
        msg = OutboundMessage cid (non_empty contents) parsemode webview
        contents = reply_contents rep
        parsemode = if reply_markdown rep then Just "Markdown" else Nothing
        webview = if reply_webview rep then Just True else Nothing
        redirect err = void $ reqSend_ tok "sendMessage" $ OutboundMessage cid err Nothing Nothing
        non_empty txt = if T.null txt then "No result for this command." else txt

setWebhook :: MonadIO m => BotToken -> T.Text -> m ()
-- registers a webhook for the given bot at the given url
setWebhook tok webhook = do
    resp <- withReqManager $ runReq defaultHttpConfig . pure request 
    let code = responseStatusCode (resp :: JsonResponse Value) :: Int
        message = responseStatusMessage resp
    if code /= 200 then liftIO . throwIO . userError $ "Failed to set webhook, error message reads:" ++ show message else pure ()
    where
        request = req Network.HTTP.Req.GET (https "api.telegram.org" /: tok /: "setWebhook") NoReqBody jsonResponse $
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