module Requests where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Value)
import qualified Data.Text as T
import Network.HTTP.Req
import TgramOutJson (Outbound)
import AppTypes (BotToken)
import qualified Data.ByteString.Lazy as LB

reqSend_ :: MonadIO m => BotToken -> T.Text -> Outbound -> m IgnoreResponse
-- sends HTTP requests to Telegram service, ignoring the response
reqSend_ tok postMeth encodedMsg = withReqManager $ runReq defaultHttpConfig . pure request where
    request = 
        let reqUrl = https "api.telegram.org" /: tok /: postMeth
        in  req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) ignoreResponse mempty

reqSend :: (MonadIO m, FromJSON a) => BotToken -> T.Text -> Outbound -> m (JsonResponse a)
-- sends HTTP requests to Telegram service, capturing the response
reqSend tok postMeth encodedMsg = withReqManager (runReq defaultHttpConfig . pure request) where
    request =
        let reqUrl = https "api.telegram.org" /: tok /: postMeth
        in  req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) jsonResponse mempty

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
fetchFeed url = do
    resp <- withReqManager $ runReq defaultHttpConfig . pure request
    let contents = responseBody resp
        code = responseStatusCode resp :: Int
    if code /= 200 then pure Nothing else pure . Just $ contents
    where request = req GET url NoReqBody lbsResponse mempty