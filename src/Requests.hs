{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Requests where

import AppTypes (App, BotToken, Job (JobPin, JobTgAlert, JobSetPagination), Reply (ChatReply, EditReply, ServiceReply, reply_contents, reply_disable_webview, reply_markdown, reply_pin_on_send, reply_pagination))
import Control.Concurrent (Chan, writeChan)
import Control.Exception (SomeException (SomeException), throwIO, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Value)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Text as T
import Network.HTTP.Req
import TgramInJson (Message (message_id), TgGetMessageResponse (resp_msg_result))
import TgramOutJson (ChatId, InlineKeyboardButton (InlineKeyboardButton), InlineKeyboardMarkup (InlineKeyboardMarkup), Outbound (EditMessage, OutboundMessage, out_chat_id, out_disable_web_page_preview, out_parse_mode, out_reply_markup, out_text))

{- Interface -}

class MonadIO m => TgReqM m where
    runSend :: (FromJSON a) => BotToken -> T.Text -> Outbound -> m (Either T.Text (JsonResponse a))
    runSend_ :: BotToken -> T.Text -> Outbound -> m (Either T.Text ())

instance MonadIO m => TgReqM (App m) where
    runSend = reqSend
    runSend_ = reqSend_

instance TgReqM IO where
    runSend = reqSend
    runSend_ = reqSend_

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

reqSend :: (MonadIO m, FromJSON a) => BotToken -> T.Text -> Outbound -> m (Either T.Text (JsonResponse a))
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
reqSend_ a b c = reqSend a b c >>= \case
    Left txt -> pure $ Left txt
    Right (_ :: JsonResponse Value) -> pure $ Right ()

mkKeyboard :: Int -> Int -> Maybe InlineKeyboardMarkup
mkKeyboard tgt tot
    | tot <= 1 || tot < tgt = Nothing
    | tgt == 1 = Just $ InlineKeyboardMarkup [[curr, next]]
    | tgt == tot = Just $ InlineKeyboardMarkup [[prev, curr], [reset]]
    | otherwise = Just $ InlineKeyboardMarkup [[prev, curr, next], [reset]]
    where
        out_of = (T.pack . show $ tgt) `T.append` "/" `T.append` (T.pack . show $ tot)
        curr = InlineKeyboardButton out_of "*"
        prev = InlineKeyboardButton "Prev." (T.pack . show $ tgt - 1)
        next = InlineKeyboardButton "Next" (T.pack . show $ tgt + 1)
        reset = InlineKeyboardButton "Page 1" "1"

mkPagination :: T.Text -> Maybe ([T.Text], InlineKeyboardMarkup)
mkPagination orig_txt =
    if pages_nb < 2 then Nothing
    else mkKeyboard 1 pages_nb >>= Just . (,) cuts
    where
        pages_nb = length cuts
        cuts = go [] $ T.lines orig_txt
        go acc [] = acc
        go [] (l:ls) = go [l] ls
        go (p:ps) (l:ls) =
            let pl = T.intercalate "\n" [p,l] in
            case T.compareLength pl 606 of
            GT -> go (l:p:ps) ls
            _ -> go (pl:ps) ls

reply :: TgReqM m => 
    BotToken -> 
    ChatId -> 
    Reply -> 
    Chan Job -> 
    m ()
reply tok cid rep chan =
    let mid_from resp =
            let b = responseBody resp :: TgGetMessageResponse
            in  message_id . resp_msg_result $ b
        fromReply ChatReply{..} =
            let base = OutboundMessage {
                    out_chat_id = cid,
                    out_text = non_empty reply_contents,
                    out_parse_mode = if reply_markdown then Just "Markdown" else Nothing,
                    out_disable_web_page_preview = if reply_disable_webview then Just True else Nothing,
                    out_reply_markup = Nothing
                }
            in
                if not reply_pagination || isNothing (mkPagination reply_contents)
                then base else let (pages, keyboard) = fromJust $ mkPagination reply_contents
                in OutboundMessage {
                    out_chat_id = cid,
                    out_text = non_empty $ last pages,
                    out_parse_mode = if reply_markdown then Just "Markdown" else Nothing,
                    out_disable_web_page_preview = if reply_disable_webview then Just True else Nothing,
                    out_reply_markup = Just keyboard
                    }
        fromReply (ServiceReply contents) = OutboundMessage cid (non_empty contents) Nothing (Just True) Nothing
        fromReply (EditReply mid contents markdown keyboard) = EditMessage cid mid contents (if markdown then Just "Markdown" else Nothing) keyboard
        report err =
            let err_msg = "Chat " `T.append` (T.pack . show $ cid) `T.append` " ran into this error: " `T.append` err
            in  liftIO . writeChan chan $ JobTgAlert err_msg
        non_empty txt = if T.null txt then "No result for this command." else txt
        triage_replies msg@ChatReply{..} =
            let jobs mid = [
                    if reply_pin_on_send then Just $ JobPin cid mid else Nothing,
                    if reply_pagination && isJust (mkPagination reply_contents)
                    then let (pages, _) = fromJust $ mkPagination reply_contents in Just $ JobSetPagination cid mid pages
                    else Nothing
                    ]
            in  runSend tok "sendMessage" (fromReply msg) >>= \case
                Left err -> report err
                Right resp ->
                    let mid = mid_from resp
                    in  for_ (jobs mid) $ \case 
                            Just j -> liftIO $ writeChan chan j
                            Nothing -> pure ()
        triage_replies msg@EditReply{} = runSend_ tok "editMessageText" (fromReply msg) >>= \case
            Left err -> report err
            Right _ -> pure ()
        triage_replies msg@ServiceReply{} = runSend_ tok "sendMessage" (fromReply msg) >>= \case
            Left err -> report err
            Right _ ->  pure ()
    in  triage_replies rep