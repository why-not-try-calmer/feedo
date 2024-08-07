{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Requests (alertAdmin, fetchFeed, mkPagination, runSend, runSend_, answer, mkKeyboard, reply, TgReqM) where

import Control.Concurrent (Chan, writeChan)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad.Reader
import Control.Retry (constantDelay, limitRetries)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import Replies (render)
import TgramInJson (Message (message_id), TgGetMessageResponse (resp_msg_result))
import TgramOutJson (AnswerCallbackQuery, ChatId, InlineKeyboardButton (InlineKeyboardButton), InlineKeyboardMarkup (InlineKeyboardMarkup), OutTgMsg (..), TgRequestMethod (TgEditMessage, TgSendMessage))
import Types
import Utils (sliceIfAboveTelegramMax)

{- Interface -}

class (MonadIO m) => TgReqM m where
  runSend :: (FromJSON a) => BotToken -> TgRequestMethod -> OutTgMsg -> m (Either T.Text (JsonResponse a))
  runSend_ :: BotToken -> TgRequestMethod -> OutTgMsg -> m (Either T.Text ())

instance (MonadIO m) => TgReqM (App m) where
  runSend = reqSend
  runSend_ = reqSend_

instance TgReqM IO where
  runSend = reqSend
  runSend_ = reqSend_

-- sendReqBs :: BotToken -> T.Text -> ChatId -> IO ()
-- sendReqBs tok meth chatid = do
--   let payload = GetChatMessage chatid
--       params = https "api.telegram.org" /: tok /: meth
--       request = req Network.HTTP.Req.POST params (ReqBodyJson payload) bsResponse mempty
--   print $ renderUrl params
--   print $ encode payload
--   response <- runReq defaultHttpConfig request
--   B.putStrLn $ responseBody response
--   pure ()

reqSend :: (TgReqM m, FromJSON a) => BotToken -> TgRequestMethod -> OutTgMsg -> m (Either T.Text (JsonResponse a))
reqSend tok postMeth encodedMsg = liftIO $ do
  try action >>= \case
    Left (SomeException err) ->
      let msg =
            "Tried to send a request, but failed for this reason: "
              `T.append` (T.pack . show $ err)
              `T.append` ". Method was "
              `T.append` (T.pack . show $ postMeth)
              `T.append` ". The following outbounds were to be sent: "
              `T.append` T.intercalate "; " (map (T.pack . show) outbounds)
       in pure . Left $ msg
    Right resps -> pure . Right . head $ resps
 where
  action =
    mapM
      ( \outbound -> withReqManager $
          \manager ->
            runReq defaultHttpConfig $
              pure request manager outbound
      )
      outbounds
  outbounds = case encodedMsg of
    NewMessage{..} -> map (\part -> encodedMsg{out_text = part}) (sliceIfAboveTelegramMax out_text)
    EditMessage{..} -> map (\part -> encodedMsg{out_text = part}) (sliceIfAboveTelegramMax out_text)
    _ -> pure encodedMsg
  -- request :: (TgReqM m, FromJSON a) => OutTgMsg -> m (JsonResponse a)
  request outbound =
    let reqUrl = https "api.telegram.org" /: tok /: render postMeth
     in req Network.HTTP.Req.POST reqUrl (ReqBodyJson outbound) jsonResponse mempty

reqSend_ :: (TgReqM m) => BotToken -> TgRequestMethod -> OutTgMsg -> m (Either T.Text ())
reqSend_ a b c =
  reqSend a b c >>= \case
    Left txt -> pure $ Left txt
    Right (_ :: JsonResponse Value) -> pure $ Right ()

reqManagerConfig :: HttpConfig
reqManagerConfig = defaultHttpConfig{httpConfigRetryPolicy = constantDelay 30000 <> limitRetries 2}

{- Telegram -}

answer :: (TgReqM m) => BotToken -> AnswerCallbackQuery -> m (Either T.Text ())
answer tok query =
  liftIO (try action) >>= \case
    Left (SomeException err) -> pure . Left . T.pack . show $ err
    Right (_ :: JsonResponse Value) -> pure $ Right ()
 where
  action = withReqManager $ runReq reqManagerConfig . pure request
  request =
    let reqUrl = https "api.telegram.org" /: tok /: "answerCallbackQuery"
     in req Network.HTTP.Req.POST reqUrl (ReqBodyJson query) jsonResponse mempty

mkPermaLinkBtn :: T.Text -> InlineKeyboardButton
mkPermaLinkBtn url = InlineKeyboardButton "Show in full" (Just url) Nothing

mkPermLinkKeyboard :: T.Text -> InlineKeyboardMarkup
mkPermLinkKeyboard txt = InlineKeyboardMarkup [[mkPermaLinkBtn txt]]

mkKeyboard :: Int -> Int -> Maybe T.Text -> Maybe InlineKeyboardMarkup
mkKeyboard tgt tot mb_url
  | tot < 1 || tgt > tot = Nothing
  | tot == 1 =
      case mb_url of
        Nothing -> Nothing
        Just url -> Just $ InlineKeyboardMarkup [[mkPermaLinkBtn url]]
  | tgt == 1 = Just $
      InlineKeyboardMarkup $
        case mb_url of
          Nothing -> [[curr, next]]
          Just url -> [[curr, next], [mkPermaLinkBtn url]]
  | tgt == tot = Just $
      InlineKeyboardMarkup $
        case mb_url of
          Nothing -> [[prev, curr], [reset]]
          Just url -> [[prev, curr], [reset, mkPermaLinkBtn url]]
  | otherwise = Just $
      InlineKeyboardMarkup $
        case mb_url of
          Nothing -> [[prev, curr, next], [reset]]
          Just url -> [[prev, curr, next], [reset, mkPermaLinkBtn url]]
 where
  out_of = (T.pack . show $ tgt) `T.append` "/" `T.append` (T.pack . show $ tot)
  curr = InlineKeyboardButton out_of Nothing (Just "*")
  prev = InlineKeyboardButton "Prev." Nothing (Just . T.pack . show $ tgt - 1)
  next = InlineKeyboardButton "Next" Nothing (Just . T.pack . show $ tgt + 1)
  reset = InlineKeyboardButton "Page 1" Nothing (Just "1")

mkPagination :: T.Text -> Maybe T.Text -> Maybe ([T.Text], InlineKeyboardMarkup)
mkPagination orig_txt mb_url =
  if pages_nb < 2
    then Nothing
    else mkKeyboard 1 pages_nb mb_url >>= Just . (,) cuts
 where
  pages_nb = length cuts
  cuts = go [] $ T.lines orig_txt
  go acc [] = acc
  go [] (l : ls) = go [l] ls
  go (p : ps) (l : ls) =
    let pl = T.intercalate "\n" [p, l]
     in case T.compareLength pl 1290 of
          GT -> go (l : p : ps) ls
          _ -> go (pl : ps) ls

reply ::
  (TgReqM m, MonadReader AppConfig m) =>
  ChatId ->
  Reply ->
  m ()
reply cid rep = do
  env <- ask
  let tok = bot_token . tg_config $ env
      chan = postjobs env
      mid_from resp =
        let b = responseBody resp :: TgGetMessageResponse
         in message_id . resp_msg_result $ b
      fromReply ChatReply{..} =
        let base =
              NewMessage
                { out_chat_id = cid
                , out_text = reply_contents
                , out_parse_mode = if reply_markdown then Just "Markdown" else Nothing
                , out_disable_web_page_preview = pure reply_disable_webview
                , out_reply_markup = Nothing
                }
         in if reply_pagination && isJust (mkPagination reply_contents reply_permalink)
              then
                let (pages, keyboard) = fromJust $ mkPagination reply_contents reply_permalink
                 in base{out_text = last pages, out_reply_markup = Just keyboard}
              else base{out_reply_markup = mkPermLinkKeyboard <$> reply_permalink}
      fromReply (ServiceReply contents) = NewMessage cid contents Nothing (Just True) Nothing
      fromReply (ForwardServiceReply cid' txt) = NewMessage cid' txt Nothing (Just True) Nothing
      fromReply (EditReply mid contents markdown keyboard) = EditMessage cid mid contents has_markdown no_webview keyboard
       where
        no_webview = pure True
        has_markdown = if markdown then Just "Markdown" else Nothing
      report err =
        let err_msg = "Chat " `T.append` (T.pack . show $ cid) `T.append` " ran into this error: " `T.append` err
         in alertAdmin chan err_msg
      send msg@ChatReply{..} =
        let jobs mid =
              [ if reply_pin_on_send then Just $ JobPin cid mid else Nothing
              , if reply_pagination && isJust (mkPagination reply_contents reply_permalink)
                  then
                    let (pages, _) = fromJust $ mkPagination reply_contents reply_permalink
                     in Just $ JobSetPagination cid mid pages reply_permalink
                  else Nothing
              ]
         in runSend tok TgSendMessage (fromReply msg) >>= \case
              Left err ->
                let forbidden = any (`T.isInfixOf` err) ["blocked", "kicked"]
                 in if not forbidden
                      then report err
                      else do
                        liftIO $ writeChan chan . JobPurge $ cid
                        report $
                          "Bot blocked in private chat "
                            `T.append` (T.pack . show $ cid)
                            `T.append` "Chat purged."
              Right resp ->
                let mid = mid_from resp
                 in for_ (jobs mid) $ \case
                      Just j -> liftIO $ writeChan chan j
                      Nothing -> pure ()
      send msg@EditReply{} =
        runSend_ tok TgEditMessage (fromReply msg) >>= \case
          Left err -> report err
          Right _ -> pure ()
      send msg@(ServiceReply _) = do
        runSend_ tok TgSendMessage (fromReply msg) >>= \case
          Left err -> report err
          Right _ -> pure ()
      send msg@ForwardServiceReply{} =
        runSend_ tok TgSendMessage (fromReply msg) >>= \case
          Left err -> report err
          Right _ -> pure ()
      replyTxt = case rep of
        ChatReply txt _ _ _ _ _ -> txt
        ServiceReply txt -> txt
        ForwardServiceReply _ txt -> txt
        EditReply _ txt _ _ -> txt
      processReply
        | T.null replyTxt =
            let alert_msg = "Cancelled an empty reply that was heading to this chat: " `T.append` (T.pack . show $ cid)
             in alertAdmin chan alert_msg
        | otherwise = send rep
  processReply

fetchFeed :: (MonadIO m) => Url scheme -> m (Either FeedError LB.ByteString)
fetchFeed url =
  liftIO $ do
    now <- getCurrentTime
    (try action :: IO (Either HttpException LbsResponse)) >>= \case
      Left (VanillaHttpException exc) -> case exc of
        HTTP.HttpExceptionRequest _ cont -> pure . Left $ FeedError (renderUrl url) Nothing (T.pack . show $ cont) "Invalid request." now
        HTTP.InvalidUrlException url' reason -> pure . Left $ FeedError (T.pack url') Nothing (T.pack reason) "Invalid URL." now
      Left (JsonHttpException msg) -> pure . Left $ FeedError (renderUrl url) Nothing (T.pack msg) "Invalid JSON." now
      Right resp ->
        let code = responseStatusCode resp :: Int
            status_message = T.decodeUtf8 $ responseStatusMessage resp
            contents = responseBody resp
         in if code == 200
              then pure . Right $ contents
              else pure . Left $ FeedError (renderUrl url) (Just code) status_message "Response received but non-200 status code." now
 where
  action = withReqManager $ runReq reqManagerConfig . pure request
  request = req GET url NoReqBody lbsResponse mempty

alertAdmin :: (MonadIO m) => Chan Job -> T.Text -> m ()
alertAdmin chan = liftIO . writeChan chan . JobTgAlertAdmin
