{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Requests (buildFeed, mkPagination, fetchFeed, runSend, runSend_, answer, mkKeyboard, reply, TgReqM) where

import Control.Concurrent (readMVar, writeChan)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad.Reader
import Control.Retry (constantDelay, limitRetries)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (Foldable (..), for_)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import Notifications (alertAdmin)
import Replies (render)
import Text.XML
import Text.XML.Cursor
import TgramInJson (Message (message_id), TgGetMessageResponse (resp_msg_result))
import TgramOutJson (AnswerCallbackQuery, ChatId, InlineKeyboardButton (InlineKeyboardButton), InlineKeyboardMarkup (InlineKeyboardMarkup), Outbound (EditMessage, OutboundMessage, out_chat_id, out_disable_web_page_preview, out_parse_mode, out_reply_markup, out_text), TgRequestMethod (TgEditMessage, TgSendMessage), UserId)
import Types
import Utils (averageInterval, mbTime, sliceIfAboveTelegramMax)

{- Interface -}

class (MonadIO m) => TgReqM m where
  runSend :: (FromJSON a) => BotToken -> TgRequestMethod -> Outbound -> m (Either T.Text (JsonResponse a))
  runSend_ :: BotToken -> TgRequestMethod -> Outbound -> m (Either T.Text ())

instance (MonadIO m) => TgReqM (App m) where
  runSend = reqSend
  runSend_ = reqSend_

instance TgReqM IO where
  runSend = reqSend
  runSend_ = reqSend_

reqSend :: (TgReqM m, FromJSON a) => BotToken -> TgRequestMethod -> Outbound -> m (Either T.Text (JsonResponse a))
reqSend tok postMeth encodedMsg =
  liftIO (try action) >>= \case
    Left (SomeException err) ->
      let msg = "Tried to send a request, but failed for this reason: " `T.append` (T.pack . show $ err)
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
    OutboundMessage{..} -> map (\part -> encodedMsg{out_text = part}) (sliceIfAboveTelegramMax out_text)
    EditMessage{..} -> map (\part -> encodedMsg{out_text = part}) (sliceIfAboveTelegramMax out_text)
    _ -> pure encodedMsg
  -- request :: (TgReqM m, FromJSON a) => Outbound -> m (JsonResponse a)
  request outbound =
    let reqUrl = https "api.telegram.org" /: tok /: render postMeth
     in req Network.HTTP.Req.POST reqUrl (ReqBodyJson outbound) jsonResponse mempty

reqSend_ :: (TgReqM m) => BotToken -> TgRequestMethod -> Outbound -> m (Either T.Text ())
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

{-
mkDigestLinkButton :: T.Text -> Maybe InlineKeyboardButton
mkDigestLinkButton link
    | T.null link = Nothing
    | otherwise = Just $ InlineKeyboardButton label (Just link) Nothing
  where
    label = "Permalink"
-}

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
              OutboundMessage
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
      fromReply (ServiceReply contents) = OutboundMessage cid contents Nothing (Just True) Nothing
      fromReply (ForwardServiceReply cid' txt) = OutboundMessage cid' txt Nothing (Just True) Nothing
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
      send msg@(ServiceReply contents) = do
        active_admins <-
          liftIO (readMVar $ subs_state env)
            >>= \subs -> pure $ getActiveChatAdminsIfForwarding subs cid Nothing
        let msg'
              | null active_admins = msg
              | otherwise = ForwardServiceReply (head active_admins) contents
        runSend_ tok TgSendMessage (fromReply msg') >>= \case
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

getActiveChatAdminsIfForwarding :: SubChats -> ChatId -> Maybe UTCTime -> [UserId]
getActiveChatAdminsIfForwarding subchats cid after =
  case HMS.lookup cid subchats of
    Nothing -> mempty
    Just c ->
      if settings_forward_to_admins . sub_settings $ c
        then HMS.keys $ HMS.filter pr $ sub_active_admins c
        else mempty
 where
  pr x = case after of
    Nothing -> True
    Just t -> t <= x

{- Feeds -}

fetchFeed :: (MonadIO m) => Url scheme -> m (Either FeedError LB.ByteString)
fetchFeed url =
  liftIO (try action :: IO (Either HttpException LbsResponse)) >>= \case
    Left (VanillaHttpException exc) -> case exc of
      HTTP.HttpExceptionRequest _ cont -> pure . Left $ FeedError (renderUrl url) Nothing (T.pack . show $ cont) "Invalid request."
      HTTP.InvalidUrlException url' reason -> pure . Left $ FeedError (T.pack url') Nothing (T.pack reason) "Invalid URL."
    Left (JsonHttpException msg) -> pure . Left $ FeedError (renderUrl url) Nothing (T.pack msg) "Invalid JSON."
    Right resp ->
      let code = responseStatusCode resp :: Int
          status_message = T.decodeUtf8 $ responseStatusMessage resp
          contents = responseBody resp
       in if code == 200
            then pure . Right $ contents
            else pure . Left $ FeedError (renderUrl url) (Just code) status_message "Response received but non-200 status code."
 where
  action = withReqManager $ runReq reqManagerConfig . pure request
  request = req GET url NoReqBody lbsResponse mempty

buildFeed :: (MonadIO m) => FeedType -> Url scheme -> m (Either TgEvalError (Feed, Maybe T.Text))
-- tries parsing bytes into a Feed
-- tries as Atom if Rss fails
buildFeed ty url = do
  now <- liftIO getCurrentTime
  fetchFeed url >>= \case
    Left other -> pure . Left $ BadFeed other
    Right feed -> case parseLBS def feed of
      Left (SomeException ex) ->
        pure
          . Left
          . ParseError
          $ "Unable to parse feed at "
            `T.append` (T.pack . show $ url)
            `T.append` ", bumped on this exception: "
            `T.append` (T.pack . show $ ex)
      Right doc ->
        let root = fromDocument doc
            desc = case ty of
              Atom -> T.concat $ child root >>= laxElement "subtitle" >>= child >>= content
              Rss -> T.concat $ child root >>= child >>= element "description" >>= child >>= content
            title = case ty of
              Atom -> T.concat $ child root >>= laxElement "title" >>= child >>= content
              Rss -> T.concat $ child root >>= child >>= element "title" >>= child >>= content
            get_date el = case ty of
              Atom -> mbTime $ T.unpack (T.concat $ child el >>= laxElement "updated" >>= child >>= content)
              Rss -> mbTime $ T.unpack (T.concat $ child el >>= element "pubDate" >>= child >>= content)
            make_item el = case ty of
              Atom ->
                Item
                  (T.concat $ child el >>= laxElement "title" >>= child >>= content)
                  (T.concat $ child el >>= laxElement "content" >>= child >>= content)
                  (T.concat . attribute "href" . head $ child el >>= laxElement "link")
                  (renderUrl url)
                  (fromMaybe now $ get_date el)
              Rss ->
                Item
                  (T.concat $ child el >>= element "title" >>= child >>= content)
                  (T.concat $ child el >>= element "description" >>= child >>= content)
                  (T.concat $ child el >>= element "link" >>= child >>= content)
                  (renderUrl url)
                  (fromMaybe now $ get_date el)
            items = case ty of
              Atom -> map make_item $ descendant root >>= laxElement "entry"
              Rss -> map make_item $ descendant root >>= element "item"
            interval = averageInterval . map i_pubdate $ items
            built_feed =
              Feed
                { f_type = ty
                , f_desc = if T.null desc then title else desc
                , f_title = title
                , f_link = renderUrl url
                , f_items = items
                , f_avg_interval = interval
                , f_last_refresh = Just now
                }
         in pure $ faultyFeed built_feed
 where
  faultyFeed f =
    let holes = [T.null $ f_desc f, T.null $ f_title f, T.null $ f_link f, null . f_items $ f]
        required = ["desc", "title", "url", "items", "interval"]
        optional = ["pubdate"]
        missing =
          foldl' (\acc v -> if snd v then fst v : acc else acc) [] $
            zip (required ++ optional) holes
        missing_required = filter (`elem` required) missing
        render_required =
          "The required feed could be constructed, but it's missing well-defined tags or items: "
            `T.append` T.intercalate ", " missing_required
            `T.append` ". Perhaps the source exports an alternative feed (RSS/Atom) that could work?"
        missing_optional = filter (`elem` optional) missing
        render_optional =
          if null missing_optional
            then Nothing
            else
              Just $
                "A valid feed could be constructed. However, notice that the following fields could not be \
                \ set appropriately: "
                  `T.append` T.intercalate ", " missing_optional
     in if null missing_required
          then Right (f, render_optional)
          else Left . ParseError $ render_required
