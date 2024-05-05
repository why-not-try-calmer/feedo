{-# LANGUAGE RecordWildCards #-}

module Web where

import Chats (withChats, getChats)
import Control.Concurrent (readMVar)
import Control.Monad.Reader (MonadIO (liftIO), ask, forM_)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as S
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, toGregorian)
import Mongo (HasMongo (evalDb))
import Network.HTTP.Req (renderUrl)
import Network.URI.Encode (decodeText)
import Parsing (eitherUrlScheme)
import Replies (render)
import Text.Blaze (Markup, textValue, (!))
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as Attr
import Types
import Utils (mbTime)

renderDbRes :: DbRes -> H.Html
renderDbRes (Left err) = H.toHtml . render $ err
renderDbRes (Right res) = case res of
  DbNoDigest -> "No item found for this digest. Make sure to use a valid reference to digests."
  DbBadOID -> "Unable to find a digest matching this identifier."
  DbDigest Digest{..} ->
    let flt =
          let titles = if null digest_titles then digest_links else digest_titles
           in Map.fromList $ zip digest_links titles
     in H.docTypeHtml $ do
          H.head $ do
            H.title . toHtml $ "feedo_the_bot/digest/" `T.append` digest_id_txt digest_id
            H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedo_the_bot"
          H.body $ do
            H.h2 . toHtml $ "digest id: " `T.append` digest_id_txt digest_id
            H.h3 . toHtml $ "Feeds (" `T.append` nbOfFlinks digest_items `T.append` ")"
            H.ul $ forM_ (flinks digest_items) (\fl -> let t = flt Map.! fl in H.li $ H.a ! Attr.href (textValue fl) $ toHtml t)
            H.h3 . toHtml $ "Items (" `T.append` nbOf digest_items `T.append` ")"
            H.ul $
              forM_
                (ordered_items digest_items)
                ( \is -> do
                    H.p . toHtml $ (let fl = i_feed_link . head $ is in flt Map.! fl) `T.append` " (" `T.append` nbOf is `T.append` ") items"
                    H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is
                )
            H.p "To get your favorite web feeds posted to your Telegram account, start talking to "
            H.a ! Attr.href (textValue "https://t.me/feedo_the_bot") $ "https://t.me/feedo_the_bot"
  DbView [] _ _ -> "No item found for your query. Try using different values for the time and urls parameters."
  DbView items f t ->
    let (from, to) = (T.pack . show $ f, T.pack . show $ t)
     in H.docTypeHtml $ do
          H.head $ do
            H.title "feedo_the_bot/view/"
            H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedo_the_bot"
          H.body $ do
            H.h3 . toHtml $ "Feeds found (" `T.append` nbOfFlinks items `T.append` ")"
            H.ul $ forM_ (flinks items) (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
            H.p . toHtml $ "Time window: between " `T.append` from `T.append` " and " `T.append` to `T.append` "."
            H.h3 . toHtml $ "Items found (" `T.append` nbOf items `T.append` ")"
            H.ul $
              forM_
                (ordered_items items)
                ( \is -> do
                    H.p . toHtml $ (i_feed_link . head $ is) `T.append` " (" `T.append` nbOf is `T.append` ") items"
                    H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is
                )
            H.p "To get your favorite web feeds posted to your Telegram account, start talking to "
            H.a ! Attr.href (textValue "https://t.me/feedo_the_bot") $ "https://t.me/feedo_the_bot"
  _ -> "Invalid operation."
 where
  digest_id_txt mid = case mid :: Maybe T.Text of
    Nothing -> "_id could not be determined"
    Just oid -> T.pack . show $ oid
  nbOf = T.pack . show . length
  nbOfFlinks = nbOf . flinks
  flinks items = S.toList . S.fromList $ map i_feed_link items
  go m _ [] = m
  go m d (i : is) =
    let day = utctDay $ i_pubdate i
        (_, _, d') = toGregorian day
        date = T.pack . formatTime defaultTimeLocale "%A, %B %e, %Y" $ i_pubdate i
        date_p = H.p $ toHtml date
        item_li = H.li $ H.article $ H.a ! Attr.href (textValue $ i_link i) $ toHtml (i_title i)
     in if d == d' then go (m >> item_li) d' is else go (m >> date_p >> item_li) d' is
  ordered_items =
    foldl'
      ( \acc i ->
          let flink = i_feed_link i
           in case Map.lookup flink acc of
                Nothing -> Map.insert flink [i] acc
                Just _ -> Map.update (\vs -> Just $ i : vs) flink acc
      )
      Map.empty

home :: (MonadIO m) => App m Markup
home = pure . H.docTypeHtml $ do
  H.head $ do
    H.title "feedo_the_bot/view/home"
    H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedo_the_bot"
  H.body $ do
    H.h3 "This might have been"
    H.p "...your query..."
    H.ul $ forM_ [] (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
    H.p "and this the time window"
    H.p . toHtml $ "between " `T.append` "a date to specifiy" `T.append` " and " `T.append` fromMaybe "now" (Just "another date to specify") `T.append` "."
    H.h3 "This might have been the results"
    H.ul $
      forM_
        ["item1", "item2", "item3"]
        ( \i -> do
            H.p "To start the day with something beautiful"
            H.ul $ H.li $ H.article i
        )
    H.p "But mostly useful, start talking to "
      >> (H.a ! Attr.href (textValue "https://t.me/feedo_the_bot") $ "https://t.me/feedo_the_bot")
      >> H.p " and get your favorite web feeds posted to your Telegram account!"

viewDigests :: (MonadIO m) => T.Text -> App m Markup
viewDigests _id = evalDb (ReadDigest _id) <&> renderDbRes

viewSearchRes :: (MonadIO m) => Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> App m Markup
viewSearchRes Nothing _ _ =
  pure
    "Missing a anti-slash-separated list of urls. Example of a full query: \
    \ /view?flinks=https://my_url1.org\\https://myurl2.org&from=2022-01-28&to=2022-01-30"
viewSearchRes _ Nothing _ = pure "Missing a datestring (format: '2022-01-28')"
viewSearchRes (Just flinks_txt) (Just fr) m_to = do
  if null flinks
    then
      abortWith
        "No links given or invalid links. \
        \ Make sure to pass a string of valid, anti-slash-separated links, respecting URI encoding."
    else case sequence parsed of
      Nothing ->
        abortWith
          "Unable to parse values for 'start' and/or 'end'. \
          \ Make sure to use the format as in '2022-28-01,2022-30-01' \
          \ for the 2-days interval between January 28 and January 30, 2022. \
          \ The second parameter is optional."
      Just [f] -> liftIO getCurrentTime >>= \now -> evalDb (View flinks f now) <&> renderDbRes
      Just [f, t] -> evalDb (View flinks f t) <&> renderDbRes
      _ -> abortWith "Unable to parse values for 'start' and/or 'end'"
 where
  parsed =
    foldl'
      ( \acc m -> case m of
          Nothing -> acc
          Just v -> acc ++ [mbTime . T.unpack $ v]
      )
      []
      [Just fr, m_to]
  flinks = case mapM (eitherUrlScheme . decodeText) $ T.splitOn "\\" flinks_txt of
    Left _ -> []
    Right lks -> map renderUrl lks
  abortWith = pure

readSettings :: (MonadIO m) => ReadReq -> App m ReadResp
readSettings (ReadReq hash) = do
  env <- ask
  evalDb (CheckLogin hash) >>= \case
    Left err -> pure $ failedWith (render err)
    Right (DbLoggedIn cid) ->
      getChats
        >>= \chats -> case HMS.lookup cid chats of
          Nothing -> pure $ failedWith "Chat does not exist."
          Just c -> pure $ ok cid c
    _ -> undefined
 where
  failedWith err = ReadResp Nothing Nothing (Just err)
  ok cid c = ReadResp (Just $ sub_settings c) (Just cid) Nothing

writeSettings :: (MonadIO m) => WriteReq -> App m WriteResp
writeSettings (WriteReq hash new_settings Nothing) = do
  env <- ask
  evalDb (CheckLogin hash) >>= \case
    Left err -> pure (WriteResp 504 (Just . render $ err) Nothing)
    Right (DbLoggedIn cid) -> do
      chats <- getChats
      case HMS.lookup cid chats of
        Nothing -> pure (WriteResp 504 (Just "Unable to find the target chat") Nothing)
        Just c ->
          let diffed = diffing (sub_settings c) new_settings
           in if null diffed
                then pure (WriteResp 200 (Just "You didn't change any of your settings") Nothing)
                else pure (WriteResp 200 (Just $ "These settings have changed:\n" `T.append` T.intercalate "\n" diffed) Nothing)
    _ -> undefined
 where
  diff (v1, v2) = if v1 == v2 then mempty else T.intercalate "=>" $ map (T.pack . show) [v1, v2]
  diffing s s' =
    filter
      (not . T.null)
      [ diff (settings_digest_collapse s, settings_digest_collapse s')
      , diff (settings_digest_interval s, settings_digest_interval s')
      , diff (settings_digest_size s, settings_digest_size s')
      , diff (settings_digest_start s, settings_digest_start s')
      , diff (settings_digest_title s, settings_digest_title s')
      , diff (settings_disable_web_view s, settings_disable_web_view s')
      , diff (settings_paused s, settings_paused s')
      , diff (settings_pin s, settings_pin s')
      , diff (settings_word_matches s, settings_word_matches s')
      , diff (settings_share_link s, settings_share_link s')
      , diff (settings_pagination s, settings_pagination s')
      ]
writeSettings (WriteReq _ _ (Just False)) = pure $ WriteResp 200 (Just "Update aborted.") Nothing
writeSettings (WriteReq hash settings (Just True)) =
  evalDb (CheckLogin hash) >>= \case
    Left err -> pure $ noLogin err
    Right (DbLoggedIn cid) ->
      withChats (SetChatSettings $ Immediate settings) Nothing cid >>= \case
        Left err -> pure . noUpdate . render $ err
        Right _ -> pure ok
    _ -> undefined
 where
  noLogin err = WriteResp 401 (Just . render $ err) Nothing
  noUpdate err = WriteResp 500 (Just err) Nothing
  ok = WriteResp 200 (Just "Updated.") Nothing
