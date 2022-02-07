{-# LANGUAGE RecordWildCards #-}
module HtmlViews where

import AppTypes
import Control.Concurrent (readMVar)
import Control.Monad.Reader (MonadIO (liftIO), ask, forM_)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as S
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime, toGregorian)
import Database (Db (evalDb))
import Network.HTTP.Req (renderUrl)
import Network.URI.Encode (decodeText)
import Parsing (eitherUrlScheme)
import Text.Blaze (Markup, textValue, (!))
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as Attr
import TgActions (checkIfAdmin)
import Utils (mbTime)

renderDbRes :: DbRes -> H.Html
renderDbRes res = case res of
    DbNoDigest -> "No item found for this digest. Make sure to use a valid reference to digests."
    DbDigest Digest{..} -> 
        let flt = 
                let titles = if null digest_titles then digest_links else digest_titles
                in Map.fromList $ zip digest_links titles
        in
        H.docTypeHtml $ do
        H.head $ do
            H.title . toHtml $ "feedfarer_bot/digest/" `T.append` digest_id_txt digest_id
            H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedfarer_bot"
        H.body $ do
            H.h2 . toHtml $ "digest id: " `T.append` digest_id_txt digest_id
            H.h3 . toHtml $ "Feeds (" `T.append` nbOfFlinks digest_items `T.append` ")"
            H.ul $ forM_ (flinks digest_items) (\fl -> let t = flt Map.! fl in H.li $ H.a ! Attr.href (textValue fl) $ toHtml t)
            H.h3 . toHtml $ "Items (" `T.append` nbOf digest_items `T.append` ")"
            H.ul $ forM_ (ordered_items digest_items) (\is -> do
                H.p . toHtml $ (let fl = i_feed_link . head $ is in flt Map.! fl) `T.append` " (" `T.append` nbOf is `T.append` ") items"
                H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is)
            H.p "To get your favorite web feeds posted to your Telegram account, start talking to "
            H.a ! Attr.href (textValue "https://t.me/feedfarer_bot") $ "https://t.me/feedfarer_bot"
    DbView [] _ _ -> "No item found for your query. Try using different values for the time and urls parameters."
    DbView items f t ->
        let (from, to) = (T.pack . show $ f, T.pack . show $ t) in H.docTypeHtml $ do
        H.head $ do
            H.title "feedfarer_bot/view/"
            H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedfarer_bot"
        H.body $ do
            H.h3 . toHtml $ "Feeds found (" `T.append` nbOfFlinks items `T.append` ")"
            H.ul $ forM_ (flinks items) (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
            H.p . toHtml $ "Time window: between " `T.append` from `T.append` " and " `T.append` to `T.append` "."
            H.h3 . toHtml $ "Items found (" `T.append` nbOf items `T.append` ")"
            H.ul $ forM_ (ordered_items items) (\is -> do
                H.p . toHtml $ (i_feed_link . head $ is) `T.append` " (" `T.append` nbOf is `T.append` ") items"
                H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is)
            H.p "To get your favorite web feeds posted to your Telegram account, start talking to "
            H.a ! Attr.href (textValue "https://t.me/feedfarer_bot") $ "https://t.me/feedfarer_bot"
    _ -> "Invalid operation."
    where
        digest_id_txt = T.pack . show
        nbOf = T.pack . show . length
        nbOfFlinks items = nbOf . flinks $ items
        flinks items = S.toList . S.fromList $ map i_feed_link items
        go m _ [] = m
        go m d (i:is) =
            let day = utctDay $ i_pubdate i
                (_, _, d') = toGregorian day
                date = T.pack . formatTime defaultTimeLocale "%A, %B %e, %Y" $ i_pubdate i
                date_p = H.p $ toHtml date
                item_li = H.li $ H.article $ H.a ! Attr.href (textValue $ i_link i) $ toHtml (i_title i)
            in  if d == d' then go (m >> item_li) d' is else go (m >> date_p >> item_li) d' is
        ordered_items items = foldl' (\acc i ->
            let flink = i_feed_link i
            in  case Map.lookup flink acc of
                Nothing -> Map.insert flink [i] acc
                Just _ -> Map.update (\vs -> Just $ i:vs) flink acc) Map.empty items

renderManageSettings :: Settings -> H.Html
renderManageSettings _ = mempty

home :: MonadIO m => App m Markup
home = pure . H.docTypeHtml $ do
    H.head $ do
        H.title "feedfarer_bot/view/home"
        H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedfarer_bot"
    H.body $ do
        H.h3 "This might have been"
        H.p "...your query..."
        H.ul $ forM_ [] (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
        H.p "and this the time window"
        H.p . toHtml $ "between " `T.append` "a date to specifiy" `T.append` " and " `T.append` fromMaybe "now" (Just "another date to specify") `T.append` "."
        H.h3 "This might have been the results"
        H.ul $ forM_ ["item1", "item2", "item3"] (\i -> do
            H.p "To start the day with something beautiful"
            H.ul $ H.li $ H.article i)
        H.p "But mostly useful, start talking to "
            >> (H.a ! Attr.href (textValue "https://t.me/feedfarer_bot") $ "https://t.me/feedfarer_bot")
            >> H.p " and get your favorite web feeds posted to your Telegram account!"

viewDigests :: MonadIO m => T.Text -> App m Markup
viewDigests _id = ask >>= \env -> evalDb env (ReadDigest _id) <&> renderDbRes

viewSearchRes :: MonadIO m => Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> App m Markup
viewSearchRes Nothing _ _ = pure "Missing a anti-slash-separated list of urls. Example of a full query: \
    \ /view?flinks=https://my_url1.org\\https://myurl2.org&from=2022-01-28&to=2022-01-30"
viewSearchRes _ Nothing _ = pure "Missing a datestring (format: '2022-01-28')"
viewSearchRes (Just flinks_txt) (Just fr) m_to = do
    env <- ask
    if null flinks then abortWith "No links given or invalid links. \
        \ Make sure to pass a string of valid, anti-slash-separated links, respecting URI encoding."
    else case sequence parsed of
        Nothing -> abortWith
            "Unable to parse values for 'start' and/or 'end'. \
            \ Make sure to use the format as in '2022-28-01,2022-30-01' \
            \ for the 2-days interval between January 28 and January 30, 2022. \
            \ The second parameter is optional."
        Just [f] -> liftIO getCurrentTime >>= \now -> evalDb env (View flinks f now) <&> renderDbRes
        Just [f, t] -> evalDb env (View flinks f t) <&> renderDbRes
        _ ->  abortWith "Unable to parse values for 'start' and/or 'end'"
    where
        parsed = foldl' (\acc m -> case m of
            Nothing -> acc
            Just v -> acc ++ [mbTime . T.unpack $ v]) [] [Just fr, m_to]
        flinks = case traverse (eitherUrlScheme . decodeText) $ T.splitOn "\\" flinks_txt of
            Left _ -> []
            Right lks -> map renderUrl lks
        abortWith msg = pure msg

readSettings :: MonadIO m => Int64 -> Int64 -> App m Markup
readSettings cid uid = ask >>= \env ->
    let tok = bot_token $ tg_config env
        nope = pure "Not authorized."
    in  checkIfAdmin tok uid cid >>= \case
        Nothing -> nope
        Just v ->
            if not v then nope
            else liftIO (readMVar $ subs_state env) >>= \subs ->
                case HMS.lookup cid subs of
                Nothing -> pure "Authorized, but chat not found. Please try again later."
                Just c -> pure . renderManageSettings . sub_settings $ c

writeSettings :: MonadIO m => Settings -> App m Bool
writeSettings _ = pure True
