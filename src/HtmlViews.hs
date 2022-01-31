module HtmlViews where

import AppTypes
import Control.Monad.Reader (MonadIO (liftIO), ask, forM_)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime (utctDay), toGregorian, formatTime, defaultTimeLocale)
import Database (Db (evalDb))
import Network.HTTP.Req (renderUrl)
import Network.URI.Encode (decodeText)
import Parser (eitherUrlScheme, getTime)
import Text.Blaze (Markup, textValue, (!))
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as Attr
import Data.Maybe (fromMaybe)

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
    
view :: MonadIO m => Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> App m Markup
view Nothing _ _ = pure "Missing a anti-slash-separated list of urls. Example of a full query: \
    \ /view?flinks=https://my_url1.org\\https://myurl2.org&from=2022-01-28&to=2022-01-30"
view _ Nothing _ = pure "Missing a datestring (format: '2022-01-28')"
view (Just flinks_txt) (Just fr) m_to = do
    env <- ask
    if null flinks then abortWith "No links given or invalid links. \
        \ Make sure to pass a string of valid, anti-slash-separated links, respecting URI encoding."
    else case sequence parsed of
        Nothing -> abortWith
            "Unable to parse values for 'start' and/or 'end'. \
            \ Make sure to use the format as in '2022-28-01,2022-30-01' \
            \ for the 2-days interval between January 28 and January 30, 2022. \
            \ The second parameter is optional."
        Just [f] -> liftIO getCurrentTime >>= \now -> evalDb env (View flinks f now) <&> renderItemsView flinks fr Nothing
        Just [f, t] -> evalDb env (View flinks f t) <&> renderItemsView flinks fr m_to
        _ ->  abortWith "Unable to parse values for 'start' and/or 'end'"
    where
        parsed = foldl' (\acc m -> case m of
            Nothing -> acc
            Just v -> acc ++ [getTime . T.unpack $ v]) [] [Just fr, m_to]
        flinks = case traverse (eitherUrlScheme . decodeText) $ T.splitOn "\\" flinks_txt of
            Left _ -> []
            Right lks -> map renderUrl lks
        abortWith msg = pure . renderItemsView [] mempty Nothing . DbErr . BadQuery $ msg

renderItemsView :: [T.Text] -> T.Text -> Maybe T.Text -> DbRes -> H.Html
renderItemsView _ _ _ (DbErr err) = toHtml $ renderDbError err
renderItemsView _ _ _ (DbView []) = "Your query is valid, but no item matched. Try with different urls and/or a different time window."
renderItemsView flinks from m_to (DbView items) = H.docTypeHtml $ do
    H.head $ do
        H.title "feedfarer_bot/view/results"
        H.address ! Attr.class_ (textValue "author") $ "https://t.me/feedfarer_bot"
    H.body $ do
        H.h3 . toHtml $ "Feeds found (" `T.append` (T.pack . show . length $ flinks) `T.append` ")" 
        H.ul $ forM_ flinks (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
        H.p . toHtml $ "Time window: between " `T.append` from `T.append` " and " `T.append` fromMaybe "now" m_to `T.append` "."
        H.h3 . toHtml $ "Items found (" `T.append` (T.pack . show . length $ items) `T.append` " items)" 
        H.ul $ forM_ ordered_items (\is -> do
            H.p . toHtml $ "(" `T.append` (i_feed_link . head $ is) `T.append` ") items"
            H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is)
        H.p "To get your favorite web feeds posted to your Telegram account, start talking to "
        H.a ! Attr.href (textValue "https://t.me/feedfarer_bot") $ "https://t.me/feedfarer_bot"
    where
        go m _ [] = m
        go m d (i:is) =
            let day = utctDay $ i_pubdate i
                (_, _, d') = toGregorian day
                date = T.pack . formatTime defaultTimeLocale "%A, %B %e, %Y" $ i_pubdate i
                date_p = H.p $ toHtml date
                item_li = H.li $ H.article $ H.a ! Attr.href (textValue $ i_link i) $ toHtml (i_title i)
            in  if d == d' then go (m >> item_li) d' is else go (m >> date_p >> item_li) d' is
        ordered_items = foldl' (\acc i ->
            let flink = i_feed_link i
            in  case Map.lookup flink acc of
                Nothing -> Map.insert flink [i] acc
                Just _ -> Map.update (\vs -> Just $ i:vs) flink acc) Map.empty items
renderItemsView _ _ _ _ = mempty