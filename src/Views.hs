module Views where

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
        Just [f] -> liftIO getCurrentTime >>= \now -> evalDb env (View flinks f now) <&> renderHtml flinks fr Nothing
        Just [f, t] -> evalDb env (View flinks f t) <&> renderHtml flinks fr m_to
        _ ->  abortWith "Unable to parse values for 'start' and/or 'end'"
    where
        parsed = foldl' (\acc m -> case m of
            Nothing -> acc
            Just v -> acc ++ [getTime . T.unpack $ v]) [] [Just fr, m_to]
        flinks = case traverse (eitherUrlScheme . decodeText) $ T.splitOn "\\" flinks_txt of
            Left _ -> []
            Right lks -> map renderUrl lks
        abortWith msg = pure . renderHtml [] mempty Nothing . DbErr . BadQuery $ msg

renderHtml :: [T.Text] -> T.Text -> Maybe T.Text -> DbRes -> H.Html
renderHtml _ _ _ (DbView []) = "Your query is valid, but no item matched. Try with different urls and/or a different time window."
renderHtml flinks from m_to (DbView items) = H.docTypeHtml $ do
    H.head $ do
        H.title "@feedfarer_bot::view::results"
    H.body $ do
        H.h3 "Query"
        H.p "Feeds queried about"
        H.ul $ forM_ flinks (\fl -> H.li $ H.a ! Attr.href (textValue fl) $ toHtml fl)
        H.p "Time window"
        H.p . toHtml $ "between " `T.append` from `T.append` " and " `T.append` fromMaybe "now" m_to `T.append` "."
        H.h3 "Results"
        H.ul $ forM_ ordered_items (\is -> do
            H.p (toHtml . i_feed_link . head $ is)
            H.ul $ go mempty 0 $ sortOn (Down . i_pubdate) is)
        H.p "To get your favorite web feeds posted to your Telegram account, start talking to https://t.me/feedfarer_bot !"
    where
        go m _ [] = m
        go m d (i:is) =
            let day = utctDay $ i_pubdate i
                (_, _, d') = toGregorian day
                date = T.pack . formatTime defaultTimeLocale "%A, %B %e, %Y" $ i_pubdate i
                date_p = H.p $ toHtml date
                item_li = do
                    H.li $ H.a ! Attr.href (textValue $ i_link i) $ toHtml (i_title i)
                    H.p (toHtml . T.take 120 . i_desc $ i)
            in  if d == d' then go (m >> item_li) d' is else go (m >> date_p >> item_li) d' is
        ordered_items = foldl' (\acc i ->
            let flink = i_feed_link i
            in  case Map.lookup flink acc of
                Nothing -> Map.insert flink [i] acc
                Just _ -> Map.update (\vs -> Just $ i:vs) flink acc) Map.empty items
renderHtml _ _ _ (DbErr err) = toHtml $ renderDbError err
renderHtml _ _ _ _ = mempty