module Views where

import AppTypes
import Control.Monad.Reader (MonadIO, ask, forM_)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (Down))
import qualified Data.Text as T
import Database (Db (evalDb))
import Network.HTTP.Req (renderUrl)
import Parser (eitherUrlScheme, getTime)
import Text.Blaze (Markup, textValue, (!))
import Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as Attr

view :: MonadIO m => Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> App m Markup
view Nothing _ _ = pure "Missing a comma-separated list of urls"
view _ Nothing _ = pure "Missing a datestring (format: '2022-01-28')"
view (Just flinks_txt) (Just fr) to = do
    env <- ask
    if null flinks then abortWith "No links given or invalid links. Make sure to pass a string of valid, comma-separated links."
    else
        case sequence parsed of
        Nothing -> abortWith
            "Unable to parse values for 'start' and/or 'end'. \
            \ Make sure to use the format as in '2022-28-01,2022-30-01' \
            \ for the 2-days interval between January 28 and January 30, 2022. \
            \ The second parameter is optional."
        Just [f] -> evalDb env (View flinks f Nothing) <&> renderHtml (h2_txt fr Nothing)
        Just [f, t] -> evalDb env (View flinks f (Just t)) <&> renderHtml (h2_txt fr (Just . T.pack . show $ t))
        _ ->  abortWith "Unable to parse values for 'start' and/or 'end'"
    where
        parsed = foldl' (\acc m -> case m of
            Nothing -> acc
            Just v -> acc ++ [getTime . T.unpack $ v]) [] [Just fr, to]
        flinks = case traverse eitherUrlScheme $ T.splitOn "," flinks_txt of
            Left _ -> []
            Right lks -> map renderUrl lks
        abortWith msg = pure . renderHtml mempty . DbErr . BadQuery $ msg
        h2_txt f Nothing = T.intercalate ", " flinks `T.append` " between " `T.append` f `T.append` " and " `T.append` "now"
        h2_txt f (Just t) = T.intercalate ", " flinks `T.append` " between " `T.append` T.intercalate " and " [f, t]
renderHtml :: T.Text -> DbRes -> H.Html
renderHtml query_txt (DbView []) = toHtml $ "No item found for these query parameters"
    `T.append` query_txt
renderHtml query_txt (DbView items) = H.docTypeHtml $ do
    H.head $ do
        H.title "List of feeds and items"
    H.body $ do
        H.h2 . toHtml $ query_txt
        H.ul $ forM_ ordered_items (\is -> do
            H.p (toHtml . i_feed_link . head $ is)
            H.ul $ forM_ (sortOn (Down . i_pubdate) is) (\i -> H.li $ H.a ! Attr.href (textValue $ i_link i) $ toHtml (i_title i)))
        H.p "To get your favorite web feeds posted to your Telegram account, start talking to https://t.me/feedfarer_bot !"
    where
        ordered_items = foldl' (\acc i ->
            let flink = i_feed_link i
            in  case Map.lookup flink acc of
                Nothing -> Map.insert flink [i] acc
                Just _ -> Map.update (\vs -> Just $ i:vs) flink acc) Map.empty items
renderHtml _ (DbErr err) = toHtml $ renderDbError err
renderHtml query_txt _ = toHtml $ "No item found for these query parameters"
    `T.append` query_txt