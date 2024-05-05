module Feeds where

import Control.Exception (SomeException (SomeException))
import Control.Monad.Reader
import Data.Foldable (Foldable (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.HTTP.Req
import Requests
import Text.XML
import Text.XML.Cursor
import Types
import Utils (averageInterval, mbTime)

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
