{-# LANGUAGE DataKinds #-}

module Parser where

import AppTypes
import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.HTTP.Req
import Requests (fetchFeed)
import Text.XML
import Text.XML.Cursor

getTime :: String -> Maybe UTCTime
-- tries to parse input string against various time formats
getTime s = if isNothing first_pass then iso8601ParseM s else first_pass
    where
        first_pass = foldr step Nothing formats
        formats = [rfc822DateFormat, "%Y-%m-%dT%H:%M:%S%z"]
        step f acc = maybe acc pure $ parseTimeM True defaultTimeLocale f s

averageInterval :: [UTCTime] -> Maybe NominalDiffTime
averageInterval [] = Nothing
averageInterval (x:xs) = go [] x (sort xs)
    where
        {-
        nroot :: (Integral a, Floating b) => a -> b -> b 
        n `nroot` x = x ** (1 / fromIntegral n)
        geo_avg acc = realToFrac $ nroot (length acc) (realToFrac . product . map abs $ acc) :: NominalDiffTime
        -}
        avg acc = realToFrac $ floor (sum $ map abs acc) `div` length acc :: NominalDiffTime
        go !acc _ [] = Just $ avg acc
        go !acc tn (tm:ts) =
            let diffed = diffUTCTime tn tm
            in  go (diffed:acc) tm ts

renderAvgInterval :: Maybe NominalDiffTime -> T.Text
renderAvgInterval Nothing = "No average interval was computed."
renderAvgInterval (Just i) =
    let a = if i < 0 then -i else i
        hours = floor a `div` 3600 :: Integer
        (d, h) = go hours 0
    in  (T.pack . show $ d) `T.append` " days, " `T.append` (T.pack . show $ h) `T.append` " hours."
    where
        go !hs !d = if hs < 24 then (d, hs) else go (hs-24 :: Integer) (d+1 :: Integer)

buildFeed :: MonadIO m => FeedType -> Url scheme -> m (Either UserError Feed)
-- tries parsing bytes into a Feed
-- tries as Atom if Rss fails
buildFeed ty url = do
    now <- liftIO getCurrentTime
    fetchFeed url >>= \case
        Nothing -> pure. Left . BadFeedUrl $ renderUrl url
        Just feed -> case parseLBS def feed of
            Left (SomeException _) -> pure . Left . ParseError $ "Unable to parse feed at " `T.append` (T.pack . show $ url)
            Right doc ->
                let root = fromDocument doc
                in case ty of
                    Rss ->
                        let desc = T.concat $ child root >>= child >>= element "description" >>= child >>= content
                            title = T.concat $ child root >>= child >>= element "title" >>= child >>= content
                            link = T.concat $ child root >>= child >>= element "link" >>= child >>= content
                            get_item_data el =
                                Item
                                (T.concat $ child el >>= element "title" >>= child >>= content)
                                (T.concat $ child el >>= element "description" >>= child >>= content)
                                (T.concat $ child el >>= element "link" >>= child >>= content)
                                (fromMaybe now . getTime $ T.unpack (T.concat $ child el >>= element "pubDate" >>= child >>= content))
                            items = map get_item_data $ descendant root >>= element "item"
                            interval = averageInterval $ map i_pubdate items
                            res = Feed
                                {
                                    f_type = Rss,
                                    f_desc = desc,
                                    f_title = title,
                                    f_link = ensureValidFeedLink link,
                                    f_items = items,
                                    f_avgInterval = interval,
                                    f_created = now,
                                    f_lastRefresh = Just now,
                                    f_reads = 0
                                }
                        in  faultyOrValid url res
                    Atom ->
                        let desc = T.concat $ child root >>= laxElement "subtitle" >>= child >>= content
                            title = T.concat $ child root >>= laxElement "title" >>= child >>= content
                            link = T.concat . attribute "href" . head $ child root >>= laxElement "link"
                            get_item_data el =
                                Item
                                (T.concat $ child el >>= laxElement "title" >>= child >>= content)
                                (T.concat $ child el >>= laxElement "content" >>= child >>= content)
                                (T.concat . attribute "href" . head $ child el >>= laxElement "link")
                                (fromMaybe now . getTime $ T.unpack (T.concat $ child el >>= laxElement "updated" >>= child >>= content))
                            items = map get_item_data $ descendant root >>= laxElement "entry"
                            interval = averageInterval $ map i_pubdate items
                            res = Feed
                                {
                                    f_type = Atom,
                                    f_desc = desc,
                                    f_title = title,
                                    f_link = ensureValidFeedLink link,
                                    f_items = items,
                                    f_avgInterval = interval,
                                    f_created = now,
                                    f_lastRefresh = Just now,
                                    f_reads = 0
                                }
                        in  faultyOrValid url res
    where
        ensureValidFeedLink lk
            | T.null lk = lk
            | end3 == "rss" = lk
            | otherwise = if end == "/" then lk `T.append` "rss" else lk `T.append` "/rss"
            where
                end3 = T.takeEnd 3 lk
                end = T.singleton . T.last $ lk
        faultyOrValid u res =
            if faultyFeed res
            then pure . Left . ParseError $ T.pack . show . renderUrl $ u
            else pure . Right $ res
        faultyFeed f =
            let predicates = [any T.null [f_desc f, f_title f, f_link f], null . f_items $ f]
            in  or predicates

eitherUrlScheme :: T.Text -> Either UserError (Url 'Https)
-- tries to make a valid Url Scheme from the given string
eitherUrlScheme s
    | T.null s = Left . BadInput $ s
    | head split /= "https:" = Left . BadInput $ s
    | length body >= 2 = Right toScheme
    | otherwise = Left . BadInput $ "Unable to parse this input." `T.append` s
    where
        s' = if T.last s == T.last "/" then T.dropEnd 1 s else s
        split = T.splitOn "/" s'
        body = drop 2 split
        (host:rest) = body
        toScheme = foldl' (/:) (https host) rest

getFeedFromUrlScheme :: MonadIO m => Url scheme -> m (Either T.Text Feed)
getFeedFromUrlScheme scheme = buildFeed Rss scheme >>= \case
    Left _ -> buildFeed Atom scheme >>= \case
        Left err -> pure . Left .  renderUserError $ err
        Right feed -> finish_successfully feed
    Right feed -> finish_successfully feed
    where
        finish_successfully feed = pure . Right $ feed

getFeedFromHref :: MonadIO m => T.Text -> m (Either T.Text Feed)
-- parses url and tries to fetch & build  a new feed to the 'store'
getFeedFromHref url = case eitherUrlScheme url of
    Left err -> pure . Left .  renderUserError $ err
    Right urlScheme -> getFeedFromUrlScheme urlScheme

rebuildFeed :: MonadIO m => T.Text -> m (Either T.Text Feed)
-- updates one singular feed and returns the result to the caller
rebuildFeed key = case eitherUrlScheme key of
    Left _ -> pure . Left $ key
    Right url -> buildFeed Rss url >>= \case
        Left _ -> buildFeed Atom url >>= \case
            Left _ ->
                let faulty_key = T.pack . show $ url
                in pure $ Left faulty_key
            Right feed -> done feed
        Right feed -> done feed
  where done feed = liftIO getCurrentTime >>= \now -> pure . Right $ feed {f_lastRefresh = Just now}