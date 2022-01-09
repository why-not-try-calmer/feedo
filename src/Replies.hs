{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Replies (render, Reply(..), toReply, FromContents(..)) where

import AppTypes
import Data.Foldable (foldl')
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), defaultTimeLocale, formatTime, toGregorian)
import Parser (renderAvgInterval)
import Utils (secsToReadable)

escapeWhere :: T.Text -> [T.Text] -> T.Text
escapeWhere txt suspects =
    T.foldl' (\t c ->
        let c' = T.singleton c
        in  if c' `elem` suspects
            then t `T.append` "\\" `T.append` T.singleton c
            else t `T.append` T.singleton c
    ) mempty txt

skipWhere :: T.Text -> [T.Text] -> T.Text
skipWhere txt suspects = T.filter (\t -> T.singleton t `notElem` suspects) txt

mkdSingles :: [T.Text]
mkdSingles = ["_", "*", "`"]

mkdDoubles :: [T.Text]
mkdDoubles = ["[", "]"]

class Renderable e where
    render :: e -> T.Text

instance Renderable Feed where
    render Feed{..} =
        T.intercalate "\n" $ map (\(k, v) -> k `T.append` ": " `T.append` v)
            [
                ("Url", f_link),
                ("Type", T.pack $ show f_type),
                ("Title", f_desc),
                ("Current items", T.pack . show $ length f_items),
                ("Avg. interval between items", renderAvgInterval f_avg_interval),
                ("Last refresh", maybe "None" (T.pack . show) f_last_refresh),
                ("Total reads", T.pack . show $ f_reads)
            ]

instance Renderable SubChat where
    render SubChat{..} =
        let adjust c = if c == "0" then "00" else c
            at = case batch_at . settings_batch_interval $ sub_settings of
                Nothing -> mempty
                Just ts -> "every day at " `T.append` foldl' (\s (!h, !m) ->
                        let body = (T.pack . show $ h) `T.append` ":" `T.append` (adjust . T.pack . show $ m)
                            s' = if s == mempty then s else s `T.append` ", "
                        in  s' `T.append` body) mempty ts
            every = maybe
              mempty secsToReadable
              (batch_secs . settings_batch_interval $ sub_settings)
            blacklist =
                let bs = filters_blacklist . settings_filters $ sub_settings
                in  if null bs then "none" else T.intercalate "," bs
        in  T.intercalate "\n" $ map (\(k, v) -> k `T.append` ": " `T.append` v)
            [   ("Chat id", T.pack . show $ sub_chatid),
                ("Status", if settings_is_paused sub_settings then "paused" else "active"),
                ("Last notification", maybe "none" (T.pack . show) sub_last_notification),
                ("Next notication", maybe "none scheduled" (T.pack . show) sub_next_notification),
                ("Feeds subscribed to", T.intercalate ", " $ S.toList sub_feeds_links),
                ("Blacklist", blacklist),
                ("Batch size", (T.pack . show . settings_batch_size $ sub_settings) `T.append` " items"),
                ("Batch at", if T.null at then " not defined" else at),
                ("Batch every", if T.null every then " not defined" else every),
                ("Webview", if settings_webview sub_settings then "enabled" else "disabled"),
                ("Pin new update", if settings_pin sub_settings then "enabled" else "disabled"),
                ("Remove bot messages", if settings_pin sub_settings then "enabled" else "disabled")
            ]

instance Renderable [Item] where
    render = fst . foldl' step (mempty, 0)
        where
        step (!str, !d) i =
            let day = utctDay $ i_pubdate i
                (_, _, d') = toGregorian day
                date = T.pack . formatTime defaultTimeLocale "%A, %B %e, %Y" $ i_pubdate i
            in  if d == d'
                then (str `T.append` finish (i_title i) (i_link i), d)
                else (str `T.append` "_" `T.append` date `T.append` "_ \n" `T.append`
                    finish (i_title i) (i_link i), d')
        finish title link = "- " `T.append` toHrefEntities Nothing title link `T.append` "\n"

toHrefEntities :: Maybe Int -> T.Text -> T.Text -> T.Text
toHrefEntities mbcounter tag link =
    let tag' = "[" `T.append` skipWhere tag (mkdSingles ++ mkdDoubles) `T.append` "]"
        link' = "(" `T.append` link `T.append` ")"
    in  case mbcounter of
        Nothing -> tag' `T.append` link'
        Just c ->
            let counter = T.pack . show $ c
            in  counter `T.append` tag' `T.append` link'

data FromContents a where
    FromChatFeeds :: SubChat -> [Feed] -> FromContents a
    FromFeedDetails :: Feed -> FromContents a
    FromFeedItems :: Feed -> FromContents a
    FromFeedLinkItems :: [(FeedLink, [Item])] -> FromContents a
    FromFeedsItems :: [(Feed, [Item])] -> FromContents a
    FromSearchRes :: [Item] -> FromContents a
    FromStart :: FromContents a

toReply :: FromContents a -> Maybe Settings -> Reply
toReply FromStart _ = ChatReply renderCmds True False False False
toReply (FromChatFeeds _ feeds) _ =
    let start = ("Feeds subscribed to (#, link):\n", 1 :: Int)
        step = (\(!txt, !counter) f ->
            let link = f_link f
                title = f_title f
                rendered = toHrefEntities (Just counter) title link
            in  (T.append txt rendered `T.append` "\n", counter + 1))
        payload = fst $ foldl' step start feeds
    in  ChatReply payload True False False False
toReply (FromFeedDetails feed) _ = ServiceReply $ render feed
toReply (FromFeedItems f) _ =
    let rendered_items =
            render .
            sortBy (comparing $ Down . i_pubdate) .
            f_items $ f
    in  ChatReply rendered_items True False False False
toReply (FromFeedsItems items) mbs =
    let step = (\acc (!f, !i) -> acc `T.append` "*" `T.append` f_title f `T.append` "*:\n"
            `T.append` (render . sortBy (comparing $ Down . i_pubdate) $ i)
            `T.append` "\n")
        payload = foldl' step mempty items
    in  case mbs of
        Just s -> ChatReply {
            reply_contents = payload,
            reply_markdown = True,
            reply_pin_on_send = settings_pin s,
            reply_webview =  settings_webview s,
            reply_clean_behind = False
        }
        Nothing -> ServiceReply payload
toReply (FromFeedLinkItems flinkitems) _ =
    let step = ( \acc (!f, !items) -> acc `T.append` "New item(s) for " `T.append` escapeWhere f mkdSingles `T.append` ":\n" `T.append` render items)
        payload = foldl' step mempty flinkitems
    in  ChatReply payload True False False False
toReply (FromSearchRes items) _ = ChatReply (render items) True False False False

renderCmds :: T.Text
renderCmds = T.intercalate "\n"
    [
        "/channel_settings, /cset `optional <linebreak + key:value single lines>` (*admins only with argument*)\n",
        "/feed, /f `<url or #>`\n",
        "/fresh `<n>`\n",
        "/help, /start\n",
        "/items, /i `<url or #>`\n",
        "/link, /link_channel `<channel id>`\n",
        "/list, /l\n",
        "/pause, /p\n",
        "/purge (*chat admins only*)\n",
        "/reset (*chat admins only*)\n",
        "/resume\n",
        "/search, /se `<space-separated keywords>`\n",
        "/settings, /set `optional <linebreak + key:value single lines>` (*admins only with argument*)\n",
        "/sub, /s (*chat admins only*) `<list of comma-separated full url addresses>`\n",
        "/sub_channel, /csub `<channel id> <list of comma-separated url addresses>`",
        "/unsub (*chat admins only*) `<list of 1-space-separated full url addresses>`\n"
    ] `T.append` "\n\nCheck out this [link](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md) for more details."