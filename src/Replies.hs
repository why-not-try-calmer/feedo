{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Replies (render, Reply(..), toReply, ToReply(..)) where

import AppTypes
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Ord (Down (Down))
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
            every = maybe mempty secsToReadable (batch_every_secs . settings_batch_interval $ sub_settings)
            blacklist =
                let bl = S.toList $ match_blacklist . settings_word_matches $ sub_settings
                in  if null bl then "none" else T.intercalate "," bl
            searches =
                let se = S.toList $ match_searchset . settings_word_matches $ sub_settings
                in  if null se then "none" else T.intercalate "," se
            only_search_results = 
                let sr = S.toList $ match_only_search_results . settings_word_matches $ sub_settings
                in  if null sr then "none" else T.intercalate "," sr
        in  T.intercalate "\n" $ map (\(k, v) -> k `T.append` ": " `T.append` v)
            [   ("Chat id", T.pack . show $ sub_chatid),
                ("Status", if settings_paused sub_settings then "paused" else "active"),
                ("Feeds subscribed to", T.intercalate ", " $ S.toList sub_feeds_links),
                ("Batch every", if T.null every then " not defined" else every),
                ("Batch at", if T.null at then " not defined" else at),
                ("Batch size", (T.pack . show . settings_batch_size $ sub_settings) `T.append` " items"),
                ("Last notification", maybe " none" (T.pack . show) sub_last_notification),
                ("Next notication", maybe " none scheduled" (T.pack . show) sub_next_notification),
                ("Blacklist", blacklist),
                ("Searches", searches),
                ("Ignore unless search results", only_search_results),
                ("Webview", if settings_disable_web_view sub_settings then " disabled" else " enabled"),
                ("Pin new updates", if settings_pin sub_settings then " enabled" else " disabled")
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

instance Renderable ([T.Text], [Item]) where
    render (keys, items) = "Results from scheduled search with keywords " 
        `T.append` T.intercalate ", " keys
        `T.append` ":\n" 
        `T.append` render items
        `T.append` "\nSend '/set search: false' to disable search updates."

toHrefEntities :: Maybe Int -> T.Text -> T.Text -> T.Text
toHrefEntities mbcounter tag link =
    let tag' = "[" `T.append` skipWhere tag (mkdSingles ++ mkdDoubles) `T.append` "]"
        link' = "(" `T.append` link `T.append` ")"
    in  case mbcounter of
        Nothing -> tag' `T.append` link'
        Just c ->
            let counter = T.pack . show $ c
            in  counter `T.append` ") " `T.append` tag' `T.append` link'

defaultReply :: T.Text -> Reply
defaultReply payload = ChatReply {
    reply_contents = payload,
    reply_markdown = True,
    reply_pin_on_send = False,
    reply_disable_webview = False
    }

toReply :: ToReply -> Maybe Settings -> Reply
toReply FromChangelog _ = ServiceReply "check out https://t.me/feedfarer"
toReply (FromChatFeeds _ feeds) _ =
    let start = ("Feeds subscribed to (#, link):\n", 1 :: Int)
        step = (\(!txt, !counter) f ->
            let link = f_link f
                title = f_title f
                rendered = toHrefEntities (Just counter) title link
            in  (T.append txt rendered `T.append` "\n", counter + 1))
        payload = fst $ foldl' step start feeds
    in  defaultReply payload
toReply (FromFeedDetails feed) _ = ServiceReply $ render feed
toReply (FromFeedItems f) _ =
    let rendered_items =
            render .
            sortOn (Down . i_pubdate) .
            f_items $ f
    in  defaultReply rendered_items
toReply (FromFeedsItems items) mbs =
    let step = (\acc (!f, !i) -> acc `T.append` "*" `T.append` f_title f `T.append` "*:\n"
            `T.append` (render . sortOn (Down . i_pubdate) $ i)
            `T.append` "\n")
        payload = foldl' step mempty items
    in  case mbs of
        Just s -> ChatReply {
            reply_contents = payload,
            reply_markdown = True,
            reply_pin_on_send = settings_pin s,
            reply_disable_webview = settings_disable_web_view s
        }
        Nothing -> ServiceReply payload
toReply (FromFeedLinkItems flinkitems) _ =
    let step = ( \acc (!f, !items) -> acc `T.append` "New item(s) for " `T.append` escapeWhere f mkdSingles `T.append` ":\n" `T.append` render items)
        payload = foldl' step mempty flinkitems
    in  defaultReply payload
toReply (FromSearchRes (keys, items)) _ = ChatReply (render (keys, items)) True True False
toReply FromStart _ = ChatReply renderCmds True True False

renderCmds :: T.Text
renderCmds = T.intercalate "\n"
    [
        "/changelog: link to the changelog", 
        "/feed `<# or url>`: show info about the subscribed to feed",
        "/fresh `<n>`: display n-old items, in number of days",
        "/help: show these commands",
        "/items `<# or url>`: display all the items fetched from the referenced feed",
        "/list `<optional: channel id`: list all the feeds this chat or that channel is subscribed to",
        "/migrate `<optional: chat_id of the origin> <chat_id of the destination>`: migrate this chat's settings, or the settings of the channel at the origin, to the destination.",
        "/pause `<optional: channel id>`: stop posting updates to this chat or to that channel",
        "/purge `<optional: channel id>`: delete all data about this chat or that channel",
        "/reset `<optional: channel id>`: set this chat's (or that channels') settings to their default values",
        "/resume `<optional: channel id>`: resume updates to this chat or to that channel",
        "/search `<term1 term2...>`: search all items across all feeds for the given keywords",
        "/set `<optional: channel id> <optional: linebreak + key:value pairs>` view or edit this chat's or that channel's settings",
        "/sub `<optional: channel id> <url1 url2...>`: subscribe this chat or that channel to the target feed(s)",
        "/unsub `<optional: channel id> <url url2...>`: unsubscribe this chat or that channel from the target feed(s)"
    ] `T.append` "\n\nCheck out this [document](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md) for more details."

{-
changelog - recent changes to the bot
feed - <# or url> info about the feed
fresh - <n> n-old items, in number of days
help - these commands
items - <# or url> all the items fetched from the reference feed 
list - <optional: channel_id> all the feeds the chat/channel is subscribed to
migrate - <optional: chat_id of the origin> <chat_id of the destination>: migrate this chat's settings, or the settings of the channel at the origin, to the destination.
pause - <optional: channel_id> stop posting updates to the chat/channel
purge - <optional: channel_id> erase the database from all data about the chat/channel
reset - <optional: channel_id> set the chat/channel's settings to their default values
resume - <optional: channel_id> resume updates to the chat/channel
search - <term1 term2...> search all items across all feeds for the given keywords 
set - <optional: channel_id> <optional: linebreak + key:value pairs> view or edit the chat/channel's settings
sub - <optional: channel_id> <url1 url2...> subscribe the chat/channel to the target feed(s)
unsub - <optional: channel_id> <url url2...> unsubscribe the chat/channel from the target feed(s)
-}
