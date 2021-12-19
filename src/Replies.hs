{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Replies (render, Reply(..), reply, toReply, FromContents(..)) where

import AppTypes
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Foldable (foldl')
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import qualified Data.Text as T
import Parser (renderAvgInterval)
import Requests (reqSend_)
import TgramOutJson (ChatId, Outbound (OutboundMessage))
import qualified Data.Set as S
import Data.Time (toGregorian, UTCTime (utctDay), formatTime, defaultTimeLocale)

escapeWhere :: T.Text -> [T.Text] -> T.Text
escapeWhere txt suspects =
    T.foldl' (\t c ->
        let c' = T.singleton c
        in  if c' `elem` suspects
            then t `T.append` "\\" `T.append` T.singleton c
            else t `T.append` T.singleton c
    ) mempty txt

mkdSingles :: [T.Text]
mkdSingles = ["_", "*", "`"]

-- mkdPairs :: [T.Text]
-- mkdPairs = ["[", "]"]

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
                ("Avg. interval between items", renderAvgInterval f_avgInterval),
                ("Created on,", T.pack . show $ f_created),
                ("Last refresh", maybe "None" (T.pack . show) f_lastRefresh),
                ("Total reads", T.pack . show $ f_reads)
            ]

instance Renderable SubChat where
    render SubChat{..} = T.intercalate "\n" $ map (\(k, v) -> k `T.append` ": " `T.append` v)
        [   ("Chat_Id", T.pack . show $ sub_chatid),
            ("Status", if sub_is_paused then "paused" else "active"),
            ("Last notification", T.pack . show $ sub_last_notification),
            ("Feeds subscribed to", T.intercalate "," $ S.toList sub_feeds_links),
            ("Blacklist", T.pack . show . filters_blacklist . settings_filters $ sub_settings),
            ("Batch", T.pack . show . settings_batch $ sub_settings),
            ("Batch size", T.pack . show . settings_batch_size $ sub_settings),
            ("Batch interval", T.pack . show . settings_batch_interval $ sub_settings)
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
toHrefEntities Nothing tag link =
    let tag' = "[" `T.append` tag `T.append` "]"
        link' = "(" `T.append` link `T.append` ")"
    in  tag' `T.append` link'
toHrefEntities (Just counter) tag link =
    let counter' = T.pack . show $ counter
        tag' = " [" `T.append` tag `T.append` "]"
        link' = "(" `T.append` link `T.append` ")"
    in  counter' `T.append` tag' `T.append` link'

data FromContents a where
    FromFeedDetails :: Feed -> FromContents a
    FromChatFeeds :: SubChat -> [Feed] -> FromContents a
    FromFeedItems :: Feed -> FromContents a
    FromFeedsItems :: [(Feed, [Item])] -> FromContents a
    FromFeedLinkItems :: [(FeedLink, [Item])] -> FromContents a
    FromStart :: FromContents a

toReply :: FromContents a -> Reply
toReply FromStart = MarkdownReply renderCmds
toReply (FromChatFeeds _ feeds) =
    let start = ("Feeds subscribed to (#, link):\n", 1 :: Int)
        step = (\(!txt, !counter) f ->
            let link = f_link f
                title = f_title f
                rendered = toHrefEntities (Just counter) title link
            in  (T.append txt rendered `T.append` "\n", counter + 1))
    in  MarkdownReply . fst $ foldl' step start feeds
toReply (FromFeedDetails feed) = PlainReply $ render feed
toReply (FromFeedItems f) =
    let rendered_items =
            render .
            sortBy (comparing $ Down . i_pubdate) .
            f_items $ f
    in  MarkdownReply rendered_items
toReply (FromFeedsItems items) =
    let step = (\acc (!f, !i) -> acc `T.append` "Last item(s) for " `T.append`
            escapeWhere (f_title f) mkdSingles `T.append` ":\n"
            `T.append` render i
            `T.append` "\n\n")
    in  MarkdownReply $ foldl' step mempty items
toReply (FromFeedLinkItems flinkitems) =
    let step = ( \acc (!f, !items) -> acc `T.append` "New item(s) for " `T.append` f `T.append` ":\n" `T.append` render items)
    in  MarkdownReply $ foldl' step mempty flinkitems

renderCmds :: T.Text
renderCmds = T.intercalate "\n"
    [
        "/fresh, /f `<n>`: Get all the most recent items (less than n-days old, where n is the argument) from all the feeds the chat is subscribed.\n",
        "/help, /start:  Get the list of commands this bot answers to.\n",
        "/info `<url or #>`: Get information about the feed at the url or # passed as argument. Does not require that the calling chat has subscribed as long as another chat has. Example:\n- `/info 2`, `/info https://www.compositional.fm/rss`.\n",
        "/items, /i `<url or #>`: Get the most recent items from the feed at the url or #s passed as argument, if any. Examples:\n- `/items 2`\n-`/i https://www.compositional.fm/rss`.\n",
        "/list, /l: Get all the urls and #s of the feeds to which the chat is subscribed to, if any.\n",
        "/pause, /p, /resume:  Whether the bot is allowed to send notification messages to the chat.\n",
        "/purge (*chat admins only*): Make the bot and associated database forget entirely about this chat.\n",
        "/search, /se `<space-separated keywords>`: Search all items of all feeds the current chat is subscribed to. Example:\n- `/se cheap cloud host`.\n", 
        "/settings, /set (*chat admins only*) `optional <line-separated key-value pairs>`: Get the settings for the referenced chat (version without argument) or set the settings for this chat. Example:\n `/settings blacklist: Trump, bitcoin, crypto\nbatch: true\n,batch_size: 10\nbatch_interval: 9000`\n",
        "/sub, /s (*chat admins only*) `<list of comma-separated full url addresses>`: Subscribe the chat to the feeds -- if they exist -- passed as argument. Examples:\n- `/s 1 2 3`\n- `/sub https://www.compositional.fm/rss https://www.blabla.org/rss`.\n",
        "/unsub (*chat admins only*) `<list of 1-space-separated full url addresses>`: Unsubscribe from all the feeds passed as argument, if indeed they exits. Examples:\n- `/u 1 2 3`\n- `/unsub https://www.compositional.fm/rss https://www.blabla.org/`."
    ] `T.append` "\n\nCheck out our channel for more info: https://t.me/feedfarer"

reply :: MonadIO m => BotToken -> ChatId -> Reply -> m ()
reply tok cid rep = liftIO $ send `catch` handler
    where
        non_empty txt = if T.null txt then "No result for this command." else txt
        send = case rep of
            MarkdownReply txt -> void . reqSend_ tok "sendMessage" $ OutboundMessage cid (non_empty txt) (Just "Markdown")
            PlainReply txt -> void . reqSend_ tok "sendMessage" $ OutboundMessage cid (non_empty txt) Nothing
        handler (SomeException e) = putStrLn $ "Found exception in trying to reply: " ++ show e
