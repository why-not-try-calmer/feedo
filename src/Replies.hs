{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Replies (defaultReply, mkdSingles, mkdDoubles, render, Reply(..), mkReply, Replies(..), mkViewUrl, mkDigestUrl) where
import AppTypes
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), defaultTimeLocale, formatTime, toGregorian)
import Network.URI.Encode (encodeText)
import Utils (nomDiffToReadable, renderAvgInterval, utcToYmd, utcToYmdHMS)

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

mkViewUrl ::[Item] -> Maybe T.Text
mkViewUrl [] = Nothing
mkViewUrl items =
    let base_url = "https://feedfarer-webapp.azurewebsites.net/view"
        (ts, flinks) = foldl' step ([],[]) items
        flinks_txt = "?flinks=" `T.append` encodeText (T.intercalate "\\" flinks)
        from_to_txt =
            "&from=" `T.append`
            (utcToYmd . head $ ts) `T.append`
            "&to=" `T.append`
            (utcToYmd . last $ ts)
    in  if length ts < 2 || null flinks
        then Nothing
        else Just $ base_url `T.append` flinks_txt `T.append` from_to_txt
    where
        step ([], !fs) i = ([i_pubdate i], i_feed_link i:fs)
        step (x:xs, !fs) i =
            let pub_i = i_pubdate i
                flink = i_feed_link i
                fs' = if flink `notElem` fs then flink:fs else fs
            in  if pub_i < x then (pub_i:x:xs, fs') else (x:pub_i:xs, fs')

mkDigestUrl :: T.Text -> T.Text -> T.Text
mkDigestUrl base _id = base `T.append` "/digests/" `T.append` _id

mkAccessSettingsUrl :: T.Text -> T.Text -> T.Text
mkAccessSettingsUrl base token = base `T.append` "/settings/index.html?access_token=" `T.append` token

intoTimeLine :: [Item] -> T.Text
intoTimeLine = fst . foldl' step (mempty, 0)
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
                ("Last refresh", maybe "None" utcToYmdHMS f_last_refresh),
                ("Total reads", T.pack . show $ f_reads)
            ]

instance Renderable SubChat where
    render SubChat{..} =
        let adjust c = if c == "0" then "00" else c
            at = case digest_at . settings_digest_interval $ sub_settings of
                Nothing -> mempty
                Just ts -> foldl' (\s (!h, !m) ->
                    let body = (T.pack . show $ h) `T.append` ":" `T.append` (adjust . T.pack . show $ m)
                        s' = if s == mempty then s else s `T.append` ", "
                    in  s' `T.append` body) mempty ts
            every_txt =
                let k = "Digest step (time between two digests)"
                in  case digest_every_secs . settings_digest_interval $ sub_settings of
                        Nothing -> (mempty, mempty)
                        Just e -> if e == 0 then (k, "not set") else (k, nomDiffToReadable e)
            blacklist =
                let bl = S.toList $ match_blacklist . settings_word_matches $ sub_settings
                in  if null bl then "none" else T.intercalate ", " bl
            searches =
                let se = S.toList $ match_searchset . settings_word_matches $ sub_settings
                in  if null se then "none" else T.intercalate ", " se
            only_search_results =
                let sr = S.toList $ match_only_search_results . settings_word_matches $ sub_settings
                in  if null sr then "none" else T.intercalate ", " sr
            rendered =
                let mapper = T.intercalate "\n" . map (\(k, v) -> k `T.append` ": " `T.append` v)
                    status_part = mapper
                        [
                            ("Chat id", T.pack . show $ sub_chatid),
                            ("Status", if settings_paused sub_settings then "paused" else "active"),
                            ("Feeds subscribed to", T.intercalate ", " $ S.toList sub_feeds_links)
                        ]
                    digest_part = mapper
                        [
                            maybe (mempty,mempty) (\t -> ("First digest", utcToYmd t)) $ settings_digest_start sub_settings,
                            ("Digest time(s)", if T.null at then "none" else at),
                            every_txt,
                            ("Digest size", (T.pack . show . settings_digest_size $ sub_settings) `T.append` " items"),
                            ("Digest collapse", maybe "false" (\v -> if v == 0 then "false" else T.pack . show $ v) $ settings_digest_collapse sub_settings),
                            ("Digest title", settings_digest_title sub_settings),
                            ("Last digest", maybe "none" utcToYmdHMS sub_last_digest),
                            ("Next digest", maybe "none scheduled yet" utcToYmdHMS sub_next_digest),
                            ("Follow", if settings_follow sub_settings then "true" else "false"),
                            ("Pagination", if settings_pagination sub_settings then "true" else "false"),
                            ("Display 'share link' button", if settings_share_link sub_settings then "true" else "false")
                        ]
                    search_part = mapper
                        [
                            ("Blacklist", blacklist),
                            ("Feeds ignored unless a search keyword matches", only_search_results),
                            ("Search keywords", searches)
                        ]
                    telegram_part = mapper
                        [
                            ("Webview", if settings_disable_web_view sub_settings then "false" else "true"),
                            ("Pin new updates", if settings_pin sub_settings then "true" else "false")
                        ]
                in  status_part
                    `T.append` "\n--\n"
                    `T.append` digest_part
                    `T.append` "\n--\n"
                    `T.append` search_part
                    `T.append` "\n--\n"
                    `T.append` telegram_part
            in T.append rendered "\n\nToo many settings? Check out the docs for examples: https://github.com/why-not-try-calmer/feedfarer2/blob/master/SETTINGS_EXAMPLES.md"

instance Renderable [Item] where
    render = intoTimeLine

instance Renderable ([(FeedLink, [Item])], Int) where
    render (!f_items, !collapse_size) =
        let out_of i
                | collapse_size < length i = 
                    " (" `T.append` (T.pack . show $ collapse_size)
                    `T.append` " out of "
                    `T.append` (T.pack . show . length $ i)
                    `T.append` " new):\n"
                | otherwise = ":\n"
            into_list acc (!t, !i) = acc
                `T.append` "\n*| "
                `T.append` t
                `T.append` "*:\n"
                `T.append` (render . take 30 . sortOn (Down . i_pubdate) $ i)
            into_folder acc (!t, !i) = acc
                `T.append` "\n*| "
                `T.append` t
                `T.append` "*"
                `T.append` out_of i
                `T.append` (render . take collapse_size . sortOn (Down . i_pubdate) $ i)
        in  foldl' (if collapse_size == 0 then into_list else into_folder) mempty f_items

instance Renderable (S.Set T.Text, [SearchResult]) where
    render (keys, search_res) =
        let items = map (\SearchResult{..} -> Item sr_title mempty sr_link sr_feedlink sr_pubdate) search_res
        in  "Results from your search with keywords "
                `T.append` T.intercalate ", " (map (`escapeWhere` mkdSingles) . S.toList $ keys)
                `T.append` ":\n"
                `T.append` render items

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
    reply_disable_webview = False,
    reply_pagination = True,
    reply_permalink = Nothing
    }

mkReply :: Replies -> Reply
mkReply (FromAdmin base hash) = ServiceReply . mkAccessSettingsUrl base $ hash
mkReply FromChangelog = ServiceReply "check out https://t.me/feedfarer"
mkReply (FromChatFeeds _ feeds) =
    let start = ("Feeds subscribed to (#, link):\n", 1 :: Int)
        step = (\(!txt, !counter) f ->
            let link = f_link f
                title = f_title f
                rendered = toHrefEntities (Just counter) title link
            in  (T.append txt rendered `T.append` "\n", counter + 1))
        payload = fst $ foldl' step start feeds
    in  defaultReply payload
mkReply (FromChat chat confirmation) = ServiceReply $ confirmation `T.append` render chat
mkReply (FromFeedDetails feed) = ServiceReply $ render feed
mkReply (FromFeedItems f) =
    let rendered_items =
            render .
            take 30 .
            sortOn (Down . i_pubdate) .
            f_items $ f
    in  defaultReply rendered_items
mkReply (FromFollow fs _) = 
    let fitems = map (\f -> (f_title f, f_items f)) fs
        payload = "New 'follow update'.\n--\n" 
            `T.append` render (fitems, 0 :: Int)     
    in  ChatReply payload True True False True Nothing
mkReply (FromDigest fs mb_link s) =
    let fitems = map (\f -> (f_title f, f_items f)) fs
        -- pagination preempting collapse when both are enabled and 
        -- collapsing would have occurred  
        collapse = maybe 0 (\v -> if settings_pagination s then 0 else v) $ settings_digest_collapse s 
        header = settings_digest_title s `T.append` "\n--" 
        body = render (fitems, collapse) 
        payload = header `T.append` body
    in  ChatReply {
            reply_contents = payload,
            reply_markdown = True,
            reply_pin_on_send = settings_pin s,
            reply_disable_webview = settings_disable_web_view s,
            reply_pagination = settings_pagination s,
            reply_permalink = mb_link
            }
mkReply (FromFeedLinkItems flinkitems) =
    let step = ( \acc (!f, !items) -> acc `T.append` "New item(s) for " `T.append` escapeWhere f mkdSingles `T.append` ":\n" `T.append` render items)
        payload = foldl' step mempty flinkitems
    in  defaultReply payload
mkReply (FromSearchRes keys sr_res) = ChatReply (render (keys, sr_res)) True True False True Nothing
mkReply FromStart = ChatReply renderCmds True True False False Nothing

renderCmds :: T.Text
renderCmds = T.intercalate "\n"
    [
        "/admin: manage the chat settings from the comfort of a web browser",
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
admin - manage your chat settings from the Web.
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
