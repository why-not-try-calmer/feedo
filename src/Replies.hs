{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Replies (defaultReply, mkdSingles, mkdDoubles, renderCmds, render, Reply (..), mkReply, Replies (..), mkViewUrl, mkDigestUrl) where

import qualified Data.HashMap.Strict as HMS
import Data.List (sortOn)
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), defaultTimeLocale, formatTime, toGregorian)
import Network.URI.Encode (encodeText)
import TgramOutJson (TgRequestMethod (TgDeleteMessage, TgEditMessage, TgGetChat, TgGetChatAdministrators, TgPinChatMessage, TgSendMessage))
import Types
import Utils (nomDiffToReadable, sortFeedsOnSettings, renderAvgInterval, utcToYmd, utcToYmdHMS)

escapeWhere :: T.Text -> [T.Text] -> T.Text
escapeWhere txt suspects =
  T.foldl'
    ( \t c ->
        let c' = T.singleton c
         in if c' `elem` suspects
              then t `T.append` "\\" `T.append` T.singleton c
              else t `T.append` T.singleton c
    )
    mempty
    txt

skipWhere :: T.Text -> [T.Text] -> T.Text
skipWhere txt suspects = T.filter (\t -> T.singleton t `notElem` suspects) txt

mkdSingles :: [T.Text]
mkdSingles = ["_", "*", "`"]

mkdDoubles :: [T.Text]
mkdDoubles = ["[", "]"]

mkViewUrl :: [Item] -> Maybe T.Text
mkViewUrl [] = Nothing
mkViewUrl items =
  let base_url = "https://feedo.cloudns.ph/view"
      (ts, flinks) = foldl' step ([], []) items
      flinks_txt = "?flinks=" `T.append` encodeText (T.intercalate "\\" flinks)
      from_to_txt =
        "&from="
          `T.append` (utcToYmd . head $ ts)
          `T.append` "&to="
          `T.append` (utcToYmd . last $ ts)
   in if length ts < 2 || null flinks
        then Nothing
        else Just $ base_url `T.append` flinks_txt `T.append` from_to_txt
 where
  step ([], !fs) i = ([i_pubdate i], i_feed_link i : fs)
  step (x : xs, !fs) i =
    let pub_i = i_pubdate i
        flink = i_feed_link i
        fs' = if flink `notElem` fs then flink : fs else fs
     in if pub_i < x then (pub_i : x : xs, fs') else (x : pub_i : xs, fs')

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
     in if d == d'
          then (str `T.append` finish (i_title i) (i_link i), d)
          else
            ( str
                `T.append` "_"
                `T.append` date
                `T.append` "_ \n"
                `T.append` finish (i_title i) (i_link i)
            , d'
            )
  finish title link = "- " `T.append` toHrefEntities Nothing title link `T.append` "\n"

toHrefEntities :: Maybe Int -> T.Text -> T.Text -> T.Text
toHrefEntities mbcounter tag link =
  let tag' = "[" `T.append` skipWhere tag (mkdSingles ++ mkdDoubles) `T.append` "]"
      link' = "(" `T.append` link `T.append` ")"
   in case mbcounter of
        Nothing -> tag' `T.append` link'
        Just c ->
          let counter = T.pack . show $ c
           in counter `T.append` ") " `T.append` tag' `T.append` link'

defaultReply :: T.Text -> Reply
defaultReply payload =
  ChatReply
    { reply_contents = payload
    , reply_markdown = True
    , reply_pin_on_send = False
    , reply_disable_webview = False
    , reply_pagination = True
    , reply_permalink = Nothing
    }

mkReply :: Replies -> Reply
{- Prepare a reply from digests or other commands -}
mkReply (FromAbout version) =
  ServiceReply $
    "Version: "
      `T.append` version
      `T.append` ". See https://github.com/why-not-try-calmer/feedo/commit/"
      `T.append` version
mkReply (FromAdmin base hash) = ServiceReply . mkAccessSettingsUrl base $ hash
mkReply (FromAnnounce txt) = defaultReply txt
mkReply FromChangelog = ServiceReply "check out https://t.me/feedo_the_bot_channel"
mkReply (FromChatFeeds c feeds) =
  let settings = sub_settings c
      sorted_feeds = sortFeedsOnSettings settings feeds
      step (!txt, !counter) f =
        let link = f_link f
            title = f_title f
            rendered = toHrefEntities (Just counter) title link
         in (T.append txt rendered `T.append` "\n", counter + 1)
      start = ("Feeds subscribed to (#, link):\n", 1 :: Int)
      payload = fst $ foldl' step start sorted_feeds
   in defaultReply payload
mkReply (FromChat chat confirmation) = ServiceReply $ confirmation `T.append` render chat
mkReply (FromFeedDetails feed) = ServiceReply $ render feed
mkReply (FromFeedItems f) =
  let rendered_items =
        render
          . take 30
          . sortOn (Down . i_pubdate)
          . f_items
          $ f
   in defaultReply rendered_items
mkReply (FromDigest fs mb_link s) =
  let sorted_feeds = sortFeedsOnSettings s fs
      fitems = map (\f -> (f_title f, f_items f)) sorted_feeds
      -- pagination preempting collapse when both are enabled and
      -- collapsing would have occurred
      collapse = maybe 0 (\v -> if settings_pagination s then 0 else v) $ settings_digest_collapse s
      header = "-- " `T.append` settings_digest_title s `T.append` " --"
      protected = S.toList $ settings_digest_no_collapse s
      max_items = if settings_digest_size s > 0 then settings_digest_size s else 20
      body = render (fitems, collapse, protected, max_items)
      payload = header `T.append` body
   in ChatReply
        { reply_contents = payload
        , reply_markdown = True
        , reply_pin_on_send = settings_pin s
        , reply_disable_webview = settings_disable_web_view s
        , reply_pagination = settings_pagination s
        , reply_permalink = if settings_share_link s then mb_link else Nothing
        }
mkReply (FromFeedLinkItems flinkitems) =
  let step acc (!f, !items) = acc `T.append` "New item(s) for " `T.append` escapeWhere f mkdSingles `T.append` ":\n" `T.append` render items
      payload = foldl' step mempty flinkitems
   in defaultReply payload
mkReply (FromSearchRes keys results) = ChatReply (render (keys, results)) True True False True Nothing
mkReply FromCmds = ChatReply renderCmds True True False False Nothing
mkReply FromStart =
  let txt =
        "Hello there!\nThis bot allows you to subscribe to web feeds this or any chat\
        \ or channel _where you have admin permissions_ (it works in private chats too!). Then every now and then, the bot will send messages to\
        \ the subscribed chat with a summary of all the new items found since last time, so that you never miss out on any important news!\
        \ \n\nI suggest you start with `/sub https://<your favorite web feed>` (the bot will tell you if the address and the feed are valid).\
        \ Then review your settings with `/set`.\nYou can specify a time or a period of time for receiving messages. For example, to receive one post every day at 6pm and 18pm:\
        \ \n`/set`\n`digest_at: 06:00 18:00`\nBy default the bot posts at least once a day every day.\
        \ \nA ready-to-use list of settings can be viewed [here](https://github.com/why-not-try-calmer/feedfarer2/blob/master/SETTINGS_EXAMPLES.md).\
        \ \nAll the settings and commands are explained [there](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md).\
        \ \nHave fun and don't hesitate to [get in touch](https://t.me/ad_himself) if you have questions or issues."
   in (defaultReply txt){reply_disable_webview = True}

class Renderable e where
  render :: e -> T.Text

instance Renderable TgRequestMethod where
  render TgGetChat = "getChat"
  render TgGetChatAdministrators = "getChatAdministrators"
  render TgSendMessage = "sendMessage"
  render TgEditMessage = "editMessageText"
  render TgPinChatMessage = "pinChatMessage"
  render TgDeleteMessage = "deleteMessage"

instance Renderable Feed where
  render Feed{..} =
    T.intercalate "\n" $
      map
        (\(k, v) -> k `T.append` ": " `T.append` v)
        [ ("Url", f_link)
        , ("Type", T.pack $ show f_type)
        , ("Title", f_desc)
        , ("Current items", T.pack . show $ length f_items)
        , ("Avg. interval between items", renderAvgInterval f_avg_interval)
        , ("Last refresh", maybe "None" utcToYmdHMS f_last_refresh)
        ]

instance Renderable SubChat where
  render SubChat{..} =
    let adjust c = if c == "0" then "00" else c
        at = case digest_at . settings_digest_interval $ sub_settings of
          Nothing -> mempty
          Just ts ->
            foldl'
              ( \s (!h, !m) ->
                  let body = (T.pack . show $ h) `T.append` ":" `T.append` (adjust . T.pack . show $ m)
                      s' = if s == mempty then s else s `T.append` ", "
                   in s' `T.append` body
              )
              mempty
              ts
        every_txt =
          let k = "Digest step (time between two digests)"
           in case digest_every_secs . settings_digest_interval $ sub_settings of
                Nothing -> (mempty, mempty)
                Just e -> if e == 0 then (k, "not set") else (k, nomDiffToReadable e)
        blacklist =
          let bl = S.toList $ match_blacklist . settings_word_matches $ sub_settings
           in if null bl then "none" else T.intercalate ", " bl
        searches =
          let se = S.toList $ match_searchset . settings_word_matches $ sub_settings
           in if null se then "none" else T.intercalate ", " se
        only_search_results =
          let sr = S.toList $ match_only_search_results . settings_word_matches $ sub_settings
           in if null sr then "none" else T.intercalate ", " sr
        rendered =
          let mapper = T.intercalate "\n" . map (\(k, v) -> k `T.append` ": " `T.append` v)
              status_part =
                mapper
                  [ ("Chat id", T.pack . show $ sub_chatid)
                  , ("Status", if settings_paused sub_settings then "paused" else "active")
                  , ("Feeds subscribed to", T.intercalate ", " $ S.toList sub_feeds_links)
                  , maybe mempty (\c -> ("Linked to", T.pack . show $ c)) sub_linked_to
                  ]
              digest_part =
                mapper
                  [ maybe mempty (\t -> ("First digest", utcToYmd t)) $ settings_digest_start sub_settings
                  , ("Digest time(s)", if T.null at then "none" else at)
                  , every_txt
                  , ("Digest size", (T.pack . show . settings_digest_size $ sub_settings) `T.append` " items")
                  , ("Digest collapse", maybe "false" (\v -> if v == 0 then "false" else T.pack . show $ v) $ settings_digest_collapse sub_settings)
                  , ("Digest not collapsible", if S.null (settings_digest_no_collapse sub_settings) then "none" else T.intercalate ", " $ S.toList $ settings_digest_no_collapse sub_settings)
                  , ("Digest title", settings_digest_title sub_settings)
                  , ("Last digest", maybe "none" utcToYmdHMS sub_last_digest)
                  , ("Next digest", maybe "none scheduled yet" utcToYmdHMS sub_next_digest)
                  ]
              search_part =
                mapper
                  [ ("Blacklist", blacklist)
                  , ("Feeds ignored unless a search keyword matches", only_search_results)
                  , ("Search keywords", searches)
                  ]
              telegram_part =
                mapper
                  [ ("Display 'share link' button in digests", if settings_share_link sub_settings then "true" else "false")
                  , ("Pagination in digests", if settings_pagination sub_settings then "true" else "false")
                  , ("Pin new updates", if settings_pin sub_settings then "true" else "false")
                  , ("Webview", if settings_disable_web_view sub_settings then "false" else "true")
                  ]
              admin_part =
                mapper
                  [ ("Forward errors to chat or channel admins", if settings_forward_to_admins sub_settings then "true" else "false")
                  , ("Recently active admins", T.intercalate "\n" $ map (\(u, t) -> (T.pack . show $ u) `T.append` ": " `T.append` (T.pack . show $ t)) $ HMS.toList sub_active_admins)
                  ]
           in status_part
                `T.append` T.intercalate
                  "\n--\n"
                  [ digest_part
                  , search_part
                  , telegram_part
                  , admin_part
                  ]
     in T.append rendered "\n\nToo many settings? Check out the docs for examples: https://github.com/why-not-try-calmer/feedfarer2/blob/master/SETTINGS_EXAMPLES.md"

instance Renderable [Item] where
  render = intoTimeLine

instance Renderable ([(T.Text, [Item])], Int, [FeedLink], Int) where
  render (!f_items, !collapse_size, !protected, !max_items) =
    let protected' = map T.toCaseFold protected
        collapsing i
          | collapse_size < length i =
              " ("
                `T.append` (T.pack . show $ collapse_size)
                `T.append` " out of "
                `T.append` (T.pack . show . length $ i)
                `T.append` " new):\n"
          | otherwise = ":\n"
        into_list acc (!t, !i) =
          acc
            `T.append` "\n*| "
            `T.append` t
            `T.append` "*\n"
            `T.append` (render . take max_items . sortOn (Down . i_pubdate) $ i)
        into_folder acc (!t, !i) =
          let sorted = sortOn (Down . i_pubdate)
              is_protected =
                let link = T.toCaseFold . i_feed_link $ head i
                 in link `elem` protected'
              items = if is_protected then take max_items $ sorted i else take collapse_size . sorted $ i
              acc' =
                if null items
                  then acc
                  else
                    acc
                      `T.append` "\n*| "
                      `T.append` t
                      `T.append` "*"
                      `T.append` if is_protected then ":\n" `T.append` render items else collapsing i `T.append` render items
           in acc'
     in foldl' (if collapse_size == 0 then into_list else into_folder) mempty f_items

instance Renderable (S.Set T.Text, [SearchResult]) where
  render (keys, search_res) =
    let items = map (\SearchResult{..} -> Item sr_title mempty sr_link sr_feedlink sr_pubdate) search_res
     in "Results from your search with keywords "
          `T.append` T.intercalate ", " (map (`escapeWhere` mkdSingles) . S.toList $ keys)
          `T.append` ":\n"
          `T.append` render items

instance Renderable InterpreterErr where
  render (InterpreterErr t) = T.append "I don't know what to do with this input: " t
  render (UnknownCommand cmd args) =
    "Unknown command: "
      `T.append` cmd
      `T.append` ", called with these arguments:"
      `T.append` T.intercalate "\n" enumerate
   where
    enumerate = zipWith (\n arg -> (T.pack . show $ n) `T.append` ": " `T.append` arg) [1 .. length args] args

instance Renderable TgEvalError where
  render (BadRef contents) = T.append "References to web feeds must be either single digits or full-blown urls starting with 'https://', but you sent this: " contents
  render (BadFeed feederror) = T.append "Unable to fetch this feed: " (render feederror)
  render (BadFeedUrl t) = T.append "No feed could be found at this address: " t
  render (NotAdmin _) = "Unable to perform this action, as it's reserved to admins in this chat."
  render (MaxFeedsAlready _) = "This chat has reached the limit of subscriptions (10)"
  render (ParseError input) = T.append "Settings this input failed: " input
  render (DbQueryError err) = T.append "Unable to update, because of this error: " $ render err
  render (UpdateError err) = T.append "Unable to update, because of this error: " err
  render (NotFoundFeed feed) = T.append "The feed you were looking for does not exist: " feed
  render NotFoundChat = "The chat you called from is not subscribed to any feed yet."
  render NotSubscribed = "The feed your were looking for could not be found. Make sure you are subscribed to it."
  render (TelegramErr err) = "Telegram responded with an error: " `T.append` err
  render ChatNotPrivate = "Unwilling to share authentication credentials in a non-private chat. Please use this command in a private conversation with to the bot."
  render UserNotAdmin = "Only admins can change settings."

instance Renderable DbError where
  render PipeNotAcquired = "Failed to open a connection against the database."
  render FaultyToken = "Login failed. This token is not valid, and perhaps never was."
  render (FailedToUpdate items reason) = "Unable to update the following items: " `T.append` items `T.append` ". Reason: " `T.append` reason
  render (NotFound item) = "Resource could not be found from the network: " `T.append` item
  render FailedToLog = "Failed to log."
  render FailedToLoadFeeds = "Failed to load feeds!"
  render (BadQuery txt) = T.append "Bad query parameters: " txt
  render FailedToSaveDigest = "Unable to save this digest. The database didn't return a valid identifier."
  render FailedToProduceValidId = "Db was unable to return a valid identifier"
  render FailedToInsertPage = "Db was unable to insert these pages."
  render FailedToGetAllPages = "Db was unable to retrieve all pages."

instance Renderable FeedError where
  render (FeedError url status_code error_msg user_msg last_attempt) =
    "Failed to build or fetch feed "
      `T.append` url
      `T.append` ". Status code: "
      `T.append` (T.pack . show $ status_code)
      `T.append` ". Error message: "
      `T.append` error_msg
      `T.append` ". User message: "
      `T.append` user_msg
      `T.append` ". Last attempt: "
      `T.append` (T.pack . show $ last_attempt)

renderCmds :: T.Text
renderCmds =
  T.intercalate
    "\n"
    [ "/about: Information about this service (version, statistics, etc.)"
    , "/admin: manage the chat settings from the comfort of a web browser"
    , "/changelog: link to the changelog"
    , "/feed `<optional: channel id> <# or url>`: show info about the subscribed to feed"
    , "/fresh `<n>`: display n-old items, in number of days"
    , "/help: show these commands"
    , "/items `<optional: channel id> <# or url>`: display all the items fetched from the referenced feed"
    , "/list `<optional: channel id`: list all the feeds this chat or that channel is subscribed to"
    , "/link `<channel id>`: allow the current chat to get the same permissions as the target channel when accessing feeds data. This means that /feed, /fresh, /list and /search will retrieve data as if the commands were sent from the target chat or channel"
    , "/migrate `<optional: id of the origin> <id of the destination>`: migrate this chat's settings, or the settings of the channel at the origin, to the destination."
    , "/pause `<optional: channel id>`: stop posting updates to this chat or to that channel"
    , "/purge `<optional: channel id>`: delete all data about this chat or that channel"
    , "/reset `<optional: channel id>`: set this chat's (or that channels') settings to their default values"
    , "/resume `<optional: channel id>`: resume updates to this chat or to that channel"
    , "/search `<term1 term2...>`: search all items across all feeds for the given keywords"
    , "/set `<optional: channel id> <optional: linebreak + key:value pairs>` view or edit this chat's or that channel's settings"
    , "/start: show again the starting message"
    , "/sub `<optional: channel id> <url1 url2...>`: subscribe this chat or that channel to the target feed(s)"
    , "/testdigest `<optional: channel id>`: preview a digest for the current or target chat / channel"
    , "/unsub `<optional: channel id> <url url2...>`: unsubscribe this chat or that channel from the target feed(s)"
    ]
    `T.append` "\n\nCheck out this [document](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md) for more details."

{-
about - Information about this service (version, statistics, etc.)
admin - manage the chat settings from the comfort of a web browser
changelog - link to the changelog
feed - show info about the subscribed to feeds, for the current chat or target channel_id
fresh - <n> display <n> old items, in number of days
help - show these commands
items - <optional: channel_id> <# or url> display all the items fetched from the referenced feed
list - <optional: channel_id> list all the feeds this chat or that channel is subscribed to
link - <chat_id or channel_id> allow the current chat to get the same permissions as the target chat or channel when accessing feeds data.
migrate - <optional: chat_id of the origin> <chat_id of the destination> migrate this chat's settings, or the settings of the channel at the origin, to the destination
pause - <optional: channel_id> stop posting updates to this chat or to that channel
purge - <optional: channel_id> delete all data about this chat or that channel
reset - <optional: channel_id> set this chat's (or that channels') settings to their default values
resume - <optional: channel_id> resume updates to this chat or to that channel
search - <term1 term2...> search all items across all feeds for the given keywords
set - <optional: channel_id> <optional: linebreak + key:value pairs> view or edit this chat's or that channel's settings
start - show again the starting message
sub - <optional: channel_id> <url1 url2...> subscribe this chat or that channel to the target feed(s)
testdigest - <optional: channel_id> sends a 'dry run' digest for the current chat or target channel
unsub - <optional: channel_id> <url url2...> unsubscribe this chat or that channel from the target feed(s)
-}
