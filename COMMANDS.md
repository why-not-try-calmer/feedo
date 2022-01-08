## Commands

### Subscribe a 1-1 chat, private group or public group to a web feed

_Chat admins only._

```
command  | argument
-------------------------------------------------------
/sub     | <list of comma-separated full url addresses>
------------------------------------------------------- 
example  | /sub https://www.reddit.com/r/pop_os.rss
```
### Subscribe a channel

_Both user performing this command and the bot must be admin in the target channel. The bot must have the permission to edit and pin messages._

The command may be issued from any chat with the bot.

```
command              | argument
--------------------------------------------------------------------------
/sub_channel, /subchan | <unique identifier (chat_id) of the target channel>
--------------------------------------------------------------------------
example  | /subchan -25154554 https://www.reddit.com/r/pop_os.rss
```
### Edit a chat's or channel's settings

_Chat admins only._

__Chat__:

```
command  | argument
-------------------------------------------------------
/set     | <line break + ":"-separated key-value pairs>
------------------------------------------------------- 
example  | /set
           batch_size: 10
           batch_at: 0800, 1200
           webview: true
```

__Channel__:
```
command  | argument
-----------------------------------------------------------------------------
/setchan, /set_channel | <unique identifier (chat_id)> + <line break with ":"-separated key-value pairs>
----------------------------------------------------------------------------- 
example  | /setchan -25154554
           batch_every: 3600
           pin: true
           web_view: true
```

WIP...

- /about, /a `<url or #>`

Get information about the feed at the url or # passed as argument. Does not require that the calling chat has subscribed as long as another chat has. Example:
- /info 2, /info https://www.compositional.fm/rss.

/fresh, /f <n>: Get all the most recent items (less than n-days old, where n is the argument) from all the feeds the chat is subscribed.

/help, /start:  Get the list of commands this bot answers to.

/items, /i <url or #>: Get the most recent items from the feed at the url or #s passed as argument, if any. Examples:
- /items 2
-/i https://www.compositional.fm/rss.

/list, /l: Get all the urls and #s of the feeds to which the chat is subscribed to, if any.

/pause, /p, /resume:  Whether the bot is allowed to send notification messages to the chat.

/purge (chat admins only): Make the bot and associated database forget entirely about this chat.

/reset (chat admins only): Set the chat's settings to the defaults.

/search, /se <space-separated keywords>: Search for keywords in all items in all feeds the current chat is subscribed to. Example:
- /se cheap cloud host.

/settings, /set optional <linebreak + key:value single lines> (admins only with argument): Get the settings for the referenced chat (version without argument) or set the settings for this chat. Example: /settings
blacklist: word1, word2
batch_size: 10, batch_at: 1200, 1800
webview: true

Examples:
- /s 1 2 3
- /sub https://www.compositional.fm/rss https://www.blabla.org/rss.

/unsub (chat admins only) <list of 1-space-separated full url addresses>: Unsubscribe from all the feeds passed as argument, if indeed they exits. Examples:
- /u 1 2 3
- /unsub https://www.compositional.fm/rss https://www.blabla.org/.