## Commands

### Subscribe channel, 1-1 chat, private group, or public group to a web feed

__Subscribe the current chat__:

_The user issuing the command must have administrative rights in the chat._

__Subscribe a referenced channel__:

_Both the user performing this command and the bot must be admin in the target channel. The command may be issued from any chat with the bot. For a better experience it is recommended to give bot the permission to edit and pin messages._

```
command  | argument
---------+--------------------------------------------------------
/sub     | <optional: chat_id> <mandadory: space-separated list of
            urls>                               
---------+--------------------------------------------------------
examples | /sub https://rest.of.url1 https://www.rest.of.url2 
         | /sub -25154554 https://www.reddit.com/r/pop_os.rss  
---------+--------------------------------------------------------
response | error or success message                               
---------+--------------------------------------------------------
```
### View or edit a chat's or channel's settings

__View or edit the current chat's settings__:

_The user issuing the command must have administrative rights in the chat._

__View or edit a referenced channel__:

_Both the user performing this command and the bot must be admin in the target channel. The command may be issued from any chat with the bot. For a better experience it is recommended to give bot the permission to edit and pin messages._

```
command  | argument
---------+-----------------------------------------------
/set     | <optional: channel id> <optional: line break + 
         |   ":"-separated key-value pairs 
---------+-----------------------------------------------
examples | /set
         | digest_at: 08:00 12:00 19:00
         | pin: true
         | disable_webview: true 
         |
         | /set -25154554
         | digest_every: 9000
         | pin: true
         | disable_webview: true
---------+-----------------------------------------------
returns  | error or success message                                   
---------+-----------------------------------------------
```

__Available settings__

Here are the default settings, that come into play as soon as you subscribe a chat to a web feed:
```
blacklist: undefined
    # list of keywords that exclude items
    # containing any of them from notifications
digest_at: undefined
    # the time of the day at which digests are sent
digest_every: 1d
    # every day, post a digest (summmary) of
    # all new items across all feeds this chat is subscribed to
    # since last time
digest_size: 10
    # have the digest (summary) contain max. 10 items per feed
digest_start: undefined
    # input a datestring in the yyyy-mm-dd format
    # to use a start date for your digests
disable_web_view: false
    # do not prevent Telegram from rendering the first item using the
    # InstantView feature
follow: false
    # do not try to post every new item between digest
    # as as soon as they are found
only_search: undefined
    # list of feeds whose items should be ignored from notifications unless they match search
    # keywords
pagination: false
    # slices a message into many "pages" and adds button controls beneath them 
    # allowing users to flip back and forth between pages
    # if the paginated message is a digest, this setting overrides 'digest_collapse' 
paused: false
    # not paused
pin: false
    # do not try to pin digests
search_notif: undefined
    # the list of keywords used for 'only_search'
share_link: true
    # always add a button linking a digest to its web rendered version
    # a link for sharing with non-Telegram users
```
_Remarks_: 
- all defaults can be reset to their default value with:
    ```
    /set
    <key>: reset
    ```
- when `digest_at` is set, `digest_every` is used a step in terms of number of days. Values below "1d" are ignored. This means you can set
    ```
    /set
    digest_at: 08:00
    digest_every: 2d
    ```
    for a morning digest every other day.
- when set to less than 1 day, `digest_every` is ignored and only `digest_at` is used;
- `follow` tracks all you feeds and send notifications of new updates every 20 minutes;
- `search_notif` and `only_search` allow to completely ignore certain feeds from digest or follow messages, while still getting notifications on matches. For example:
    ```
    digest_every: 1d
    follow: true
    search_notif: kernel
    only_search: url1 url2
    ```
    means that the feeds at url1 and url2 will be ignored from your daily digests and 20-minute follows, unless any one of them contains a match for the keyword "kernel".

### List subscribed to web feeds

_Both chats and channels_.

```
command   | argument
----------+----------------------------------
/list     | <optional: chat_id of the channel>    
----------+----------------------------------
example   | /lchan -25154554
----------+----------------------------------
response  | list of #s associated with   
          | each subscribed to web feed 
---------------------------------------------
```

### Show information about a subscribed to feed

_Only in non-channels_.

```
command      | argument
-------------+------------------------------------------------
/feed, /f    | <# of the target feed, or full url >           
-------------+------------------------------------------------
example      | /feed 1, /feed https://www.phoronix.com/rss.php
-------------+------------------------------------------------
response     | information about the target feed              
-------------+------------------------------------------------
```
### Previewing (dry-run) a chat's or channel's next digest

```
command      | argument
-------------+------------------------------------------------
/testdigest  | <optional: channel id>           
-------------+------------------------------------------------
example      | /testdigest, /testdigest -102333565
-------------+------------------------------------------------
response     | the chat's or channel's next digest            
-------------+------------------------------------------------
```

### Fetching fresh items

_Only in non-channels_.

```
command  | argument
---------+---------------------------------------------------------
/fresh   | <# of days representing how old the items can be at most 
---------+---------------------------------------------------------
example  | /fresh 1 
---------+---------------------------------------------------------
response | all at most #-old items collected 
         |  from all feeds the chat is subscribed to
-------------------------------------------------------------------
```

### Displaying items from a single feed

_Only in non-channels_.

```
command     | argument
------------+---------------------------------------------
/items, /i  | <# or full url of the target of days 
            |   representing the max "age" of the items
------------+---------------------------------------------
example     | /items 1
            | /i https://www.phoronix.com/rss.php
------------+---------------------------------------------
response    | all the items available from the target feed
----------------------------------------------------------
```
### Miscellaneous

- /changelog: Show the latest changelog
- /pause `<optional: channel_id>`: Suspend notification to the chat or channel.
- /link `<channel_id>`: allow the current chat to access all the data of the referenced channel with these commands: /feed, /fresh, /list and /search
- /resume `<optional: channel_id>`:  Whether the bot is allowed to send notification messages to the chat or channel.
- /migrate `<from: chat or channel_id> <to: chat or channel id>`: Copies the settings defined for the first chat or channel, to the second. Then runs '/purge' on the first.
- /purge `<optional: channel_id>` (chat admins only): Make the database forget entirely about the chat or channel.
- /reset `<optional: channel_id>` (chat admins only): Set the chat's or channel's settings to the defaults
- /search, /se `<space-separated keywords>` (non-channels only): Search for keywords in all items in all feeds the current chat is subscribed to. 
    - example: `/se cheap cloud host`
- /unsub `<optional: channel_id>`(chat admins only) `<list of 1-space-separated #s or full url addresses>`
    - unsubscribe from all the feeds passed as argument (if valid).
    - examples: `/unsub 1 2 3` for the current chat, `/unsub -1456658 https://www.compositional.fm/rss https://www.blabla.org/feed` for channel -1456658.
