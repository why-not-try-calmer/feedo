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
#### Available settings:

- `blacklist: term term, ...`
    - excludes feed items where a term is found in either their url or description
    - example: `blacklist: itsmycode, butIamproudofit`
- `digest_at: HH:MM HH:MM, ...`
    - digest all updates and post a summary (digest) at the given times
    - example: `digest_at: 0800 1200 1800`
- `digest_every: <integer><m | h | d>`
    - digest all updates and post a summary (digest) every n-minute, n-hour or n-day
    - examples:
        - `digest_every: 20m` (= 20 minutes)
        - `digest_every: 6h` (= 6 hours)
        - `digest_every: 1d` (= 1 day)

_Notice_: If `digest_at` is set and `digest_every` is set to less than 1 day, the application will ignore `digest_every` and use only `digest_at`.

- `digest_size: <integer>`
    - the number of items per feed to send in a single digest
- `digest_collapse: <integer>`
    - whether digests are reduced to a list of feed names along with the number of new items plus the last <integer> items
    - a link to read the full digest is appended.
- `disable_webview: "true" or "false"`
    - whether Telegram is prevented from displaying the last shown items in webview mode
- `follow: "true" or "false"`
    - polls every feed at short interval and notifies on new items
    - this feature works independently from the digest-like feature
- `only_search_notif: <list of urls>`
    - if an url of a feed is on the list, items from it will be ignored from all messages except for search notifications
- `paused: "true" or "false"`
    - suspend all notifications to the chat
- `pin: "true" or "false"`,
    - have the bot try to pin every digest message
- `search_notif: <list of space-separated keywords>`
    - have the bot send an extra notification whenever a feed to which the current chat is subscribed has items in which the keywords are found
- `share_link: "true" or "false"`
    - appends a link at the end of each digest update to a web rendered view

_Notice_: The last two settings allow to completely ignore certain feeds from the regular updates, while still getting notifications on matches.

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
- /resume `<optional: channel_id>`:  Whether the bot is allowed to send notification messages to the chat or channel.
- /migrate `<from: chat or channel_id> <to: chat or channel id>`: Copies the settings defined for the first chat or channel, to the second. Then runs '/purge' on the first.
- /purge `<optional: channel_id>` (chat admins only): Make the database forget entirely about the chat or channel.
- /reset `<optional: channel_id>` (chat admins only): Set the chat's or channel's settings to the defaults
- /search, /se `<space-separated keywords>` (non-channels only): Search for keywords in all items in all feeds the current chat is subscribed to. 
    - example: `/se cheap cloud host`
- /unsub `<optional: channel_id>`(chat admins only) `<list of 1-space-separated #s or full url addresses>`
    - unsubscribe from all the feeds passed as argument (if valid).
    - examples: `/unsub 1 2 3` for the current chat, `/unsub -1456658 https://www.compositional.fm/rss https://www.blabla.org/feed` for channel -1456658.
