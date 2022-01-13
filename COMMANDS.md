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
         | batch_at: 08:00 12:00 19:00
         | pin: true
         | disable_webview: true 
         |
         | /set -25154554
         | batch_every: 9000
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
- `batch_at: HHMM HHMM, ...`
    - batch all updates and post them at the given times
    - example: `batch_at: 0800 1200 1800`
- `batch_every: <integer><m | h | d>`
    - batch all updates and post them every n-minute, n-hour or n-day
    - examples:
        - `batch_every: 20m` (= 20 minutes)
        - `batch_every: 6h` (= 6 hours)
        - `batch_every: 1d` (= 1 day)

_Notice_: As of the latest version, this settings _is_ compatible with `batch_at` provided that the value of `batch_every` is 1 or more __days__. If the value of `batch_every` is expressed in minutes or hours and `batch_at` is defined in the settings, the application will ignore `batch_every` and use only `batch_at`.

- `paused: "true" or "false"`
    - suspend all notifications to the chat
- `disable_webview: "true" or "false"`
    - allow Telegram to display the last shown items in disable_webview mode
- `pin: "true" or "false"`,
    - have the bot try to pin every batch message

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
- /purge `<optional: channel_id>` (chat admins only): Make the database forget entirely about the chat or channel.
- /reset `<optional: channel_id>` (chat admins only): Set the chat's or channel's settings to the defaults
- /search, /se `<space-separated keywords>` (non-channels only): Search for keywords in all items in all feeds the current chat is subscribed to. 
    - example: `/se cheap cloud host`
- /unsub `<optional: channel_id>`(chat admins only) `<list of 1-space-separated #s or full url addresses>`
    - unsubscribe from all the feeds passed as argument (if valid).
    - examples: `/unsub 1 2 3` for the current chat, `/unsub -1456658 https://www.compositional.fm/rss https://www.blabla.org/feed` for channel -1456658.
