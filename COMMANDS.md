## Commands

### Subscribe a channel, 1-1 chat, private group, or public group to a web feed

__Any chat but a channel__:

_The user issuing the command must have administrative rights in the chat._

```
command  | argument
---------+---------------------------------------------
/sub     | <list of comma-separated full url addresses> 
---------+---------------------------------------------
example  | /sub https://www.reddit.com/r/pop_os.rss     
---------+---------------------------------------------
response | error or success message                    
---------+---------------------------------------------
```

__Channel__:

_Both the user performing this command and the bot must be admin in the target channel. The command may be issued from any chat with the bot. For a better experience it is recommended to give bot the permission to edit and pin messages._

```
command  | argument
---------+-------------------------------------------------------
/subchan | <chat_id>                                 
---------+-------------------------------------------------------
example  | /subchan -25154554 https://www.reddit.com/r/pop_os.rss  
---------+-------------------------------------------------------
response | error or success message                               
---------+-------------------------------------------------------
```

### View or edit a chat's or channel's settings

Available values:

- blacklist: term, term, ...
    - excludes feed items where a term is found in either their url or description
    - example: `blacklist: itsmycode, butIamproudofit`
- batch_at: HHMM, HHMM, ...
    - incompatible with "batch_every"
    - batch all updates and post them at the given times
    - example: `batch_at: 0800, 1200, 1800`
- batch_every: n
    - incompatible with "batch_at"
    - batch all updates and post them every n seconds
    - example: `batch_every: 9000` (= 20 minutes)
- paused: "true" or "false"
    - suspend all notifications to the chat
- webview: "true" or "false"
    - allow Telegram to display the last shown items in webview mode
- pin: "true" or "false",
    - have the bot try to pin every batch message
- clean_behind: "true" or "false"
    - have the bot try to delete their own messages
    - does not apply to batch messages

_The user issuing the command must have administrative rights in the chat._

__Any chat but a channel__:

```
command  | argument
---------+------------------------------------------
/set     | optional argument after line break
         | <":"-separated key-value pairs>            
---------+------------------------------------------
example  |  /set 
         |  batch_size: 10
         |  batch_at: 0800, 1200
         |  webview: true >
---------+------------------------------------------
response | with no argument: list of settings
         | with argument: error or success message
---------+------------------------------------------
```
__Channel__:

_Both the user performing this command and the bot must be admin in the target channel. The command may be issued from any chat with the bot. For a better experience it is recommended to give bot the permission to edit and pin messages._

```
command  | argument
---------+---------------------------------------------
/setchan | <line break + ":"-separated key-value pairs 
---------+--------------+------------------------------
example  |  /setchan -25154554
         |  batch_every: 9000
         |  pin: true
         |  webview: true
---------+---------------------------------------------
returns  | error or success message                                   
---------+---------------------------------------------
```

### List subscribed to web feeds

__Any chat but a channel__:

```
command   | argument
----------+---------
/list, /l | none    
----------+---------
example   | /l      
----------+-----------------------------
response  | list of #s associated with   
          | each subscripbed to web feed 
----------------------------------------
```

```
command   | argument
----------+---------------------------------
/listchan, /lchan | <chat_id of the channel>    
----------+---------------------------------
example   | /lchan -25154554
----------+---------------------------------
response  | list of #s associated with   
          | each subscripbed to web feed 
--------------------------------------------
```


### Show information about a subscribed to feed

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

```
command  | argument
---------+----------------------------------------------------------
/fresh   | <# of days representing how old the items can be at most 
---------+----------------------------------------------------------
example  | /fresh 1 
---------+----------------------------------------------------------
response | all at most #-old items collected from
         |  all feeds the chat is subscribed to
--------------------------------------------------------------------
```

### Displaying items from a single feed

```
command     | argument
------------+--------------------------------------------
/items, /i  | <# or full url of the target of days 
            |   representing the max "age" of the items
------------+--------------------------------------------
example     | /items 1
            | /i https://www.phoronix.com/rss.php
------------+---------------------------------------------
response    | all the items available from the target feed
----------------------------------------------------------
```
### Miscellaneous
- /pause, /p: Suspend notification to the chat
- /pausechan `<chat_id>`: Like the previous one but for channels
- /resume:  Whether the bot is allowed to send notification messages to the chat.
- /resumechan `<chat_id>`: like the previous one but for channels
- /purge (chat admins only): Make the database forget entirely about the chat
- /purgechan `<chat_id>` (chat admins only): Make the database forget entirely about the channel
- /reset (chat admins only): Set the chat's settings to the defaults
- /resetchan `<chat_id>` (chat admins only) `<channel_id>`: Set the channels's settings to the defaults
- /search, /se `<space-separated keywords>`: Search for keywords in all items in all feeds the current chat is subscribed to. 
    - Example: `/se cheap cloud host`
- /unsub (chat admins only) `<list of 1-space-separated #s or full url addresses>`
    - unsubscribe from all the feeds passed as argument, if indeed they exits
    - examples, `/u 1 2 3`, `/unsub https://www.compositional.fm/rss https://www.blabla.org/feed`
- /unsubchan `<chat_id>` (chat admins only) `<channel id> + <list of 1-space-separated # or full urls>`: like the previous one but for channels 