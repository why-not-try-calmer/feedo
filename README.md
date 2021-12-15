# feedfarer
A fast, efficient network app for subscribing Telegram chats to web feeds. All chats types are supported (public and private group chats as well as direct chats with the bot).

The bot posts any update to any Atom or RSS feed a chat is subscribed to. It also replies directly to commands for consuming feeds at one's favorite pace.

The bot relies upon the Haskell `warp` server. By default it provides an endpoint at `https://<your.nice.domain>/webhook/bot<your token>` handling inbound and outbound HTTP requests from / to Telegram services via webhooks (only webhooks are supported as they provide a more resource-efficient communication method). 

This application is written from scratch, using exclusively HTTP requests-responses as specified by the [Telegram Bot API](https://core.telegram.org/bots/api). It relies on no Telegram SDK or third-party library.

## How to (Telegram)
1. Start a chat with the bot (1-1 chat) or invite it to any chat (group or private). _The bot needs only the permission to send text messages including links_; it listens only to messages that use of a command defined for the bot.
2. Subscribe the chat and have fun with the other commands. Posts will arrive in due time.

## List of commands
`/fresh, /f <n>`: Get all the most recent items (less than n-days old, where n is the argument) from all the feeds the chat is subscribed.

`/help, /start`:  Get the list of commands this bot answers to.

`/info <url or #>`: Get information about the feed at the url or # passed as argument. Does not require that the calling chat has subscribed as long as another chat has. Examples:

- `/info 2`
- `/info https://www.compositional.fm/rss`

`/items, /i <url or #>`: Get the most recent items from the feed at the url or #s passed as argument, if any. Examples:

- `/items 2`
- `/i https://www.compositional.fm/rss`

`/list, /l`: Get all the urls and #s of the feeds to which the chat is subscribed to, if any.

`/pause, /p, /resume, /r`: Whether the bot is allowed to send notification messages to the chat.

`/purge (chat admins only)`: Make the bot and associated database forget entirely about this chat.

`/settings (chat admins only), with optional argument <line-separated key-value pairs>`: Get the settings for the calling chat (version without argument), or set the settings for this chat. Example:

```
/settings
blacklist: Trump, bitcoin, crypto
batch: true
batch_size: 10
batch_interval: 9000
```

Explanation of the supported key-value pairs:
- _blacklist_: comma-separated list of words each of which suffices to exclude any item containing it from being posted
- _batch_: **true** or **false**: whether notifications to the chat are sent as new items are available (the default) or they come in custom batches.
- _batch-size_: the maximal number of items any batch can hold.
- _batch_interval_: the time-window during which items should be put on hold before sending. 

`/sub, /s (chat admins only) <list of comma-separated full url addresses>`: Subscribe the chat to the feeds -- if they exist -- passed as argument. Examples:
- `/sub https://www.compositional.fm/rss https://www.blabla.org/rss`


`/unsub, /u (chat admins only) <list of 1-space-separated full url addresses>`: Unsubscribe from all the feeds passed as argument, if indeed they exits. Examples:
- `/unsub https://www.compositional.fm/rss https://www.blabla.org`

Check out our channel for more info: https://t.me/feedfarer.

## Demo
Ask for an invite with https://t.me/+zMdPlkeEu7w2NjM0.

## Status
This application has been deployed but only for live testing. Public release coming up soon.

## Roadmap & todo
- [x] Architecture, business logic
- [x] Implementation
- [x] Tests
- [x] Test deployment
- [] Deployment
- [] Tests in deployment
- [] Factoring out database details to avoid depending on mongoDB

## Test build and run (stack)

__Build__

This step requires having a working installation of [stack](https://docs.haskellstack.org/).

`cd` to a parent directory and then clone the repository with:
```
git clone https://github.com/why-not-try-calmer/feedfarer2.git
```
Then build with
```
stack build
```
__Run__

This step requires having an `.env` file in the root directory set as appropriate. Then:
```
stack build
stack exec feedfarer-exe
```

## Deployment (docker / podman)
This step requires having an `.env` file in the root directory set as appropriate (see below). Substitute `docker` for `podman` in the instructions if you don't use _podman_.

`cd` to to a parent directory. Then:
```
git clone https://github.com/why-not-try-calmer/feedfarer2.git
podman build . -t feedfarer-img
podman run --rm --name feedfarer -d -p 80:80 --env-file=.env feedfarer-img
```

For compatibility with most cloud providers, the container exposes port 80. If you need to prototype the app on your local machine you might need to pick another port and forward it to 80. It's recommended to pick a port between [443, 80, 88 or 8443] to forward to 80, as only those are compatible with Telegram HTTP API. I usually use 8443 as it does not require root permissions, e.g.:

```
podman run --rm --name feedfarer -it -p 8443:80 --env-file=.env feedfarer-img
```

Telegram requires HTTPS for sending and receiving, but this application __does not__ do any of that. You will need to make sure that the host to which the application is deployed takes care of the HTTPS business.

## Configuration
Your `.env` file should be located in the app root directory (typically the directory from which you run the application).

It should define a few variables (do not include the `<`, `>`):

- `TELEGRAM_TOKEN=< your Telegram bot token >` mandatory
- `WEBHOOK_URL=< the url at which updates are sent >` mandatory
- `MONGODB_SHARDS=<shard name>:<username>:<password>` mandatory until I factor this out, as I understand is a problem to rely on this particular database
- `PORT=<define to override the default (80)>` optional
- `WORKER_INTERVAL=<define to override the default (1 run every 15 minutes)>` optional
- `STARTING_FEEDS=<comma-separated feed urls to initialize the app with, http://www.domain.org/feed1/rss,http://www.domain.org/feed2/rss,http://www.domain.org/feed3/rss>` optional
