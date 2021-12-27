# feedfarer
A fast, efficient network app for subscribing Telegram chats to web feeds.

## What this package provides
- a Telegram bot able to post updates to any web feed (Atom or RSS) feed a chat is subscribed to; the bot is also replies directly to commands for consuming feeds at one's favorite pace
- a library consumed by the said bot so that any user can host and play with it.

## Features of the bot
- web feed notifications on updates as they come or in batches
- subscription management
- filters, blacklists
- full-text search
- supports all Telegram group types
- does not require permissions

## Usage
The bot is offered as a service to any Telegram user; just summon up [@feedfarer_bot](https://t.me/feedfarer_bot).

All chats types are supported:
- one-to-one chats (with the bot)
- group private
- group public

The bot does not require any permission; it just need to be a member of the group to which it posts updates.

__Getting started__:

1. Start a chat with the bot (1-1 chat) or invite it to any chat (group or private). The bot listens only to messages using commands defined for it.
2. Subscribe the chat and have fun with the other commands. By default updates are checked and posted every 20 minutes.

## Deployment 
The bot relies upon the Haskell `warp` server. By default it provides an endpoint at `https://<your.nice.domain>/webhook/bot<your token>` handling inbound and outbound HTTP requests from / to Telegram services via webhooks (only webhooks are supported as they provide a more resource-efficient communication method). 

This application is written from scratch, using exclusively HTTP requests-responses as specified by the [Telegram Bot API](https://core.telegram.org/bots/api). It relies on no Telegram SDK or third-party library.

## Beta testing
Ask for an invite with https://t.me/+zMdPlkeEu7w2NjM0.

Check out our channel for more info: https://t.me/feedfarer.

## Roadmap & todo
- [x] Architecture, business logic
- [x] Implementation
- [x] Tests
- [x] Test deployment
- [x] Deployment
- [x] Tests in deployment
- [...] Factoring out database details to avoid depending on mongoDB

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
### Local

This step requires having an `.env` file in the root directory set as appropriate (see below). Substitute `docker` for `podman` in the instructions if you don't use _podman_.

`cd` to to a parent directory. Then:
```
git clone https://github.com/why-not-try-calmer/feedfarer2.git
podman build . -t feedfarer-img
podman run --rm --name feedfarer -it -p 8443:80 --env-file=.env feedfarer-img
```

For compatibility with most cloud providers, the container exposes port 80.

If you need to prototype the app on your local machine you might need to pick another port and forward it to 80. It's recommended to pick a port between [443, 80, 88 or 8443] to forward to 80, as only those are compatible with Telegram HTTP API. I usually use 8443 as it does not require root permissions.

### Remote
Run with 

```
docker run --rm --name feedfarer -d -p 80:80
```

Make sure to use the appropriate method to provide the containerized app the proper environment variables (see below). You could use the `docker run` flags `--env` or `--env-file` for this purpose.

Also keep in mind that Telegram requires HTTPS for sending and receiving, but this application _does not_ do any of that. You will need to make sure that the host to which the application is deployed takes care of the HTTPS business.

## Configuration
The application expects the following environment variables:
```
ALERT_CHATID=<Telegram chat id>
MONGODB_SHARDS=<shard name>:<username>:<password>
PORT=<port number>
STARTING_FEEDS=<comma-separated feed urls>
TELEGRAM_TOKEN=<Telegram bot token>
WORKER_INTERVAL=<fetch interval in seconds>
WEBHOOK_URL=<webhook url>
```
Example:
```
ALERT_CHATID=1234567890
MONGODB_SHARDS=cluster1:my-app:passwd
STARTING_FEEDS=https://blog.system76.com/rss,https://www.reddit.com/r/pop_os.rss
TELEGRAM_TOKEN=1202309djkj@@kskdjkcjkjxkj
WEBHOOK_URL=https://mydomain.org/path/to/wehbook

```