[![GitHub Test suite](https://github.com/why-not-try-calmer/feedfarer2/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/why-not-try-calmer/feedfarer2/actions/workflows/test.yml) [![Build Then Store](https://github.com/why-not-try-calmer/feedo/actions/workflows/build-then-store.yml/badge.svg)](https://github.com/why-not-try-calmer/feedo/actions/workflows/build-then-store.yml) [![Push Then Deploy](https://github.com/why-not-try-calmer/feedo/actions/workflows/push-then-deploy.yml/badge.svg)](https://github.com/why-not-try-calmer/feedo/actions/workflows/push-then-deploy.yml)

# feedo (formerly 'feedfarer')

[@feedo](https://t.me/feedo_the_bot) is a fast, efficient network app for subscribing Telegram chats to web feeds, using 0 "Telegram SDK-style" framework or dependency.

Check out [our channel](https://t.me/feedo_the_bot_channel) for the latest updates & news.

## Contents
- [What this package provides](#what-this-package-provides)
- [Bot features](#key-features)
- [Usage](#usage)
- [Roadmap](#roadmap)
- [Testing & deploying](#testing-and-deploying)
- [Support this project](#support-this-project)

## What this package provides
- a Telegram bot able to post updates to any web feed (Atom or RSS) feed a chat is subscribed to; the bot is also replies directly to commands for consuming feeds at one's favorite pace
- a server-side application consumed by the said bot so that any user can host and play with it.

## Key features
- web feed notifications on updates as they come or in digests
- subscription management
- filters, blacklists
- full-text search
- supports all Telegram group types
- web interface for managing per-group settings (currently in beta)
- does __not__ require permissions

All chats types are supported:
- one-to-one chats (with the bot)
- group private
- group public
- channels

The bot does not require any permission; it just need to be a member of the group to which it posts updates, including channels. Bot used in channels can be managed from any chats, provided that user is an admin in both the channel and the chat.

For the exhaustive list of commands, see this [document](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md).

For typical settings, see [that one](https://github.com/why-not-try-calmer/feedfarer2/blob/master/SETTINGS_EXAMPLES.md).

__Preview__:

![Demo](../assets/demo.gif?raw=true)

## Usage

### As a service
The bot is offered as a service to any Telegram user; just summon up [@feedo](https://t.me/feedo_the_bot).

__Getting started__:

1. Start a chat with the bot (1-1 chat) or invite it to any chat (group or private). The bot listens only to messages using commands defined for it.
2. Subscribe the chat to a bunch of web feeds with `/sub <url1 url2 ur3>`.
3. Adjust your settings with `/set`, as in:

```
/set  # mind the linebreak!
digest_at: 12:00 
```
for a daily noon digest.

For more typical settings, see [that one](https://github.com/why-not-try-calmer/feedfarer2/blob/master/SETTINGS_EXAMPLES.md).

### As a self-hosted application 

The bot relies upon the Haskell `warp` server. By default it provides an endpoint at `https://<your.nice.domain>/webhook/bot<your token>` handling inbound and outbound HTTP requests from / to Telegram services via webhooks.

Only webhooks are supported as they provide a more resource-efficient communication method. 

I've written the application from scratch, relying exclusively on HTTP requests-responses as specified by the [Telegram Bot API](https://core.telegram.org/bots/api). It relies on no Telegram SDK or third-party library.

## Roadmap
- [x] Architecture, business logic
- [x] Implementation
- [x] Tests
- [x] CD/CI
- [x] Deployment
- [x] Tests in production

## Testing and deploying

### Requisites

_Compose_. Deployment and integration tests depend on having running instances of the images defined in `docker-compose.yml`. Make sure you have Docker installed and appropriately configured on your system. 

_Environment variables_. Also, make sure you have an `.env` file in the root directory set as appropriate (see below).

### Testing 

`cd` to a parent directory. Then:
```
git clone https://github.com/why-not-try-calmer/feedfarer2.git
docker-compose -f docker-compose-test.yml up
```

### Deploying

Assuming you have run the tests following the method from the previous section, all you need to do is edit the following files with the values that suit your use case:

- `Dockerfile`: edit the value of `EXPOSE` as fits
- `docker-compose.yml`: edit the value of `image` to the correct destination for the output image (i.e. typically your container registry); also make sure the both `ports` fields are associated with the appropriate values.

Finally:

```
docker-compose build
docker-compose push
```

### Configuration

When testing locally, simply create an `.env` file at the root of the directory.

When deploying, use your provider's proposed method. Most cloud providers offer a "Configuration" menu defining custom environment variables. If you plan on deploying with remote CI/CD, make use of "secrets" to pass the values to Compose (you can take a look at `.github/workflows` for examples).

__Schema__

The application expects the following environment variables:
```
ALERT_CHATID=<Telegram chat id where you want to receive application alerts>

MONGODB_CONNECTION_STRING=< host_name:db_name:user_name:password >

STARTING_FEEDS=< comma-separated feed urls if you want to preload the application with some well-known feeds >

TELEGRAM_TOKEN=< the token of your Telegram bot >

WEBHOOK_URL=< webhook url >
```

__Example__
```
ALERT_CHATID=1234567890

MONGODB_CONNECTION_STRING=<host_name:db_name:user_name:password>

STARTING_FEEDS=https://blog.system76.com/rss,https://www.reddit.com/r/pop_os.rss

TELEGRAM_TOKEN=1202309djkj@@kskdjkcjkjxkj

WEBHOOK_URL=https://mydomain.org/path/to/wehbook

```

## Support this project
I am hosting the bot on my own. If you want to [pay me a coffee or a beer](https://www.buymeacoffee.com/WhyNotTryCalmer) to show your appreciation, that will help me maintain the code and pay the electricity bills...
