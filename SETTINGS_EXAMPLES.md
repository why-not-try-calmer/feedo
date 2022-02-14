## Quick examples

For the exhaustive list of commands, see this [document](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md).

1 digest every day at noon (UTC):

```
/set
digest_at: 12:00 
```

1 digest every day at noon (UTC) with frequent updates as soon as new updates are found, presented as a condensed list of feed titles, showing only the last 3 items and a link to view the full digest:

```
/set
digest_at: 12:00
follow: true
digest_collapse: 3
```

1 digest every other day at noon (UTC) with frequent updates as soon as new items are found across all feeds, with a shareable link for non-Telegram users:

```
/set
digest_at: 12:00
digest_every: 2d
follow: true 
share_link: true
```

Exclude the words _meanword_ from all messages, and set search notifications on items from the feed at `https://news.ycombinator.com/rss` for just the items that contain the string _pop\_os_:

```
/set
blacklist: meanword
search_notif: pop_os
only_search_notif: https://news.ycombinator.com/rss
```

## Detailed explanation of the default settings
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
paused: false
    # not paused
pin: false
    # do not try to pin digests
search_notif: undefined
    # the list of keywords used for 'only_search'
share_link: true
    # append at the end of the digest (summary)
    # a link for sharing with non-Telegram users
```