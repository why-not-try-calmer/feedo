## Quick examples

For the exhaustive list of commands, see this [document](https://github.com/why-not-try-calmer/feedfarer2/blob/master/COMMANDS.md).

1 digest every day at noon (UTC):

```
/set
digest_at: 12:00 
```

1 digest every day at noon (UTC) with frequent updates as soon as new updates are found:

```
/set
digest_at: 12:00
follow: true 
```

1 digest every other day at noon (UTC) with frequent updates as soon as new items are found across all feeds, with a shareable link for non-Telegram users:

```
/set
digest_at: 12:00
digest_every: 2d
follow: true 
share_link: true
```

Exclude the words _meanWord_ from all messages, and set search notifications on items from the feed at `https://www.phoronix.com/scan.php/feed` containing the words _linux kernel_:

```
/set
blacklist: meanword
search_notif: linux kernel
only_search_notif: https://www.phoronix.com/scan.php/feed
```

## Detailed explanation of the default settings
Here are the default settings, that come into play as soon as you subscribe a chat to a web feed:
```
1   blacklist: undefined
2   digest_every: 1d
3   digest_at: undefined
4   digest_size: 10
5   disable_web_view: false
6   follow: false
7   only_search: undefined
8   paused: false
9   pin: false
10  search_notif: undefined
11  share_link: true
```

That means:
1. (undefined) list of keywords that exclude items containing any of them from notifications
2. every day, post a digest (summmary) of all new items across all feeds this chat is subscribed to since last time
3. (undefined) the time of the day at which digests are sent
4. have the digest (summary) contain max. 10 items per feed
5. do not prevent Telegram from rendering the first item using the InstantView feature
6. do not try to post every new item between digests as soon as they are found
7. (undefined) list of feeds whose items should be ignored from notifications unless they match search keywords
8. not paused
9. do not try to pin digests
10. (undefined) the list of keywords used for (6)
11. append at the end of the digest (summary) a link to share it
