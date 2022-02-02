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

## Explanation of the default settings
Here are the default settings, that come into play as soon as you subscribe a chat to a web feed:
```
1   digest_every: 1d
2   digest_at: undefined
3   digest_size: 10
4   disable_web_view: false
5   follow: false
6   only_search: undefined
7   paused: false
8   pin: false
9   search_notif: undefined
10  share_link: true
```

That means:
1. every day, post a digest (summmary) of all new items across all feeds this chat is subscribed to since last time
2. (undefined) the time of the day at which digests are sent
3. have the digest (summary) contain max. 10 items per feed
4. do not prevent Telegram from rendering the first item using the InstantView feature
5. do not try to post every new item between digests as soon as they are found
6. (undefined) list of feeds whose items should be ignored from notifications unless they match search keywords
7. not paused
8. do not try to pin digests
9. (undefined) the list of keywords used for (6)
10. append at the end of the digest (summary) a link to share it
