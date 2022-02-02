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

