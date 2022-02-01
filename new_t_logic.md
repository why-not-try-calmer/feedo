New settings
------------
digest_at: <hh:mm>
digest_step: <int> <s|h|d>
last_digest: <time>
next_digest: <time>
follow: <bool>

Logic
-----
- if "follow" is true, the worker will poll all feeds subscribed to every 20 minutes and post the results
- if "digest_at" is defined, the worker will post a digest of all feeds subscribed to every <digest_step> fetching all items found since the last <last_digest>.
- if both are defined, 
