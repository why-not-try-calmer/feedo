# Changelog for feedfarer

## January

## 10th 
_Part 1_
- fixed a typo that would cause erroneous reports of currently subscribed to feeds
- fixed bad formatting of MarkdownReply link entities
- deprecated /add command
- deprecated "Last read" field on feed

_Part 2_
- fixed missing line break between distinct lists of feed items
- added new command: "/settings" witch takes either 0 argument or three mandatory arguments, <url or #>, <max number of items in each batch> and <max interval between each batch>.
- fixed links getting excessively escaping