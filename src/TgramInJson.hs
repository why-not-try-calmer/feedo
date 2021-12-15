{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains objects which represent data of Telegram Bot API responses
module TgramInJson (Update (..), TgGetChatResponse (..), TgGetChatMembersResponse (..), Chat (..), Message (..), ChatMember (..), User (..), ChatType (..)) where

import Data.Aeson
import qualified Data.Char as Char
import Data.Int (Int64)
import Data.List
import Data.Text (Text)
import GHC.Generics
import TgramOutJson (parseJsonDrop, toJsonDrop)

data TgGetChatResponse = TgGetChatResponse {resp_ok :: Bool, resp_result :: Chat} deriving (Show, Generic)

instance ToJSON TgGetChatResponse where
  toJSON = toJsonDrop 5

instance FromJSON TgGetChatResponse where
  parseJSON = parseJsonDrop 5

data TgGetChatMembersResponse = TgGetChatMembersResponse {resp_cm_ok :: Bool, resp_cm_result :: [ChatMember]} deriving (Show, Generic)

instance ToJSON TgGetChatMembersResponse where
  toJSON = toJsonDrop 8

instance FromJSON TgGetChatMembersResponse where
  parseJSON = parseJsonDrop 8

-- | This object represents a Telegram user or bot.
data User = User
  { -- | Unique identifier for this user or bot
    user_id :: Int64,
    -- | True, if this user is a bot
    user_is_bot :: Bool,
    -- | User‘s or bot’s first name
    user_first_name :: Text,
    -- | User‘s or bot’s last name
    user_last_name :: Maybe Text,
    -- | User‘s or bot’s username
    user_username :: Maybe Text,
    user_language_code :: Maybe LanguageCode
  }
  deriving (Show, Generic)

instance ToJSON User where
  toJSON = toJsonDrop 5

instance FromJSON User where
  parseJSON = parseJsonDrop 5

newtype LanguageCode = LanguageCode Text
  deriving (Show, Eq, Ord)

instance ToJSON LanguageCode where
  toJSON (LanguageCode code) = toJSON code

instance FromJSON LanguageCode where
  parseJSON (String code) = pure $ LanguageCode code
  parseJSON _ = fail "Unable to parse LanguageCode"

data VideoNote = VideoNote
  { -- | Unique identifier for this file
    vid_note_file_id :: Text,
    -- | Video width and height as defined by sender
    vid_note_length :: Int,
    -- | Duration of the video in seconds as defined by sender
    vid_note_duration :: Int,
    -- | Video thumbnail
    vid_note_thumb :: Maybe PhotoSize,
    -- | File size
    vid_note_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON VideoNote where
  toJSON = toJsonDrop 9

instance FromJSON VideoNote where
  parseJSON = parseJsonDrop 9

-- | This object represents a phone contact.
data Contact = Contact
  { -- | Contact's phone number
    contact_phone_number :: Text,
    -- | Contact's first name
    contact_first_name :: Text,
    -- | Contact's last name
    contact_last_name :: Maybe Text,
    -- | Contact's user identifier in Telegram
    contact_user_id :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = toJsonDrop 8

instance FromJSON Contact where
  parseJSON = parseJsonDrop 8

-- | This object represents a chat.
data Chat = Chat
  { -- | Unique identifier for this chat.
    -- This number may be greater than 32 bits and some programming languages
    -- may have difficulty/silent defects in interpreting it.
    -- But it is smaller than 52 bits,
    -- so a signed 64 bit integer or double-precision float type are safe for
    -- storing this identifier.
    chat_id :: Int64,
    -- | Type of chat, can be either 'Private', 'Group', 'Supergroup' or 'Channel'
    chat_type :: ChatType,
    -- | Title, for channels and group chats
    chat_title :: Maybe Text,
    -- | Username, for private chats and channels if available
    chat_username :: Maybe Text,
    -- | First name of the other party in a private chat
    chat_first_name :: Maybe Text,
    -- | Last name of the other party in a private chat
    chat_last_name :: Maybe Text,
    -- | True if a group has ‘All Members Are Admins’ enabled.
    chat_all_members_are_administrators :: Maybe Bool,
    -- | Chat photo. Returned only in 'getChat'.
    chat_photo :: Maybe ChatPhoto,
    -- | Description, for supergroups and channel chats. Returned only in `getChat`.
    chat_bio :: Maybe Text,
    chat_description :: Maybe Text,
    -- | Chat invite link, for supergroups and channel chats. Returned only in `getChat`.
    chat_invite_link :: Maybe Text,
    -- | Pinned message, for supergroups. Returned only in 'getChat'.
    chat_pinned_message :: Maybe Message,
    -- | For supergroups, name of group sticker set. Returned only in 'getChat'.
    chat_permissions :: Maybe ChatPermissions,
    chat_slow_mode_delay :: Maybe Int,
    chat_message_auto_delete_timer :: Maybe Int,
    chat_sticker_set_name :: Maybe Text,
    -- | True, if the bot can change the group sticker set. Returned only in 'getChat'.
    chat_can_set_sticker_set :: Maybe Bool,
    chat_linked_chat_id :: Maybe Int,
    chat_location :: Maybe Location
  }
  deriving (Show, Generic)

instance ToJSON Chat where
  toJSON = toJsonDrop 5

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

data ChatPermissions = ChatPermissions
  { p_can_send_messages :: Maybe Bool,
    p_can_send_media_messages :: Maybe Bool,
    p_can_send_polls :: Maybe Bool,
    p_can_send_other_messages :: Maybe Bool,
    p_can_add_web_page_previews :: Maybe Bool,
    p_can_change_info :: Maybe Bool,
    p_can_invite_users :: Maybe Bool,
    p_can_pin_messages :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON ChatPermissions where
  toJSON = toJsonDrop 2

instance FromJSON ChatPermissions where
  parseJSON = parseJsonDrop 2

-- | Type of chat.
data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving (Eq, Show, Generic)

instance ToJSON ChatType where
  toJSON Private = "private"
  toJSON Group = "group"
  toJSON Supergroup = "supergroup"
  toJSON Channel = "channel"

instance FromJSON ChatType where
  parseJSON "private" = pure Private
  parseJSON "group" = pure Group
  parseJSON "supergroup" = pure Supergroup
  parseJSON "channel" = pure Channel
  parseJSON _ = fail "Failed to parse ChatType"

-- | Parse mode for text message
data ParseMode = MarkdownReply | HTML deriving (Show, Generic)

instance ToJSON ParseMode where
  toJSON MarkdownReply = "MarkdownReply"
  toJSON HTML = "HTML"

instance FromJSON ParseMode where
  parseJSON "MarkdownReply" = pure MarkdownReply
  parseJSON "HTML" = pure HTML
  parseJSON _ = fail "Failed to parse ParseMode"

-- | This object represents one size of a photo or a 'File' / 'Sticker' thumbnail.
data PhotoSize = PhotoSize
  { -- | Unique identifier for this file
    photo_file_id :: Text,
    -- | Photo width
    photo_width :: Int,
    -- | Photo height
    photo_height :: Int,
    -- | File size
    photo_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON PhotoSize where
  toJSON = toJsonDrop 6

instance FromJSON PhotoSize where
  parseJSON = parseJsonDrop 6

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { -- | Unique identifier for this file
    audio_file_id :: Text,
    -- | Duration of the audio in seconds as defined by sender
    audio_duration :: Int,
    -- | Performer of the audio as defined by sender or by audio tags
    audio_performer :: Maybe Text,
    -- | Title of the audio as defined by sender or by audio tags
    audio_title :: Maybe Text,
    -- | MIME type of the file as defined by sender
    audio_mime_type :: Maybe Text,
    -- | File size
    audio_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Audio where
  toJSON = toJsonDrop 6

instance FromJSON Audio where
  parseJSON = parseJsonDrop 6

-- | This object represents a general file (as opposed to 'PhotoSize', 'Voice' messages and 'Audio' files).
data Document = Document
  { -- | Unique file identifier
    doc_file_id :: Text,
    -- | Document thumbnail as defined by sender
    doc_thumb :: Maybe PhotoSize,
    -- | Original filename as defined by sender
    doc_file_name :: Maybe Text,
    -- | MIME type of the file as defined by sender
    doc_mime_type :: Maybe Text,
    -- | File size
    doc_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Document where
  toJSON = toJsonDrop 4

instance FromJSON Document where
  parseJSON = parseJsonDrop 4

-- | This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.
data Game = Game
  { -- | Title of the game
    game_title :: Text,
    -- | Description of the game
    game_description :: Text,
    -- | Photo that will be displayed in the game message in chats.
    game_photo :: [PhotoSize],
    -- | Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.
    game_text :: Maybe Text,
    -- | Special entities that appear in text, such as usernames, URLs, bot commands, etc.
    game_text_entities :: Maybe [MessageEntity],
    -- | Animation that will be displayed in the game message in chats. Upload via BotFather
    game_animation :: Maybe Animation
  }
  deriving (Show, Generic)

instance ToJSON Game where
  toJSON = toJsonDrop 5

instance FromJSON Game where
  parseJSON = parseJsonDrop 5

-- | This object represents an animation file to be displayed in the message containing a game.
data Animation = Animation
  { -- | Unique file identifier
    anim_file_id :: Text,
    -- | Animation thumbnail as defined by sender
    anim_thumb :: Maybe PhotoSize,
    -- | Original animation filename as defined by sender
    anim_file_name :: Maybe Text,
    -- | MIME type of the file as defined by sender
    anim_mime_type :: Maybe Text,
    -- | File size
    anim_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Animation where
  toJSON = toJsonDrop 5

instance FromJSON Animation where
  parseJSON = parseJsonDrop 5

-- | This object represents a sticker.
data Sticker = Sticker
  { -- | Unique identifier for this file
    sticker_file_id :: Text,
    -- | Sticker width
    sticker_width :: Int,
    -- | Sticker height
    sticker_height :: Int,
    -- | Sticker thumbnail in .webp or .jpg format
    sticker_thumb :: Maybe PhotoSize,
    -- | Emoji associated with the sticker
    sticker_emoji :: Maybe Text,
    sticker_set_name :: Maybe Text,
    sticker_mask_position :: Maybe MaskPosition,
    -- | File size
    sticker_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Sticker where
  toJSON = toJsonDrop 8

instance FromJSON Sticker where
  parseJSON = parseJsonDrop 8

-- | This object represents a video file.
data Video = Video
  { -- | Unique identifier for this file
    video_file_id :: Text,
    -- | Video width as defined by sender
    video_width :: Int,
    -- | Video height as defined by sender
    video_height :: Int,
    -- | Duration of the video in seconds as defined by sender
    video_duration :: Int,
    -- | Video thumbnail
    video_thumb :: Maybe PhotoSize,
    -- | MIME type of a file as defined by sender
    video_mime_type :: Maybe Text,
    -- | File size
    video_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Video where
  toJSON = toJsonDrop 6

instance FromJSON Video where
  parseJSON = parseJsonDrop 6

-- | This object represents a voice note.
data Voice = Voice
  { -- | Unique identifier for this file
    voice_file_id :: Text,
    -- | Duration of the audio in seconds as defined by sender
    voice_duration :: Int,
    -- | MIME type of the file as defined by sender
    voice_mime_type :: Maybe Text,
    -- | File size
    voice_file_size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Voice where
  toJSON = toJsonDrop 6

instance FromJSON Voice where
  parseJSON = parseJsonDrop 6

-- | This object represents an incoming inline query. When the user sends an empty query, your bot could pure some default or trending results.
data InlineQuery = InlineQuery
  { -- | Unique identifier for this query
    query_id :: Text,
    -- | Sender
    query_from :: User,
    -- | Sender location, only for bots that request user location
    query_location :: Maybe Location,
    -- | Text of the query
    query_query :: Text,
    -- | Offset of the results to be returned, can be controlled by the bot
    query_offset :: Text
  }
  deriving (Show, Generic)

instance ToJSON InlineQuery where
  toJSON = toJsonDrop 6

instance FromJSON InlineQuery where
  parseJSON = parseJsonDrop 6

-- | This object represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  { -- | The unique identifier for the result that was chosen
    chosen_result_id :: Text,
    -- | The user that chose the result
    chosen_from :: User,
    -- | Sender location, only for bots that require user location
    chosen_location :: Maybe Location,
    -- | Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
    chosen_inline_message_id :: Maybe Text,
    -- | The query that was used to obtain the result
    chosen_query :: Text
  }
  deriving (Show, Generic)

instance ToJSON ChosenInlineResult where
  toJSON = toJsonDrop 7

instance FromJSON ChosenInlineResult where
  parseJSON = parseJsonDrop 7

-- | This object represents the content of a message to be sent as a result of an inline query.
data InputMessageContent
  = -- | Represents the content of a text message to be sent as the result of an inline query.
    InputTextMessageContent
      { -- | Text of the message to be sent, 1-4096 characters
        imc_message_text :: Text,
        -- | Send 'MarkdownReply' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
        imc_parse_mode :: Maybe ParseMode,
        -- | Disables link previews for links in the sent message
        imc_disable_web_page_preview :: Maybe Bool
      }
  | -- | Represents the content of a location message to be sent as the result of an inline query.
    InputLocationMessageContent
      { -- | Latitude of the location in degrees
        imc_latitude :: Float,
        -- | Longitude of the location in degrees
        imc_longitude :: Float
      }
  | -- | Represents the content of a venue message to be sent as the result of an inline query.
    InputVenueMessageContent
      { -- | Latitude of the location in degrees
        imc_latitude :: Float,
        -- | Longitude of the location in degrees
        imc_longitude :: Float,
        -- | Name of the venue
        imc_title :: Text,
        -- | Address of the venue
        imc_address :: Text,
        -- | Foursquare identifier of the venue, if known
        imc_foursquare_id :: Maybe Text
      }
  | -- | Represents the content of a contact message to be sent as the result of an inline query.
    InputContactMessageContent
      { -- | Contact's phone number
        imc_phone_number :: Text,
        -- | Contact's first name
        imc_first_name :: Text,
        -- | Contact's last name
        imc_last_name :: Maybe Text
      }
  deriving (Show, Generic)

instance ToJSON InputMessageContent where
  toJSON = toJsonDrop 4

instance FromJSON InputMessageContent where
  parseJSON = parseJsonDrop 4

data InlineQueryResult
  = -- | Represents a link to an article or web page.
    InlineQueryResultArticle
      { -- | Unique identifier for this result, 1-64 Bytes
        iq_res_id :: Text,
        -- | Title of the result
        iq_res_title :: Maybe Text,
        -- | Content of the message to be sent
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | URL of the result
        iq_res_url :: Maybe Text,
        -- | Pass True, if you don't want the URL to be shown in the message
        iq_res_hide_url :: Maybe Bool,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Url of the thumbnail for the result
        iq_res_thumb_url :: Maybe Text,
        -- | Thumbnail width
        iq_res_thumb_width :: Maybe Int,
        -- | Thumbnail height
        iq_res_thumb_height :: Maybe Int
      }
  | -- | Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
    InlineQueryResultPhoto
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB
        iq_res_photo_url :: Text,
        -- | URL of the thumbnail for the photo
        iq_res_thumb_url :: Maybe Text,
        -- | Optional. Width of the photo
        iq_res_photo_width :: Maybe Int,
        -- | Optional. Height of the photo
        iq_res_photo_height :: Maybe Int,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Caption of the photo to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the photo
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
    InlineQueryResultGif
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL for the GIF file. File size must not exceed 1MB
        iq_res_gif_url :: Text,
        -- | Width of the GIF
        iq_res_gif_width :: Maybe Int,
        -- | Height of the GIF
        iq_res_gif_height :: Maybe Int,
        -- | URL of the static thumbnail for the result (jpeg or gif)
        iq_res_thumb_url :: Maybe Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the GIF file to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the GIF animation
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Duration of the GIF
        iq_res_gif_duration :: Maybe Int
      }
  | -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
    InlineQueryResultMpeg4Gif
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL for the MP4 file. File size must not exceed 1MB
        iq_res_mpeg4_url :: Text,
        -- | Video width
        iq_res_mpeg4_width :: Maybe Int,
        -- | Video height
        iq_res_mpeg4_height :: Maybe Int,
        -- | URL of the static thumbnail (jpeg or gif) for the result
        iq_res_thumb_url :: Maybe Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the MPEG-4 file to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the video animation
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Video duration
        iq_res_mpeg4_duration :: Maybe Int
      }
  | -- | Represents link to a page containing an embedded video player or a video file.
    InlineQueryResultVideo
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL for the embedded video player or video file
        iq_res_video_url :: Text,
        -- | Mime type of the content of video url, “text/html” or “video/mp4”
        iq_res_mime_type :: Text,
        -- | URL of the thumbnail (jpeg only) for the video
        iq_res_thumb_url :: Maybe Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the video to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Video width
        iq_res_video_width :: Maybe Int,
        -- | Video height
        iq_res_video_height :: Maybe Int,
        -- | Video duration in seconds
        iq_res_video_duration :: Maybe Int,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the video
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to an mp3 audio file. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
    InlineQueryResultAudio
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL for the audio file
        iq_res_audio_url :: Text,
        -- | Title
        iq_res_title :: Maybe Text,
        -- | Caption, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Performer
        iq_res_performer :: Maybe Text,
        -- | Audio duration in seconds
        iq_res_audio_duration :: Maybe Int,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the audio
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a voice recording in an .ogg container encoded with OPUS. By default, this voice recording will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the the voice message.
    InlineQueryResultVoice
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid URL for the voice recording
        iq_res_voice_url :: Text,
        -- | Recording title
        iq_res_title :: Maybe Text,
        -- | Caption, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Recording duration in seconds
        iq_res_voice_duration :: Maybe Int,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the voice recording
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a file. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only .PDF and .ZIP files can be sent using this method.
    InlineQueryResultDocument
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the document to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | A valid URL for the file
        iq_res_document_url :: Text,
        -- | Mime type of the content of the file, either “application/pdf” or “application/zip”
        iq_res_mime_type :: Text,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the file
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | URL of the thumbnail (jpeg only) for the file
        iq_res_thumb_url :: Maybe Text,
        -- | Thumbnail width
        iq_res_thumb_width :: Maybe Int,
        -- | Thumbnail height
        iq_res_thumb_height :: Maybe Int
      }
  | -- | Represents a location on a map. By default, the location will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the location.
    InlineQueryResultLocation
      { -- | Unique identifier for this result, 1-64 Bytes
        iq_res_id :: Text,
        -- | Location latitude in degrees
        iq_res_latitude :: Float,
        -- | Location longitude in degrees
        iq_res_longitude :: Float,
        -- | Location title
        iq_res_title :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the location
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Url of the thumbnail for the result
        iq_res_thumb_url :: Maybe Text,
        -- | Thumbnail width
        iq_res_thumb_width :: Maybe Int,
        -- | Thumbnail height
        iq_res_thumb_height :: Maybe Int
      }
  | -- | Represents a venue. By default, the venue will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the venue.
    InlineQueryResultVenue
      { -- | Unique identifier for this result, 1-64 Bytes
        iq_res_id :: Text,
        -- | Latitude of the venue location in degrees
        iq_res_latitude :: Float,
        -- | Longitude of the venue location in degrees
        iq_res_longitude :: Float,
        -- | Title of the venue
        iq_res_title :: Maybe Text,
        -- | Address of the venue
        iq_res_address :: Text,
        -- | Foursquare identifier of the venue if known
        iq_res_foursquare_id :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the venue
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Url of the thumbnail for the result
        iq_res_thumb_url :: Maybe Text,
        -- | Thumbnail width
        iq_res_thumb_width :: Maybe Int,
        -- | Thumbnail height
        iq_res_thumb_height :: Maybe Int
      }
  | -- | Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
    InlineQueryResultContact
      { -- | Unique identifier for this result, 1-64 Bytes
        iq_res_id :: Text,
        -- | Contact's phone number
        iq_res_phone_number :: Text,
        -- | Contact's first name
        iq_res_first_name :: Text,
        -- | Contact's last name
        iq_res_last_name :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the contact
        iq_res_input_message_content :: Maybe InputMessageContent,
        -- | Url of the thumbnail for the result
        iq_res_thumb_url :: Maybe Text,
        -- | Thumbnail width
        iq_res_thumb_width :: Maybe Int,
        -- | Thumbnail height
        iq_res_thumb_height :: Maybe Int
      }
  | -- | Represents a Game.
    InlineQueryResultGame
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | Short name of the game
        iq_res_game_short_name :: Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup
      }
  | -- | Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
    InlineQueryResultCachedPhoto
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier of the photo
        iq_res_photo_file_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Caption of the photo to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the photo
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to an animated GIF file stored on the Telegram servers. By default, this animated GIF file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with specified content instead of the animation.
    InlineQueryResultCachedGif
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier for the GIF file
        iq_res_gif_file_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the GIF file to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the GIF animation
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers. By default, this animated MPEG-4 file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
    InlineQueryResultCachedMpeg4Gif
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier for the MP4 file
        iq_res_mpeg4_file_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Caption of the MPEG-4 file to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the video animation
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a sticker stored on the Telegram servers. By default, this sticker will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the sticker.
    InlineQueryResultCachedSticker
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier of the sticker
        iq_res_sticker_file_id :: Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the sticker
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a file stored on the Telegram servers. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only pdf-files and zip archives can be sent using this method.
    InlineQueryResultCachedDocument
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | A valid file identifier for the file
        iq_res_document_file_id :: Text,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Caption of the document to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the file
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a video file stored on the Telegram servers. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the video.
    InlineQueryResultCachedVideo
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier for the video file
        iq_res_video_file_id :: Text,
        -- | Title for the result
        iq_res_title :: Maybe Text,
        -- | Short description of the result
        iq_res_description :: Maybe Text,
        -- | Caption of the video to be sent, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | Content of the message to be sent instead of the video
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to a voice message stored on the Telegram servers. By default, this voice message will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the voice message.
    InlineQueryResultCachedVoice
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier for the voice message
        iq_res_voice_file_id :: Text,
        -- | Voice message title
        iq_res_title :: Maybe Text,
        -- | Caption, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | ontent of the message to be sent instead of the voice message
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  | -- | Represents a link to an mp3 audio file stored on the Telegram servers. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
    InlineQueryResultCachedAudio
      { -- | Unique identifier for this result, 1-64 bytes
        iq_res_id :: Text,
        -- | A valid file identifier for the audio file
        iq_res_audio_file_id :: Text,
        -- | Caption, 0-200 characters
        iq_res_caption :: Maybe Text,
        -- | An Inline keyboard attached to the message
        iq_res_reply_markup :: Maybe InlineKeyboardMarkup,
        -- | ontent of the message to be sent instead of the audio
        iq_res_input_message_content :: Maybe InputMessageContent
      }
  deriving (Show, Generic)

dropCached :: String -> String
dropCached name = if "Cached" `isPrefixOf` name then drop 6 name else name

tagModifier :: String -> String
tagModifier "InlineQueryResultMpeg4Gif" = "mpeg4_gif"
tagModifier "InlineQueryResultCachedMpeg4Gif" = "mpeg4_gif"
tagModifier x = (fmap Char.toLower . dropCached . drop 17) x

inlineQueryJSONOptions :: Options
inlineQueryJSONOptions =
  defaultOptions
    { fieldLabelModifier = drop 7,
      omitNothingFields = True,
      sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = undefined},
      constructorTagModifier = tagModifier
    }

instance ToJSON InlineQueryResult where
  toJSON = genericToJSON inlineQueryJSONOptions

instance FromJSON InlineQueryResult where
  parseJSON = genericParseJSON inlineQueryJSONOptions

inlineQueryResultArticle :: Text -> Text -> InputMessageContent -> InlineQueryResult
inlineQueryResultArticle id title content = InlineQueryResultArticle id (Just title) (Just content) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultPhoto :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultPhoto id photoUrl thumbUlr = InlineQueryResultPhoto id photoUrl (Just thumbUlr) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultGif :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultGif id gifUrl thumbUrl = InlineQueryResultGif id gifUrl Nothing Nothing (Just thumbUrl) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultMpeg4Gif :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultMpeg4Gif id mpeg4Url thumbUrl = InlineQueryResultMpeg4Gif id mpeg4Url Nothing Nothing (Just thumbUrl) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultVideo :: Text -> Text -> Text -> Text -> Text -> InlineQueryResult
inlineQueryResultVideo id videoUrl mimeType thumbUrl title = InlineQueryResultVideo id videoUrl mimeType (Just thumbUrl) (Just title) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultAudio :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultAudio id audioUrl title = InlineQueryResultAudio id audioUrl (Just title) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultVoice :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultVoice id voiceUrl title = InlineQueryResultVoice id voiceUrl (Just title) Nothing Nothing Nothing Nothing

inlineQueryResultDocument :: Text -> Text -> Text -> Text -> InlineQueryResult
inlineQueryResultDocument id title docUrl mimeType = InlineQueryResultDocument id (Just title) Nothing docUrl mimeType Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultLocation :: Text -> Float -> Float -> Text -> InlineQueryResult
inlineQueryResultLocation id lat lon title = InlineQueryResultLocation id lat lon (Just title) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultVenue :: Text -> Float -> Float -> Text -> Text -> InlineQueryResult
inlineQueryResultVenue id lat lon title address = InlineQueryResultVenue id lat lon (Just title) address Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultContact :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultContact id phoneNumber firstName = InlineQueryResultContact id phoneNumber firstName Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultGame :: Text -> Text -> InlineQueryResult
inlineQueryResultGame id gameShortName = InlineQueryResultGame id gameShortName Nothing

inlineQueryResultCachedPhoto :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedPhoto id fileId = InlineQueryResultCachedPhoto id fileId Nothing Nothing Nothing Nothing Nothing

inlineQueryResultCachedGif :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedGif id fileId = InlineQueryResultCachedGif id fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedMpeg4Gif :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedMpeg4Gif id fileId = InlineQueryResultCachedMpeg4Gif id fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedSticker :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedSticker id fileId = InlineQueryResultCachedSticker id fileId Nothing Nothing

inlineQueryResultCachedDocument :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedDocument id fileId title = InlineQueryResultCachedDocument id (Just title) fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedVideo :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedVideo id fileId title = InlineQueryResultCachedVideo id fileId (Just title) Nothing Nothing Nothing Nothing

inlineQueryResultCachedVoice :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedVoice id fileId title = InlineQueryResultCachedVoice id fileId (Just title) Nothing Nothing Nothing

inlineQueryResultCachedAudio :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedAudio id fileId = InlineQueryResultCachedAudio id fileId Nothing Nothing Nothing

data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inline_keyboard :: [[InlineKeyboardButton]]
  }
  deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  { -- | Label text on the button
    ikb_text :: Text,
    -- | HTTP url to be opened when button is pressed
    ikb_url :: Maybe Text,
    -- | Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
    ikb_callback_data :: Maybe Text,
    -- |  If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
    ikb_switch_inline_query :: Maybe Text,
    -- |  Description of the game that will be launched when the user presses the button. NOTE: This type of button must always be the first button in the first row.
    ikb_callback_game :: Maybe CallbackGame,
    -- | If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.
    ikb_switch_inline_query_current_chat :: Maybe Text,
    -- | Specify True, to send a Pay button. NOTE: This type of button must always be the first button in the first row.
    ikb_pay :: Maybe Bool
  }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4

inlineKeyboardButton :: Text -> InlineKeyboardButton
inlineKeyboardButton buttonText =
  InlineKeyboardButton buttonText Nothing Nothing Nothing Nothing Nothing Nothing

data CallbackGame = CallbackGame
  {
  }
  deriving (Show, Generic)

instance ToJSON CallbackGame where
  toJSON = toJsonDrop 3

instance FromJSON CallbackGame where
  parseJSON = parseJsonDrop 3

data CallbackQuery = CallbackQuery
  { cq_id :: Text,
    cq_from :: User,
    cq_message :: Maybe Message,
    cq_inline_message_id :: Maybe Text,
    cq_chat_instance :: Text,
    cq_data :: Maybe Text,
    cq_game_short_name :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 3

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

-- | This object represents an incoming update.
-- Only one of the optional parameters can be present in any given update.
data Update = Update
  { -- | The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
    update_id :: Int,
    -- | New incoming message of any kind — text, photo, sticker, etc.
    message :: Maybe Message,
    -- | New version of a message that is known to the bot and was edited
    edited_message :: Maybe Message,
    -- | New incoming channel post of any kind — text, photo, sticker, etc.
    channel_post :: Maybe Message,
    -- | New version of a channel post that is known to the bot and was edited
    edited_channel_post :: Maybe Message,
    -- | New incoming inline query
    inline_query :: Maybe InlineQuery,
    -- | The result of a inline query that was chosen by a user and sent to their chat partner
    chosen_inline_result :: Maybe ChosenInlineResult,
    -- | This object represents an incoming callback query from a callback button in an inline keyboard. If the button that originated the query was attached to a message sent by the bot, the field message will be presented. If the button was attached to a message sent via the bot (in inline mode), the field inline_message_id will be presented.
    callback_query :: Maybe CallbackQuery,
    -- |  New incoming shipping query. Only for invoices with flexible price
    shipping_query :: Maybe ShippingQuery,
    -- | New incoming pre-checkout query. Contains full information about checkout
    pre_checkout_query :: Maybe PreCheckoutQuery
  }
  deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a point on the map.
data Location = Location
  { -- | Longitude as defined by sender
    longitude :: Float,
    -- | Latitude as defined by sender
    latitude :: Float
  }
  deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a file ready to be downloaded. The file can be downloaded via the link
--   @https://api.telegram.org/file/bot<token>/<file_path>@. It is guaranteed that the link will be valid
--   for at least 1 hour. When the link expires, a new one can be requested by calling 'getFile'.
--
--       Maximum file size to download is 20 MB
data File = File
  { -- | Unique identifier for this file
    file_id :: Text,
    -- | File size, if known
    file_size :: Maybe Int,
    -- | File path. Use @https://api.telegram.org/file/bot<token>/<file_path>@ to get the file.
    file_path :: Maybe Text
  }
  deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { -- | Total number of profile pictures the target user has
    total_count :: Int,
    -- | Requested profile pictures (in up to 4 sizes each)
    photos :: [[PhotoSize]]
  }
  deriving (FromJSON, ToJSON, Show, Generic)

data ChatMember = ChatMember
  { -- | Information about the user
    cm_user :: User,
    -- | The member's status in the chat. Can be “creator”, “administrator”, “member”, “left” or “kicked”
    cm_status :: Text,
    -- | Restictred and kicked only. Date when restrictions will be lifted for this user, unix time
    cm_until_date :: Maybe Integer,
    -- | Administrators only. True, if the bot is allowed to edit administrator privileges of that user
    cm_can_be_edited :: Maybe Bool,
    -- | Administrators only. True, if the administrator can change the chat title, photo and other settings
    cm_can_change_info :: Maybe Bool,
    -- | Administrators only. True, if the administrator can post in the channel, channels only
    cm_can_post_messages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can edit messages of other users and can pin messages, channels only
    cm_can_edit_messages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can delete messages of other users
    cm_can_delete_messages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can invite new users to the chat
    cm_can_invite_users :: Maybe Bool,
    -- | Administrators only. True, if the administrator can restrict, ban or unban chat members
    cm_can_restrict_members :: Maybe Bool,
    -- | Administrators only. True, if the administrator can pin messages, supergroups only
    cm_can_pin_messages :: Maybe Bool,
    -- | Administrators only. True, if the administrator can add new administrators with a subset of his own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user)
    cm_can_promote_members :: Maybe Bool,
    -- | Restricted only. True, if the user can send text messages, contacts, locations and venues
    cm_can_send_messages :: Maybe Bool,
    -- | Restricted only. True, if the user can send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages
    cm_can_send_media_messages :: Maybe Bool,
    -- | Restricted only. True, if the user can send animations, games, stickers and use inline bots, implies can_send_media_messages
    cm_can_send_other_messages :: Maybe Bool,
    -- | Restricted only. True, if user may add web page previews to his messages, implies can_send_media_messages
    cm_can_add_web_page_previews :: Maybe Bool
  }
  deriving (Show, Generic)

instance ToJSON ChatMember where
  toJSON = toJsonDrop 3

instance FromJSON ChatMember where
  parseJSON = parseJsonDrop 3

data ChatPhoto = ChatPhoto
  { -- | Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
    chat_photo_small_file_id :: Text,
    -- | Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
    chat_photo_big_file_id :: Text
  }
  deriving (Show, Generic)

instance ToJSON ChatPhoto where
  toJSON = toJsonDrop 11

instance FromJSON ChatPhoto where
  parseJSON = parseJsonDrop 11

-- | This object represents a message.
data Message = Message
  { -- | Unique message identifier
    message_id :: Int,
    -- | Sender, can be empty for messages sent to channels
    from :: Maybe User,
    -- | Date the message was sent in Unix time
    date :: Int,
    -- | Conversation the message belongs to
    chat :: Chat,
    -- | For forwarded messages, sender of the original message
    forward_from :: Maybe User,
    -- | For messages forwarded from a channel, information about the original channel
    forward_from_chat :: Maybe Chat,
    -- | For forwarded channel posts, identifier of the original message in the channel
    forward_from_message_id :: Maybe Int,
    -- | For messages forwarded from channels, signature of the post author if present
    forward_signature :: Maybe Text,
    -- | For forwarded messages, date the original message was sent in Unix time
    forward_date :: Maybe Int,
    -- | For replies, the original message. Note that the 'Message' object in this field will not contain further 'reply_to_message' fields even if it itself is a reply.
    reply_to_message :: Maybe Message,
    -- | Date the message was last edited in Unix time
    edit_date :: Maybe Int,
    -- | The unique identifier of a media message group this message belongs to
    media_group_id :: Maybe Text,
    -- | Signature of the post author for messages in channels
    author_signature :: Maybe Text,
    -- | For text messages, the actual UTF-8 text of the message
    text :: Maybe Text,
    -- | For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
    entities :: Maybe [MessageEntity],
    -- | or messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
    caption_entities :: Maybe [MessageEntity],
    -- | Message is an audio file, information about the file
    audio :: Maybe Audio,
    -- | Message is a general file, information about the file
    document :: Maybe Document,
    -- | Message is a game, information about the game
    game :: Maybe Game,
    -- | Message is a photo, available sizes of the photo
    photo :: Maybe [PhotoSize],
    -- | Message is a sticker, information about the sticker
    sticker :: Maybe Sticker,
    -- | Message is a video, information about the video
    video :: Maybe Video,
    -- | Message is a voice message, information about the file
    voice :: Maybe Voice,
    -- | Message is a video note, information about the video message
    video_note :: Maybe VideoNote,
    -- | Caption for the photo or video
    caption :: Maybe Text,
    -- | Message is a shared contact, information about the contact
    contact :: Maybe Contact,
    -- | Message is a shared location, information about the location
    location :: Maybe Location,
    -- | Message is a venue, information about the venue
    venue :: Maybe Venue,
    -- | A new member was added to the group, information about them (this member may be the bot itself)
    new_chat_member :: Maybe User,
    -- | New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
    new_chat_members :: Maybe [User],
    -- | A member was removed from the group, information about them (this member may be the bot itself)
    left_chat_member :: Maybe User,
    -- | A chat title was changed to this value
    new_chat_title :: Maybe Text,
    -- | A chat photo was change to this value
    new_chat_photo :: Maybe [PhotoSize],
    -- | Service message: the chat photo was deleted
    delete_chat_photo :: Maybe Bool,
    -- | Service message: the group has been created
    group_chat_created :: Maybe Bool,
    -- | Service message: the supergroup has been created
    supergroup_chat_created :: Maybe Bool,
    -- | Service message: the channel has been created
    channel_chat_created :: Maybe Bool,
    -- | The group has been migrated to a supergroup with the specified identifier, not exceeding 1e13 by absolute value
    migrate_to_chat_id :: Maybe Int64,
    -- | The supergroup has been migrated from a group with the specified identifier, not exceeding 1e13 by absolute value
    migrate_from_chat_id :: Maybe Int64,
    -- | Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
    pinned_message :: Maybe Message,
    -- |  Message is an invoice for a payment, information about the invoice.
    invoice :: Maybe Invoice,
    -- |  Message is a service message about a successful payment, information about the payment.
    successful_payment :: Maybe SuccessfulPayment
  }
  deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { -- | Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
    me_type :: Text,
    -- | Offset in UTF-16 code units to the start of the entity
    me_offset :: Int,
    -- | Length of the entity in UTF-16 code units
    me_length :: Int,
    -- | For “text_link” only, url that will be opened after user taps on the text
    me_url :: Maybe Text,
    -- | For “text_mention” only, the mentioned user
    me_user :: Maybe User
  }
  deriving (Show, Generic)

instance ToJSON MessageEntity where
  toJSON = toJsonDrop 3

instance FromJSON MessageEntity where
  parseJSON = parseJsonDrop 3

-- | This object represents a venue.
data Venue = Venue
  { -- | Venue location
    venue_location :: Location,
    -- | Name of the venue
    venue_title :: Text,
    -- | Address of the venue
    venue_address :: Text,
    -- | Foursquare identifier of the venue
    venue_foursquare_id :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON Venue where
  toJSON = toJsonDrop 6

instance FromJSON Venue where
  parseJSON = parseJsonDrop 6

data KeyboardButton = KeyboardButton
  { -- | Text of the button. If none of the optional fields are used, it will be sent to the bot as a message when the button is pressed
    kb_text :: Text,
    -- | If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only
    kb_request_contact :: Maybe Bool,
    -- | If True, the user's current location will be sent when the button is pressed. Available in private chats only
    kb_request_location :: Maybe Bool
  }
  deriving (Show, Generic)

instance ToJSON KeyboardButton where
  toJSON = toJsonDrop 3

instance FromJSON KeyboardButton where
  parseJSON = parseJsonDrop 3

keyboardButton :: Text -> KeyboardButton
keyboardButton buttonText = KeyboardButton buttonText Nothing Nothing

data WebhookInfo = WebhookInfo
  { -- | Webhook URL, may be empty if webhook is not set up
    whi_url :: Text,
    -- | True, if a custom certificate was provided for webhook certificate checks
    whi_has_custom_certificate :: Bool,
    -- | Number of updates awaiting delivery
    whi_pending_update_count :: Int,
    -- | Unix time for the most recent error that happened when trying to deliver an update via webhook
    whi_last_error_date :: Maybe Int,
    -- | Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook
    whi_last_error_message :: Maybe Text,
    -- | Maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery
    whi_max_connections :: Maybe Int,
    -- | A list of update types the bot is subscribed to. Defaults to all update types
    whi_allowed_updates :: Maybe [Text]
  }
  deriving (Show, Generic)

instance ToJSON WebhookInfo where
  toJSON = toJsonDrop 4

instance FromJSON WebhookInfo where
  parseJSON = parseJsonDrop 4

-- Payments

data LabeledPrice = -- | This object represents a portion of the price for goods or services.
  LabeledPrice
  { -- | Portion label
    lp_label :: Text,
    -- | Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in <https://core.telegram.org/bots/payments/currencies.json currencies.json>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
    lp_amount :: Int
  }
  deriving (Show, Generic)

instance ToJSON LabeledPrice where
  toJSON = toJsonDrop 3

instance FromJSON LabeledPrice where
  parseJSON = parseJsonDrop 3

newtype CurrencyCode = CurrencyCode Text
  deriving (Show, Eq, Ord)

instance ToJSON CurrencyCode where
  toJSON (CurrencyCode code) = toJSON code

instance FromJSON CurrencyCode where
  parseJSON (String code) = pure $ CurrencyCode code
  parseJSON _ = fail "Unable to parse CurrencyCode"

data Invoice = -- | This object contains basic information about an invoice.
  Invoice
  { -- | Product name
    inv_title :: Text,
    -- | Product description
    inv_description :: Text,
    -- | Unique bot deep-linking parameter that can be used to generate this invoice
    inv_start_parameter :: Text,
    -- | Three-letter ISO 4217 <https://core.telegram.org/bots/payments#supported-currencies currency> code
    inv_currency :: CurrencyCode,
    -- | Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in <https://core.telegram.org/bots/payments/currencies.json currencies.json>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
    inv_total_amount :: Int
  }
  deriving (Show, Generic)

instance ToJSON Invoice where
  toJSON = toJsonDrop 4

instance FromJSON Invoice where
  parseJSON = parseJsonDrop 4

data ShippingAddress = ShippingAddress
  { -- | ISO 3166-1 alpha-2 country code
    ship_addr_country_code :: Text,
    -- | State, if applicable
    ship_addr_state :: Maybe Text,
    -- | City
    ship_addr_city :: Text,
    -- | First line for the address
    ship_addr_street_line1 :: Text,
    -- | Second line for the address
    ship_addr_street_line2 :: Text,
    -- | Address post code
    ship_addr_post_code :: Text
  }
  deriving (Show, Generic)

instance ToJSON ShippingAddress where
  toJSON = toJsonDrop 10

instance FromJSON ShippingAddress where
  parseJSON = parseJsonDrop 10

data OrderInfo = OrderInfo
  { -- | User name
    ord_info_name :: Maybe Text,
    -- | User's phone number
    ord_info_phone_number :: Maybe Text,
    -- | User email
    ord_info_email :: Maybe Text,
    -- | User shipping address
    ord_info_shipping_address :: Maybe ShippingAddress
  }
  deriving (Show, Generic)

instance ToJSON OrderInfo where
  toJSON = toJsonDrop 9

instance FromJSON OrderInfo where
  parseJSON = parseJsonDrop 9

data ShippingOption = ShippingOption
  { -- | Shipping option identifier
    ship_opt_id :: Text,
    -- | Option title
    ship_opt_title :: Text,
    -- | List of price portions
    ship_opt_prices :: [LabeledPrice]
  }
  deriving (Show, Generic)

instance ToJSON ShippingOption where
  toJSON = toJsonDrop 9

instance FromJSON ShippingOption where
  parseJSON = parseJsonDrop 9

data SuccessfulPayment = SuccessfulPayment
  { -- | Three-letter ISO 4217 <https://core.telegram.org/bots/payments#supported-currencies currency> code
    suc_pmnt_currency :: CurrencyCode,
    -- | Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in <https://core.telegram.org/bots/payments/currencies.json currencies.json>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
    suc_pmnt_total_amount :: Int,
    -- | Bot specified invoice payload
    suc_pmnt_invoice_payload :: Text,
    -- |  Identifier of the shipping option chosen by the user
    suc_pmnt_shipping_option_id :: Maybe Text,
    -- | Order info provided by the user
    suc_pmnt_order_info :: Maybe OrderInfo,
    -- | Telegram payment identifier
    suc_pmnt_telegram_payment_charge_id :: Text,
    -- | Provider payment identifier
    suc_pmnt_provider_payment_charge_id :: Text
  }
  deriving (Show, Generic)

instance ToJSON SuccessfulPayment where
  toJSON = toJsonDrop 9

instance FromJSON SuccessfulPayment where
  parseJSON = parseJsonDrop 9

data ShippingQuery = ShippingQuery
  { -- | Unique query identifier
    ship_q_id :: Text,
    -- | User who sent the query
    ship_q_from :: User,
    -- | Bot specified invoice payload
    ship_q_invoice_payload :: Text,
    -- | User specified shipping address
    ship_q_shipping_address :: ShippingAddress
  }
  deriving (Show, Generic)

instance ToJSON ShippingQuery where
  toJSON = toJsonDrop 7

instance FromJSON ShippingQuery where
  parseJSON = parseJsonDrop 7

data PreCheckoutQuery = PreCheckoutQuery
  { -- | Unique query identifier
    pre_che_id :: Text,
    -- | User who sent the query
    pre_che_from :: User,
    -- | Three-letter ISO 4217 <https://core.telegram.org/bots/payments#supported-currencies currency> code
    pre_che_currency :: CurrencyCode,
    -- | Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in <https://core.telegram.org/bots/payments/currencies.json currencies.json>, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
    pre_che_total_amount :: Int,
    -- | Bot specified invoice payload
    pre_che_invoice_payload :: Text,
    -- | Identifier of the shipping option chosen by the user
    pre_che_shipping_option_id :: Maybe Text,
    -- | Order info provided by the user
    pre_che_order_info :: Maybe OrderInfo
  }
  deriving (Show, Generic)

instance ToJSON PreCheckoutQuery where
  toJSON = toJsonDrop 8

instance FromJSON PreCheckoutQuery where
  parseJSON = parseJsonDrop 8

data StickerSet = StickerSet
  { -- | Sticker set name
    stcr_set_name :: Text,
    -- | Sticker set title
    stcr_set_title :: Text,
    -- | True, if the sticker set contains masks
    stcr_set_contains_masks :: Bool,
    -- | List of all set stickers
    stcr_set_stickers :: [Sticker]
  }
  deriving (Show, Generic)

instance ToJSON StickerSet where
  toJSON = toJsonDrop 9

instance FromJSON StickerSet where
  parseJSON = parseJsonDrop 9

data MaskPositionPoint
  = Forehead
  | Eyes
  | Mouth
  | Chin
  deriving (Show, Generic)

instance ToJSON MaskPositionPoint where
  toJSON Forehead = "forehead"
  toJSON Eyes = "eyes"
  toJSON Mouth = "mouth"
  toJSON Chin = "chin"

instance FromJSON MaskPositionPoint where
  parseJSON "forehead" = pure Forehead
  parseJSON "eyes" = pure Eyes
  parseJSON "mouth" = pure Mouth
  parseJSON "chin" = pure Chin
  parseJSON _ = fail $ "Failed to parse MaskPositionPoint"

data MaskPosition = MaskPosition
  { -- | The part of the face relative to which the mask should be placed
    mask_pos_point :: MaskPositionPoint,
    -- | Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
    mask_pos_x_shift :: Float,
    -- | Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
    mask_pos_y_shift :: Float,
    -- | Mask scaling coefficient. For example, 2.0 means double size.
    mask_pos_scale :: Float
  }
  deriving (Show, Generic)

instance ToJSON MaskPosition where
  toJSON = toJsonDrop 9

instance FromJSON MaskPosition where
  parseJSON = parseJsonDrop 9

-- | This object represents the content of a media message to be sent.
data InputMedia
  = InputMediaPhoto
      { -- | File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet
        input_media_media :: Text,
        -- | Caption of the photo to be sent, 0-200 characters
        input_media_caption :: Maybe Text
      }
  | InputMediaVideo
      { -- | File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet
        input_media_media :: Text,
        -- | Caption of the video to be sent, 0-200 characters
        input_media_caption :: Maybe Text,
        -- | Video width
        input_media_width :: Maybe Int,
        -- | Video height
        input_media_height :: Maybe Int,
        -- | Video duration
        input_media_duration :: Maybe Int
      }
  deriving (Show, Generic)

inputMediaPhoto :: Text -> InputMedia
inputMediaPhoto media = InputMediaPhoto media Nothing

inputMediaVideo :: Text -> InputMedia
inputMediaVideo media = InputMediaVideo media Nothing Nothing Nothing Nothing

inputMediaTagModifier :: String -> String
inputMediaTagModifier "InputMediaPhoto" = "photo"
inputMediaTagModifier "InputMediaVideo" = "video"
inputMediaTagModifier x = x

inputMediaJSONOptions :: Options
inputMediaJSONOptions =
  defaultOptions
    { fieldLabelModifier = drop 12,
      omitNothingFields = True,
      sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = undefined},
      constructorTagModifier = inputMediaTagModifier
    }

instance ToJSON InputMedia where
  toJSON = genericToJSON inputMediaJSONOptions

instance FromJSON InputMedia where
  parseJSON = genericParseJSON inputMediaJSONOptions