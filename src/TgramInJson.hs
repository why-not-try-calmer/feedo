{-# LANGUAGE TemplateHaskell #-}

module TgramInJson where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import TgramOutJson (ChatId, UserFirstName, UserId)

data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving (Eq, Show)

data UserStatus
  = Creator
  | Admin
  | Member
  | Restricted
  deriving (Eq, Show)

instance ToJSON UserStatus where
  toJSON Creator = "creator"
  toJSON Admin = "admin"
  toJSON Member = "member"
  toJSON Restricted = "restricted"

instance FromJSON UserStatus where
  parseJSON "creator" = pure Creator
  parseJSON "admin" = pure Admin
  parseJSON "member" = pure Member
  parseJSON "restricted" = pure Restricted
  parseJSON _ = fail "Failed to parse UserStatus"

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

data Chat = Chat
  { chat_id :: ChatId
  , chat_type :: ChatType
  , title :: Maybe Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True} ''Chat)

data TgGetChatResponse = TgGetChatResponse {resp_ok :: Bool, resp_result :: Chat} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''TgGetChatResponse)

data User = User
  { user_id :: UserId
  , user_is_bot :: Bool
  , user_first_name :: UserFirstName
  , user_last_name :: Maybe Text
  , user_username :: Maybe Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''User)

data ChatMember = ChatMember
  { cm_user :: User
  , cm_status :: UserStatus
  }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''ChatMember)

data TgGetChatMembersResponse = TgGetChatMembersResponse {resp_cm_ok :: Bool, resp_cm_result :: [ChatMember]}

$(deriveJSON defaultOptions{fieldLabelModifier = drop 8} ''TgGetChatMembersResponse)

data Message = Message
  { message_id :: Int
  , from :: Maybe User
  , chat :: Chat
  , text :: Maybe Text
  , reply_to_message :: Maybe Message
  }
  deriving (Show)

$(deriveJSON defaultOptions ''Message)

data CallbackQuery = CallbackQuery
  { cbq_id :: Text
  , cbq_from :: User
  , cbq_message :: Maybe Message
  , cbq_chat_instance :: Text
  , cbq_data :: Maybe Text
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, omitNothingFields = True} ''CallbackQuery)

data Update = Update
  { update_id :: Int
  , message :: Maybe Message
  , callback_query :: Maybe CallbackQuery
  }
  deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Update)

data TgGetMessageResponse = TgGetMessageResponse {resp_msg_ok :: Bool, resp_msg_result :: Message} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 9} ''TgGetMessageResponse)
