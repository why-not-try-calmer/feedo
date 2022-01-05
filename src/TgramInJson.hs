{-# LANGUAGE TemplateHaskell #-}

module TgramInJson where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)

data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving (Eq, Show)

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
  { chat_id :: Int64,
    chat_type :: ChatType
  } deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Chat)

data TgGetChatResponse = TgGetChatResponse {resp_ok :: Bool, resp_result :: Chat} deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''TgGetChatResponse)

data User = User
  { 
    user_id :: Int64,
    user_is_bot :: Bool,
    user_first_name :: Text,
    user_last_name :: Maybe Text,
    user_username :: Maybe Text
  } deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''User)

data ChatMember = ChatMember
  { cm_user :: User,
    cm_status :: Text
  }

$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''ChatMember)

data TgGetChatMembersResponse = TgGetChatMembersResponse {resp_cm_ok :: Bool, resp_cm_result :: [ChatMember]}

$(deriveJSON defaultOptions {fieldLabelModifier = drop 8} ''TgGetChatMembersResponse)

data Message = Message
  { message_id :: Int,
    from :: Maybe User,
    chat :: Chat,
    text :: Maybe Text,
    reply_to_message :: Maybe Message
  } deriving (Show)

$(deriveJSON defaultOptions ''Message)

data Update = Update
  { update_id :: Int,
    message :: Maybe Message
  } deriving (Show)

$(deriveJSON defaultOptions ''Update)

data TgGetMessageResponse = TgGetMessageResponse {resp_msg_ok :: Bool, resp_msg_result :: Message} deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 9} ''TgGetMessageResponse)
