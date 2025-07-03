{-# LANGUAGE TemplateHaskell #-}

module TgramInJson where

import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import TgramOutJson (ChatId, UserFirstName, UserId)

data TgChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving (Eq, Show)

data TgChatMemberType
  = TgChatMemberOwner
  | TgChatMemberAdministrator
  | TgChatMemberMember
  | TgChatMemberRestricted
  | TgChatMemberLeft
  | TgChatMemberBanned
  deriving (Eq, Show)

instance FromJSON TgChatMemberType where
  parseJSON "creator" = pure TgChatMemberOwner
  parseJSON "administrator" = pure TgChatMemberAdministrator
  parseJSON "member" = pure TgChatMemberMember
  parseJSON "restricted" = pure TgChatMemberRestricted
  parseJSON "left" = pure TgChatMemberLeft
  parseJSON "kicked" = pure TgChatMemberBanned
  parseJSON _ = fail "Failed to parse ChatMemberType"

instance ToJSON TgChatType where
  toJSON Private = "private"
  toJSON Group = "group"
  toJSON Supergroup = "supergroup"
  toJSON Channel = "channel"

instance FromJSON TgChatType where
  parseJSON "private" = pure Private
  parseJSON "group" = pure Group
  parseJSON "supergroup" = pure Supergroup
  parseJSON "channel" = pure Channel
  parseJSON _ = fail "Failed to parse ChatType"

data TgChat = TgChat
  { chat_id :: ChatId
  , chat_type :: TgChatType
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''TgChat)

data TgChatFullInfo = TgChatFullInfo
  { cfi_id :: ChatId
  , cfi_type :: TgChatType
  , cfi_title :: Maybe Text
  , cfi_username :: Maybe Text
  , cfi_is_forum :: Maybe Bool
  , cfi_description :: Maybe Text
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{allowOmittedFields = True, fieldLabelModifier = drop 4} ''TgChatFullInfo)

data TgGetChatResponse = TgGetChatResponse {resp_ok :: Bool, resp_result :: TgChatFullInfo} deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''TgGetChatResponse)

data TgUser = TgUser
  { user_id :: UserId
  , user_is_bot :: Bool
  , user_first_name :: UserFirstName
  , user_last_name :: Maybe Text
  , user_username :: Maybe Text
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''TgUser)

data ChatMember
  = ChatMemberOwner
      { cmo_user :: TgUser
      , cmo_status :: TgChatMemberType
      , cmo_is_anonymous :: Bool
      , cmo_custom_title :: Maybe Bool
      }
  | ChatMemberAdministrator
      { cma_user :: TgUser
      , cma_status :: TgChatMemberType
      , cma_can_be_edited :: Bool
      , cma_is_anonymous :: Bool
      , cma_can_manage_chat :: Bool
      , cma_can_delete_messages :: Bool
      , cma_can_manage_video_chats :: Bool
      , cma_can_restrict_members :: Bool
      , cma_can_promote_members :: Bool
      , cma_can_change_info :: Bool
      , cma_can_invite_users :: Bool
      , cma_can_post_stories :: Bool
      , cma_can_edit_stories :: Bool
      , cma_can_delete_stories :: Bool
      , cma_can_post_messages :: Maybe Bool
      , cma_can_edit_messages :: Maybe Bool
      , cma_can_pin_messages :: Maybe Bool
      , cma_can_manage_topics :: Maybe Bool
      , cma_custom_title :: Maybe Text
      }
  | ChatMemberMember
      { cmm_user :: TgUser
      , cmm_status :: TgChatMemberType
      }

$(deriveFromJSON defaultOptions{sumEncoding = UntaggedValue, fieldLabelModifier = drop 4, omitNothingFields = True} ''ChatMember)

getUserStatus :: ChatMember -> (UserId, TgChatMemberType)
getUserStatus member@ChatMemberMember{} = (user_id $ cmm_user member, cmm_status member)
getUserStatus member@ChatMemberAdministrator{} = (user_id $ cma_user member, cma_status member)
getUserStatus member@ChatMemberOwner{} = (user_id $ cmo_user member, cmo_status member)

data TgGetChatMembersResponse = TgGetChatMembersResponse {resp_cm_ok :: Bool, resp_cm_result :: [ChatMember]}

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 8} ''TgGetChatMembersResponse)

data Message = Message
  { message_id :: Int
  , from :: Maybe TgUser
  , chat :: TgChat
  , text :: Maybe Text
  , reply_to_message :: Maybe Message
  }
  deriving (Show)

$(deriveFromJSON defaultOptions ''Message)

data CallbackQuery = CallbackQuery
  { cbq_id :: Text
  , cbq_from :: TgUser
  , cbq_message :: Maybe Message
  , cbq_chat_instance :: Text
  , cbq_data :: Maybe Text
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 4, omitNothingFields = True} ''CallbackQuery)

data Update = Update
  { update_id :: Int
  , message :: Maybe Message
  , callback_query :: Maybe CallbackQuery
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{omitNothingFields = True} ''Update)

data TgGetMessageResponse = TgGetMessageResponse {resp_msg_ok :: Bool, resp_msg_result :: Message} deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 9} ''TgGetMessageResponse)
