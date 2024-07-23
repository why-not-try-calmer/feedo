{-# LANGUAGE TemplateHaskell #-}

module TgramInJson where

import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
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
  | Administrator
  | Member
  | UserRestricted
  | UserLeft
  | UserKicked
  deriving (Eq, Show)

instance FromJSON UserStatus where
  parseJSON "creator" = pure Creator
  parseJSON "administrator" = pure Administrator
  parseJSON "member" = pure Member
  parseJSON "restricted" = pure UserRestricted
  parseJSON "left" = pure UserLeft
  parseJSON "kicked" = pure UserKicked
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
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''Chat)

data ChatFullInfo = ChatFullInfo
  { cfi_chat_id :: ChatId
  , cfi_type :: ChatType
  , cfi_accent_color_id :: Maybe Int
  , cfi_max_reaction_count :: Maybe Int
  }
  deriving (Show)

instance FromJSON ChatFullInfo where
  -- Cannot rely on `allowOmittedFields` here as not implemented as of this old version of Aeson
  parseJSON = withObject "ChatFullInfo" $ \o ->
    ChatFullInfo
      <$> o .: "chat_id"
      <*> o .: "type"
      -- not sure about these two
      <*> o .:? "accent_color_id"
      <*> o .:? "max_reaction_count"

data TgGetChatResponse = TgGetChatResponse {resp_ok :: Bool, resp_result :: ChatFullInfo} deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''TgGetChatResponse)

data User = User
  { user_id :: UserId
  , user_is_bot :: Bool
  , user_first_name :: UserFirstName
  , user_last_name :: Maybe Text
  , user_username :: Maybe Text
  }
  deriving (Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 5} ''User)

data ChatMember
  = ChatMemberOwner
      { cmo_user :: User
      , cmo_status :: UserStatus
      , cmo_is_anonymous :: Bool
      , cmo_custom_title :: Maybe Bool
      }
  | ChatMemberAdministrator
      { cma_user :: User
      , cma_status :: UserStatus
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
      { cmm_user :: User
      , cmm_status :: UserStatus
      }

$(deriveFromJSON defaultOptions{sumEncoding = UntaggedValue, fieldLabelModifier = drop 4, omitNothingFields = True} ''ChatMember)

getUserStatus :: ChatMember -> (UserId, UserStatus)
getUserStatus member@ChatMemberMember{} = (user_id $ cmm_user member, cmm_status member)
getUserStatus member@ChatMemberAdministrator{} = (user_id $ cma_user member, cma_status member)
getUserStatus member@ChatMemberOwner{} = (user_id $ cmo_user member, cmo_status member)

data TgGetChatMembersResponse = TgGetChatMembersResponse {resp_cm_ok :: Bool, resp_cm_result :: [ChatMember]}

$(deriveFromJSON defaultOptions{fieldLabelModifier = drop 8} ''TgGetChatMembersResponse)

data Message = Message
  { message_id :: Int
  , from :: Maybe User
  , chat :: Chat
  , text :: Maybe Text
  , reply_to_message :: Maybe Message
  }
  deriving (Show)

$(deriveFromJSON defaultOptions ''Message)

data CallbackQuery = CallbackQuery
  { cbq_id :: Text
  , cbq_from :: User
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
