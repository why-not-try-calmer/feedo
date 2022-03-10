{-# LANGUAGE TemplateHaskell #-}

module TgramOutJson where

import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Aeson.TH ( deriveJSON )

type ChatId = Int64
type UserId = Int64

data InlineKeyboardButton = InlineKeyboardButton {
    inline_text :: T.Text,
    inline_callback_data :: T.Text
} deriving (Eq, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 7, omitNothingFields = True} ''InlineKeyboardButton)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {
    inline_keyboard :: [[InlineKeyboardButton]]
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''InlineKeyboardMarkup)

data Outbound
  = OutboundMessage
      { out_chat_id :: ChatId,
        out_text :: T.Text,
        out_parse_mode :: Maybe T.Text,
        out_disable_web_page_preview :: Maybe Bool,
        out_reply_markup :: Maybe InlineKeyboardMarkup
      }
  | EditMessage {
      out_chat_id :: ChatId,
      out_message_id :: Int,
      out_text :: T.Text,
      out_parse_mode :: Maybe T.Text,
      out_reply_markup :: Maybe InlineKeyboardMarkup
  }
  | DeleteMessage {
      out_chat_id :: ChatId,
      out_message_id :: Int
    }
  | PinMessage {
      out_chat_id :: ChatId,
      out_message_id :: Int
    }
  | SetWebHook
      { out_url :: T.Text,
        out_certificates :: Maybe T.Text,
        out_ip_address :: Maybe T.Text,
        out_max_connections :: Maybe Int,
        out_allowed_updates :: Maybe [T.Text]
      } 
  | GetChatAdministrators
      {
        out_chat_id :: ChatId
      }
  deriving (Eq, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4, omitNothingFields = True} ''Outbound)