{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module TgramOutJson where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import qualified Data.Text as T

type ChatId = Int64
type UserId = Int64
type UserFirstName = T.Text

data TgRequestMethod
  = TgDeleteMessage
  | TgGetChat
  | TgGetChatAdministrators
  | TgEditMessage
  | TgPinChatMessage
  | TgSendMessage
  deriving (Show, Eq)

data InlineKeyboardButton = InlineKeyboardButton
  { -- exactly one Maybe must be set on pain of throwing
    -- an error.
    inline_text :: T.Text
  , inline_url :: Maybe T.Text
  , inline_callback_data :: Maybe T.Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True} ''InlineKeyboardButton)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inline_keyboard :: [[InlineKeyboardButton]]
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''InlineKeyboardMarkup)

data Outbound
  = OutboundMessage
      { out_chat_id :: ChatId
      , out_text :: T.Text
      , out_parse_mode :: Maybe T.Text
      , out_disable_web_page_preview :: Maybe Bool
      , out_reply_markup :: Maybe InlineKeyboardMarkup
      }
  | EditMessage
      { out_chat_id :: ChatId
      , out_message_id :: Int
      , out_text :: T.Text
      , out_parse_mode :: Maybe T.Text
      , out_disable_web_page_preview :: Maybe Bool
      , out_reply_markup :: Maybe InlineKeyboardMarkup
      }
  | DeleteMessage
      { out_chat_id :: ChatId
      , out_message_id :: Int
      }
  | PinMessage
      { out_chat_id :: ChatId
      , out_message_id :: Int
      }
  | GetChat
      { out_chat_id :: ChatId
      }
  deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, omitNothingFields = True} ''Outbound)

data AnswerCallbackQuery = AnswerCallbackQuery
  { answer_callback_query_id :: T.Text
  , answer_text :: Maybe T.Text
  , answer_url :: Maybe T.Text
  , answer_show_alert :: Maybe Bool
  , answer_cache_time :: Maybe Int
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True} ''AnswerCallbackQuery)
