{-# LANGUAGE TemplateHaskell #-}

module TgramOutJson where

import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Aeson.TH ( deriveJSON )

type ChatId = Int64
type UserId = Int64

data Outbound
  = OutboundMessage
      { out_chat_id :: ChatId,
        out_text :: T.Text,
        out_parse_mode :: Maybe T.Text,
        out_disable_web_page_preview :: Bool
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
