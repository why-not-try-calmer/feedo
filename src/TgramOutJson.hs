{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | This module contains helper functions to work with JSON
module TgramOutJson where

import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a. (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = drop prefix,
        omitNothingFields = True
      }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions {fieldLabelModifier = drop prefix}

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
  deriving (Eq, Show, Generic)

instance ToJSON Outbound where
    toJSON = toJsonDrop 4