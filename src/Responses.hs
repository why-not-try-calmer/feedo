{-# LANGUAGE DeriveGeneric #-}

module Responses where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Ok = Ok T.Text deriving (Eq, Show)

data ServerResponse
  = RespError
      { err_message :: T.Text,
        err_request :: T.Text
      }
  | RespOk
      { ok_message :: T.Text,
        ok_request :: T.Text
      }
  deriving (Generic)

instance ToJSON ServerResponse