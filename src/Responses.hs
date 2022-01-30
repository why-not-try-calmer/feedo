{-# LANGUAGE TemplateHaskell #-}

module Responses where

import AppTypes (App)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T

data ServerResponse
  = RespError
      { err_message :: T.Text,
        err_request :: T.Text
      }
  | RespOk
      { ok_message :: T.Text,
        ok_request :: T.Text
      }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ServerResponse)

root :: MonadIO m => App m ServerResponse
root = pure $ RespOk "ok" "testing" 