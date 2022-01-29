{-# LANGUAGE TemplateHaskell #-}

module Responses where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text as T
import qualified Text.Blaze.Html as Html
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

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

mkForm :: Html.Html
mkForm =
    Html.div ! Attr.class_ "row" $ pure mempty