{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Spec where

import Test.Hspec

main = do
  env <- getEnvironment
  print $
    "Running tests for " ++ T.pack . fromMaybe "(unspecified)" $
      lookup "APP_VERSION" env
  hspec spec
