{-# LANGUAGE ScopedTypeVariables #-}

module WebBrowserSpec where

import Test.Hspec
import AppTypes (Settings (Settings), DigestInterval (DigestInterval), WordMatches (WordMatches))
import Data.Aeson
import qualified Data.Set as S
import Parsing (parseSettings)

spec :: Spec
spec = go where
    go =
        let desc as = describe "JSON parsing" as
            as func = it "JSON sent from the browser should get decoded and parsed correctely" func
            target = 
                let null_json_string = "{\"word_matches\":{\"blacklist\":[],\"searchset\":[],\"only_search_results\":[]},\"digest_start\": null, \"digest_interval\":{\"digest_every_secs\":null,\"digest_at\":null},\"digest_collapse\":null,\"digest_size\":0,\"digest_title\":\"\",\"disable_web_view\":false,\"paused\":false,\"follow\":false,\"pin\":false,\"share_link\":false}"
                    null_settings = Settings Nothing (DigestInterval Nothing Nothing) 0 Nothing mempty False False False (WordMatches S.empty S.empty S.empty) False False
                in  case eitherDecode null_json_string :: Either String Settings of
                    Right s -> s `shouldBe` null_settings
                    Left s -> print s >> undefined
        in  desc $ as target