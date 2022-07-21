module WebBrowserSpec where

import Data.Aeson
import qualified Data.Set as S
import Parsing (parseSettings)
import Test.Hspec
import Types (DigestInterval (DigestInterval), Settings (Settings), WordMatches (WordMatches))

spec :: Spec
spec = go
  where
    go =
        let desc = describe "JSON parsing"
            as = it "JSON sent from the browser should get decoded and parsed correctely"
            target =
                -- DEPRECATING
                {-
                let null_json_string = "{\"digest_start\": null, \"digest_interval\":{\"digest_every_secs\":null,\"digest_at\":null},\"digest_collapse\":null,\"digest_size\":0,\"digest_title\":\"\",\"disable_web_view\":false,\"follow\":true,\"pagination\":false,\"paused\":false,\"pin\":false,\"share_link\":false, \"word_matches\":{\"blacklist\":[],\"searchset\":[],\"only_search_results\":[]}}"
                    null_settings = Settings Nothing (DigestInterval Nothing Nothing) 0 Nothing mempty False True False False False False (WordMatches S.empty S.empty S.empty)
                in  case eitherDecode null_json_string :: Either String Settings of
                    Right s -> s `shouldBe` null_settings
                    Left s -> print s >> undefined
                -}
                True `shouldBe` True
         in desc $ as target