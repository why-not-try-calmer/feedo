module ParsingSpec where

import Test.Hspec
import Parsing (parseSettings, eitherUrlScheme)
import AppTypes (DigestInterval(DigestInterval), ParsingSettings (PDigestAt))
import qualified Data.Text as T
import Network.HTTP.Req (renderUrl)

spec :: Spec
spec = go >> go1
    where
        go =
            let desc as = describe "parseSettings" as
                as func = it "parse settings sent over Telegram in view of updating a Settings value" func
                target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8,0)]]
            in  desc $ as target
        go1 =
            let desc as = describe "eitherUrlScheme" as
                as func = it "parse url sent over Telegram in view of fetching a web feed" func
                target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
                    Left _ -> undefined
                    Right res -> 
                        let s = renderUrl res
                        in  s `shouldSatisfy` (not . T.null)
            in  desc $ as target