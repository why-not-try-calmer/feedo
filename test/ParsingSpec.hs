module ParsingSpec where

import Test.Hspec
import Parsing (parseSettings)
import AppTypes (DigestInterval(DigestInterval), ParsingSettings (PDigestAt))

spec :: Spec
spec = go
    where
        go =
            let desc as = describe "parseSettings" as
                as func = it "parse settings sent over Telegram in view of updating a Settings value" func
                target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8,0)]]
            in  desc $ as target