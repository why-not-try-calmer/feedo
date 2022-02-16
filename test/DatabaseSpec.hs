module DatabaseSpec where
import Test.Hspec
import Database (checkDbMapper)
import qualified Data.Text as T
import Control.Monad.IO.Class

spec :: Spec
spec = go
    where
        go =
            let desc as = describe "checkDbMapper" as
                as func = it "make sure the ORM matches the application values" func
                target = do
                    has <- checkDbMapper >> pure "ok"
                    let wants = "ok" :: T.Text
                    has `shouldBe` wants
            in  desc $ as target