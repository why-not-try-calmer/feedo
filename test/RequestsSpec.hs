{-# LANGUAGE FlexibleContexts #-}
module RequestsSpec where

import AppTypes
import Control.Monad.IO.Class (liftIO, MonadIO)
import Requests
    ( TgReqM(runSend, runSend_), mkPagination, mkKeyboard, reply )
import AppServer (makeConfig)
import System.Environment
import Test.Hspec
import qualified Data.Text as T
import Data.Maybe (fromJust)
import TgramOutJson

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go >> go1 >> go2 where
    go =
        let desc as = describe "mkKeyboard" as
            as func = it "makes a keyboard adjusted for pagination" func
            target = case mkKeyboard 1 2 Nothing of
                Nothing -> undefined
                Just (InlineKeyboardMarkup keyboard) -> length keyboard `shouldBe` 1
        in  desc $ as target
    go1 =
        let desc as = describe "pagination" as
            as func = it "splits a text into many sub-texts no longer than 606 bytes" func
            target = case mkPagination lorec Nothing of
                Nothing -> undefined
                Just (texts, InlineKeyboardMarkup keyboard) -> do
                    let [p1, p2, p3] = texts
                    print p1 >> print "***" >> print p2 >> print "***" >> print p3
                    length texts `shouldSatisfy` (>=3)
                    length (head keyboard) `shouldBe` 2
                    filter T.null texts `shouldBe` []
        in  desc $ as target
    go2 =
        let desc as = describe "pagination live test" as
            as func = it "sends a Telegram message with pagination" func
            target = do
                let (texts, keyboard) = fromJust $ mkPagination lorec Nothing
                    rep = ChatReply lorec False False False True Nothing
                env <- getConns
                res <- runApp env $ reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) rep (postjobs env)
                res `shouldBe` ()
        in  desc $ as target

lorec :: T.Text
lorec = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur tincidunt et urna a aliquam. Pellentesque sit amet aliquet dolor. In egestas vitae mi non viverra. Integer venenatis bibendum interdum. Cras metus leo, efficitur a nibh ac, congue cursus purus. Etiam id dolor sed lorem vestibulum aliquam posuere quis enim. Vivamus sapien turpis, pretium sit amet ligula id, scelerisque vehicula urna. Nam vulputate magna vitae hendrerit hendrerit. Nunc ultrices feugiat est consequat convallis. Pellentesque tellus tortor, luctus et turpis ut, aliquam facilisis est. \
    \Quisque purus ante, imperdiet eu sapien et, tincidunt accumsan neque. Nam eleifend tincidunt ex. Morbi id tempor justo. Sed gravida consequat tortor, ac condimentum arcu condimentum quis. Cras ac euismod neque. Donec ut dignissim massa. Mauris quis mi sed augue cursus scelerisque. Donec pellentesque lorem a odio scelerisque pellentesque. Mauris laoreet urna lacus, et mollis tellus finibus non. Curabitur malesuada ante in augue eleifend varius. Sed non leo ut velit dictum fermentum non ac ipsum. Aliquam luctus urna eget quam ultricies, in mattis turpis venenatis. Suspendisse nisl libero, volutpat sed aliquet et, eleifend vel ligula. \
    \Suspendisse congue accumsan nunc, eget rutrum ex luctus quis. Fusce in orci mauris. Sed placerat blandit consequat. Donec id turpis vitae tellus porta finibus in facilisis ante. Phasellus sagittis, nulla euismod suscipit ornare, ipsum nisi venenatis dui, vel vestibulum augue nulla et arcu. Mauris ac quam eget leo imperdiet fringilla. Mauris luctus semper mi, et ultrices turpis. \n\
    \In hendrerit lacus feugiat sodales egestas. Maecenas sed lectus velit. Donec nulla urna, maximus ut blandit ut, sagittis vitae tortor. Nulla dictum porttitor justo sit amet gravida. Suspendisse vestibulum elit lectus, sed mattis quam viverra a. Quisque eu dolor lacus. Aliquam a accumsan tortor. Sed vitae leo neque. Phasellus placerat consectetur erat quis faucibus.\
    \Vestibulum magna justo, fringilla vel massa volutpat, dictum interdum enim. Duis pellentesque sollicitudin odio, ut luctus ligula lacinia sit amet. Aenean neque augue, finibus vel arcu a, porttitor placerat mi. Vestibulum sapien nunc, suscipit auctor congue in, mattis nec est. Quisque nec tincidunt magna, sed sodales dolor. Pellentesque pellentesque enim quis metus pretium mollis. Donec et turpis a erat mollis rhoncus. Vestibulum at dolor sollicitudin, rutrum nibh dictum, tristique mauris. Suspendisse ullamcorper magna ac nibh aliquet, eget fermentum nisl maximus. Quisque viverra tincidunt ornare. Curabitur auctor tellus nec mollis ullamcorper. Ut id mi sem.\n\
    \Mauris id congue nisl, id posuere arcu. Maecenas id mattis purus. Proin varius volutpat scelerisque. Suspendisse porttitor diam sit amet maximus mattis. Cras feugiat mattis sapien quis efficitur. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus id sagittis arcu. Nulla non tellus ut tellus rhoncus blandit. Donec rhoncus accumsan."