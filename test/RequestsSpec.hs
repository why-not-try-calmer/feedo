{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestsSpec where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (toJSON), Value, eitherDecodeStrict', encode)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import Database.MongoDB (ObjectId, genObjectId)
import Database.Redis (objectIdletime)
import Debug.Trace (trace)
import Network.HTTP.Req (responseBody)
import Requests (
    TgReqM (runSend, runSend_),
    fetchApi,
    mkKeyboard,
    mkPagination,
    reply,
 )
import Server (makeConfig)
import System.Environment
import Test.Hspec
import Text.Read (readMaybe)
import TgramOutJson
import Types

getConns :: IO AppConfig
getConns = do
    env <- getEnvironment
    (config, _) <- makeConfig env
    pure config

spec :: Spec
spec = go >> go1 >> go2 >> runIO getConns >>= \config -> go4 config >> go5 config
  where
    go =
        let desc = describe "mkKeyboard"
            as = it "makes a keyboard adjusted for pagination"
            target = case mkKeyboard 1 2 Nothing of
                Nothing -> undefined
                Just (InlineKeyboardMarkup keyboard) -> length keyboard `shouldBe` 1
         in desc $ as target
    go1 =
        let desc = describe "pagination"
            as = it "splits a text into many sub-texts no longer than 606 bytes"
            target = case mkPagination lorec Nothing of
                Nothing -> undefined
                Just (texts, InlineKeyboardMarkup keyboard) -> do
                    let [p1, p2, p3] = texts
                    print p1 >> print "***" >> print p2 >> print "***" >> print p3
                    length texts `shouldSatisfy` (>= 3)
                    length (head keyboard) `shouldBe` 2
                    filter T.null texts `shouldBe` []
         in desc $ as target
    go2 =
        let desc = describe "pagination live test"
            as = it "sends a Telegram message with pagination"
            target = do
                let (texts, keyboard) = fromJust $ mkPagination lorec Nothing
                    rep = ChatReply lorec False False False True Nothing
                env <- getConns
                res <- runApp env $ reply (bot_token . tg_config $ env) (alert_chat . tg_config $ env) rep (postjobs env)
                res `shouldBe` ()
         in desc $ as target
    {-
    go3 =
        let desc = describe "slicing replies to avoid running into Telegram's upper bound"
            as = it "slices up messages to keep all of them under 4096 characters"
            target = do
                let lorecs = [lorec, lorec, lorec]
                    lorecs_txt = T.concat lorecs
                    reply = ChatReply lorecs_txt False False False False Nothing
                    sliced = sliceReplies reply
                    sliced_txts = map (T.length . reply_contents) sliced
                    total_length = sum sliced_txts
                print total_length
                print sliced_txts
                print sliced
                sliced `shouldSatisfy` all (\r -> (T.length . reply_contents $ r) < 4096)
        in  desc $ as target
    -}
    go4 config =
        let desc = describe "Digests: Ensures correct JSON encoding "
            as = it "Verifies an APIReq's (Digest) JSON encoding"
            target = do
                surrogate <- genObjectId
                let oid = fromMaybe surrogate (readMaybe "62d7ecdedb218a0001000003" :: Maybe ObjectId)
                    f = APIFilter Nothing Nothing (Just oid)
                    r = APIReq (renderCollection CDigests) database cluster (Just f)
                res <- fetchApi (api_key config) r
                res `shouldSatisfy` \case
                    Right body -> case eitherDecodeStrict' $ responseBody body :: Either String APIDigest of
                        Left err -> trace err False
                        Right _ -> True
                    Left err -> trace err False
         in desc $ as target
    go5 config =
        let desc = describe "Pages: Ensures correct JSON encoding "
            as = it "Verifies an APIReq's (Pages) JSON encoding"
            target = do
                let f = Just $ APIFilter Nothing (Just 40620) Nothing
                    r = APIReq (renderCollection CPages) database cluster f
                print $ encode r
                res <- fetchApi (api_key config) r
                res `shouldSatisfy` \case
                    Right body -> case eitherDecodeStrict' $ responseBody body :: Either String APIPages of
                        Left err -> trace err False
                        Right _ -> True
                    Left err -> trace err False
         in desc $ as target

lorec :: T.Text
lorec =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur tincidunt et urna a aliquam. Pellentesque sit amet aliquet dolor. In egestas vitae mi non viverra. Integer venenatis bibendum interdum. Cras metus leo, efficitur a nibh ac, congue cursus purus. Etiam id dolor sed lorem vestibulum aliquam posuere quis enim. Vivamus sapien turpis, pretium sit amet ligula id, scelerisque vehicula urna. Nam vulputate magna vitae hendrerit hendrerit. Nunc ultrices feugiat est consequat convallis. Pellentesque tellus tortor, luctus et turpis ut, aliquam facilisis est. \
    \Quisque purus ante, imperdiet eu sapien et, tincidunt accumsan neque. Nam eleifend tincidunt ex. Morbi id tempor justo. Sed gravida consequat tortor, ac condimentum arcu condimentum quis. Cras ac euismod neque. Donec ut dignissim massa. Mauris quis mi sed augue cursus scelerisque. Donec pellentesque lorem a odio scelerisque pellentesque. Mauris laoreet urna lacus, et mollis tellus finibus non. Curabitur malesuada ante in augue eleifend varius. Sed non leo ut velit dictum fermentum non ac ipsum. Aliquam luctus urna eget quam ultricies, in mattis turpis venenatis. Suspendisse nisl libero, volutpat sed aliquet et, eleifend vel ligula. \
    \Suspendisse congue accumsan nunc, eget rutrum ex luctus quis. Fusce in orci mauris. Sed placerat blandit consequat. Donec id turpis vitae tellus porta finibus in facilisis ante. Phasellus sagittis, nulla euismod suscipit ornare, ipsum nisi venenatis dui, vel vestibulum augue nulla et arcu. Mauris ac quam eget leo imperdiet fringilla. Mauris luctus semper mi, et ultrices turpis. \n\
    \In hendrerit lacus feugiat sodales egestas. Maecenas sed lectus velit. Donec nulla urna, maximus ut blandit ut, sagittis vitae tortor. Nulla dictum porttitor justo sit amet gravida. Suspendisse vestibulum elit lectus, sed mattis quam viverra a. Quisque eu dolor lacus. Aliquam a accumsan tortor. Sed vitae leo neque. Phasellus placerat consectetur erat quis faucibus.\
    \Vestibulum magna justo, fringilla vel massa volutpat, dictum interdum enim. Duis pellentesque sollicitudin odio, ut luctus ligula lacinia sit amet. Aenean neque augue, finibus vel arcu a, porttitor placerat mi. Vestibulum sapien nunc, suscipit auctor congue in, mattis nec est. Quisque nec tincidunt magna, sed sodales dolor. Pellentesque pellentesque enim quis metus pretium mollis. Donec et turpis a erat mollis rhoncus. Vestibulum at dolor sollicitudin, rutrum nibh dictum, tristique mauris. Suspendisse ullamcorper magna ac nibh aliquet, eget fermentum nisl maximus. Quisque viverra tincidunt ornare. Curabitur auctor tellus nec mollis ullamcorper. Ut id mi sem.\n\
    \Mauris id congue nisl, id posuere arcu. Maecenas id mattis purus. Proin varius volutpat scelerisque. Suspendisse porttitor diam sit amet maximus mattis. Cras feugiat mattis sapien quis efficitur. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus id sagittis arcu. Nulla non tellus ut tellus rhoncus blandit. Donec rhoncus accumsan."
