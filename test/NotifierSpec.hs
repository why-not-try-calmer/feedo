{-# LANGUAGE OverloadedStrings #-}

module NotifierSpec where

import Control.Concurrent.Async
import qualified Data.HashMap.Internal.Strict as HMS
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Notifications
import Parsing
import Server (makeConfig)
import System.Environment (getEnvironment)
import Test.Hspec
import Types
import Utils

getConns :: IO AppConfig
getConns = do
  env <- getEnvironment
  (config, _) <- makeConfig env
  pure config

spec :: Spec
spec = do
  env <- runIO getConns
  mapM_ (\t -> t env) [go1, go2, go3, go4]
 where
  go1 env =
    let desc = describe "test digests"
        as = it "creates a digest"
        target = do
          let feedlinks = ["https://www.reddit.com/r/pop_os/.rss", "https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
              -- every day
              settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty) S.empty
              -- last 31th of May
              chat = SubChat 123 (mbTime "2022-05-31") Nothing (S.fromList feedlinks) Nothing settings
              chats_recipes = HMS.singleton 123 (chat, DigestFeedLinks feedlinks)
          (failed, done) <- partitionEither <$> mapConcurrently rebuildFeed feedlinks
          now <- getCurrentTime
          let notifs = postNotifier (HMS.fromList $ map (\f -> (f_link f, f)) done) feedlinks (Pre feedlinks chats_recipes Nothing)
              only_feeds =
                HMS.map
                  ( \(_, Digests ds) ->
                      foldMap
                        ( \f ->
                            [ f_title f
                            , f_link f
                            , T.pack . show . f_type $ f
                            , T.pack . show . length $ f_items f
                            , "Faulty (default) pubdates: " `T.append` (faulty . f_items $ f)
                            , "Proof: " `T.append` T.intercalate "," (map (T.pack . show . i_pubdate) $ f_items f)
                            , T.intercalate "," $ map i_title $ f_items f
                            ]
                        )
                        ds
                  )
                  (batches notifs)
          print only_feeds
          batches notifs `shouldSatisfy` (not . null . HMS.toList)
     in desc $ as target
   where
    faulty [] = "No item found!"
    faulty [i] = "Only one item found, cannot check if faulty."
    faulty (i : is) =
      let initial = i_pubdate i
          final = i_pubdate . last $ is
       in if length is == 1 && initial == final
            then "2"
            else T.pack . show . length $ filter (\i -> i_pubdate i == initial) is
  go2 env =
    let desc = describe "validate equivalence between notifiers"
        as = it "proves that the two notifiers are equivalent"
        target = do
          let feedlinks = ["https://www.reddit.com/r/pop_os/.rss", "https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
              -- every day
              settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty) S.empty
              -- last 31th of May
              chat = SubChat 123 (mbTime "2022-05-31") Nothing (S.fromList feedlinks) Nothing settings
          (failed, done) <- partitionEither <$> mapConcurrently rebuildFeed feedlinks
          now <- getCurrentTime
          let feedsmap = HMS.fromList . map (\f -> (f_link f, f)) $ done
              pre = preNotifier now (Just now) (HMS.singleton 123 chat)
              notifier = postNotifier feedsmap mempty pre
              batch = batches notifier
              get_items ba = foldMap f_items $ foldMap (get_batch . snd) ba
              sumItems = length . get_items
              get_batch (Follows fs) = fs
              get_batch (Digests fs) = fs
          print $ "Second batch: " ++ show (sumItems batch)
          print "Items only on batch 2"
          print $ "Discarded: " ++ (show . length . discarded_items_links $ notifier)
          sumItems batch `shouldSatisfy` (> 0)
     in desc $ as target
  go3 env =
    let desc = describe "makes sure 'only_search_notif' filters out items as desired"
        as = it "filters out items based on chat settings 'only_search_notif' and 'search_notif'"
        target = do
          let feedlinks = ["https://news.ycombinator.com/rss"]
          now <- getCurrentTime
          (failed, done) <- partitionEither <$> mapConcurrently rebuildFeed feedlinks
          now <- getCurrentTime
          let feedsmap = HMS.fromList . map (\f -> (f_link f, f)) $ done
              sub_to = S.singleton "haskell"
              matches = WordMatches{match_blacklist = S.empty, match_searchset = sub_to, match_only_search_results = S.singleton "https://news.ycombinator.com/rss"}
              interval = DigestInterval Nothing Nothing
              settings = Settings Nothing interval 10 Nothing mempty False False False False False False matches S.empty
              subs = HMS.singleton 0 $ SubChat 0 Nothing Nothing sub_to Nothing settings
              pre = preNotifier now Nothing subs
              post = postNotifier feedsmap [] pre
              d = discarded_items_links post
          d `shouldSatisfy` (not . null)
     in desc $ as target
  go4 env =
    let desc = describe "validates the `collectDue`"
        as = it "filters out chats that don't need a refresh"
        target = do
          let settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty) S.empty
              feedlinks = ["https://www.reddit.com/r/pop_os/.rss", "https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
              chat1 = SubChat 123 (mbTime "2022-05-31") (mbTime "2022-06-04") (S.fromList feedlinks) Nothing settings
              chat2 = SubChat 234 (mbTime "2022-05-31") (mbTime "2022-06-02") (S.fromList feedlinks) Nothing settings
              chats_recipes = HMS.insert 234 chat2 $ HMS.singleton 123 chat1
              collected = collectDue chats_recipes Nothing (fromJust . mbTime $ "2022-06-03")
          collected `shouldSatisfy` HMS.member 234
          collected `shouldNotSatisfy` HMS.member 123
     in desc $ as target
