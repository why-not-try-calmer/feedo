{-# LANGUAGE OverloadedStrings #-}

module NotifierSpec where

import Control.Concurrent.Async
import qualified Data.HashMap.Internal.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Notifications
import Parsing
impot Server (makeConfig)
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
spec = runIO getConns >>= \env -> go1 env >> go2 env >> go3 env
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
                (failed, done) <- partitionEither <$> mapConcurrently (rebuildFeed env) feedlinks
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
                (failed, done) <- partitionEither <$> mapConcurrently (rebuildFeed env) feedlinks
                now <- getCurrentTime
                let feedsmap = HMS.fromList . map (\f -> (f_link f, f)) $ done
                    -- due = collectDue (HMS.singleton 123 chat) Nothing now
                    -- batch1 = notifFrom (Just now) feedlinks feedsmap due
                    pre = preNotifier now (Just now) (HMS.singleton 123 chat)
                    notifier = postNotifier feedsmap mempty pre
                    batch2 = batches notifier
                    get_items ba = foldMap f_items $ foldMap (get_batch . snd) ba
                    sumItems = length . get_items
                    get_batch (Follows fs) = fs
                    get_batch (Digests fs) = fs
                -- only_on_batch1 = filter (\i -> i_link i `notElem` map i_link (get_items batch2)) $ get_items batch1
                -- only_on_batch2 = filter (\i -> i_link i `notElem` map i_link (get_items batch1)) $ get_items batch2
                -- print $ "First batch: " ++ show (sumItems batch1)
                print $ "Second batch: " ++ show (sumItems batch2)
                -- print "Items from batch 1:"
                -- print $ map (\i -> (i_link i, i_pubdate i)) $ get_items batch1
                -- print "Items only on batch 1"
                -- print $ map (\i -> (i_link i, i_pubdate i)) only_on_batch1
                print "Items only on batch 2"
                -- print $ map i_pubdate only_on_batch2
                -- print $ map (\i -> (i_link i, i_pubdate i)) only_on_batch2
                print $ "Discarded: " ++ (show . length . discarded_items_links $ notifier)
                sumItems batch2 `shouldSatisfy` (> 0)
         in desc $ as target
    go3 env =
        let desc = describe "makes sure 'only_search_notif' filters out items as desired"
            as = it "filters out items based on chat settings 'only_search_notif' and 'search_notif'"
            target = do
                let feedlinks = ["https://news.ycombinator.com/rss"]
                now <- getCurrentTime
                (failed, done) <- partitionEither <$> mapConcurrently (rebuildFeed env) feedlinks
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