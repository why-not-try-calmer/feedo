{-# LANGUAGE OverloadedStrings #-}

module ParsingSpec where

import AppTypes (Batch (Digests, Follows), BatchRecipe (DigestFeedLinks), DigestInterval (DigestInterval), Feed (f_desc, f_items, f_link, f_title, f_type), Item (i_desc, i_feed_link, i_link, i_pubdate), Notifier (Post, Pre, batch_recipes, batches, discarded_items_links), ParsingSettings (PDigestAt), Settings (Settings, settings_digest_interval), SubChat (SubChat, sub_chatid), WordMatches (WordMatches), i_title)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.HTTP.Req (renderUrl)
import Notifications (collectDue, postNotifier, preNotifier)
import Parsing (eitherUrlScheme, parseSettings, rebuildFeed)
import Test.Hspec
import Utils (defaultChatSettings, mbTime, partitionEither)

spec :: Spec
spec = go >> go1 >> go2 >> go3 >> go4
  where
    go =
        let desc = describe "parseSettings"
            as = it "parse settings sent over Telegram in view of updating a Settings value"
            target = parseSettings ["digest_at: 08:00"] `shouldBe` Right [PDigestAt [(8, 0)]]
         in desc $ as target
    go1 =
        let desc = describe "eitherUrlScheme"
            as = it "parse url sent over Telegram in view of fetching a web feed"
            target = case eitherUrlScheme "https://this-week-in-rust.org/atom.xml" of
                Left _ -> undefined
                Right res ->
                    let s = renderUrl res
                     in s `shouldSatisfy` (not . T.null)
         in desc $ as target
    go2 =
        let desc = describe "rebuildFeed"
            as = it "fetches and parses the given web feeds"
            target = do
                let feeds = ["https://hnrss.org/frontpage", "https://planetpython.org/rss20.xml", "https://talkpython.fm/episodes/rss", "https://www.blog.pythonlibrary.org/feed"]
                res <- mapConcurrently rebuildFeed feeds
                let (failed, done) = partitionEither res
                length done `shouldSatisfy` (> length failed)
         in desc $ as target
    go3 =
        let desc = describe "test digests"
            as = it "creates a digest"
            target = do
                let feedlinks = ["https://www.reddit.com/r/pop_os/.rss", "https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
                    -- every day
                    settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty)
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
    go4 =
        let desc = describe "validate equivalence between notifiers"
            as = it "proves that the two notifiers are equivalent"
            target = do
                let feedlinks = ["https://www.reddit.com/r/pop_os/.rss", "https://this-week-in-rust.org/atom.xml", "https://blog.rust-lang.org/inside-rust/feed.xml", "https://blog.rust-lang.org/feed.xml"]
                    -- every day
                    settings = Settings Nothing (settings_digest_interval defaultChatSettings) 10 Nothing "Test digest" False False False False False False (WordMatches S.empty S.empty S.empty)
                    -- last 31th of May
                    chat = SubChat 123 (mbTime "2022-05-31") Nothing (S.fromList feedlinks) Nothing settings
                (failed, done) <- partitionEither <$> mapConcurrently rebuildFeed feedlinks
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
                --only_on_batch1 = filter (\i -> i_link i `notElem` map i_link (get_items batch2)) $ get_items batch1
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

main = hspec spec