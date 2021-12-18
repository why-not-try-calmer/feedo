{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import AppServer (makeConfig, registerWebhook)
import AppTypes
import Backend
import Control.Concurrent
import Control.Exception
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Database (purgeCollections, withMongo, evalMongoAct)
import Database.MongoDB
import qualified Database.MongoDB as M
import Jobs
import Network.HTTP.Req (renderUrl)
import Parser (eitherUrlScheme, getFeedFromHref)
import Text.Read (readMaybe)
import TgActions
import TgramInJson (Message (Message))
import Replies (reply)
import TgramOutJson (ChatId)
import Data.IORef
import AppTypes (DbAction(Get100Feeds))
-- import Data.SearchEngine (query)
-- import Search (prepareFeeds, fetchResults, makeSearch, KeyedItem(item, key))

failedTest :: String -> IO a
failedTest test_name = throwIO . userError $ test_name ++ ": FAILED"

okTest :: String -> IO ()
okTest test_name = print $ test_name ++ ": OK"

diagnoseHMS :: KnownFeeds -> T.Text
diagnoseHMS hmap =
  let keys = HMS.keys hmap
      values = HMS.foldl' (\acc f -> acc `T.append` "\ntitle: " `T.append` f_title f `T.append` ", link: " `T.append` f_link f `T.append` "(" `T.append` (T.pack . show . length . f_items $ f) `T.append` ") items.") mempty hmap
   in "Feeds:" `T.append` T.intercalate " " keys `T.append` ", details: " `T.append` values

testConfig :: IO AppConfig
testConfig =
    let vars =  [
            ("MONGODB_SHARDS", "cluster0-shard-00-02.6eean.mongodb.net:feedfarer2-app:RG8ShwEdpDMFOXkZ;cluster0-shard-00-00.6eean.mongodb.net:feedfarer2-app:RG8ShwEdpDMFOXkZ;cluster0-shard-00-01.6eean.mongodb.net:feedfarer2-app:RG8ShwEdpDMFOXkZ"),
            ("TELEGRAM_TOKEN", "2103664842:AAGD6N-yJf7fxfwQ8KKtebAwXhvoDHmc2eY"),
            ("WEBHOOK_URL", "https://9e77-2a02-aa12-540-e480-58ea-7ea9-5c03-bc0f.ngrok.io"),
            ("ALERT_CHATID", "226151044")
            ] :: [(String, String)]
    in  makeConfig vars >>= \(_, config, _) -> pure config

test_make_config :: IO ()
test_make_config = do
    config <- testConfig
    db <- readIORef $ db_config config
    let res = shard db
    print res

test_run_forever :: IO ()
test_run_forever = do
    mvar <- newMVar 0
    let handler (SomeException e) = do
            print $ "run_worker bumped on exception " `T.append` (T.pack . show $ e)
            print "Rescheduling run_worker now."
        action = threadDelay (3 * 1000000 :: Int) >> modifyMVar_ mvar (\n -> pure $ n + 1) >> throwIO (userError "Lel")
    runForever_ action handler
    print "Running now... (10 secs)"
    threadDelay (10 * 1000000 :: Int)
    readMVar mvar >>= \n -> do
        when (n /= 3) (throwIO $ userError "test_run_forever_ FAILED")
        print "test_run_forever: OK"

test_interpretation :: IO ()
test_interpretation = do
    let valid_cmds = ["/sub https://fitgirl-repacks.site/feed/", "/unsub https://fitgirl-repacks.site/feed/", "/list", "/items 1", "/addfilter 1 ahui"] :: [T.Text]
    case traverse interpretCmd valid_cmds of
        Left err -> print $ renderUserError err
        Right res -> do
            when (length res /= length valid_cmds) (throwIO $ userError "test_interpretation_ FAILED")
            print "test_interpretation: OK"

test_url_scheme :: IO ()
test_url_scheme = do
    let urls = [
            "https://fitgirl-repacks.site/feed",
            "https://stephendiehl.org/rss",
            "https://www.reddit.com/pop_os/.rss"
            ]
    case traverse eitherUrlScheme urls of
        Left err -> do
            print $ renderUserError err
            throwIO $ userError "test_url_scheme: FAILED"
        Right res -> do
            print "test_url_scheme: OK"
            print $ map renderUrl res

test_build_feed :: IO ()
test_build_feed = testConfig >>= \env ->
    let link = "https://www.phoronix.com/rss.php"
        action = getFeedFromHref link
        verdict res = case res of
            Left _ ->  failedTest "test_build_feed"
            Right f -> okTest "test_build_feed" >> print f
    in  action >>= verdict

test_withchats :: IO ()
test_withchats = testConfig >>= \env ->
    let sub = runApp env $ withChat (Sub ["https://fitgirl-repacks.site/feed/"]) 1234
        more_sub = runApp env $ withChat (Sub ["https://stephendiehl.org/rss"]) 1234
        unsub = runApp env $ withChat (UnSub $ map ByUrl ["https://fitgirl-repacks.site/feed/"]) 1234
        verdict = readMVar (subs_state env) >>= \hmap ->
            let links = sub_feeds_links $ hmap HMS.! 1234
            in  unless (length links == 1) (throwIO $ userError "test_withchats: FAILED")
    in  sub >> more_sub >> unsub >> verdict >> print "test_withchats: OK"

test_load_chats :: IO ()
test_load_chats = testConfig >>= \env ->
    let load = runApp env loadChats
        check = readMVar (subs_state env) >>= \res ->
            when (null res) (throwIO $ userError "test_load_chats: FAILED") >> print res
    in  load >> check

test_list_subscribed :: IO ()
test_list_subscribed = testConfig >>= \env ->
    let load = runApp env (evalFeedsAct LoadF >> loadChats)
        subbed = runApp env $ evalTgAct 0 ListSubs 226151044 >>= \case
            Right (MarkdownReply contents) -> liftIO $ do
                print contents
                print "test_list_subscribed: OK"
            _ -> liftIO . throwIO $ userError "test_list_subscribed: FAILED"
    in  load >> subbed

test_send_markdown :: IO ()
test_send_markdown = testConfig >>= \env ->
    let message = MarkdownReply "[Pop!_OS is ready to kick some asses!](http://system76.org/pop!_os21.10)"
        action = reply (bot_token . tg_config $ env) 226151044 message
    in  action >> print "done"

test_getFeed :: IO ()
test_getFeed = do
    res <- getFeedFromHref "https://www.compositional.fm/rss"
    case res of
        Left err -> do
            print err
            throwIO $ userError "test_getFeed: FAILED"
        Right feed -> print "test_getFeed: OK"

test_withfeeds :: IO ()
test_withfeeds = testConfig >>= \env ->
    let starting_urls = ["https://www.reddit.com/r/pop_os/.rss", "https://www.compositional.fm/rss"]
        add = runApp env . evalFeedsAct $ InitF starting_urls
        remove = runApp env . evalFeedsAct $ RemoveF starting_urls
        check1 = readMVar (feeds_state env) >>= \hmap ->
            let links = HMS.keys hmap
            in  when (length links /= length starting_urls) $ do
                    print "failing on check1."
                    throwIO $ userError "test_fithfeeds: FAILED"
        check2 = readMVar (feeds_state env) >>= \hmap ->
            let links = HMS.keys hmap
            in  unless (null links) $ do
                    print "failing on check2."
                    throwIO $ userError "test_fithfeeds: FAILED"
    in add >> check1 >> remove >> check2 >> print "test_withfeeds: OK"

test_refresh_notify :: IO ()
test_refresh_notify = testConfig >>= \env ->
    let starting_urls = ["https://www.reddit.com/r/pop_os/.rss", "https://www.compositional.fm/rss"]
        refresh_notify = runApp env $ do
            evalFeedsAct $ InitF starting_urls
            withChat (Sub [head starting_urls]) 1234
            evalFeedsAct RefreshNotifyF
        verdict res = case res of
            FeedBatches package ->
                let keys = HMS.keys package
                    unique_val = concat $ HMS.elems package
                    items = foldMap snd unique_val
                in  do
                    when (null items) $ throwIO $ userError "test_refresh_notify: FAILED"
                    print $ "Items: " ++ show (length items)
            _ -> throwIO $ userError "test_refresh_notify: FAILED"
    in refresh_notify >>= verdict >> print "test_refresh_notify: OK"

test_tg_actions :: IO ()
test_tg_actions = testConfig >>= \env ->
    let starting_urls = ["https://www.reddit.com/r/pop_os/.rss", "https://www.compositional.fm/rss"]
        init_sub_get_items = runApp env $ do
            evalFeedsAct $ InitF starting_urls
            withChat (Sub [head starting_urls]) 1234
            evalTgAct 0 (GetItems $ ByUrl "https://www.reddit.com/r/pop_os/.rss") 1234
        check1 res = case res of
            Left err -> throwIO $ userError "test_tg_actions: FAILED on check1"
            Right r -> pure ()
        list_subscribed = runApp env $ evalTgAct 0 ListSubs 1234
        check2 res = case res of
            Left err -> throwIO $ userError "test_tg_actions: FAILED on check2"
            Right r -> pure ()
        get_last_x = runApp env $ evalTgAct 0 (GetLastXDaysItems 2) 1234
        check3 res = case res of
            Left err -> throwIO $ userError "test_tg_actions: FAILED on check3"
            Right r -> pure ()
        in init_sub_get_items >>= check1 >>
            list_subscribed >>= check2 >>
            get_last_x >>= check3 >>
            print "test_refresh_actions: OK"

test_inc :: IO ()
test_inc = do
    env <- testConfig
    config <- readIORef $ db_config env
    let sample = ["_id" =: ("mon_id" :: T.Text), "counter" =: (1::Int)]
        sel = ["_id" =: ("mon_id" :: T.Text)]
        action = withMongo config $ do
            insert "test" sample
            modify (select sel "test") ["$inc" =: ["counter" =: (1 :: Int)]]
            findOne (select sel "test")
        verdict res = case res of
            Nothing -> throwIO $ userError "test_inc: FAILED"
            Just doc ->
                let val = fromMaybe undefined $ M.lookup "counter" doc :: Int
                in  unless (val == 2) (throwIO $ userError "test_inc: FAILED") >> print "test_inc: OK"
    action >>= verdict

test_pause :: IO ()
test_pause = testConfig >>= \env ->
    let action = runApp env $ loadChats >> evalTgAct 0 (Pause True) 226151044
        verdict res = case res of
            Left _ -> failedTest "test_pause"
            Right (PlainReply contents) -> print contents
            Right (MarkdownReply contents) -> print contents
    in  action >>= verdict >> okTest "test_pause"

test_purge_db :: IO ()
test_purge_db = do
    env <- testConfig
    config <- readIORef $ db_config env
    purgeCollections config >>= \case
        Left err -> print err
        Right _ -> print "test_purge_db: OK"

test_parse_settings_message :: IO ()
test_parse_settings_message =
    let m = "/settings 123\nk1:v1\nk2 :v2\nk3: v3"
        prepare = tail . T.lines . T.toLower . T.strip
        op = case parseUpdateLines $ prepare m of
            Nothing -> throwIO $ userError "test_parse_settings_message: FAILED on op1"
            Just parsed -> pure parsed
    in  op >>= \res -> print res >> print "test_parse_settings_message: OK"
{-
test_search_engine :: IO ()
test_search_engine =
    let failure = failedTest "test_search_engine"
        success = okTest "test_search_engine"
        action = do
            env <- testConfig
            config <- readIORef $ db_config env
            evalMongoAct config Get100Feeds
        search res = case res of
            DbFeeds feeds ->
                let items = prepareFeeds feeds
                    engine = makeSearch items
                    res_keys = query engine ["performance"]
                in  if null res_keys then failure
                    else print (fetchResults res_keys items) >> success
            _ -> failure
    in  action >>= search
-}

main = test_build_feed