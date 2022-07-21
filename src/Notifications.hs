{-# LANGUAGE RecordWildCards #-}

module Notifications where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HMS
import Data.List (sortOn)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime (utctDayTime), addUTCTime)
import Mongo (HasMongo (evalDb))
import TgramOutJson (ChatId)
import Types
import Utils (renderDbError)

preNotifier :: UTCTime -> Maybe UTCTime -> SubChats -> Notifier
preNotifier now mb_last_run chats =
    Pre
        { feeds_to_refresh = to_refresh
        , n_last_run = mb_last_run
        , batch_recipes = due_chats
        }
  where
    to_refresh = foldMap (get_recipe . snd) due_chats
    due_chats = collectDue chats mb_last_run now
    get_recipe (FollowFeedLinks fs) = fs
    get_recipe (DigestFeedLinks fs) = fs

postNotifier :: HMS.HashMap FeedLink Feed -> [T.Text] -> Notifier -> Notifier
postNotifier rebuilt_feeds previously_sent_items (Pre _ due_chats mb_last_run) =
    Post
        { discarded_items_links = discarded
        , batches = ba
        }
  where
    (discarded, ba) =
        let rebuilt_items_links = foldMap (map i_link . f_items) rebuilt_feeds
            get_batch (Follows fs) = fs
            get_batch (Digests fs) = fs
            items_notified =
                let fs = foldMap (foldMap f_items . get_batch . snd) makeBatches
                 in map i_link fs
            missing = filter (`notElem` items_notified) rebuilt_items_links
         in (missing, makeBatches)
    makeBatches = foldl' step HMS.empty due_chats
    step batches (!sub, FollowFeedLinks !fs) =
        let collected = relevant_to fs sub mb_last_run
         in if null collected then batches else HMS.insert (sub_chatid sub) (sub, Follows collected) batches
    step batches (!sub, DigestFeedLinks !fs) =
        let collected = relevant_to fs sub $ sub_last_digest sub
         in if null collected then batches else HMS.insert (sub_chatid sub) (sub, Digests collected) batches
    relevant_to fs sub time_ref =
        let select =
                filter
                    ( \i ->
                        let out_scope_or_matching = i_feed_link i `notElem` scope sub || has_keywords i (words_searched sub)
                         in fresher_than i time_ref && i_link i `notElem` previously_sent_items && out_scope_or_matching && not (has_keywords i $ blacklisted sub)
                    )
         in foldl'
                ( \acc f ->
                    let selected = slice . sort . select $ f_items f
                     in if f_link f `elem` targets && (not . null $ selected) then f{f_items = selected} : acc else acc
                )
                []
                rebuilt_feeds
      where
        sort = sortOn i_pubdate
        slice = take . settings_digest_size . sub_settings $ sub
        targets = S.toList (scope sub) ++ fs
    fresher_than _ Nothing = True
    fresher_than i (Just t) = t < i_pubdate i
    has_keywords i = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` T.toCaseFold t) [i_desc i, i_link i, i_title i])
    scope = match_only_search_results . settings_word_matches . sub_settings
    blacklisted = match_blacklist . settings_word_matches . sub_settings
    words_searched = match_searchset . settings_word_matches . sub_settings
postNotifier _ _ _ = undefined

collectDue ::
    SubChats ->
    Maybe UTCTime ->
    UTCTime ->
    HMS.HashMap ChatId (SubChat, BatchRecipe)
-- Peeling off conditions for notifications:
-- new start or new digest or new follow
collectDue chats last_run now =
    foldl'
        ( \hmap c@SubChat{..} ->
            let interval = settings_digest_interval sub_settings
             in if settings_paused sub_settings
                    then hmap
                    else case settings_digest_start sub_settings of
                        Nothing ->
                            if nextWasNow sub_next_digest sub_last_digest interval
                                then -- 'digests' take priority over 'follow notifications'
                                    HMS.insert sub_chatid (c, DigestFeedLinks $ S.toList sub_feeds_links) hmap
                                else
                                    if settings_follow sub_settings
                                        then case last_run of
                                            Nothing -> HMS.insert sub_chatid (c, FollowFeedLinks $ S.toList sub_feeds_links) hmap
                                            Just t ->
                                                if addUTCTime 1200 t < now
                                                    then HMS.insert sub_chatid (c, FollowFeedLinks $ S.toList sub_feeds_links) hmap
                                                    else hmap
                                        else hmap
                        Just new ->
                            if new < now
                                then HMS.insert sub_chatid (c, DigestFeedLinks $ S.toList sub_feeds_links) hmap
                                else hmap
        )
        HMS.empty
        chats
  where
    nextWasNow Nothing Nothing _ = True
    nextWasNow (Just next_t) _ _ = next_t < now
    nextWasNow Nothing (Just last_t) i = findNextTime last_t i < now

findNextTime :: UTCTime -> DigestInterval -> UTCTime
--  No 'digest_at' or 'digest_every' set? In 1.5 hour
--  No 'digest_at' but 'digest_every' is set? In the <number of seconds> to which
--      'digest_every' is set.
--  Both set? The closest instant in the future among the two instants determined
--      from both, respectively.
findNextTime now (DigestInterval Nothing Nothing) = addUTCTime 9000 now
findNextTime now (DigestInterval (Just xs) Nothing) = addUTCTime xs now
findNextTime now (DigestInterval mbxs (Just ts)) = case mbxs of
    Nothing ->
        if null times
            then next_day
            else still_today
    Just xs ->
        if xs > 86400
            then addUTCTime (xs - 86400) next_day
            else
                if null times
                    then minimum [addUTCTime xs now, next_day]
                    else minimum [addUTCTime xs now, still_today]
  where
    toNominalDifftime h m = realToFrac $ h * 3600 + m * 60
    from_midnight = realToFrac $ utctDayTime now
    times =
        foldl'
            ( \acc (!h, !m) ->
                let t = toNominalDifftime h m
                 in if from_midnight < t then (t - from_midnight) : acc else acc
            )
            []
            ts
    (early_h, early_m) =
        foldl'
            ( \(!h, !m) (!h', !m') ->
                if h' < h || h == h' && m' < m
                    then (h', m')
                    else (h, m)
            )
            (23, 59)
            ts
    to_midnight = realToFrac $ 86400 - utctDayTime now
    until_midnight = addUTCTime to_midnight now
    next_day = addUTCTime (toNominalDifftime early_h early_m) until_midnight
    still_today = addUTCTime (minimum times) now

collectNoDigest :: [ChatId] -> HMS.HashMap k (SubChat, b) -> [ChatId]
collectNoDigest has_digest =
    HMS.foldl'
        ( \acc (!c, _) ->
            let cid = sub_chatid c
             in if cid `notElem` has_digest then cid : acc else acc
        )
        []

feedlinksWithMissingPubdates :: HMS.HashMap FeedLink Feed -> [FeedLink]
feedlinksWithMissingPubdates fs =
    HMS.keys $
        HMS.filter
            ( \f ->
                not (null $ f_items f)
                    && (let items = f_items f; (h, l) = (head items, last items) in i_pubdate h == i_pubdate l)
            )
            fs

partitionDigests :: HMS.HashMap ChatId (SubChat, Batch) -> (S.Set T.Text, S.Set T.Text)
partitionDigests =
    foldl'
        ( \(!not_found, !found) (c, bat) ->
            let subs = sub_feeds_links c
                found' = case bat of
                    Follows fs -> S.fromList $ map f_link fs
                    Digests fs -> S.fromList $ map f_link fs
                not_found' = S.filter (`notElem` found') subs
             in (not_found `S.union` not_found', found `S.union` found')
        )
        (mempty, mempty)

markNotified :: MonadIO m => AppConfig -> [ChatId] -> UTCTime -> m ()
-- marking input chats as notified
markNotified env notified_chats now = liftIO $
    modifyMVar_ (subs_state env) $ \subs ->
        let updated_chats = updated_notified_chats notified_chats subs
         in evalDb env (UpsertChats updated_chats) >>= \case
                DbErr err -> do
                    writeChan (postjobs env) $
                        JobTgAlert $
                            "notifier: failed to \
                            \ save updated chats to db because of this error"
                                `T.append` renderDbError err
                    pure subs
                DbOk -> pure updated_chats
                _ -> pure subs
  where
    updated_notified_chats notified =
        HMS.mapWithKey
            ( \cid c ->
                if cid `elem` notified
                    then
                        let next = Just . findNextTime now . settings_digest_interval . sub_settings $ c
                         in -- updating last, next, and consuming 'settings_digest_start'
                            case settings_digest_start . sub_settings $ c of
                                Nothing -> c{sub_last_digest = Just now, sub_next_digest = next}
                                Just _ ->
                                    c
                                        { sub_last_digest = Just now
                                        , sub_next_digest = next
                                        , sub_settings = (sub_settings c){settings_digest_start = Nothing}
                                        }
                    else c
            )