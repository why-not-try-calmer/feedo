{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Notifications where

import Data.Time (UTCTime (utctDayTime), addUTCTime)
import AppTypes
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import TgramOutJson (ChatId)
import Mongo (HasMongo (evalDb))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent

collectDue ::
    SubChats ->
    Maybe UTCTime ->
    UTCTime ->
    HMS.HashMap ChatId (SubChat, BatchRecipe)
-- Peeling off conditions for notifications:
-- new start or new digest or new follow
collectDue chats last_run now =
    foldl' (\hmap c@SubChat{..} ->
        let interval = settings_digest_interval sub_settings
        in  if settings_paused sub_settings
            then hmap
            else case settings_digest_start sub_settings of
                Nothing ->
                    if nextWasNow sub_next_digest sub_last_digest interval
                    -- 'digests' take priority over 'follow notifications'
                    then HMS.insert sub_chatid (c, DigestFeedLinks $ S.toList sub_feeds_links) hmap
                    else
                        if settings_follow sub_settings then case last_run of
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
            ) HMS.empty chats
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
        if null times then next_day
        else still_today
    Just xs ->
        if null times then minimum [addUTCTime xs now, next_day]
        else minimum [addUTCTime xs now, still_today]
    where
        toNominalDifftime h m = realToFrac $ h * 3600 + m * 60
        from_midnight = realToFrac $ utctDayTime now
        times = foldl' (\acc (!h, !m) ->
            let t = toNominalDifftime h m
            in  if from_midnight < t then (t - from_midnight):acc else acc
            ) [] ts
        (early_h, early_m) =
            foldl' (\(!h, !m) (!h', !m') ->
                if h' < h || h == h' && m' < m then (h', m')
                else (h, m)) (23, 59) ts
        to_midnight = realToFrac $ 86400 - utctDayTime now
        until_midnight = addUTCTime to_midnight now
        next_day = addUTCTime (toNominalDifftime early_h early_m) until_midnight
        still_today = addUTCTime (minimum times) now

notifFrom ::
    Maybe UTCTime ->
    [FeedLink] ->
    FeedsMap ->
    HMS.HashMap ChatId (SubChat, BatchRecipe) ->
    HMS.HashMap ChatId (SubChat, Batch)
notifFrom last_run flinks feeds_map = foldl' (\hmap (!c, !batch) -> 
    let (recipes, time_ref) = case batch of 
            DigestFeedLinks fs -> (fs, sub_last_digest c)
            FollowFeedLinks fs -> (fs, last_run)
    in  let collected = foldl' (\fs f ->
                let feeds_items =
                        let fresh = take (settings_digest_size . sub_settings $ c) $ fresh_filtered c (f_items f) time_ref
                        in  if f_link f `notElem` recipes || null fresh then fs else f { f_items = fresh }:fs
                in  if f_link f `notElem` flinks then fs else feeds_items) [] feeds_map
        in  if null collected then hmap else HMS.insert (sub_chatid c) (c, mkBatch batch collected) hmap) HMS.empty
    where
        has_keywords i = any (\w -> any (\t -> T.toCaseFold w `T.isInfixOf` T.toCaseFold t) [i_desc i, i_link i, i_title i])
        fresh_filtered c is time_ref =
            let bl = match_blacklist . settings_word_matches . sub_settings $ c
                only_search_notif = match_only_search_results . settings_word_matches . sub_settings $ c
                search_notif = match_searchset . settings_word_matches . sub_settings $ c
            in  foldl' (\acc i ->
                    let off_scope = [fresher_than i time_ref, i `lacks_keywords` bl]
                        in_scope = off_scope ++ [i `has_keywords` search_notif]
                    in  if i_feed_link i `S.member` only_search_notif then
                            if and in_scope then i:acc else acc
                        else if and off_scope then i:acc else acc) [] is
        fresher_than _ Nothing = True
        fresher_than i (Just t) = t < i_pubdate i
        lacks_keywords i kws = not $ has_keywords i kws


feedlinksWithMissingPubdates :: HMS.HashMap FeedLink  Feed -> [FeedLink ]
feedlinksWithMissingPubdates fs = HMS.keys $ HMS.filter 
    (\f -> not (null $ f_items f) && 
    (let items = f_items f; (h, l) = (head items, last items) in  i_pubdate h == i_pubdate l)) fs

keepNew :: HMS.HashMap T.Text Feed -> [Feed] -> ([T.Text], HMS.HashMap T.Text Feed)
-- filters out duplicate items from rebuilt feeds kept
keepNew feeds_map feeds =
    let delKey f = HMS.delete (f_link f)
        step (discarded, new_feeds_hmap) old_f = case HMS.lookup (f_link old_f) new_feeds_hmap of
            Nothing -> (discarded, new_feeds_hmap)
            Just new_f ->
                let new_items = f_items new_f
                    old_items = f_items old_f
                    maybe_first_last = if null new_items then Nothing else Just (head new_items, last new_items)
                    diffed = filter (\new_item -> all (\old_item -> i_link new_item /= i_link old_item) old_items) new_items
                    discarded' = discarded ++ map i_link new_items
                in  if null diffed
                    then (discarded', delKey old_f new_feeds_hmap)
                    else case maybe_first_last of
                        Nothing -> (discarded, delKey old_f new_feeds_hmap)
                        Just (fi, la) ->
                            if i_pubdate fi /= i_pubdate la then (discarded, new_feeds_hmap)
                            else 
                                let updated = HMS.update (\f -> Just $ f { f_items = diffed } ) (f_link old_f) new_feeds_hmap
                                in  (discarded', updated)
    in  foldl' step ([], feeds_map) feeds

markNotified :: MonadIO m => AppConfig -> [ChatId] -> UTCTime -> m ()
-- marking input chats as notified 
markNotified env notified_chats now = liftIO $ modifyMVar_ (subs_state env) $ \subs ->
    let updated_chats = updated_notified_chats notified_chats subs
    in  evalDb env (UpsertChats updated_chats) >>= \case
        DbErr err -> do
            writeChan (postjobs env) $ JobTgAlert $ "notifier: failed to \
                \ save updated chats to db because of this error" `T.append` renderDbError err
            pure subs
        DbOk -> pure updated_chats
        _ -> pure subs
    where
        updated_notified_chats notified = HMS.mapWithKey (\cid c ->
            if cid `elem` notified
            then
                let next = Just . findNextTime now . settings_digest_interval . sub_settings $ c
                in  -- updating last, next, and consuming 'settings_digest_start'
                    case settings_digest_start . sub_settings $ c of
                    Nothing -> c { sub_last_digest = Just now, sub_next_digest = next }
                    Just _ -> c {
                        sub_last_digest = Just now,
                        sub_next_digest = next,
                        sub_settings = (sub_settings c) { settings_digest_start = Nothing }
                        }
            else c)