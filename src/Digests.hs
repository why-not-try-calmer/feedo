{-# LANGUAGE FlexibleContexts #-}

module Digests (getPrebatch, fillBatch, makeDigests) where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadReader (..))
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.MongoDB (find, rest, select, (=:))
import Feeds (rebuildFeed)
import Mongo (HasMongo (evalDb), MongoDoc (readDoc), withDb)
import Replies (render)
import Requests (alertAdmin)
import TgramOutJson
import Types (App, AppConfig (..), DbAction (..), Feed (..), FeedError, FeedLink, FromCache (..), Item (i_pubdate), SubChat (..))
import Utils (partitionEither)

getPrebatch :: (HasMongo m, MonadIO m) => m (Either T.Text (S.Set FeedLink, [SubChat]))
getPrebatch = do
  now <- liftIO getCurrentTime
  docs <- withDb (getChats now >>= rest)
  case docs of
    Left err -> pure . Left $ T.pack . show $ err
    Right bson_chats ->
      let (chats, feedlinks) =
            foldl'
              ( \(cs, fs) doc ->
                  let c = readDoc doc :: SubChat
                   in (c : cs, fs `S.union` sub_feeds_links c)
              )
              ([], S.empty)
              bson_chats
       in pure $ Right (feedlinks, chats)
 where
  getChats now = find (select ["sub_next_digest" =: ["$lt" =: now]] "chats")

fillBatch :: (MonadIO m) => (S.Set FeedLink, [SubChat]) -> m ([FeedError], HMS.HashMap ChatId (SubChat, [Feed]))
{- Return only feeds with more than 0 items, along with failure messages -}
fillBatch (links, chats) = do
  (failed, feeds) <- liftIO $ partitionEither <$> mapConcurrently rebuildFeed (S.toList links)
  let feeds_of c = filter (\f -> f_link f `S.member` sub_feeds_links c) feeds
      fresh_only c = case sub_last_digest c of
        Nothing -> feeds_of c
        Just last_time ->
          let step' !fs !f =
                let items = filter (\i -> i_pubdate i > last_time) $ f_items f
                    f' = f{f_items = items}
                 in if null items then fs else fs ++ [f']
           in foldl' step' [] $ feeds_of c
      step acc c = HMS.insert (sub_chatid c) (c, fresh_only c) acc
      results = foldl' step HMS.empty chats
  pure (failed, results)

makeDigests :: (MonadIO m) => App m (Either T.Text FromCache)
makeDigests = do
  env <- ask
  prebatch <- getPrebatch
  case prebatch of
    Left err -> pure . Left $ err
    Right (links, chats) ->
      if null links
        then pure $ Right CacheOk
        else do
          (fetch_errors, batches) <- fillBatch (links, chats)
          let feeds = foldMap snd $ HMS.elems batches
          feeds_updated <- evalDb $ UpsertFeeds feeds
          feeds_archived <- evalDb $ ArchiveItems feeds
          let (archive_errors, _) = partitionEither [feeds_archived, feeds_updated]
              error_text_request = T.intercalate "," $ map (T.pack . show) fetch_errors
              error_text_db = T.intercalate "," $ map render archive_errors
          -- log failure to rebuild or log
          unless (null archive_errors) (liftIO $ print error_text_request >> alertAdmin (postjobs env) error_text_request)
          unless (null fetch_errors) (liftIO $ print error_text_db >> alertAdmin (postjobs env) error_text_db)
          pure . Right $ CacheDigests batches
