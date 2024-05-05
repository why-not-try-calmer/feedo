{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Chats where

import Control.Monad.Reader
import qualified Data.HashMap.Strict as HMS
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Mongo (HasMongo (evalDb), evalDb)
import Replies (render)
import Requests (alertAdmin)
import TgramOutJson (ChatId, UserId)
import Types
import Utils (defaultChatSettings, findNextTime, removeByUserIdx, updateSettings)

getChats :: MonadIO m => App m SubChats
getChats = do
  env <- ask
  let  warn msg = alertAdmin (postjobs env) ("getChats: failed! Error: " `T.append` T.pack msg)
  response <- evalDb GetAllChats
  case response of
    Left err -> do
      warn $ show err
      pure mempty
    Right (DbChats chats) -> pure $ HMS.fromList $ map (\c -> (sub_chatid c, c)) chats
    something_else -> do
      warn $ show something_else
      pure mempty

withChats :: MonadIO m => UserAction -> Maybe UserId -> ChatId -> App m (Either TgEvalError ChatRes)
withChats action maybe_userid cid = do
  env <- ask
  chats <- getChats 
  res <- liftIO $ chats `withHmap` env
  case res of
    Left err -> pure $ Left err
    Right ChatOk -> pure $ Right ChatOk
    Right r -> pure . Right $ r
 where
  withHmap hmap env = case HMS.lookup cid hmap of
    Nothing ->
      let initialized now flinks linked_to =
            SubChat
              { sub_chatid = cid
              , sub_last_digest = Nothing
              , sub_next_digest = Just $ findNextTime now (settings_digest_interval defaultChatSettings)
              , sub_linked_to = linked_to
              , sub_feeds_links = S.fromList flinks
              , sub_settings = defaultChatSettings
              , sub_active_admins = HMS.empty
              }
          saveToDb c = evalDb $ UpsertChat c
       in case action of
            Link target_id ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now [] (Just target_id)
                 in runApp env $
                      saveToDb new_chat
                        >>= \case
                          Left err -> pure (Left . UpdateError $ "Db refused to link this chat: " `T.append` render err)
                          _ -> pure $ Right ChatOk
            Sub links ->
              getCurrentTime >>= \now ->
                let new_chat = initialized now links Nothing
                 in runApp env $
                      saveToDb new_chat
                        >>= \case
                          Left err -> pure (Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                          _ -> pure $ Right ChatOk
            _ -> pure (Left . UpdateError $ "Chat not found. Please add it by first using /sub with a valid web feed url.")
    Just c -> case action of
      Link target_id ->
        let updated_c = c{sub_linked_to = Just target_id}
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (Left . UpdateError $ "Db refused to link this chat to a new chat_id." `T.append` render err)
                  _ -> pure $ Right ChatOk
      Migrate to ->
        let updated_c = c{sub_chatid = to}
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (Left . UpdateError $ "Db refused to migrate this chat." `T.append` render err)
                  _ -> pure (Right ChatOk)
      Reset ->
        let updated_c = c{sub_settings = defaultChatSettings}
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (Left . UpdateError $ "Db refused to reset this chat's settings." `T.append` render err)
                  _ -> pure (Right ChatOk)
      Sub links ->
        let updated_c = c{sub_feeds_links = S.fromList $ links ++ (S.toList . sub_feeds_links $ c)}
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                  _ -> pure (Right ChatOk)
      UnSub refs -> do
        let (byurls, byids) = foldl' (\(!us, !is) v -> case v of ByUrl u -> (u : us, is); ById i -> (us, i : is)) ([], []) refs
            update_db c' =
              runApp env $
                evalDb (UpsertChat c')
                  >>= \case
                    Left err -> pure (Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
                    _ -> pure (Right ChatOk)
        if not (null byurls) && not (null byids)
          then pure (Left . BadRef $ "You cannot mix references by urls and by ids in the same command.")
          else
            if null byurls
              then case removeByUserIdx (S.toList . sub_feeds_links $ c) byids of
                Nothing -> pure (Left . BadRef $ "Invalid references. Make sure to use /list to get a list of valid references.")
                Just removed ->
                  let updated_c = c{sub_feeds_links = S.fromList removed}
                   in update_db updated_c
              else
                let updated_c = c{sub_feeds_links = S.filter (`notElem` byurls) $ sub_feeds_links c}
                 in update_db updated_c
      Purge ->
        runApp env $
          evalDb (DeleteChat cid)
            >>= \case
              Left err -> pure (Left . UpdateError $ "Db refused to subscribe you: " `T.append` render err)
              _ -> pure (Right ChatOk)
      SetChatSettings s ->
        let updated_settings = case s of
              Parsed p -> updateSettings p $ sub_settings c
              Immediate settings -> settings
            updated_next_notification now =
              let start = fromMaybe now $ settings_digest_start updated_settings
               in Just . findNextTime start . settings_digest_interval $ updated_settings
         in getCurrentTime >>= \now ->
              let updated_c =
                    c
                      { sub_next_digest = updated_next_notification now
                      , sub_settings = updated_settings
                      , sub_active_admins =
                          let prev = sub_active_admins c
                           in maybe prev (\uid -> HMS.insert uid now prev) maybe_userid
                      }
               in runApp env $
                    evalDb (UpsertChat updated_c)
                      >>= \case
                        Left _ -> pure (Left . UpdateError $ "Db refuse to update settings.")
                        _ -> pure (Right . ChatUpdated $ updated_c)
      Pause pause_or_resume ->
        let updated_sets = (sub_settings c){settings_paused = pause_or_resume}
            updated_c = c{sub_settings = updated_sets}
         in runApp env $
              evalDb (UpsertChat updated_c)
                >>= \case
                  Left err -> pure (Left . UpdateError . render $ err)
                  _ -> pure (Right ChatOk)
      _ -> pure (Right ChatOk)
