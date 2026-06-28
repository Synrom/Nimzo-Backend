{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.UserExplanationView where

import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Only(..), Query)
import Models.UserExplanationView (UserExplanationView(..))
import Models.Watermelon (DatabaseTime(..))
import Repo.Classes
import Repo.Utils (notNull, one, orMinTime, removePrefix)
import Control.Monad.IO.Class (MonadIO(liftIO))

returnFields :: Query
returnFields = " user_deck_id, user_id, id, fen, move, text, visualizers "

markString :: String -> String -> String
markString username id = username ++ id

mark :: UserExplanationView -> UserExplanationView
mark explanationView = explanationView
  { explanationViewId = markString explanationView.userId explanationView.explanationViewId
  , userDeckId = markString explanationView.userId explanationView.userDeckId
  }

unmark :: UserExplanationView -> UserExplanationView
unmark explanationView = explanationView
  { explanationViewId = removePrefix explanationView.userId explanationView.explanationViewId
  , userDeckId = removePrefix explanationView.userId explanationView.userDeckId
  }

unmarkAll :: [UserExplanationView] -> [UserExplanationView]
unmarkAll = map unmark

updatedSince :: MonadDB m => String -> Maybe UTCTime -> m [UserExplanationView]
updatedSince username since = unmarkAll <$> runQuery query (orMinTime since, orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_explanation_views WHERE last_modified > ? AND created_at < ? AND user_id = ?"

createdSince :: MonadDB m => String -> Maybe UTCTime -> m [UserExplanationView]
createdSince username since = unmarkAll <$> runQuery query (orMinTime since, username)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_explanation_views WHERE created_at > ? AND user_id = ?"

deletedSince :: MonadDB m => String -> Maybe UTCTime -> m [String]
deletedSince username since = unmarkUsernameAll <$> do
  now <- liftIO getCurrentTime
  runQuery query (orMinTime since, fromMaybe now since, username)
  where
    query :: Query
    query = "SELECT id FROM deleted_explanation_views WHERE deleted_at > ? AND created_at <= ? AND user_id = ?"
    unmarkUsername = removePrefix username
    unmarkUsernameAll = map (unmarkUsername . fromOnly)

modified :: MonadDB m => String -> UTCTime -> [UserExplanationView] -> m Bool
modified username since _ = do
  items :: [UserExplanationView] <- runQuery query (since, since, username)
  pure (notNull items)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_explanation_views WHERE (created_at > ? OR last_modified > ?) AND user_id = ?"

insertOrUpdate :: MonadDB m => UTCTime -> UserExplanationView -> m UserExplanationView
insertOrUpdate time unmarked = one =<< runQuery query
  ( explanation.explanationViewId
  , explanation.userDeckId
  , explanation.userId
  , explanation.fen
  , explanation.move
  , explanation.text
  , explanation.visualizers
  , time
  , time
  )
  where
    query :: Query
    query = "INSERT INTO user_explanation_views \
    \(id, user_deck_id, user_id, fen, move, text, visualizers, last_modified, created_at) \
    \VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT (id) \
    \DO UPDATE SET \
    \user_deck_id = EXCLUDED.user_deck_id \
    \, user_id = EXCLUDED.user_id \
    \, fen = EXCLUDED.fen \
    \, move = EXCLUDED.move \
    \, text = EXCLUDED.text \
    \, visualizers = EXCLUDED.visualizers \
    \, last_modified = EXCLUDED.last_modified \
    \RETURNING" <> returnFields
    explanation = mark unmarked

findOfUDV :: MonadDB m => String -> m [String]
findOfUDV markedDeckId = do
  rows :: [Only String] <- runQuery
    "SELECT id FROM user_explanation_views WHERE user_deck_id = ?"
    (Only markedDeckId)
  pure $ map fromOnly rows

delete :: MonadDB m => String -> UTCTime -> String -> m ()
delete username deleteAt id = do
  createdRows <- runQuery "SELECT created_at FROM user_explanation_views WHERE id = ?" (Only markedId)
  case createdRows of
    [] -> pure ()
    [DatabaseTime createdAt] -> do
      execute "DELETE FROM user_explanation_views WHERE id = ?" (Only markedId)
      execute "INSERT INTO deleted_explanation_views (id, user_id, created_at, deleted_at) VALUES (?, ?, ?, ?)" (markedId, username, createdAt, deleteAt)
      pure ()
    _ -> pure ()
  where
    markedId = markString username id
