{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Feedback where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only (..), Query)
import Models.Feedback (Feedback (..), FeedbackPayload (..))
import Repo.Classes
import Repo.Utils (one)

returnFields :: Query
returnFields = " id, username, text, stars, created_at "

insert :: MonadDB m => String -> FeedbackPayload -> m Feedback
insert username payload = one =<< runQuery query (username, payload.text, payload.stars)
  where
    query :: Query
    query =
      "INSERT INTO feedback (username, text, stars) VALUES (?, ?, ?) RETURNING "
        <> returnFields

findLatestByUsername :: MonadDB m => String -> m (Maybe Feedback)
findLatestByUsername username =
  listToMaybe <$> runQuery query (Only username)
  where
    query :: Query
    query =
      "SELECT"
        <> returnFields
        <> "FROM feedback WHERE username = ? ORDER BY id DESC LIMIT 1"
