{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.UserCardView where

import Data.Time (UTCTime)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query, Only(..))
import Models.UserCardView (UserCardView(..))
import Repo.Classes
import Repo.Utils (notNull, one)

returnFields :: Query
returnFields = " num_correct_trials, next_request, card_id, user_id, id, moves, title, color "

updatedSince :: MonadDB m  => UTCTime -> m [UserCardView]
updatedSince since = runQuery query (Only since)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE last_modified > ?"

createdSince :: MonadDB m  => UTCTime -> m [UserCardView]
createdSince since = runQuery query (Only since)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE created_at > ?"

modified :: MonadDB m => UTCTime -> [UserCardView] -> m Bool
modified since cards = do
  items :: [UserCardView] <- runQuery query (since, since)
  return (notNull items)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_card_views WHERE created_at > ? OR last_modified > ?"

insertOrUpdate :: MonadDB m => UserCardView -> m UserCardView
insertOrUpdate card = one =<< runQuery query (card.ucvId , card.cardId, card.userDeckId, card.numCorrectTrials, card.nextRequest, card.userId)
  where
    query :: Query
    query = "INSERT INTO user_card_views \
    \(id, card_id, user_deck_id, \
    \num_correct_trials, next_request \
    \ moves, title, color, user_id) \
    \VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT (id) \
    \DO UPDATE SET \
    \card_id = EXCLUDED.card_id \
    \user_deck_id = EXCLUDED.user_deck_id \
    \user_id = EXCLUDED.user_id \
    \num_correct_trials = EXCLUDED.num_correct_trials \
    \moves = EXCLUDED.moves \
    \title = EXCLUDED.title \
    \color = EXCLUDED.color \
    \next_request = EXCLUDED.next_request \
    \last_modified = CURRENT_TIMESTAMP \
    \RETURNING" <> returnFields