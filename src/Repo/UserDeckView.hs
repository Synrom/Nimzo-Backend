{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.UserDeckView where

import Data.Time (UTCTime)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Query, Only(..))
import Models.UserDeckView (UserDeckView(..))
import Repo.Utils (notNull, one)
import Repo.Classes

returnFields :: Query
returnFields = " num_cards_today, cards_per_day, num_cards_learnt, is_author, user_id, deck_id, id, name, is_public, description, num_cards_total "

updatedSince :: MonadDB m  => UTCTime -> m [UserDeckView]
updatedSince since = runQuery query (Only since)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE last_modified > ?"

createdSince :: MonadDB m  => UTCTime -> m [UserDeckView]
createdSince since = runQuery query (Only since)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE created_at > ?"

modified :: MonadDB m => UTCTime -> [UserDeckView] -> m Bool
modified since cards = do
  items :: [UserDeckView] <- runQuery query (since, since)
  return (notNull items)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM user_deck_views WHERE created_at > ? OR last_modified > ?"

insertOrUpdate :: MonadDB m => UserDeckView -> m UserDeckView
insertOrUpdate deck = one =<< runQuery query 
    (deck.udvId, deck.userId, deck.deckId
    , deck.numCardsToday, deck.isAuthor
    , deck.cardsPerDay, deck.numCardsLearnt
    , deck.name, deck.isPublic, deck.description
    , deck.numCardsTotal)
  where
    query :: Query
    query = "INSERT INTO user_deck_views \
    \(id, user_id, deck_id, num_cards_today, \
    \is_author, cards_per_day, num_cards_learnt, \
    \ name, is_public, description, num_cards_total) \
    \VALUES (?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT (id) \
    \DO UPDATE SET \
    \user_id = EXCLUDED.user_id \
    \deck_id = EXCLUDED.deck_id \
    \num_cards_today = EXCLUDED.num_cards_today \
    \is_author = EXCLUDED.is_author \
    \cards_per_day = EXCLUDED.cards_per_day \
    \num_cards_learnt = EXCLUDED.num_cards_learnt \
    \name = EXCLUDED.name \
    \is_public = EXCLUDED.is_public \
    \description = EXCLUDED.description \
    \num_cards_total = EXCLUDED.num_cards_total \
    \author = EXCLUDED.author \
    \last_modified = CURRENT_TIMESTAMP\
    \RETURNING" <> returnFields