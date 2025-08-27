{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Card where

import Models.Card (Card(..))
import Database.PostgreSQL.Simple (Only(..), Query)
import Control.Monad
import Repo.Classes
import Repo.Utils (one)

returnFields :: Query
returnFields = " color, moves, title, deck_id, id "

insert :: MonadDB m => Card -> m Card
insert card = one =<< runQuery 
      ("INSERT INTO cards (deck_id, moves, title, color) VALUES (?, ?, ?, ?) RETURNING" <> returnFields)
      (card.deckId, card.moves, card.title, card.color)
  
find :: MonadDB m => Integer -> m [Card]
find = runQuery ("SELECT" <> returnFields <> "FROM cards WHERE deck_id = ?") . Only

update :: MonadDB m => Card -> m Card
update card = one =<< runQuery query (card.moves, card.title, card.color, card.cardId)
  where
    query :: Query
    query = 
      "UPDATE cards SET moves = ?, title = ?\
      \,color = ?, last_modified = CURRENT_TIMESTAMP \
      \WHERE id = ? RETURNING" <> returnFields
      

-- TODO: search (moves) and update