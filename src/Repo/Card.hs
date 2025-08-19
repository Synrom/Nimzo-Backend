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
insert card = do
    result :: Card <- one =<< runQuery 
      ("INSERT INTO cards (deck_id, moves, title, color) VALUES (?, ?, ?, ?) RETURNING" <> returnFields)
      (card.deckId, card.moves, card.title, card.color)
    _ <- execute
      "INSERT INTO user_card_views (user_id, card_id, user_deck_id) \
      \ SELECT udv.user_id, c.id, udv.id \
      \ FROM user_deck_views AS udv \
      \ JOIN cards AS c ON c.deck_id = udv.deck_id \
      \ WHERE c.id = ?\
      \ ON CONFLICT DO NOTHING"
      (Only result.cardId)
    return result

update :: MonadDB m => Card -> m Card
update card = one =<< runQuery query (card.moves, card.title, card.color, card.cardId)
  where
    query :: Query
    query = 
      "UPDATE cards SET moves = ?, title = ?\
      \,color = ?, last_modified = CURRENT_TIMESTAMP \
      \WHERE id = ? RETURNING" <> returnFields
      

-- TODO: search (moves) and update