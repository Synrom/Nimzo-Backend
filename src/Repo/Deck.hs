{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Deck where

import Database.PostgreSQL.Simple (Only(..), Query)
import Data.String (fromString)
import Control.Monad
import Repo.Classes
import Repo.Utils (one)
import Models.Deck (Deck(..))
import Models.User (User(..))

returnFields :: Query
returnFields = " id, name, is_public, description, num_cards_total, author "

insert :: MonadDB m => Deck -> m Deck
insert deck = let
  query :: Query
  query = 
    "INSERT INTO decks (name, is_public, description, \
    \num_cards_total, author) VALUES (?, ?, ?, ?, ?) RETURNING" 
    <> returnFields
  in
    one =<< runQuery 
      query 
      (deck.name, deck.isPublic, deck.description, deck.numCardsTotal, deck.author)

search :: MonadDB m => Maybe String -> m [Deck]
search Nothing = return []
search (Just s) = runQuery query (Only s)
  where
    query :: Query
    query = 
      "SELECT" <> returnFields <>
      "FROM decks WHERE name ILIKE '%' || ? || '%'" 

update :: MonadDB m => Deck -> m Deck
update deck = let
  query :: Query
  query = 
    "UPDATE decks SET name = ?, is_public = ?, description = ?, num_cards_total = ?, last_modified = CURRENT_TIMESTAMP, \
    \ author = ? WHERE id = ? RETURNING" <> returnFields
  in 
    one =<< runQuery
      query
      (deck.name, deck.isPublic, deck.description, deck.numCardsTotal, deck.author, deck.deckId)

find :: MonadDB m => Integer -> m Deck
find deckId = one =<< runQuery
  query
  (Only deckId)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "WHERE id = ?"

authorsDeck :: MonadDB m => String -> Integer -> m Bool
authorsDeck username deckid = do
  decks :: [Deck] <- runQuery query (username, deckid)
  return $ not $ null decks
  where
    query :: Query
    query = "SELECT " <> returnFields <> "FROM decks WHERE author = ? AND id = ?"