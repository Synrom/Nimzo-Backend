{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Deck where

import Database.PostgreSQL.Simple (Only(..), Query)
import Data.String (fromString)
import Data.String.HT (trim)
import Control.Monad
import Repo.Classes
import Repo.Utils (one, notNull, removePrefix, orMinTime, safeLast)
import Models.Deck (Deck(..))
import Models.User (User(..))
import Models.UserDeckView (UserDeckView(..))
import Models.Card (CardQuery(..), Card(..), PagedCards (..), PendingCard (..))
import Database.PostgreSQL.Simple.ToField (ToField(toField))

returnFields :: Query
returnFields = " id, name, is_public, description, num_cards_total, author, user_deck_id "

insertOrUpdate :: MonadDB m => Deck -> m Deck
insertOrUpdate deck = let
  query :: Query
  query = 
    "INSERT INTO decks (name, is_public, description, \
    \num_cards_total, author, user_deck_id) VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (user_deck_id) \
    \DO UPDATE SET \
    \ name = EXCLUDED.name \
    \, is_public = EXCLUDED.is_public \
    \, description = EXCLUDED.description \
    \, author = EXCLUDED.author \
    \, num_cards_total = EXCLUDED.num_cards_total \
    \, last_modified = CURRENT_TIMESTAMP \
    \RETURNING" 
    <> returnFields
  in
    one =<< runQuery 
      query 
      (deck.name, deck.isPublic, deck.description, deck.numCardsTotal, deck.author, deck.user_deck_id)

searchInstant :: MonadDB m => Maybe String -> m [Deck]
searchInstant Nothing = return []
searchInstant (Just s)
  | stripped == "" = return []
  | otherwise = runQuery query (stripped, stripped)
  where
    stripped = trim s
    query :: Query
    query =
      "SELECT" <> returnFields <>
      "FROM decks \
      \WHERE is_public = TRUE \
      \AND search_vector_name @@ to_tsquery('english', ? || ':*') \
      \ORDER BY ts_rank(search_vector_name, to_tsquery('english', ? || ':*')) DESC \
      \LIMIT 10"

search :: MonadDB m => Maybe String -> m [Deck]
search Nothing = return []
search (Just s) = runQuery query (s, s)
  where
    query :: Query
    query = 
      "SELECT" <> returnFields <>
      "FROM decks \
      \WHERE is_public = TRUE \
      \AND search_vector @@ plainto_tsquery('english', ?) \
      \ORDER BY ts_rank(search_vector, plainto_tsquery('english', ?)) DESC"

find :: MonadDB m => Integer -> m Deck
find deckId = one =<< runQuery
  query
  (Only deckId)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM decks WHERE id = ?"

alreadyExists :: MonadDB m => Deck -> m Bool
alreadyExists deck = do
  decks :: [Deck] <- runQuery query (Only $ "%" ++ removePrefix deck.author deck.user_deck_id)
  return (notNull decks)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM decks WHERE user_deck_id LIKE ?"

paginateCards :: [PendingCard] -> PagedCards
paginateCards pendingCards = case safeLast pendingCards of
  Nothing   -> PagedCards Nothing cards
  Just card -> PagedCards (Just card.id) cards
  where
    unpend (PendingCard moves title color _) = Card moves title color
    cards = map unpend pendingCards
    

listCardsOfDeck :: MonadDB m => CardQuery -> m PagedCards
listCardsOfDeck rawQuery = do
  let cardQuery = rawQuery { limit = min 100 rawQuery.limit }
  deck <- find cardQuery.deckId
  let finalParams = [toField deck.user_deck_id] <> params <> [toField cardQuery.limit]
  paginateCards <$> runQuery finalSql finalParams
  where
  fields = " moves, title, color, id "
  baseSql = "SELECT" <> fields <> "FROM user_card_views WHERE user_deck_id=?"
  (condition, params) = case rawQuery.cursor of
    Nothing   -> ("", [])
    Just next -> (" AND id > ?", [toField next])
  finalSql = baseSql <> condition <> " ORDER BY id LIMIT ?"