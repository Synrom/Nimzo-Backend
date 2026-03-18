{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Deck where

import Database.PostgreSQL.Simple (Only(..), Query, fromOnly)
import Database.PostgreSQL.Simple.ToField (Action, ToField(toField))
import Data.String (fromString)
import Data.String.HT (trim)
import Control.Monad
import Repo.Classes
import Repo.Utils (one, notNull, removePrefix, orMinTime, safeLast)
import Models.Deck (Deck(..))
import Models.DeckSearch (SearchContinuation(..), SearchContinuationsResponse(..))
import Models.User (User(..))
import Models.UserDeckView (UserDeckView(..))
import Models.Card (CardQuery(..), Card(..), PagedCards (..), PendingCard (..))

returnFields :: Query
returnFields = " id, name, is_public, description, color, num_cards_total, author, user_deck_id "

qualifiedReturnFields :: Query
qualifiedReturnFields = " d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id "

insertOrUpdate :: MonadDB m => Deck -> m Deck
insertOrUpdate deck = let
  query :: Query
  query = 
    "INSERT INTO decks (name, is_public, description, color, \
    \num_cards_total, author, user_deck_id) VALUES (?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (user_deck_id) \
    \DO UPDATE SET \
    \ name = EXCLUDED.name \
    \, is_public = EXCLUDED.is_public \
    \, description = EXCLUDED.description \
    \, color = COALESCE(EXCLUDED.color, decks.color) \
    \, author = EXCLUDED.author \
    \, num_cards_total = EXCLUDED.num_cards_total \
    \, last_modified = CURRENT_TIMESTAMP \
    \RETURNING" 
    <> returnFields
  in
    one =<< runQuery 
      query 
      (deck.name, deck.isPublic, deck.description, deck.color, deck.numCardsTotal, deck.author, deck.user_deck_id)

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

buildContinuationQuery :: Query -> Query -> [Action] -> String -> (Query, [Action])
buildContinuationQuery sourceSql movesExpr baseParams prefix
  | null prefix =
      ( "SELECT DISTINCT split_part(" <> movesExpr <> ", ' ', 1) "
        <> sourceSql
        <> " AND " <> movesExpr <> " != '' "
        <> "ORDER BY split_part(" <> movesExpr <> ", ' ', 1)"
      , baseParams
      )
  | otherwise =
      ( "SELECT next_move FROM ( "
        <> "SELECT DISTINCT split_part(substring(" <> movesExpr <> " from ? + 2), ' ', 1) AS next_move "
        <> sourceSql
        <> " AND " <> movesExpr <> " LIKE ? || ' %' "
        <> ") sub "
        <> "WHERE next_move != '' "
        <> "ORDER BY next_move"
      , [toField $ length prefix] <> baseParams <> [toField prefix]
      )

buildSearchContinuationsQuery :: Query -> Query -> [Action] -> String -> Maybe Integer -> (Query, [Action])
buildSearchContinuationsQuery sourceSql movesExpr fixedParams prefix mLimit =
  (querySql <> limitSql, queryParams <> limitParams)
  where
    (querySql, queryParams)
      | null prefix =
          ( "SELECT split_part(" <> movesExpr <> ", ' ', 1) AS move, COUNT(*) AS nr_cards "
            <> sourceSql
            <> " AND " <> movesExpr <> " != '' "
            <> "GROUP BY move "
            <> "ORDER BY nr_cards DESC, move"
          , fixedParams
          )
      | otherwise =
          ( "SELECT next_move AS move, COUNT(*) AS nr_cards FROM ( "
            <> "SELECT split_part(substring(" <> movesExpr <> " from ? + 2), ' ', 1) AS next_move "
            <> sourceSql
            <> " AND " <> movesExpr <> " LIKE ? || ' %' "
            <> ") sub "
            <> "WHERE next_move != '' "
            <> "GROUP BY next_move "
            <> "ORDER BY nr_cards DESC, next_move"
          , [toField $ length prefix] <> fixedParams <> [toField prefix]
          )
    limitParams = maybe [] (\limit' -> [toField limit']) (normalizeLimit mLimit)
    limitSql
      | null limitParams = ""
      | otherwise = " LIMIT ?"

buildColorFilter :: Maybe String -> (Query, [Action])
buildColorFilter Nothing = ("", [])
buildColorFilter (Just color) = (" AND d.color = ?", [toField color])

buildDecksQuery :: String -> Maybe String -> Maybe Integer -> (Query, [Action])
buildDecksQuery prefix mColor mLimit =
  ( baseSql <> colorSql <> prefixSql <> orderSql <> limitSql
  , colorParams <> prefixParams <> limitParams
  )
  where
    baseSql =
      "SELECT" <> qualifiedReturnFields <> ", COUNT(*) AS nr_cards " <>
      "FROM user_card_views ucv \
      \JOIN decks d ON d.user_deck_id = ucv.user_deck_id \
      \WHERE d.is_public = TRUE"
    (colorSql, colorParams) = buildColorFilter mColor
    (prefixSql, prefixParams)
      | null prefix = ("", [])
      | otherwise =
          ( " AND (ucv.moves = ? OR ucv.moves LIKE ? || ' %')"
          , [toField prefix, toField prefix]
          )
    orderSql =
      " GROUP BY d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id \
      \ORDER BY COUNT(*) DESC, d.name"
    limitParams = maybe [] (\limit' -> [toField limit']) (normalizeLimit mLimit)
    limitSql
      | null limitParams = ""
      | otherwise = " LIMIT ?"

normalizeLimit :: Maybe Integer -> Maybe Integer
normalizeLimit = fmap (max 0)

listContinuations :: MonadDB m => String -> String -> m [String]
listContinuations userDeckId prefix
  = map fromOnly <$> runQuery sql params
  where
    (sql, params) =
      buildContinuationQuery
        "FROM user_card_views WHERE user_deck_id = ?"
        "moves"
        [toField userDeckId]
        prefix

searchContinuations :: MonadDB m => String -> Maybe String -> Maybe Integer -> Maybe Integer -> m SearchContinuationsResponse
searchContinuations prefix mColor mDeckLimit mContinuationLimit = SearchContinuationsResponse
  <$> runQuery continuationSql continuationParams
  <*> runQuery decksSql decksParams
  where
    (colorSql, colorParams) = buildColorFilter mColor
    (continuationSql, continuationParams) =
      buildSearchContinuationsQuery
        ("FROM user_card_views ucv JOIN decks d ON d.user_deck_id = ucv.user_deck_id WHERE d.is_public = TRUE" <> colorSql)
        "ucv.moves"
        colorParams
        prefix
        mContinuationLimit
    (decksSql, decksParams) = buildDecksQuery prefix mColor mDeckLimit

listCardsOfDeck :: MonadDB m => CardQuery -> m PagedCards
listCardsOfDeck rawQuery = do
  let cardQuery = rawQuery { limit = min 100 rawQuery.limit }
  deck <- find cardQuery.deckId
  let finalParams = [toField deck.user_deck_id] <> cursorParams <> prefixParams <> [toField cardQuery.limit]
  paginateCards <$> runQuery finalSql finalParams
  where
  fields = " moves, title, color, id "
  baseSql = "SELECT" <> fields <> "FROM user_card_views WHERE user_deck_id=?"
  (cursorCondition, cursorParams) = case rawQuery.cursor of
    Nothing   -> ("", [])
    Just next -> (" AND id > ?", [toField next])
  (prefixCondition, prefixParams) = case rawQuery.prefix of
    Nothing -> ("", [])
    Just p  -> (" AND (moves = ? OR moves LIKE ? || ' %')", [toField p, toField p])
  finalSql = baseSql <> cursorCondition <> prefixCondition <> " ORDER BY id LIMIT ?"
