{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Deck.Query
  ( returnFields,
    searchReturnFields,
    insertOrUpdate,
    search,
    searchInstant,
    find,
    findWithRating,
    alreadyExists,
    listContinuations,
    searchContinuations,
    listCardsOfDeck,
    saveRating,
  )
where

import Database.PostgreSQL.Simple (Only (..), Query, fromOnly)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import Data.String.HT (trim)
import Repo.Classes
import Repo.Utils (ensure, notNull, one, removePrefix, safeLast)
import App.Error (AppError (..))
import Models.Card (Card (..), CardQuery (..), PagedCards (..), PendingCard (..))
import Models.Deck (Deck (..))
import Models.DeckDetails (DeckDetails (..))
import Models.DeckSearch (DeckSearchResult, SearchContinuation (..), SearchContinuationsResponse (..))
import Models.Watermelon (JsonableMsg (Msg))

returnFields :: Query
returnFields = " id, name, is_public, description, color, num_cards_total, author, user_deck_id, image_url, featured_source, featured_rank, video_url "

qualifiedReturnFields :: Query
qualifiedReturnFields = " d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id, d.image_url, d.featured_source, d.featured_rank, d.video_url "

searchReturnFields :: Query
searchReturnFields =
  " d.id, d.name, d.is_public, d.description, d.color \
  \, d.num_cards_total \
  \, d.author, d.user_deck_id \
  \, d.image_url \
  \, d.featured_source \
  \, d.featured_rank \
  \, d.video_url \
  \, d.rating_avg \
  \, d.rating_count \
  \, d.download_count \
  \, COALESCE(d.starting_position, '') AS preview_moves \
  \, CASE \
  \    WHEN lower(COALESCE(d.color, '')) IN ('w', 'wh', 'white') THEN 'White repertoire' \
  \    WHEN lower(COALESCE(d.color, '')) IN ('b', 'bl', 'black') THEN 'Black repertoire' \
  \    ELSE 'Both sides' \
  \  END AS repertoire "

insertOrUpdate :: MonadDB m => Deck -> m Deck
insertOrUpdate deck =
  one
    =<< runQuery
      query
      (deck.name, deck.isPublic, deck.description, deck.color, deck.numCardsTotal, deck.author, deck.user_deck_id)
  where
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

searchInstant :: MonadDB m => Maybe String -> m [DeckSearchResult]
searchInstant Nothing = return []
searchInstant (Just s)
  | stripped == "" = return []
  | otherwise = runQuery query (Only stripped)
  where
    stripped = trim s
    query :: Query
    query =
      "WITH q AS ( \
      \  SELECT to_tsquery('english', array_to_string(array_agg(token || ':*'), ' & ')) AS tsq \
      \  FROM regexp_split_to_table(lower(?), '[^[:alnum:]]+') AS t(token) \
      \  WHERE token <> '' \
      \) \
      \SELECT" <> searchReturnFields <>
      "FROM decks d \
      \CROSS JOIN q \
      \WHERE d.is_public = TRUE \
      \AND q.tsq IS NOT NULL \
      \AND d.search_vector_name @@ q.tsq \
      \ORDER BY ts_rank(d.search_vector_name, q.tsq) DESC \
      \LIMIT 10"

search :: MonadDB m => Maybe String -> m [DeckSearchResult]
search Nothing = return []
search (Just s)
  | stripped == "" = return []
  | otherwise = runQuery query (Only stripped)
  where
    stripped = trim s
    query :: Query
    query =
      "WITH q AS ( \
      \  SELECT to_tsquery('english', array_to_string(array_agg(token || ':*'), ' & ')) AS tsq \
      \  FROM regexp_split_to_table(lower(?), '[^[:alnum:]]+') AS t(token) \
      \  WHERE token <> '' \
      \) \
      \SELECT" <> searchReturnFields <>
      "FROM decks d \
      \CROSS JOIN q \
      \WHERE d.is_public = TRUE \
      \AND q.tsq IS NOT NULL \
      \AND d.search_vector @@ q.tsq \
      \ORDER BY \
      \  ts_rank(d.search_vector, q.tsq) DESC, \
      \  d.download_count DESC, \
      \  d.rating_avg DESC NULLS LAST"

find :: MonadDB m => Integer -> m Deck
find deckId = one =<< runQuery query (Only deckId)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM decks WHERE id = ?"

findWithRating :: MonadDB m => Maybe String -> Integer -> m DeckDetails
findWithRating username deckId = one =<< runQuery query (username, deckId)
  where
    query :: Query
    query =
      "SELECT d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id, d.image_url, d.featured_source, d.featured_rank, d.video_url \
      \, COALESCE(dr.user_id IS NOT NULL, FALSE) AS has_rated \
      \, dr.rating AS user_rating \
      \FROM decks d \
      \LEFT JOIN deck_ratings dr ON dr.deck_id = d.id AND dr.user_id = ? \
      \WHERE d.id = ?"

alreadyExists :: MonadDB m => Deck -> m Bool
alreadyExists deck = do
  decks :: [Deck] <- runQuery query (Only $ "%" ++ removePrefix deck.author deck.user_deck_id)
  return (notNull decks)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM decks WHERE user_deck_id LIKE ?"

paginateCards :: [PendingCard] -> PagedCards
paginateCards pendingCards = case safeLast pendingCards of
  Nothing -> PagedCards Nothing cards
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
  ( baseSql <> colorSql <> prefixSql <> orderSql <> limitSql,
    colorParams <> prefixParams <> limitParams
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
          ( " AND (ucv.moves = ? OR ucv.moves LIKE ? || ' %')",
            [toField prefix, toField prefix]
          )
    orderSql =
      " GROUP BY d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id, d.image_url, d.featured_source, d.featured_rank, d.video_url \
      \ORDER BY COUNT(*) DESC, d.name"
    limitParams = maybe [] (\limit' -> [toField limit']) (normalizeLimit mLimit)
    limitSql
      | null limitParams = ""
      | otherwise = " LIMIT ?"

normalizeLimit :: Maybe Integer -> Maybe Integer
normalizeLimit = fmap (max 0)

listContinuations :: MonadDB m => String -> String -> m [String]
listContinuations userDeckId prefix =
  map fromOnly <$> runQuery sql params
  where
    (sql, params) =
      buildContinuationQuery
        "FROM user_card_views WHERE user_deck_id = ?"
        "moves"
        [toField userDeckId]
        prefix

searchContinuations :: MonadDB m => String -> Maybe String -> Maybe Integer -> Maybe Integer -> m SearchContinuationsResponse
searchContinuations prefix mColor mDeckLimit mContinuationLimit =
  SearchContinuationsResponse
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
  let cardQuery = rawQuery {limit = min 100 rawQuery.limit}
  deck <- find cardQuery.deckId
  case rawQuery.cursor of
    Nothing -> do
      _ <- execute "UPDATE decks SET download_count = download_count + 1 WHERE id = ?" (Only cardQuery.deckId)
      pure ()
    Just _ -> pure ()
  let finalParams = [toField deck.user_deck_id] <> cursorParams <> prefixParams <> [toField cardQuery.limit]
  paginateCards <$> runQuery finalSql finalParams
  where
    fields = " moves, title, color, id "
    baseSql = "SELECT" <> fields <> "FROM user_card_views WHERE user_deck_id=?"
    (cursorCondition, cursorParams) = case rawQuery.cursor of
      Nothing -> ("", [])
      Just next -> (" AND id > ?", [toField next])
    (prefixCondition, prefixParams) = case rawQuery.prefix of
      Nothing -> ("", [])
      Just p -> (" AND (moves = ? OR moves LIKE ? || ' %')", [toField p, toField p])
    finalSql = baseSql <> cursorCondition <> prefixCondition <> " ORDER BY id LIMIT ?"

saveRating :: MonadDB m => String -> Integer -> Integer -> m JsonableMsg
saveRating username deckId ratingValue = do
  ensure invalidRating (ratingValue >= 1 && ratingValue <= 5)
  _ <- find deckId
  _ <- execute query (deckId, username, ratingValue)
  return $ Msg "Successfully saved deck rating."
  where
    query :: Query
    query =
      "INSERT INTO deck_ratings (deck_id, user_id, rating) VALUES (?, ?, ?) \
      \ON CONFLICT (deck_id, user_id) DO UPDATE \
      \SET rating = EXCLUDED.rating, last_modified = CURRENT_TIMESTAMP"
    invalidRating :: AppError
    invalidRating = Unauthorized "Invalid rating value."
