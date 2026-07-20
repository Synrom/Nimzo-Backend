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
import Models.Card (Card (..), DeckContentQuery (..), PagedCards (..), PendingCard (..))
import Models.Deck (Deck (..))
import Models.DeckDetails (DeckDetails (..))
import Models.DeckSearch (DeckSearchResult, SearchContinuation (..), SearchContinuationsResponse (..))
import Models.Watermelon (JsonableMsg (Msg))

returnFields :: Query
returnFields = " id, name, is_public, description, color, num_cards_total, author, user_deck_id, image_url, featured_source, featured_card_id, featured_rank, video_url "

searchReturnFields :: Query
searchReturnFields =
  " d.id, d.name, d.is_public, d.description, d.color \
  \, d.num_cards_total \
  \, d.author, d.user_deck_id \
  \, d.image_url \
  \, d.featured_source \
  \, fc.moves \
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
      \LEFT JOIN user_card_views fc ON fc.id = d.featured_card_id \
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
      \LEFT JOIN user_card_views fc ON fc.id = d.featured_card_id \
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
      "SELECT d.id, d.name, d.is_public, d.description, d.color, d.num_cards_total, d.author, d.user_deck_id, d.image_url, d.featured_source, d.featured_card_id, d.featured_rank, d.video_url \
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
    unpend (PendingCard moves title color fen _) = Card moves title color fen
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
  ( baseSql <> colorSql <> prefixSql <> groupSql <> outerSql <> orderSql <> limitSql,
    colorParams <> prefixParams <> limitParams
  )
  where
    baseSql =
      "SELECT" <> searchReturnFields <> ", deck_counts.nr_cards " <>
      "FROM ( \
      \  SELECT d.id AS deck_id, COUNT(*) AS nr_cards \
      \  FROM user_card_views ucv \
      \  JOIN decks d ON d.user_deck_id = ucv.user_deck_id \
      \  WHERE d.is_public = TRUE \
      \    AND ucv.fen IS NULL"
    (colorSql, colorParams) = buildColorFilter mColor
    (prefixSql, prefixParams)
      | null prefix = ("", [])
      | otherwise =
          ( " AND (ucv.moves = ? OR ucv.moves LIKE ? || ' %')",
            [toField prefix, toField prefix]
          )
    groupSql = " GROUP BY d.id"
    outerSql =
      ") deck_counts \
      \JOIN decks d ON d.id = deck_counts.deck_id \
      \LEFT JOIN user_card_views fc ON fc.id = d.featured_card_id"
    orderSql =
      " ORDER BY deck_counts.nr_cards DESC, d.name"
    limitParams = maybe [] (\limit' -> [toField limit']) (normalizeLimit mLimit)
    limitSql
      | null limitParams = ""
      | otherwise = " LIMIT ?"

normalizeLimit :: Maybe Integer -> Maybe Integer
normalizeLimit = fmap (max 0)

maxDeckContentLimit :: Integer
maxDeckContentLimit = 100

clampDeckContentQuery :: DeckContentQuery -> DeckContentQuery
clampDeckContentQuery query = query {limit = min maxDeckContentLimit query.limit}

buildDeckContentPageQuery
  :: DeckContentQuery
  -> String
  -> Query
  -> (String -> (Query, [Action]))
  -> (Query, [Action])
buildDeckContentPageQuery rawQuery userDeckId baseSql buildPrefixCondition =
  (baseSql <> cursorCondition <> prefixCondition <> " ORDER BY id LIMIT ?", finalParams)
  where
    pageQuery = clampDeckContentQuery rawQuery
    finalParams = [toField userDeckId] <> cursorParams <> prefixParams <> [toField pageQuery.limit]
    (cursorCondition, cursorParams) = case rawQuery.cursor of
      Nothing -> ("", [])
      Just next -> (" AND id > ?", [toField next])
    (prefixCondition, prefixParams) = case rawQuery.prefix of
      Nothing -> ("", [])
      Just p -> buildPrefixCondition p

movesPrefixCondition :: Maybe String -> String -> (Query, [Action])
movesPrefixCondition rawStartingFen prefix =
  ( " AND ((moves = ? OR moves LIKE ? || ' %') OR " <> differentFenSql <> ")",
    [toField prefix, toField prefix] <> differentFenParams
  )
  where
    startingFen = normalizeStartingFen rawStartingFen
    positionMatchSql = "(fen = ? OR fen LIKE ? || ' %')"
    positionMatchParams fen = [toField fen, toField fen]
    customFenSql = "(fen IS NOT NULL AND fen <> '' AND NOT " <> positionMatchSql <> ")"
    (differentFenSql, differentFenParams) = case startingFen of
      Nothing -> (customFenSql, positionMatchParams startingPositionKey)
      Just fen -> ("(" <> customFenSql <> " AND NOT " <> positionMatchSql <> ")", positionMatchParams startingPositionKey <> positionMatchParams fen)

startingPositionKey :: String
startingPositionKey = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"

fenPositionKey :: String -> String
fenPositionKey = unwords . take 4 . words

normalizeStartingFen :: Maybe String -> Maybe String
normalizeStartingFen (Just fen)
  | positionKey == startingPositionKey = Nothing
  | otherwise = Just positionKey
  where
    positionKey = fenPositionKey fen
normalizeStartingFen Nothing = Nothing

listContinuations :: MonadDB m => String -> Maybe String -> String -> m [String]
listContinuations userDeckId rawStartingFen prefix =
  map fromOnly <$> runQuery sql params
  where
    startingFen = normalizeStartingFen rawStartingFen
    positionMatchSql = "(fen = ? OR fen LIKE ? || ' %')"
    positionMatchParams fen = [toField fen, toField fen]
    (fenSql, fenParams) = case startingFen of
      Nothing -> ("(fen IS NULL OR fen = '' OR " <> positionMatchSql <> ")", positionMatchParams startingPositionKey)
      Just fen -> (positionMatchSql, positionMatchParams fen)
    (sql, params) =
      buildContinuationQuery
        ("FROM user_card_views WHERE user_deck_id = ? AND " <> fenSql)
        "moves"
        ([toField userDeckId] <> fenParams)
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
        ("FROM user_card_views ucv JOIN decks d ON d.user_deck_id = ucv.user_deck_id WHERE d.is_public = TRUE AND ucv.fen IS NULL" <> colorSql)
        "ucv.moves"
        colorParams
        prefix
        mContinuationLimit
    (decksSql, decksParams) = buildDecksQuery prefix mColor mDeckLimit

listCardsOfDeck :: MonadDB m => Bool -> DeckContentQuery -> m PagedCards
listCardsOfDeck includeFenCards rawQuery = do
  let pageQuery = clampDeckContentQuery rawQuery
  deck <- find pageQuery.deckId
  if shouldIncrementDownloadCount
    then do
      _ <- execute "UPDATE decks SET download_count = download_count + 1 WHERE id = ?" (Only pageQuery.deckId)
      pure ()
    else pure ()
  let (sql, params) = buildDeckContentPageQuery rawQuery deck.user_deck_id baseSql (movesPrefixCondition rawQuery.startingFen)
  paginateCards <$> runQuery sql params
  where
    -- Released clients before isDownload was added always used a full 100-card
    -- root page for downloads; their deck browser uses a 10-card page. Keep
    -- that request shape working while allowing newer clients to be explicit.
    isLegacyDownload = rawQuery.isDownload == Nothing && rawQuery.limit >= maxDeckContentLimit
    shouldIncrementDownloadCount =
      rawQuery.cursor == Nothing
        && rawQuery.prefix == Nothing
        && (rawQuery.isDownload == Just True || isLegacyDownload)
    fields = " moves, title, color, fen, id "
    fenFilter
      | includeFenCards = ""
      | otherwise = " AND fen IS NULL"
    baseSql = "SELECT" <> fields <> "FROM user_card_views WHERE user_deck_id=?" <> fenFilter

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
