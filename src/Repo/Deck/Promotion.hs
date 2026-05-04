{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Deck.Promotion
  ( listFeatured,
    savePromotion,
    savePromotionByModerator,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.List (elem)
import Database.PostgreSQL.Simple (Only(..))
import App.Env (deckPromotionModerators)
import App.Error (AppError (Unauthorized))
import qualified Models.Deck as DeckModel
import Models.FeaturedSource (featuredSourceToString)
import Models.DeckPromotion (DeckPromotionRequest (..), DeckPromotionResponse (..))
import Models.DeckSearch (DeckSearchResult)
import Repo.Classes
import qualified Repo.Deck.Query as DeckQuery
import Repo.Deck.Validation
  ( normalizeFeaturedLimit,
    normalizeUsername,
    resolveFeaturedSource,
    validateFeaturedCardIdRequired,
    validateFeaturedRank,
    validateFeaturedSourceStrict,
    validateVideoUrl,
  )
import Repo.Utils (ensure)

listFeatured :: MonadDB m => Maybe String -> Maybe Integer -> m [DeckSearchResult]
listFeatured maybeSource maybeLimit = do
  source <- fromEither $ resolveFeaturedSource maybeSource
  let finalLimit = normalizeFeaturedLimit maybeLimit
  runQuery query (featuredSourceToString source, finalLimit)
  where
    query =
      "SELECT" <> DeckQuery.searchReturnFields <>
      "FROM decks d \
      \WHERE d.is_public = TRUE \
      \AND d.featured_source = ? \
      \ORDER BY d.created_at DESC, COALESCE(d.featured_rank, 2147483647) ASC, d.download_count DESC, d.rating_avg DESC NULLS LAST, d.name ASC \
      \LIMIT ?"

savePromotion :: MonadDB m => String -> Integer -> DeckPromotionRequest -> m DeckPromotionResponse
savePromotion username deckId payload = do
  deck <- DeckQuery.find deckId
  ensure notAuthor (DeckModel.author deck == username)
  upsertPromotion deckId payload
  where
    notAuthor :: AppError
    notAuthor = Unauthorized "Only the deck author can set deck promotion metadata."

savePromotionByModerator :: MonadDB m => String -> Integer -> DeckPromotionRequest -> m DeckPromotionResponse
savePromotionByModerator username deckId payload = do
  env <- askEnv
  let moderators = map normalizeUsername env.deckPromotionModerators
  let who = normalizeUsername username
  ensure notModerator (who `elem` moderators)
  _ <- DeckQuery.find deckId
  upsertPromotion deckId payload
  where
    notModerator :: AppError
    notModerator = Unauthorized "No access. Moderator role required."

upsertPromotion :: MonadDB m => Integer -> DeckPromotionRequest -> m DeckPromotionResponse
upsertPromotion deckId payload = do
  deck <- DeckQuery.find deckId
  normalizedSource <- fromEither $ validateFeaturedSourceStrict payload.featuredSource
  normalizedFeaturedCardId <- case normalizedSource of
    Nothing -> pure Nothing
    Just _ -> Just <$> fromEither (validateFeaturedCardIdRequired payload.featuredCardId)
  normalizedRank <- fromEither $ validateFeaturedRank payload.featuredRank
  normalizedVideo <- fromEither $ validateVideoUrl payload.videoUrl
  case normalizedSource of
    Nothing -> withTransaction $ do
      _ <- execute
        "UPDATE decks \
        \SET featured_source = NULL, featured_card_id = NULL, featured_rank = ?, video_url = ?, last_modified = CURRENT_TIMESTAMP \
        \WHERE id = ?"
        (normalizedRank, normalizedVideo, deckId)
      pure ()
    Just source -> withTransaction $ do
      let featuredCardId = case normalizedFeaturedCardId of
            Just cardId -> cardId
            Nothing -> ""
      valid <- cardBelongsToDeck deck.user_deck_id featuredCardId
      ensure (Unauthorized "featuredCardId must belong to the deck.") valid
      _ <- execute
        "UPDATE decks \
        \SET featured_source = ?, featured_card_id = ?, featured_rank = ?, video_url = ?, last_modified = CURRENT_TIMESTAMP \
        \WHERE id = ?"
        (Just (featuredSourceToString source), featuredCardId, normalizedRank, normalizedVideo, deckId)
      pure ()
  pure $ DeckPromotionResponse deckId (featuredSourceToString <$> normalizedSource) normalizedFeaturedCardId normalizedRank normalizedVideo

cardBelongsToDeck :: MonadDB m => String -> String -> m Bool
cardBelongsToDeck userDeckId cardId = do
  result <- runQuery
    "SELECT EXISTS (SELECT 1 FROM user_card_views WHERE id = ? AND user_deck_id = ?)"
    (cardId, userDeckId)
  pure $ case result of
    [Only exists] -> exists
    _ -> False

fromEither :: MonadError e m => Either e a -> m a
fromEither = either throwError pure
