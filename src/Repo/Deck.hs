module Repo.Deck
  ( insertOrUpdate,
    search,
    searchInstant,
    listFeatured,
    find,
    findWithRating,
    alreadyExists,
    listContinuations,
    searchContinuations,
    listCardsOfDeck,
    saveRating,
    saveDeckImage,
    savePromotion,
    savePromotionByModerator,
  )
where

import Models.Card (CardQuery, PagedCards)
import Models.Deck (Deck)
import Models.DeckDetails (DeckDetails)
import Models.DeckImage (DeckImageUploadRequest, DeckImageUploadResponse)
import Models.DeckPromotion (DeckPromotionRequest, DeckPromotionResponse)
import Models.DeckSearch (DeckSearchResult, SearchContinuationsResponse)
import Models.Watermelon (JsonableMsg)
import Repo.Classes (MonadDB)
import qualified Repo.Deck.Media as Media
import qualified Repo.Deck.Promotion as Promotion
import qualified Repo.Deck.Query as Query

insertOrUpdate :: MonadDB m => Deck -> m Deck
insertOrUpdate = Query.insertOrUpdate

search :: MonadDB m => Maybe String -> m [DeckSearchResult]
search = Query.search

searchInstant :: MonadDB m => Maybe String -> m [DeckSearchResult]
searchInstant = Query.searchInstant

listFeatured :: MonadDB m => Maybe String -> Maybe Integer -> m [DeckSearchResult]
listFeatured = Promotion.listFeatured

find :: MonadDB m => Integer -> m Deck
find = Query.find

findWithRating :: MonadDB m => Maybe String -> Integer -> m DeckDetails
findWithRating = Query.findWithRating

alreadyExists :: MonadDB m => Deck -> m Bool
alreadyExists = Query.alreadyExists

listContinuations :: MonadDB m => String -> String -> m [String]
listContinuations = Query.listContinuations

searchContinuations :: MonadDB m => String -> Maybe String -> Maybe Integer -> Maybe Integer -> m SearchContinuationsResponse
searchContinuations = Query.searchContinuations

listCardsOfDeck :: MonadDB m => CardQuery -> m PagedCards
listCardsOfDeck = Query.listCardsOfDeck

saveRating :: MonadDB m => String -> Integer -> Integer -> m JsonableMsg
saveRating = Query.saveRating

saveDeckImage :: MonadDB m => String -> Integer -> DeckImageUploadRequest -> m DeckImageUploadResponse
saveDeckImage = Media.saveDeckImage

savePromotion :: MonadDB m => String -> Integer -> DeckPromotionRequest -> m DeckPromotionResponse
savePromotion = Promotion.savePromotion

savePromotionByModerator :: MonadDB m => String -> Integer -> DeckPromotionRequest -> m DeckPromotionResponse
savePromotionByModerator = Promotion.savePromotionByModerator
