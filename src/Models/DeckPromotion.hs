{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.DeckPromotion where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data DeckPromotionRequest = DeckPromotionRequest
  { featuredSource :: Maybe String
  , featuredRank :: Maybe Integer
  , videoUrl :: Maybe String
  }
  deriving (Eq, Show, Generic)

data DeckPromotionResponse = DeckPromotionResponse
  { deckId :: Integer
  , featuredSource :: Maybe String
  , featuredRank :: Maybe Integer
  , videoUrl :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON DeckPromotionRequest
instance ToJSON DeckPromotionRequest
instance FromJSON DeckPromotionResponse
instance ToJSON DeckPromotionResponse
