{-# LANGUAGE DeriveGeneric #-}

module Models.DeckRating where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data DeckRatingRequest = DeckRatingRequest
  { rating :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON DeckRatingRequest
instance FromJSON DeckRatingRequest
