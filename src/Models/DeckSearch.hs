{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Models.DeckSearch where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)
import Models.Deck (Deck)

data SearchContinuation = SearchContinuation
  { move :: String,
    move_nr_cards :: Integer
  }
  deriving (Eq, Show, Generic)

data SearchContinuationsResponse = SearchContinuationsResponse
  { continuations :: [SearchContinuation],
    decks :: [Deck]
  }
  deriving (Eq, Show, Generic)

searchContinuationJsonOpts :: Options
searchContinuationJsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "move_nr_cards" -> "nr_cards"
      other           -> other
  }

instance ToJSON SearchContinuation where
  toJSON = genericToJSON searchContinuationJsonOpts
  toEncoding = genericToEncoding searchContinuationJsonOpts

instance FromJSON SearchContinuation where
  parseJSON = genericParseJSON searchContinuationJsonOpts

instance ToJSON SearchContinuationsResponse
instance FromJSON SearchContinuationsResponse
instance FromRow SearchContinuation
