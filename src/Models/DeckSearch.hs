{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Models.DeckSearch where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import Models.Deck (Deck)

data SearchContinuation = SearchContinuation
  { move :: String,
    move_nr_cards :: Integer
  }
  deriving (Eq, Show, Generic)

data SearchDeck = SearchDeck
  { deck :: Deck,
    deck_nr_cards :: Integer
  }
  deriving (Eq, Show, Generic)

data SearchContinuationsResponse = SearchContinuationsResponse
  { continuations :: [SearchContinuation],
    decks :: [SearchDeck]
  }
  deriving (Eq, Show, Generic)

searchContinuationJsonOpts :: Options
searchContinuationJsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "move_nr_cards" -> "nr_cards"
      other           -> other
  }

searchDeckJsonOpts :: Options
searchDeckJsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "deck_nr_cards" -> "nr_cards"
      other           -> other
  }

instance ToJSON SearchContinuation where
  toJSON = genericToJSON searchContinuationJsonOpts
  toEncoding = genericToEncoding searchContinuationJsonOpts

instance FromJSON SearchContinuation where
  parseJSON = genericParseJSON searchContinuationJsonOpts

instance ToJSON SearchDeck where
  toJSON = genericToJSON searchDeckJsonOpts
  toEncoding = genericToEncoding searchDeckJsonOpts

instance FromJSON SearchDeck where
  parseJSON = genericParseJSON searchDeckJsonOpts

instance ToJSON SearchContinuationsResponse
instance FromJSON SearchContinuationsResponse
instance FromRow SearchContinuation

instance FromRow SearchDeck where
  fromRow = SearchDeck <$> fromRow <*> field
