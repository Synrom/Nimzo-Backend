{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Models.Deck where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import GHC.Generics
import Database.PostgreSQL.Simple (ToRow, FromRow)

data Deck = Deck
  { deckId :: Integer,
    name :: String,
    isPublic :: Bool,
    description :: String,
    numCardsTotal :: Integer,
    author :: String,
    user_deck_id :: String
  }
  deriving (Eq, Show, Generic)

data SearchContinuationDeck = SearchContinuationDeck
  { deck_name :: String,
    nr_cards :: Integer
  }
  deriving (Eq, Show, Generic)

data SearchContinuation = SearchContinuation
  { move :: String,
    move_nr_cards :: Integer
  }
  deriving (Eq, Show, Generic)

data SearchContinuationsResponse = SearchContinuationsResponse
  { continuations :: [SearchContinuation],
    decks :: [SearchContinuationDeck]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Deck
instance FromJSON Deck

searchContinuationDeckJsonOpts :: Options
searchContinuationDeckJsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "deck_name" -> "name"
      other       -> other
  }

searchContinuationJsonOpts :: Options
searchContinuationJsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "move_nr_cards" -> "nr_cards"
      other           -> other
  }

instance ToJSON SearchContinuationDeck where
  toJSON = genericToJSON searchContinuationDeckJsonOpts
  toEncoding = genericToEncoding searchContinuationDeckJsonOpts

instance FromJSON SearchContinuationDeck where
  parseJSON = genericParseJSON searchContinuationDeckJsonOpts

instance ToJSON SearchContinuation where
  toJSON = genericToJSON searchContinuationJsonOpts
  toEncoding = genericToEncoding searchContinuationJsonOpts

instance FromJSON SearchContinuation where
  parseJSON = genericParseJSON searchContinuationJsonOpts
instance ToJSON SearchContinuationsResponse
instance FromJSON SearchContinuationsResponse
instance FromRow Deck
instance ToRow Deck
instance FromRow SearchContinuationDeck
instance FromRow SearchContinuation
