{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.Deck where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Database.PostgreSQL.Simple (ToRow, FromRow)

data Deck = Deck
  { deckId :: Integer,
    name :: String,
    isPublic :: Bool,
    description :: String,
    numCardsTotal :: Integer,
    author :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Deck
instance FromJSON Deck
instance FromRow Deck
instance ToRow Deck