{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.DeckDetails where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data DeckDetails = DeckDetails
  { deckId :: Integer,
    name :: String,
    isPublic :: Bool,
    description :: String,
    color :: Maybe String,
    numCardsTotal :: Integer,
    author :: String,
    user_deck_id :: String,
    hasRated :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON DeckDetails
instance FromJSON DeckDetails
instance FromRow DeckDetails
