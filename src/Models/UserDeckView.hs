{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.UserDeckView where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data UserDeckView = UserDeckView
  { numCardsToday :: Integer,
    cardsPerDay :: Integer,
    numCardsLearnt :: Integer,
    isAuthor :: Bool,
    userId :: String,
    deckId :: Integer,
    udvId :: String,
    name :: String,
    isPublic :: Bool,
    description :: String,
    numCardsTotal :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserDeckView
instance FromJSON UserDeckView
instance FromRow UserDeckView