{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Card where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import GHC.Generics
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Data.Time (UTCTime)

data DeckContentQuery = DeckContentQuery
  { cursor :: Maybe String,
    limit  :: Integer,
    deckId :: Integer,
    prefix :: Maybe String,
    schemaVersion :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

data Card  = Card
  { moves :: String,
    title :: String,
    color :: String
  }
  deriving (Eq, Show, Generic)

data PendingCard = PendingCard
  { moves :: String,
    title :: String,
    color :: String,
    id    :: String
  }
  deriving (Eq, Show, Generic)

data PagedCards = PagedCards
  { next_cursor :: Maybe String,
    cards       :: [Card]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DeckContentQuery where
  parseJSON = withObject "DeckContentQuery" $ \obj -> do
    DeckContentQuery
      <$> obj .:? "cursor"
      <*> obj .: "limit"
      <*> obj .: "deckId"
      <*> obj .:? "prefix"
      <*> obj .:? "schemaVersion"
instance ToJSON Card
instance ToJSON PagedCards
instance FromRow Card
instance FromRow PendingCard
