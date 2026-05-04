{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Card where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Data.Time (UTCTime)

data CardQuery = CardQuery
  { cursor :: Maybe String,
    limit  :: Integer,
    deckId :: Integer,
    prefix :: Maybe String
  }
  deriving (Eq, Show, Generic)

data Card  = Card
  { moves :: String,
    title :: String,
    color :: String,
    cardId :: String,
    isFeatured :: Bool
  }
  deriving (Eq, Show, Generic)

data PendingCard = PendingCard
  { moves :: String,
    title :: String,
    color :: String,
    id    :: String,
    isFeatured :: Bool
  }
  deriving (Eq, Show, Generic)

data PagedCards = PagedCards
  { next_cursor :: Maybe String,
    cards       :: [Card]
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardQuery
instance ToJSON Card
instance ToJSON PagedCards
instance FromRow Card
instance FromRow PendingCard
