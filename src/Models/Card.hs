{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Card where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Database.PostgreSQL.Simple (ToRow, FromRow)
import Data.Time (UTCTime)

data DeckContentQuery = DeckContentQuery
  { cursor :: Maybe String,
    limit  :: Integer,
    deckId :: Integer,
    prefix :: Maybe String
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

instance FromJSON DeckContentQuery
instance ToJSON Card
instance ToJSON PagedCards
instance FromRow Card
instance FromRow PendingCard
