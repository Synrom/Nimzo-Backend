{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.Card where

import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Data.String (IsString(..))

import Database.PostgreSQL.Simple (ToRow, FromRow, query, execute, Query, Connection, Only(Only))

data Card = Card
  { color :: String,
    moves :: String,
    title :: String,
    deckId :: Integer,
    cardId :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON Card
instance FromJSON Card
instance FromRow Card
instance ToRow Card