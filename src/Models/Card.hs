{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Card where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, genericToEncoding, genericToJSON, withObject, (.:), (.:?))
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data DeckContentQuery = DeckContentQuery
  { cursor :: Maybe String,
    limit  :: Integer,
    deckId :: Integer,
    prefix :: Maybe String,
    schemaVersion :: Maybe Integer,
    startingFen :: Maybe String,
    isDownload :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data Card  = Card
  { moves :: String,
    title :: String,
    color :: String,
    fen :: Maybe String,
    likelihood :: Maybe Double
  }
  deriving (Eq, Show, Generic)

data PendingCard = PendingCard
  { moves :: String,
    title :: String,
    color :: String,
    fen :: Maybe String,
    likelihood :: Maybe Double,
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
      <*> obj .:? "fen"
      <*> obj .:? "isDownload"

cardJsonOpts :: Options
cardJsonOpts = defaultOptions { omitNothingFields = True }

instance ToJSON Card where
  toJSON = genericToJSON cardJsonOpts
  toEncoding = genericToEncoding cardJsonOpts

instance ToJSON PagedCards
instance FromRow Card
instance FromRow PendingCard
