{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Models.UserDeckView where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericToJSON, genericToEncoding, genericParseJSON)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data UserDeckView = UserDeckView
  { numCardsToday :: Integer,
    cardsPerDay :: Integer,
    numCardsLearnt :: Integer,
    isAuthor :: Bool,
    userId :: String,
    udvId :: String,
    name :: String,
    isPublic :: Bool,
    description :: String,
    numCardsTotal :: Integer
  }
  deriving (Eq, Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "numCardsToday"  -> "num_cards_today"
      "cardsPerDay"    -> "cards_per_day"
      "numCardsLearnt" -> "num_cards_learnt"
      "isAuthor"       -> "is_author"
      "userId"         -> "user_id"
      "udvId"          -> "id"
      "isPublic"       -> "is_public"
      "numCardsTotal"  -> "num_cards_total"
      other            -> other
  }

instance ToJSON   UserDeckView where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON UserDeckView where
  parseJSON  = genericParseJSON jsonOpts

instance FromRow UserDeckView