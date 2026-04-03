{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.UserDeckView where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericToJSON, genericToEncoding, withObject, (.:), (.:?), (.!=))
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data UserDeckView = UserDeckView
  { numCardsToday :: Integer,
    newCardsToday :: Integer,
    lastStudyDate :: String,
    cardsPerDay :: Integer,
    numCardsLearnt :: Integer,
    isAuthor :: Bool,
    userId :: String,
    udvId :: String,
    name :: String,
    isPublic :: Bool,
    description :: String,
    color :: Maybe String,
    numCardsTotal :: Integer
  }
  deriving (Eq, Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "numCardsToday"  -> "num_cards_today"
      "newCardsToday"  -> "new_cards_today"
      "lastStudyDate"  -> "last_study_date"
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
  parseJSON = withObject "UserDeckView" $ \obj -> do
    numCardsToday <- obj .: "num_cards_today"
    newCardsToday <- obj .:? "new_cards_today" .!= numCardsToday
    lastStudyDate <- obj .:? "last_study_date" .!= ""
    cardsPerDay <- obj .: "cards_per_day"
    numCardsLearnt <- obj .: "num_cards_learnt"
    isAuthor <- obj .: "is_author"
    userId <- obj .: "user_id"
    udvId <- obj .: "id"
    name <- obj .: "name"
    isPublic <- obj .: "is_public"
    description <- obj .: "description"
    color <- obj .:? "color"
    numCardsTotal <- obj .: "num_cards_total"
    pure $ UserDeckView
      { numCardsToday = numCardsToday
      , newCardsToday = newCardsToday
      , lastStudyDate = lastStudyDate
      , cardsPerDay = cardsPerDay
      , numCardsLearnt = numCardsLearnt
      , isAuthor = isAuthor
      , userId = userId
      , udvId = udvId
      , name = name
      , isPublic = isPublic
      , description = description
      , color = color
      , numCardsTotal = numCardsTotal
      }

instance FromRow UserDeckView
