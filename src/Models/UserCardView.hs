{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.UserCardView where

import Data.Aeson (FromJSON (..), ToJSON (..), Options(..), defaultOptions, genericToJSON, genericToEncoding, genericParseJSON)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)
import Data.Time (UTCTime)

data UserCardView = UserCardView
  { numCorrectTrials :: Integer,
    nextRequest :: Integer,
    userId :: String,
    userDeckId :: String,
    ucvId :: String,
    moves :: String,
    title :: String,
    color :: String
  }
  deriving (Eq, Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "numCorrectTrials" -> "num_correct_trials"
      "nextRequest"      -> "next_request_at"
      "userId"           -> "user_id"
      "userDeckId"       -> "user_deck_id"
      "ucvId"            -> "id"
      other              -> other
  }

instance ToJSON   UserCardView where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON UserCardView where
  parseJSON  = genericParseJSON jsonOpts

instance FromRow UserCardView