{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Onboarding where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

data OnboardingPreferencesPayload = OnboardingPreferencesPayload
  { chess_level :: String,
    elo :: String,
    organization :: String,
    motivation :: String,
    study_goal :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON OnboardingPreferencesPayload
instance FromJSON OnboardingPreferencesPayload

data OnboardingPreferences = OnboardingPreferences
  { user_id :: String,
    chess_level :: String,
    elo :: String,
    organization :: String,
    motivation :: String,
    study_goal :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON OnboardingPreferences
instance FromJSON OnboardingPreferences
instance FromRow OnboardingPreferences
