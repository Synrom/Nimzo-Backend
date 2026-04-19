{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Onboarding where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

sessionIdMaxLength :: Int
sessionIdMaxLength = 128

lastStepMaxLength :: Int
lastStepMaxLength = 100

onboardingShortFieldMaxLength :: Int
onboardingShortFieldMaxLength = 50

motivationMaxLength :: Int
motivationMaxLength = 250

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

data AnonymousOnboardingProgressPayload = AnonymousOnboardingProgressPayload
  { onboarding_session_id :: String,
    last_step :: String,
    stopped :: Bool,
    chess_level :: Maybe String,
    elo :: Maybe String,
    organization :: Maybe String,
    motivation :: Maybe String,
    study_goal :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance ToJSON AnonymousOnboardingProgressPayload
instance FromJSON AnonymousOnboardingProgressPayload

data AnonymousOnboardingProgress = AnonymousOnboardingProgress
  { onboarding_session_id :: String,
    last_step :: String,
    stopped :: Bool,
    chess_level :: Maybe String,
    elo :: Maybe String,
    organization :: Maybe String,
    motivation :: Maybe String,
    study_goal :: Maybe String,
    claimed_by_user :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance ToJSON AnonymousOnboardingProgress
instance FromJSON AnonymousOnboardingProgress
instance FromRow AnonymousOnboardingProgress

newtype ClaimAnonymousOnboardingPayload = ClaimAnonymousOnboardingPayload
  { onboarding_session_id :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ClaimAnonymousOnboardingPayload
instance FromJSON ClaimAnonymousOnboardingPayload
