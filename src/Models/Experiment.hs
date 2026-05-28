{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Experiment where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

experimentKeyMaxLength :: Int
experimentKeyMaxLength = 100

variantKeyMaxLength :: Int
variantKeyMaxLength = 100

eventNameMaxLength :: Int
eventNameMaxLength = 100

appVersionMaxLength :: Int
appVersionMaxLength = 50

platformMaxLength :: Int
platformMaxLength = 20

data SupportedExperiment = SupportedExperiment
  { experiment_key :: String,
    supported_variants :: [String]
  }
  deriving (Eq, Show, Generic)

instance ToJSON SupportedExperiment
instance FromJSON SupportedExperiment

data ExperimentBootstrapPayload = ExperimentBootstrapPayload
  { onboarding_session_id :: String,
    app_version :: Maybe String,
    platform :: Maybe String,
    supported_experiments :: [SupportedExperiment]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ExperimentBootstrapPayload
instance FromJSON ExperimentBootstrapPayload

data ExperimentAssignment = ExperimentAssignment
  { experiment_key :: String,
    variant_key :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ExperimentAssignment
instance FromJSON ExperimentAssignment
instance FromRow ExperimentAssignment

newtype ExperimentBootstrapResponse = ExperimentBootstrapResponse
  { assignments :: [ExperimentAssignment]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ExperimentBootstrapResponse
instance FromJSON ExperimentBootstrapResponse

data ExperimentEventPayload = ExperimentEventPayload
  { onboarding_session_id :: String,
    experiment_key :: String,
    variant_key :: String,
    event_name :: String,
    app_version :: Maybe String,
    platform :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ExperimentEventPayload
instance FromJSON ExperimentEventPayload
