{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.StreakNotification where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data APNSEnvironment = Sandbox | Production deriving (Eq, Show)

instance ToJSON APNSEnvironment where
  toJSON Sandbox = String "sandbox"
  toJSON Production = String "production"

instance FromJSON APNSEnvironment where
  parseJSON = withText "APNs environment" $ \value -> case value of
    "sandbox" -> pure Sandbox
    "production" -> pure Production
    _ -> fail "apnsEnvironment must be sandbox or production"

environmentText :: APNSEnvironment -> Text
environmentText Sandbox = "sandbox"
environmentText Production = "production"

data UpsertIOSInstallationRequest = UpsertIOSInstallationRequest
  { platform :: Text
  , osVersion :: Text
  , appVersion :: Text
  , buildNumber :: Text
  , liveActivitiesEnabled :: Bool
  , supportsLocallyScheduledLiveActivities :: Maybe Bool
  , apnsEnvironment :: APNSEnvironment
  } deriving (Eq, Show, Generic)
instance FromJSON UpsertIOSInstallationRequest

data IOSInstallationResponse = IOSInstallationResponse
  { installationId :: Text
  , serverTime :: UTCTime
  } deriving (Eq, Show, Generic)
instance ToJSON IOSInstallationResponse

data PushToStartTokenRequest = PushToStartTokenRequest
  { token :: Text
  , attributesType :: Text
  , apnsEnvironment :: APNSEnvironment
  } deriving (Eq, Show, Generic)
instance FromJSON PushToStartTokenRequest

data CardPlayedRequest = CardPlayedRequest
  { eventId :: Text
  , installationId :: Text
  , playedAt :: UTCTime
  } deriving (Eq, Show, Generic)
instance FromJSON CardPlayedRequest

data Delivery = ActivityKit | LocalNotifications deriving (Eq, Show)
instance ToJSON Delivery where
  toJSON ActivityKit = String "activitykit"
  toJSON LocalNotifications = String "local-notifications"

data StreakScheduleResponse = StreakScheduleResponse
  { scheduleId :: Text
  , generation :: Integer
  , lastPlayedAt :: UTCTime
  , startsAt :: UTCTime
  , completesAt :: UTCTime
  , delivery :: Delivery
  , requiresLocalFallback :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON StreakScheduleResponse

data ActivityTokenRequest = ActivityTokenRequest
  { installationId :: Text
  , activityId :: Text
  , generation :: Integer
  , token :: Text
  , apnsEnvironment :: APNSEnvironment
  } deriving (Eq, Show, Generic)
instance FromJSON ActivityTokenRequest
