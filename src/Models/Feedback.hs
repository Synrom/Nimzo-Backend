{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Feedback where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

feedbackTextMaxLength :: Int
feedbackTextMaxLength = 5000

data FeedbackPayload = FeedbackPayload
  { text :: String,
    stars :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON FeedbackPayload
instance FromJSON FeedbackPayload

data Feedback = Feedback
  { id :: Integer,
    username :: String,
    text :: String,
    stars :: Integer,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Feedback
instance FromJSON Feedback
instance FromRow Feedback
