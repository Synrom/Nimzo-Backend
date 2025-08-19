{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.UserCardView where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)
import Data.Time (UTCTime)

data UserCardView = UserCardView
  { numCorrectTrials :: Integer,
    nextRequest :: UTCTime,
    cardId :: Integer,
    userId :: String,
    ucvId :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserCardView
instance FromJSON UserCardView
instance FromRow UserCardView