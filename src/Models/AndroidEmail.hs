{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.AndroidEmail where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics

androidEmailMaxLength :: Int
androidEmailMaxLength = 250

newtype AndroidEmailPayload = AndroidEmailPayload
  { email :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON AndroidEmailPayload
instance FromJSON AndroidEmailPayload

data AndroidEmail = AndroidEmail
  { id :: Int,
    email :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON AndroidEmail
instance FromJSON AndroidEmail
instance FromRow AndroidEmail
