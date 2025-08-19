{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.User where

import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics

import Database.PostgreSQL.Simple (ToRow, FromRow, Connection, Query, query)

data User = User
  { username :: String,
    password :: T.Text,
    salt :: T.Text,
    premium :: Bool,
    xp :: Integer,
    email :: String,
    verified :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
instance FromRow User
instance ToRow User