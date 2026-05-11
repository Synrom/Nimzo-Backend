{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.User where

import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics
import Data.Time (UTCTime)

import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data User = User
  { username :: String,
    password :: T.Text,
    salt :: T.Text,
    premium :: Bool,
    elo :: Maybe Integer,
    xp :: Integer,
    streak :: Integer,
    last_activity :: UTCTime, 
    rank :: Integer,
    email :: String,
    verified :: Bool
  }
  deriving (Eq, Show, Generic)

data UserID = UID
  { email :: String,
    username :: String,
    premium :: Bool
  }
  deriving (Eq, Show, Generic)

newtype UserEmail = UEmail
  { email :: String }
  deriving (Eq, Show, Generic)


data PublicUser = PublicUser
  { username :: String,
    premium :: Bool,
    xp :: Integer,
    streak :: Integer,
    rank :: Integer,
    email :: String,
    verified :: Bool
  }
  deriving (Eq, Show, Generic)

newtype NewPassword = NewPwd
  { pwd :: T.Text }
  deriving (Eq, Show, Generic)

data UserXP = UserXP
  { username :: String,
    xp :: Integer,
    rank :: Integer
  }
  deriving (Eq, Show, Generic)


instance FromRow UserID
instance ToJSON NewPassword
instance FromJSON NewPassword
instance ToJSON UserEmail
instance FromJSON UserEmail
instance ToJSON User
instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field
      <*> field
      <*> field
      <*> pure Nothing
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
instance ToRow User
instance ToJSON UserXP
instance ToRow UserXP
instance FromRow UserXP
instance ToJSON PublicUser
