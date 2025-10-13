{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Watermelon where

import GHC.Generics
import Data.Text
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Data.Proxy
import Data.Int (Int64)
import Control.Monad (filterM)
import Servant (Handler)
import Control.Exception (try)
import Data.Time
import Data.Maybe
import Models.UserCardView (UserCardView(..))
import Models.UserDeckView (UserDeckView(..))


data TableChanges a = TableChanges {
  created :: [a],
  updated :: [a],
  deleted :: [String]
} deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (TableChanges a)
instance FromJSON a => FromJSON (TableChanges a)

data Changes = Changes {
  user_card_views :: TableChanges UserCardView,
  user_deck_views :: TableChanges UserDeckView
} deriving (Eq, Show, Generic)

instance ToJSON Changes
instance FromJSON Changes

data ChangesResponse = ChangesResponse {
  changes :: Changes,
  timestamp :: Integer
} deriving (Eq, Show, Generic)

instance ToJSON ChangesResponse
instance FromJSON ChangesResponse

data ColumnMigration = Column {
  table :: String,
  columns :: [String]
} deriving (Eq, Show, Generic)

instance FromJSON ColumnMigration

data Migration = Migration {
  from :: Integer,
  tables :: [String],
  columns :: [ColumnMigration]
} deriving (Eq, Show, Generic)

instance FromJSON Migration

data PullParams = PullParams {
  lastPulledAt :: Maybe Integer,
  schemaVersion :: Integer,
  migration :: Maybe Migration
} deriving (Eq, Show, Generic)

instance FromJSON PullParams

data PushParams = PushParams {
  lastPulledAt :: Integer,
  changes :: Changes
} deriving (Eq, Show, Generic)

instance FromJSON PushParams

data Success = Success {
  xp :: Integer,
  streak :: Integer,
  msg :: String
} deriving (Eq, Show, Generic)

instance ToJSON Success