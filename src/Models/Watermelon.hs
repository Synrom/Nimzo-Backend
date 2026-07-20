{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Watermelon where

import GHC.Generics
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?), (.!=))
import Database.PostgreSQL.Simple (FromRow)
import Data.Time
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
instance FromJSON Changes where
  parseJSON = withObject "Changes" $ \obj -> do
    user_card_views <- obj .: "user_card_views"
    user_deck_views <- obj .: "user_deck_views"
    pure $ Changes
      { user_card_views = user_card_views
      , user_deck_views = user_deck_views
      }

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
  schemaVersion :: Integer,
  changes :: Changes
} deriving (Eq, Show, Generic)

instance FromJSON PushParams where
  parseJSON = withObject "PushParams" $ \obj -> do
    lastPulledAt <- obj .: "lastPulledAt"
    -- Pushes predate an explicit schema version. Treat those payloads as v4,
    -- the last schema before likelihood, so they remain valid unchanged.
    schemaVersion <- obj .:? "schemaVersion" .!= 4
    changes <- obj .: "changes"
    pure $ PushParams
      { lastPulledAt = lastPulledAt
      , schemaVersion = schemaVersion
      , changes = changes
      }

data Success = Success {
  xp :: Integer,
  streak :: Integer,
  msg :: String
} deriving (Eq, Show, Generic)

newtype JsonableMsg = Msg {
  msg :: String
} deriving (Eq, Show, Generic)

instance ToJSON JsonableMsg
instance FromJSON JsonableMsg

newtype DatabaseTime = DatabaseTime {
  time :: UTCTime
} deriving (Eq, Show, Generic)

instance FromRow DatabaseTime

instance ToJSON Success
