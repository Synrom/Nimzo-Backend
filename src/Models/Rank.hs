{-# LANGUAGE DeriveGeneric #-}

module Models.Rank where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Database.PostgreSQL.Simple (ToRow, FromRow, Connection, Query, query)
import GHC.Generics

data Direction = Up | Down | Both
  deriving (Eq, Show, Generic)

data RankQuery = RankQuery
  { rank :: Integer
  , limit :: Integer
  , direction :: Direction
  }
  deriving (Eq, Show, Generic)

instance FromJSON Direction
instance FromJSON RankQuery