{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.UserIdentity where

import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data UserIdentity = UserIdentity
  { username :: String,
    provider :: String,
    providerSubject :: String,
    providerEmail :: Maybe String,
    emailVerified :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromRow UserIdentity
