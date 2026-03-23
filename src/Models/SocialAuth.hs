{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Models.SocialAuth where

import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import GHC.Generics

data SocialAuthRequest = SocialAuthRequest
  { idToken :: String,
    username :: Maybe String
  }
  deriving (Eq, Show, Generic)

data SocialProvider = Google | Apple
  deriving (Eq, Show)

data SocialProfile = SocialProfile
  { providerSubject :: String,
    email :: Maybe String,
    emailVerified :: Bool
  }
  deriving (Eq, Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "idToken" -> "id_token"
      "emailVerified" -> "email_verified"
      "providerSubject" -> "provider_subject"
      other -> other
  }

instance ToJSON SocialAuthRequest where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON SocialAuthRequest where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON SocialProfile where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON SocialProfile where
  parseJSON = genericParseJSON jsonOpts
