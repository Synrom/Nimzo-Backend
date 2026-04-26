{-# LANGUAGE DeriveGeneric #-}

module Models.DeckImage where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data DeckImageUploadRequest = DeckImageUploadRequest
  { mimeType :: String
  , base64Data :: String
  }
  deriving (Eq, Show, Generic)

data DeckImageUploadResponse = DeckImageUploadResponse
  { imageUrl :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON DeckImageUploadRequest
instance ToJSON DeckImageUploadRequest
instance FromJSON DeckImageUploadResponse
instance ToJSON DeckImageUploadResponse
