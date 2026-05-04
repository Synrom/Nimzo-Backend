{-# LANGUAGE DeriveGeneric #-}

module Models.FeaturedSource
  ( FeaturedSource (..),
    parseFeaturedSource,
    featuredSourceToString,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data FeaturedSource
  = FeaturedSourceTikTok
  | FeaturedSourceInstagram
  | FeaturedSourceYouTube
  | FeaturedSourceX
  deriving (Eq, Show, Generic)

featuredSourceToString :: FeaturedSource -> String
featuredSourceToString FeaturedSourceTikTok = "tiktok"
featuredSourceToString FeaturedSourceInstagram = "instagram"
featuredSourceToString FeaturedSourceYouTube = "youtube"
featuredSourceToString FeaturedSourceX = "x"

parseFeaturedSource :: String -> Maybe FeaturedSource
parseFeaturedSource raw = case map toLower raw of
  "tiktok" -> Just FeaturedSourceTikTok
  "instagram" -> Just FeaturedSourceInstagram
  "youtube" -> Just FeaturedSourceYouTube
  "x" -> Just FeaturedSourceX
  _ -> Nothing

instance ToJSON FeaturedSource where
  toJSON = Aeson.String . T.pack . featuredSourceToString

instance FromJSON FeaturedSource where
  parseJSON = withText "FeaturedSource" $ \t ->
    case parseFeaturedSource (T.unpack (T.toLower t)) of
      Just source -> pure source
      Nothing -> fail "Invalid featured source."
