{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.UserExplanationView where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Options (..)
  , Value (..)
  , defaultOptions
  , encode
  , genericToEncoding
  , genericToJSON
  , withObject
  , (.:)
  )
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

data Explanation = Explanation
  { fen :: String,
    move :: String,
    text :: String,
    visualizers :: String
  }
  deriving (Eq, Show, Generic)

data PendingExplanation = PendingExplanation
  { fen :: String,
    move :: String,
    text :: String,
    visualizers :: String,
    id :: String
  }
  deriving (Eq, Show, Generic)

data PagedExplanations = PagedExplanations
  { next_cursor :: Maybe String,
    explanations :: [Explanation]
  }
  deriving (Eq, Show, Generic)

data UserExplanationView = UserExplanationView
  { userDeckId :: String,
    userId :: String,
    explanationViewId :: String,
    fen :: String,
    move :: String,
    text :: String,
    visualizers :: String
  }
  deriving (Eq, Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions
  { fieldLabelModifier = \case
      "userDeckId" -> "user_deck_id"
      "userId"     -> "user_id"
      "explanationViewId"      -> "id"
      other        -> other
  }

instance ToJSON UserExplanationView where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance ToJSON Explanation
instance ToJSON PagedExplanations

instance FromJSON UserExplanationView where
  parseJSON = withObject "UserExplanationView" $ \obj -> do
    userDeckId <- obj .: "user_deck_id"
    userId <- obj .: "user_id"
    explanationViewId <- obj .: "id"
    fen <- obj .: "fen"
    move <- obj .: "move"
    text <- obj .: "text"
    visualizersValue <- obj .: "visualizers"
    let visualizers = case visualizersValue of
          String t -> T.unpack t
          value -> LBS.unpack (encode value)
    pure $ UserExplanationView
      { userDeckId = userDeckId
      , userId = userId
      , explanationViewId = explanationViewId
      , fen = fen
      , move = move
      , text = text
      , visualizers = visualizers
      }

instance FromRow UserExplanationView
instance FromRow Explanation
instance FromRow PendingExplanation
