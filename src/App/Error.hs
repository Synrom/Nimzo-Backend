{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module App.Error where

import qualified Data.Text as T
import Servant (ServerError(..), err401, err404, err500, (:<|>) (..))
import Database.PostgreSQL.Simple (SqlError(..))
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
import Network.HTTP.Types.Header (Header)
import Data.Aeson (encode, object, (.=), ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (toString)
import Data.CaseInsensitive  (mk)
import Control.Monad.Except
import GHC.Generics

data AppError = NotFound String | Unauthorized String | Internal String | AlreadyExists String 
  deriving (Generic, Show)

class ThrowAppError a where
  throwAppError :: AppError -> a

instance (ThrowAppError a, ThrowAppError b) => ThrowAppError (a :<|> b) where
  throwAppError e = throwAppError e :<|> throwAppError e

instance ThrowAppError b => ThrowAppError (a -> b) where
  throwAppError e = const $ throwAppError e

newtype Msg = Msg {
  msg :: String
} deriving (Generic, Show)
instance ToJSON Msg

jsonMsg :: String -> ByteString
jsonMsg m = encode $ Msg m

jsonHeader :: [Header]
jsonHeader = [(mk $ pack "Content-Type",
              pack "application/json;charset=utf-8")]

jsonize :: ServerError -> String -> ServerError
jsonize err m = err { errBody = jsonMsg m, errHeaders = jsonHeader }

toServerError :: AppError -> ServerError
toServerError = \case
  NotFound m -> jsonize err404 m
  Unauthorized m -> jsonize err401 m
  Internal m -> jsonize err500 m
  AlreadyExists m -> jsonize err404 m

parsingError :: SqlError -> AppError
parsingError sqlerror = Internal $ show sqlerror

fromSqlError :: SqlError -> AppError
fromSqlError sqlerror = maybe error parse (constraintViolation sqlerror)
  where
    parse (NotNullViolation field) = Internal $ "NotNullViolation on field " ++ toString field ++ "."
    parse (ForeignKeyViolation table _) = NotFound $ "Reference not found in " ++ toString table ++ "."
    parse (UniqueViolation _) = AlreadyExists "Item already exists."
    parse (CheckViolation table constraint) = Internal $ "CheckViolation: " ++ toString table ++ " " ++ toString constraint ++ "."
    parse (ExclusionViolation name) = Internal $ "ExclusionViolation: " ++ toString name ++ "."
    error = parsingError sqlerror
    
