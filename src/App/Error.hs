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

data AppError = NotFound String | Unauthorized String | Internal String | AlreadyExists String | MergeConflict String
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
  MergeConflict m -> jsonize err404 m

parsingError :: SqlError -> IO AppError
parsingError sqlerror = do 
  putStrLn "Failed parsing SqlError"
  print sqlerror
  return $ Internal $ show sqlerror

parseConstraintViolation :: ConstraintViolation -> IO AppError
parseConstraintViolation (NotNullViolation field) = do 
  putStrLn $ "NotNullViolation on field " ++ toString field ++ "."
  return $ Internal "All fields need to be set"
parseConstraintViolation (ForeignKeyViolation table constraint) = do
  putStrLn $ "ForeignKeyViolation in " ++ toString table ++ ":" ++ toString constraint
  return $ NotFound "Referencing unknown instance."
parseConstraintViolation (UniqueViolation violation) = do
  putStrLn $ "UniqueViolation: " ++ toString violation
  return $ AlreadyExists "Item already exists."
parseConstraintViolation (CheckViolation table constraint) = do
  putStrLn $ "CheckViolation: " ++ toString table ++ " " ++ toString constraint ++ "."
  return $ Internal "Check Violation."
parseConstraintViolation (ExclusionViolation name) = do
  putStrLn $ "ExclusionViolation: " ++ toString name ++ "."
  return $ Internal "Exclusion Violation: "


fromSqlError :: SqlError -> IO AppError
fromSqlError sqlerror = maybe error parseConstraintViolation (constraintViolation sqlerror)
  where
    error = parsingError sqlerror
    
