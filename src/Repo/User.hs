{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.User where

import Database.PostgreSQL.Simple (Only(..), Query)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Control.Exception (tryJust)
import Data.String (fromString)
import Repo.Classes
import Models.Deck (Deck(..))
import Repo.Utils (one)
import App.Error (AppError(..))
import Models.User (User(..))

returnFields :: String
returnFields = " username, password, salt, premium, xp, email, verified "

insert :: MonadDB m => User -> m User
insert user = one =<< runQuery query (user.username, user.password, user.salt, user.email)
  where
    query :: Query
    query = fromString $ "INSERT INTO users (username, password, salt, email) VALUES (?,?,?,?) RETURNING " <> returnFields

find :: MonadDB m => String -> m (Maybe User)
find name = do
  listToMaybe <$> runQuery query (name, name)
  where
    query :: Query
    query = fromString $ "SELECT" <> returnFields <> "FROM users WHERE username = ? OR email = ?"
