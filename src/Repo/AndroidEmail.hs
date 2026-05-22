{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.AndroidEmail where

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only (..), Query)
import Repo.Classes
import Models.AndroidEmail (AndroidEmail(..), AndroidEmailPayload(..))

returnFields :: Query
returnFields = " id, email "

insert :: MonadDB m => AndroidEmailPayload -> m AndroidEmail
insert payload = one =<< runQuery query (Only payload.email)
  where
    query :: Query
    query =
      "INSERT INTO android_emails (email) VALUES (?) RETURNING " <> returnFields

findByEmail :: MonadDB m => String -> m (Maybe AndroidEmail)
findByEmail email = listToMaybe <$> runQuery query (Only email)
  where
    query :: Query
    query = "SELECT" <> returnFields <> "FROM android_emails WHERE email = ? ORDER BY id DESC LIMIT 1"

one :: Monad m => [a] -> m a
one [x] = pure x
one _ = error "Expected exactly one android email row"
