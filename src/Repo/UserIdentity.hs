{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.UserIdentity where

import Database.PostgreSQL.Simple (Only(..), Query)
import Data.Maybe (listToMaybe)
import Repo.Classes
import Models.User (User)
import Models.UserIdentity (UserIdentity(..))

identityReturnFields :: Query
identityReturnFields = " username, provider, provider_subject, email, email_verified "

userReturnFields :: Query
userReturnFields = " u.username, u.password, u.salt, u.premium, u.xp, u.streak, u.last_activity, u.rank, u.email, u.verified "

find :: MonadDB m => String -> String -> m (Maybe UserIdentity)
find providerName subject = listToMaybe <$> runQuery query (providerName, subject)
  where
    query :: Query
    query =
      "SELECT" <> identityReturnFields <>
      "FROM user_identities WHERE provider = ? AND provider_subject = ?"

findUser :: MonadDB m => String -> String -> m (Maybe User)
findUser providerName subject = listToMaybe <$> runQuery query (providerName, subject)
  where
    query :: Query
    query =
      "SELECT" <> userReturnFields <>
      "FROM user_identities ui \
      \JOIN users u ON u.username = ui.username \
      \WHERE ui.provider = ? AND ui.provider_subject = ?"

insertOrUpdate :: MonadDB m => UserIdentity -> m ()
insertOrUpdate identity = do
  _ <- execute query (identity.username, identity.provider, identity.providerSubject, identity.providerEmail, identity.emailVerified)
  pure ()
  where
    query :: Query
    query =
      "INSERT INTO user_identities (username, provider, provider_subject, email, email_verified) \
      \VALUES (?, ?, ?, ?, ?) \
      \ON CONFLICT (provider, provider_subject) \
      \DO UPDATE SET \
      \ email = EXCLUDED.email \
      \, email_verified = EXCLUDED.email_verified \
      \, last_modified = CURRENT_TIMESTAMP"

listByUsername :: MonadDB m => String -> m [UserIdentity]
listByUsername username = runQuery query (Only username)
  where
    query :: Query
    query = "SELECT" <> identityReturnFields <> "FROM user_identities WHERE username = ?"
