{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.Onboarding where

import Database.PostgreSQL.Simple (Only(..), Query)
import Data.Maybe (listToMaybe)
import Repo.Classes
import Models.Onboarding (OnboardingPreferences(..), OnboardingPreferencesPayload(..))

returnFields :: Query
returnFields =
  " user_id, chess_level, elo, organization, motivation, study_goal "

upsertForUser :: MonadDB m => String -> OnboardingPreferencesPayload -> m ()
upsertForUser username payload = do
  _ <- execute query (username, payload.chess_level, payload.elo, payload.organization, payload.motivation, payload.study_goal)
  pure ()
  where
    query :: Query
    query =
      "INSERT INTO user_onboarding_preferences \
      \(user_id, chess_level, elo, organization, motivation, study_goal) \
      \VALUES (?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (user_id) \
      \DO UPDATE SET \
      \ chess_level = EXCLUDED.chess_level, \
      \ elo = EXCLUDED.elo, \
      \ organization = EXCLUDED.organization, \
      \ motivation = EXCLUDED.motivation, \
      \ study_goal = EXCLUDED.study_goal, \
      \ last_modified = CURRENT_TIMESTAMP"

findByUser :: MonadDB m => String -> m (Maybe OnboardingPreferences)
findByUser username = listToMaybe <$> runQuery query (Only username)
  where
    query :: Query
    query =
      "SELECT" <> returnFields <> "FROM user_onboarding_preferences WHERE user_id = ?"
