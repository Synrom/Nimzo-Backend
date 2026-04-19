{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Repo.Onboarding where

import Database.PostgreSQL.Simple (Only(..), Query)
import Data.Maybe (listToMaybe)
import Control.Monad.Except (throwError)
import Repo.Classes
import App.Error (AppError(..))
import Models.Onboarding (
  OnboardingPreferences(..),
  OnboardingPreferencesPayload(..),
  AnonymousOnboardingProgress(..),
  AnonymousOnboardingProgressPayload(..)
  )

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

anonymousReturnFields :: Query
anonymousReturnFields =
  " onboarding_session_id, last_step, stopped, chess_level, elo, organization, motivation, study_goal, claimed_by_user "

upsertAnonymousProgress :: MonadDB m => AnonymousOnboardingProgressPayload -> m ()
upsertAnonymousProgress payload = do
  _ <- execute query
    ( payload.onboarding_session_id,
      payload.last_step,
      payload.stopped,
      payload.chess_level,
      payload.elo,
      payload.organization,
      payload.motivation,
      payload.study_goal
    )
  pure ()
  where
    query :: Query
    query =
      "INSERT INTO anonymous_onboarding_progress \
      \(onboarding_session_id, last_step, stopped, chess_level, elo, organization, motivation, study_goal) \
      \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
      \ON CONFLICT (onboarding_session_id) \
      \DO UPDATE SET \
      \ last_step = EXCLUDED.last_step, \
      \ stopped = EXCLUDED.stopped, \
      \ chess_level = COALESCE(EXCLUDED.chess_level, anonymous_onboarding_progress.chess_level), \
      \ elo = COALESCE(EXCLUDED.elo, anonymous_onboarding_progress.elo), \
      \ organization = COALESCE(EXCLUDED.organization, anonymous_onboarding_progress.organization), \
      \ motivation = COALESCE(EXCLUDED.motivation, anonymous_onboarding_progress.motivation), \
      \ study_goal = COALESCE(EXCLUDED.study_goal, anonymous_onboarding_progress.study_goal), \
      \ last_modified = CURRENT_TIMESTAMP"

findAnonymousBySession :: MonadDB m => String -> m (Maybe AnonymousOnboardingProgress)
findAnonymousBySession sessionId = listToMaybe <$> runQuery query (Only sessionId)
  where
    query :: Query
    query =
      "SELECT" <> anonymousReturnFields <> "FROM anonymous_onboarding_progress WHERE onboarding_session_id = ?"

claimAnonymousForUser :: MonadDB m => String -> String -> m ()
claimAnonymousForUser username sessionId = do
  result <- runQuery claimAndUpsertQuery (username, sessionId, username, username) :: MonadDB m => m [Only Int]
  case result of
    [Only _] -> pure ()
    _ -> ensureClaimable
  where
    claimAndUpsertQuery :: Query
    claimAndUpsertQuery =
      "WITH claimed AS ( \
      \  UPDATE anonymous_onboarding_progress \
      \  SET claimed_by_user = ?, last_modified = CURRENT_TIMESTAMP \
      \  WHERE onboarding_session_id = ? \
      \    AND (claimed_by_user IS NULL OR claimed_by_user = ?) \
      \  RETURNING chess_level, elo, organization, motivation, study_goal \
      \) \
      \INSERT INTO user_onboarding_preferences (user_id, chess_level, elo, organization, motivation, study_goal) \
      \SELECT ?, \
      \  COALESCE(chess_level, ''), \
      \  COALESCE(elo, ''), \
      \  COALESCE(organization, ''), \
      \  COALESCE(motivation, ''), \
      \  COALESCE(study_goal, '') \
      \FROM claimed \
      \ON CONFLICT (user_id) \
      \DO UPDATE SET \
      \ chess_level = EXCLUDED.chess_level, \
      \ elo = EXCLUDED.elo, \
      \ organization = EXCLUDED.organization, \
      \ motivation = EXCLUDED.motivation, \
      \ study_goal = EXCLUDED.study_goal, \
      \ last_modified = CURRENT_TIMESTAMP \
      \RETURNING 1"

    ensureClaimable :: MonadDB m => m ()
    ensureClaimable = do
      claimedBy <- runQuery claimedByQuery (Only sessionId)
      case claimedBy of
        [] -> throwError $ NotFound "Unknown onboarding session."
        [Only Nothing] -> throwError $ Internal "Failed to claim onboarding session."
        [Only (Just claimedUser)]
          | claimedUser == username -> pure ()
          | otherwise -> throwError $ MergeConflict "Onboarding session has already been claimed by another user."
        _ -> throwError $ Internal "Unexpected onboarding claim state."

    claimedByQuery :: Query
    claimedByQuery =
      "SELECT claimed_by_user FROM anonymous_onboarding_progress WHERE onboarding_session_id = ?"
