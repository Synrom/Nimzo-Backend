{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Routes.Onboarding where

import Data.List (dropWhileEnd)
import Servant (type (:<|>) (..), type (:>), ReqBody, JSON, Post, Get, Capture)
import Servant.Auth.Server (AuthResult(..))
import App.AppM
import App.Error (AppError(..), throwAppError)
import App.Auth (AuthenticatedUser(..))
import Models.Onboarding (
  OnboardingPreferencesPayload(..),
  AnonymousOnboardingProgress(..),
  AnonymousOnboardingProgressPayload(..),
  ClaimAnonymousOnboardingPayload(..),
  sessionIdMaxLength,
  lastStepMaxLength,
  onboardingShortFieldMaxLength,
  motivationMaxLength
  )
import qualified Repo.Onboarding as OnboardingRepo
import Repo.Utils (orThrow, ensure)
import Models.Watermelon (JsonableMsg(Msg))

type PublicAPI =
  "onboarding" :> "anonymous" :> "progress" :> ReqBody '[JSON] AnonymousOnboardingProgressPayload :> Post '[JSON] JsonableMsg
  :<|> "onboarding" :> "anonymous" :> "progress" :> Capture "onboarding_session_id" String :> Get '[JSON] AnonymousOnboardingProgress

type PublicServer =
  (AnonymousOnboardingProgressPayload -> AppM JsonableMsg)
  :<|> (String -> AppM AnonymousOnboardingProgress)

type SecureAPI =
  "user" :> "onboarding" :> ReqBody '[JSON] OnboardingPreferencesPayload :> Post '[JSON] JsonableMsg
  :<|> "user" :> "onboarding" :> "claim" :> ReqBody '[JSON] ClaimAnonymousOnboardingPayload :> Post '[JSON] JsonableMsg

type SecureServer =
  (OnboardingPreferencesPayload -> AppM JsonableMsg)
  :<|> (ClaimAnonymousOnboardingPayload -> AppM JsonableMsg)

invalidOnboardingSession :: AppError
invalidOnboardingSession = Unauthorized "Invalid onboarding session id."

invalidOnboardingStep :: AppError
invalidOnboardingStep = Unauthorized "Invalid onboarding step."

invalidOnboardingPayload :: AppError
invalidOnboardingPayload = Unauthorized "Invalid onboarding payload."

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

ensureLength :: Int -> String -> AppError -> AppM ()
ensureLength maxLen value err = ensure err (length value <= maxLen)

ensureOptionalLength :: Int -> Maybe String -> AppError -> AppM ()
ensureOptionalLength maxLen maybeValue err = maybe (pure ()) (\value -> ensureLength maxLen value err) maybeValue

validateAnonymousPayload :: AnonymousOnboardingProgressPayload -> AppM AnonymousOnboardingProgressPayload
validateAnonymousPayload payload = do
  let normalizedPayload =
        AnonymousOnboardingProgressPayload
          (trim payload.onboarding_session_id)
          (trim payload.last_step)
          payload.stopped
          payload.chess_level
          payload.elo
          payload.organization
          payload.motivation
          payload.study_goal

  ensure invalidOnboardingSession (not (null normalizedPayload.onboarding_session_id))
  ensure invalidOnboardingStep (not (null normalizedPayload.last_step))
  ensureLength sessionIdMaxLength normalizedPayload.onboarding_session_id invalidOnboardingSession
  ensureLength lastStepMaxLength normalizedPayload.last_step invalidOnboardingStep
  ensureOptionalLength onboardingShortFieldMaxLength normalizedPayload.chess_level invalidOnboardingPayload
  ensureOptionalLength onboardingShortFieldMaxLength normalizedPayload.elo invalidOnboardingPayload
  ensureOptionalLength onboardingShortFieldMaxLength normalizedPayload.organization invalidOnboardingPayload
  ensureOptionalLength motivationMaxLength normalizedPayload.motivation invalidOnboardingPayload
  ensureOptionalLength onboardingShortFieldMaxLength normalizedPayload.study_goal invalidOnboardingPayload
  pure normalizedPayload

validateUserOnboardingPayload :: OnboardingPreferencesPayload -> AppM OnboardingPreferencesPayload
validateUserOnboardingPayload payload = do
  ensureLength onboardingShortFieldMaxLength payload.chess_level invalidOnboardingPayload
  ensureLength onboardingShortFieldMaxLength payload.elo invalidOnboardingPayload
  ensureLength onboardingShortFieldMaxLength payload.organization invalidOnboardingPayload
  ensureLength motivationMaxLength payload.motivation invalidOnboardingPayload
  ensureLength onboardingShortFieldMaxLength payload.study_goal invalidOnboardingPayload
  pure payload

saveOnboardingPreferences :: String -> OnboardingPreferencesPayload -> AppM JsonableMsg
saveOnboardingPreferences username payload = do
  validatedPayload <- validateUserOnboardingPayload payload
  OnboardingRepo.upsertForUser username validatedPayload
  return $ Msg "Successfully saved onboarding preferences."

claimAnonymousOnboarding :: String -> ClaimAnonymousOnboardingPayload -> AppM JsonableMsg
claimAnonymousOnboarding username payload = do
  let sessionId = trim payload.onboarding_session_id
  ensure invalidOnboardingSession (not (null sessionId))
  ensureLength sessionIdMaxLength sessionId invalidOnboardingSession
  OnboardingRepo.claimAnonymousForUser username sessionId
  return $ Msg "Successfully claimed anonymous onboarding session."

saveAnonymousOnboardingProgress :: AnonymousOnboardingProgressPayload -> AppM JsonableMsg
saveAnonymousOnboardingProgress payload = do
  validatedPayload <- validateAnonymousPayload payload
  OnboardingRepo.upsertAnonymousProgress validatedPayload
  return $ Msg "Successfully saved anonymous onboarding progress."

getAnonymousOnboardingProgress :: String -> AppM AnonymousOnboardingProgress
getAnonymousOnboardingProgress sessionId = do
  let normalizedSessionId = trim sessionId
  ensure invalidOnboardingSession (not (null normalizedSessionId))
  ensureLength sessionIdMaxLength normalizedSessionId invalidOnboardingSession
  OnboardingRepo.findAnonymousBySession normalizedSessionId >>= orThrow (NotFound "Unknown onboarding session.")

publicServer :: PublicServer
publicServer =
  saveAnonymousOnboardingProgress
  :<|> getAnonymousOnboardingProgress

secureServer :: AuthResult AuthenticatedUser -> SecureServer
secureServer (Authenticated user) =
  saveOnboardingPreferences user.username
  :<|> claimAnonymousOnboarding user.username
secureServer _ = throwAppError $ Unauthorized "No access."
