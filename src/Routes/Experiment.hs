{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Experiment where

import Data.List (dropWhileEnd)
import Servant (JSON, Post, ReqBody, type (:<|>) (..), type (:>))
import App.AppM
import App.Error (AppError (..))
import Models.Experiment
import Models.Watermelon (JsonableMsg (Msg))
import qualified Repo.Experiment as ExperimentRepo
import Repo.Utils (ensure)

type API =
  "experiments" :> "bootstrap" :> ReqBody '[JSON] ExperimentBootstrapPayload :> Post '[JSON] ExperimentBootstrapResponse
    :<|> "experiments" :> "event" :> ReqBody '[JSON] ExperimentEventPayload :> Post '[JSON] JsonableMsg

type Server =
  (ExperimentBootstrapPayload -> AppM ExperimentBootstrapResponse)
    :<|> (ExperimentEventPayload -> AppM JsonableMsg)

invalidExperimentPayload :: AppError
invalidExperimentPayload = Unauthorized "Invalid experiment payload."

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

ensureLength :: Int -> String -> AppM ()
ensureLength maxLen value = ensure invalidExperimentPayload (length value <= maxLen)

ensureOptionalLength :: Int -> Maybe String -> AppM ()
ensureOptionalLength maxLen maybeValue = maybe (pure ()) (ensureLength maxLen) maybeValue

validateBootstrapPayload :: ExperimentBootstrapPayload -> AppM ExperimentBootstrapPayload
validateBootstrapPayload payload = do
  let normalizedSupported =
        map
          ( \supported ->
              SupportedExperiment
                (trim supported.experiment_key)
                (map trim supported.supported_variants)
          )
          payload.supported_experiments
  let normalized =
        ExperimentBootstrapPayload
          (trim payload.onboarding_session_id)
          (trim <$> payload.app_version)
          (trim <$> payload.platform)
          normalizedSupported
  ensure invalidExperimentPayload (not (null normalized.onboarding_session_id))
  ensureLength 128 normalized.onboarding_session_id
  ensureOptionalLength appVersionMaxLength normalized.app_version
  ensureOptionalLength platformMaxLength normalized.platform
  mapM_ validateSupported normalized.supported_experiments
  pure normalized
  where
    validateSupported (SupportedExperiment experimentKey supportedVariants) = do
      ensure invalidExperimentPayload (not (null experimentKey))
      ensureLength experimentKeyMaxLength experimentKey
      mapM_ (ensureLength variantKeyMaxLength) supportedVariants

validateEventPayload :: ExperimentEventPayload -> AppM ExperimentEventPayload
validateEventPayload payload = do
  let normalized =
        ExperimentEventPayload
          (trim payload.onboarding_session_id)
          (trim payload.experiment_key)
          (trim payload.variant_key)
          (trim payload.event_name)
          (trim <$> payload.app_version)
          (trim <$> payload.platform)
  ensure invalidExperimentPayload (not (null normalized.onboarding_session_id))
  ensure invalidExperimentPayload (not (null normalized.experiment_key))
  ensure invalidExperimentPayload (not (null normalized.variant_key))
  ensure invalidExperimentPayload (not (null normalized.event_name))
  ensureLength 128 normalized.onboarding_session_id
  ensureLength experimentKeyMaxLength normalized.experiment_key
  ensureLength variantKeyMaxLength normalized.variant_key
  ensureLength eventNameMaxLength normalized.event_name
  ensureOptionalLength appVersionMaxLength normalized.app_version
  ensureOptionalLength platformMaxLength normalized.platform
  pure normalized

bootstrapExperiments :: ExperimentBootstrapPayload -> AppM ExperimentBootstrapResponse
bootstrapExperiments payload = do
  validated <- validateBootstrapPayload payload
  ExperimentBootstrapResponse <$> ExperimentRepo.assignExperiments validated

saveExperimentEvent :: ExperimentEventPayload -> AppM JsonableMsg
saveExperimentEvent payload = do
  validated <- validateEventPayload payload
  ExperimentRepo.saveEvent validated
  pure $ Msg "Successfully saved experiment event."

server :: Server
server = bootstrapExperiments :<|> saveExperimentEvent
