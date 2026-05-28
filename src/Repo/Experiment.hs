{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Repo.Experiment where

import Data.Char (ord)
import Data.List (foldl', find)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only (..), Query)
import qualified Database.PostgreSQL.Simple.Types as PG
import Repo.Classes
import Models.Experiment

data ExperimentDefinition = ExperimentDefinition
  { experiment_key :: String,
    status :: String,
    default_variant :: String,
    respect_existing_assignments :: Bool
  }
  deriving (Eq, Show)

data ExperimentVariant = ExperimentVariant
  { variant_key :: String,
    weight :: Int
  }
  deriving (Eq, Show)

findExperiment :: MonadDB m => String -> m (Maybe ExperimentDefinition)
findExperiment key = fmap rowToExperiment . listToMaybe <$> runQuery query (Only key)
  where
    query :: Query
    query =
      "SELECT experiment_key, status, default_variant, respect_existing_assignments \
      \FROM experiments \
      \WHERE experiment_key = ? AND status IN ('active', 'completed')"

    rowToExperiment :: (String, String, String, Bool) -> ExperimentDefinition
    rowToExperiment (experimentKey, status, defaultVariant, respectExistingAssignments) =
      ExperimentDefinition experimentKey status defaultVariant respectExistingAssignments

findSupportedVariants :: MonadDB m => String -> [String] -> m [ExperimentVariant]
findSupportedVariants _ [] = pure []
findSupportedVariants key supportedVariants = fmap rowToVariant <$> runQuery query (key, PG.In supportedVariants)
  where
    query :: Query
    query =
      "SELECT variant_key, weight \
      \FROM experiment_variants \
      \WHERE experiment_key = ? AND variant_key IN ? \
      \ORDER BY variant_key"

    rowToVariant :: (String, Int) -> ExperimentVariant
    rowToVariant (variantKey, weight) = ExperimentVariant variantKey weight

findAssignment :: MonadDB m => String -> String -> m (Maybe String)
findAssignment sessionId experimentKey = do
  rows <- runQuery query (sessionId, experimentKey)
  pure $ fmap (\(Only variantKey) -> variantKey) (listToMaybe rows)
  where
    query :: Query
    query =
      "SELECT variant_key FROM experiment_assignments \
      \WHERE onboarding_session_id = ? AND experiment_key = ?"

upsertAssignment :: MonadDB m => String -> String -> String -> Maybe String -> Maybe String -> m ()
upsertAssignment sessionId experimentKey variantKey appVersion platform = do
  _ <- execute query (sessionId, experimentKey, variantKey, appVersion, platform)
  pure ()
  where
    query :: Query
    query =
      "INSERT INTO experiment_assignments \
      \(onboarding_session_id, experiment_key, variant_key, app_version, platform) \
      \VALUES (?, ?, ?, ?, ?) \
      \ON CONFLICT (onboarding_session_id, experiment_key) \
      \DO UPDATE SET \
      \ variant_key = EXCLUDED.variant_key, \
      \ app_version = COALESCE(EXCLUDED.app_version, experiment_assignments.app_version), \
      \ platform = COALESCE(EXCLUDED.platform, experiment_assignments.platform), \
      \ last_modified = CURRENT_TIMESTAMP"

saveEvent :: MonadDB m => ExperimentEventPayload -> m ()
saveEvent payload = do
  _ <- execute query
    ( payload.onboarding_session_id,
      payload.experiment_key,
      payload.variant_key,
      payload.event_name,
      payload.app_version,
      payload.platform
    )
  pure ()
  where
    query :: Query
    query =
      "INSERT INTO experiment_events \
      \(onboarding_session_id, experiment_key, variant_key, event_name, app_version, platform) \
      \VALUES (?, ?, ?, ?, ?, ?)"

assignExperiments :: MonadDB m => ExperimentBootstrapPayload -> m [ExperimentAssignment]
assignExperiments payload = concat <$> mapM assignOne payload.supported_experiments
  where
    assignOne (SupportedExperiment experimentKey supportedVariants) = do
      maybeExperiment <- findExperiment experimentKey
      case maybeExperiment of
        Nothing -> pure []
        Just experiment -> do
          variants <- findSupportedVariants experimentKey supportedVariants
          case chooseVariant payload.onboarding_session_id experiment variants supportedVariants of
            Nothing -> pure []
            Just chosen -> do
              existing <- findAssignment payload.onboarding_session_id experimentKey
              let variant =
                    if experiment.respect_existing_assignments && maybe False (`elem` supportedVariants) existing
                      then maybe chosen id existing
                      else chosen
              upsertAssignment payload.onboarding_session_id experimentKey variant payload.app_version payload.platform
              pure [ExperimentAssignment experimentKey variant]

chooseVariant :: String -> ExperimentDefinition -> [ExperimentVariant] -> [String] -> Maybe String
chooseVariant _ experiment _ supported
  | experiment.status == "completed" =
      if experiment.default_variant `elem` supported
        then Just experiment.default_variant
        else Nothing
chooseVariant sessionId experiment variants supported =
  case weightedVariants of
    [] ->
      if experiment.default_variant `elem` supported
        then Just experiment.default_variant
        else Nothing
    xs -> Just $ pickWeighted xs
  where
    weightedVariants = filter ((> 0) . (.weight)) variants
    totalWeight = sum (map (.weight) weightedVariants)
    bucket = stableBucket (sessionId <> ":" <> experiment.experiment_key) totalWeight

    pickWeighted :: [ExperimentVariant] -> String
    pickWeighted xs =
      let step :: (Int, Maybe String) -> ExperimentVariant -> (Int, Maybe String)
          step (remaining, selected) variant =
            case selected of
              Just value -> (remaining, Just value)
              Nothing ->
                let nextRemaining = remaining - variant.weight
                 in if nextRemaining < 0
                      then (nextRemaining, Just variant.variant_key)
                      else (nextRemaining, Nothing)
       in maybe (last xs).variant_key id (snd (foldl' step (bucket, Nothing) xs))

stableBucket :: String -> Int -> Int
stableBucket _ total | total <= 0 = 0
stableBucket value total = hash value `mod` total
  where
    hash = foldl' (\acc c -> (acc * 33 + ord c) `mod` 2147483647) 5381
