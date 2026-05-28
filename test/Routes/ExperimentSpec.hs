{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.ExperimentSpec (spec) where

import Database.PostgreSQL.Simple (Only (..), query)
import Test.Hspec

import Models.Experiment
import qualified Repo.Experiment as ExperimentRepo
import qualified Routes.Experiment as ExperimentRoutes
import TestHelpers

spec :: Spec
spec = describe "Routes.Experiment" $ do
  describe "bootstrapExperiments" $ do
    it "assigns a supported onboarding experiment variant" $ do
      withCleanDb $ \conn -> do
        let payload =
              ExperimentBootstrapPayload
                "experiment-session-1"
                (Just "1.0")
                (Just "ios")
                [SupportedExperiment "onboarding_after_notifications_v1" ["library", "explore_openings"]]

        ExperimentBootstrapResponse assignments <- expectRight =<< runTestApp conn (ExperimentRoutes.bootstrapExperiments payload)

        length assignments `shouldBe` 1
        let ExperimentAssignment experimentKey variantKey = head assignments
        experimentKey `shouldBe` "onboarding_after_notifications_v1"
        variantKey `shouldSatisfy` (`elem` ["library", "explore_openings"])

    it "keeps repeated bootstrap calls sticky for the same session" $ do
      withCleanDb $ \conn -> do
        let payload =
              ExperimentBootstrapPayload
                "experiment-session-sticky"
                (Just "1.0")
                (Just "android")
                [SupportedExperiment "onboarding_after_notifications_v1" ["library", "explore_openings"]]

        ExperimentBootstrapResponse first <- expectRight =<< runTestApp conn (ExperimentRoutes.bootstrapExperiments payload)
        ExperimentBootstrapResponse second <- expectRight =<< runTestApp conn (ExperimentRoutes.bootstrapExperiments payload)
        stored <- expectRight =<< runTestApp conn (ExperimentRepo.findAssignment "experiment-session-sticky" "onboarding_after_notifications_v1")

        first `shouldBe` second
        stored `shouldBe` Just ((\(ExperimentAssignment _ variantKey) -> variantKey) (head first))

    it "does not assign variants unsupported by the app version" $ do
      withCleanDb $ \conn -> do
        let payload =
              ExperimentBootstrapPayload
                "experiment-session-unsupported"
                (Just "0.9")
                (Just "android")
                [SupportedExperiment "onboarding_after_notifications_v1" ["legacy_only"]]

        ExperimentBootstrapResponse assignments <- expectRight =<< runTestApp conn (ExperimentRoutes.bootstrapExperiments payload)

        assignments `shouldBe` []

  describe "saveExperimentEvent" $ do
    it "records experiment conversion events" $ do
      withCleanDb $ \conn -> do
        let payload =
              ExperimentEventPayload
                "experiment-session-event"
                "onboarding_after_notifications_v1"
                "library"
                "paywall_reached"
                (Just "1.0")
                (Just "ios")

        _ <- expectRight =<< runTestApp conn (ExperimentRoutes.saveExperimentEvent payload)
        let queryParams =
              ( "experiment-session-event" :: String,
                "onboarding_after_notifications_v1" :: String,
                "library" :: String,
                "paywall_reached" :: String
              )
        rows <- query conn
          "SELECT COUNT(*) FROM experiment_events \
          \WHERE onboarding_session_id = ? \
          \AND experiment_key = ? \
          \AND variant_key = ? \
          \AND event_name = ?"
          queryParams :: IO [Only Int]

        rows `shouldBe` [Only 1]
