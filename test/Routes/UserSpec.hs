{-# LANGUAGE OverloadedRecordDot #-}

module Routes.UserSpec (spec) where

import Test.Hspec

import TestHelpers
import qualified Routes.Auth as AuthRoutes
import qualified Routes.User as UserRoutes
import Models.Onboarding (OnboardingPreferences(..), OnboardingPreferencesPayload(..))
import Models.Watermelon (JsonableMsg(..))
import qualified Repo.Onboarding as OnboardingRepo

spec :: Spec
spec = describe "Routes.User" $ do
  describe "saveOnboardingPreferences" $ do
    it "stores onboarding preferences for a user" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "onboard-user" "onboard@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        let payload = OnboardingPreferencesPayload "beginner" "800-1600" "Lichess" "Learn new openings" "10-20 mins"
        result <- runTestApp conn $ UserRoutes.saveOnboardingPreferences "onboard-user" payload
        Msg message <- expectRight result
        message `shouldBe` "Successfully saved onboarding preferences."

        stored <- expectRight =<< runTestApp conn (OnboardingRepo.findByUser "onboard-user")
        case stored of
          Nothing -> expectationFailure "Expected onboarding preferences to be stored"
          Just (OnboardingPreferences userId chessLevel elo organization motivation studyGoal) -> do
            userId `shouldBe` "onboard-user"
            chessLevel `shouldBe` "beginner"
            elo `shouldBe` "800-1600"
            organization `shouldBe` "Lichess"
            motivation `shouldBe` "Learn new openings"
            studyGoal `shouldBe` "10-20 mins"

    it "upserts onboarding preferences when called multiple times" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "onboard-upsert" "onboard-upsert@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        let firstPayload = OnboardingPreferencesPayload "beginner" "0-800" "Chess.com" "Get better quickly" "0-5 mins"
        _ <- runTestApp conn $ UserRoutes.saveOnboardingPreferences "onboard-upsert" firstPayload

        let updatedPayload = OnboardingPreferencesPayload "advanced" "2000+" "Fide" "Prepare for tournament" "20+ mins"
        _ <- runTestApp conn $ UserRoutes.saveOnboardingPreferences "onboard-upsert" updatedPayload

        stored <- expectRight =<< runTestApp conn (OnboardingRepo.findByUser "onboard-upsert")
        case stored of
          Nothing -> expectationFailure "Expected onboarding preferences to be stored"
          Just (OnboardingPreferences _ chessLevel elo organization motivation studyGoal) -> do
            chessLevel `shouldBe` "advanced"
            elo `shouldBe` "2000+"
            organization `shouldBe` "Fide"
            motivation `shouldBe` "Prepare for tournament"
            studyGoal `shouldBe` "20+ mins"
