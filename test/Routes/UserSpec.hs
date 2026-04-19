{-# LANGUAGE OverloadedRecordDot #-}

module Routes.UserSpec (spec) where

import Test.Hspec

import TestHelpers
import qualified Routes.Auth as AuthRoutes
import qualified Routes.Onboarding as OnboardingRoutes
import Models.Onboarding (OnboardingPreferences(..), OnboardingPreferencesPayload(..), AnonymousOnboardingProgressPayload(..), ClaimAnonymousOnboardingPayload(..))
import Models.Watermelon (JsonableMsg(..))
import qualified Repo.Onboarding as OnboardingRepo

spec :: Spec
spec = describe "Routes.Onboarding (secure)" $ do
  describe "saveOnboardingPreferences" $ do
    it "stores onboarding preferences for a user" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "onboard-user" "onboard@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        let payload = OnboardingPreferencesPayload "beginner" "800-1600" "Lichess" "Learn new openings" "10-20 mins"
        result <- runTestApp conn $ OnboardingRoutes.saveOnboardingPreferences "onboard-user" payload
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
        _ <- runTestApp conn $ OnboardingRoutes.saveOnboardingPreferences "onboard-upsert" firstPayload

        let updatedPayload = OnboardingPreferencesPayload "advanced" "2000+" "Fide" "Prepare for tournament" "20+ mins"
        _ <- runTestApp conn $ OnboardingRoutes.saveOnboardingPreferences "onboard-upsert" updatedPayload

        stored <- expectRight =<< runTestApp conn (OnboardingRepo.findByUser "onboard-upsert")
        case stored of
          Nothing -> expectationFailure "Expected onboarding preferences to be stored"
          Just (OnboardingPreferences _ chessLevel elo organization motivation studyGoal) -> do
            chessLevel `shouldBe` "advanced"
            elo `shouldBe` "2000+"
            organization `shouldBe` "Fide"
            motivation `shouldBe` "Prepare for tournament"
            studyGoal `shouldBe` "20+ mins"

  describe "claimAnonymousOnboarding" $ do
    it "claims an anonymous session and copies preferences to the user onboarding table" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "claim-onboarding-user" "claim-onboarding@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        let sessionId = "anon-session-claim-1"
        let anonPayload = AnonymousOnboardingProgressPayload sessionId "motivation" True (Just "beginner") (Just "0-800") (Just "Chess.com") (Just "Build a study habit") (Just "0-5 mins")
        _ <- runTestApp conn $ OnboardingRoutes.saveAnonymousOnboardingProgress anonPayload

        let claimPayload = ClaimAnonymousOnboardingPayload sessionId
        result <- runTestApp conn $ OnboardingRoutes.claimAnonymousOnboarding "claim-onboarding-user" claimPayload
        Msg message <- expectRight result
        message `shouldBe` "Successfully claimed anonymous onboarding session."

        stored <- expectRight =<< runTestApp conn (OnboardingRepo.findByUser "claim-onboarding-user")
        case stored of
          Nothing -> expectationFailure "Expected claimed onboarding preferences to be stored"
          Just (OnboardingPreferences userId chessLevel elo organization motivation studyGoal) -> do
            userId `shouldBe` "claim-onboarding-user"
            chessLevel `shouldBe` "beginner"
            elo `shouldBe` "0-800"
            organization `shouldBe` "Chess.com"
            motivation `shouldBe` "Build a study habit"
            studyGoal `shouldBe` "0-5 mins"
