{-# LANGUAGE OverloadedRecordDot #-}

module Routes.UserSpec (spec) where

import Test.Hspec
import Servant ((:<|>)(..))
import Servant.Auth.Server (AuthResult(..))

import TestHelpers
import App.Error (AppError(..))
import App.Auth (AuthenticatedUser)
import Models.Feedback (Feedback(..), FeedbackPayload(..))
import qualified Routes.Auth as AuthRoutes
import qualified Routes.User as UserRoutes
import qualified Routes.Onboarding as OnboardingRoutes
import Models.Onboarding (OnboardingPreferences(..), OnboardingPreferencesPayload(..), AnonymousOnboardingProgressPayload(..), ClaimAnonymousOnboardingPayload(..))
import Models.Watermelon (JsonableMsg(..))
import qualified Repo.Onboarding as OnboardingRepo
import qualified Repo.Feedback as FeedbackRepo

spec :: Spec
spec = describe "Routes.User (secure)" $ do
  describe "saveFeedback" $ do
    it "stores feedback for the authenticated user" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "feedback-user" "feedback@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        result <- runTestApp conn $ UserRoutes.saveFeedback "feedback-user" (FeedbackPayload "  Great app  " 5)
        Msg message <- expectRight result
        message `shouldBe` "Successfully saved feedback."

        stored <- expectRight =<< runTestApp conn (FeedbackRepo.findLatestByUsername "feedback-user")
        case stored of
          Nothing -> expectationFailure "Expected feedback to be stored"
          Just (Feedback _ username text stars _) -> do
            username `shouldBe` "feedback-user"
            text `shouldBe` "Great app"
            stars `shouldBe` 5

    it "rejects feedback with stars outside the 1-5 range" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "feedback-invalid" "feedback-invalid@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        result <- runTestApp conn $ UserRoutes.saveFeedback "feedback-invalid" (FeedbackPayload "Needs work" 6)
        err <- expectLeft result
        case err of
          Internal message -> message `shouldBe` "{\"msg\":\"Invalid feedback.\"}"
          _ -> expectationFailure $ "Expected Internal test wrapper error but got: " ++ show err

    it "denies unauthenticated users through the secure server" $ do
      withCleanDb $ \conn -> do
        let authResult = Indefinite :: AuthResult AuthenticatedUser
        result <- runTestApp conn $ do
          let (_ :<|> _ :<|> _ :<|> feedbackHandler) = UserRoutes.server authResult
          feedbackHandler (FeedbackPayload "Great app" 5)

        err <- expectLeft result
        case err of
          Internal message -> message `shouldBe` "{\"msg\":\"No access.\"}"
          _ -> expectationFailure $ "Expected Internal test wrapper error but got: " ++ show err

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
        let anonPayload = AnonymousOnboardingProgressPayload sessionId "motivation" True (Just "beginner") (Just "0-800") (Just "Chess.com") (Just "Build a study habit") (Just "0-5 mins") Nothing
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
