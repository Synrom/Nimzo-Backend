{-# LANGUAGE OverloadedRecordDot #-}

module Repo.OnboardingSpec (spec) where

import Test.Hspec

import TestHelpers
import qualified Routes.Auth as AuthRoutes
import qualified Routes.Onboarding as OnboardingRoutes
import Models.Onboarding (AnonymousOnboardingProgressPayload(..))
import qualified Repo.Onboarding as OnboardingRepo

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _ = False

spec :: Spec
spec = describe "Repo.Onboarding" $ do
  describe "claimAnonymousForUser edge cases" $ do
    it "fails for unknown onboarding session id" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "unknown-claim-user" "unknown-claim@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        result <- runTestApp conn $ OnboardingRepo.claimAnonymousForUser "unknown-claim-user" "missing-session"
        result `shouldSatisfy` isLeft'

    it "is idempotent when claimed repeatedly by the same user" $ do
      withCleanDb $ \conn -> do
        let user = mkTestUser "idempotent-claim-user" "idempotent-claim@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser user

        let payload = AnonymousOnboardingProgressPayload "idempotent-session" "elo" True (Just "beginner") (Just "0-800") Nothing Nothing Nothing
        _ <- runTestApp conn $ OnboardingRoutes.saveAnonymousOnboardingProgress payload

        firstClaim <- runTestApp conn $ OnboardingRepo.claimAnonymousForUser "idempotent-claim-user" "idempotent-session"
        secondClaim <- runTestApp conn $ OnboardingRepo.claimAnonymousForUser "idempotent-claim-user" "idempotent-session"

        firstClaim `shouldSatisfy` isRight'
        secondClaim `shouldSatisfy` isRight'

    it "fails when a different user tries to claim an already claimed session" $ do
      withCleanDb $ \conn -> do
        let userA = mkTestUser "claim-owner-user" "claim-owner@example.com" "password"
        let userB = mkTestUser "claim-other-user" "claim-other@example.com" "password"
        _ <- runTestApp conn $ AuthRoutes.createUser userA
        _ <- runTestApp conn $ AuthRoutes.createUser userB

        let payload = AnonymousOnboardingProgressPayload "conflict-session" "study_goal" True (Just "advanced") (Just "2000+") Nothing Nothing (Just "20+ mins")
        _ <- runTestApp conn $ OnboardingRoutes.saveAnonymousOnboardingProgress payload

        _ <- runTestApp conn $ OnboardingRepo.claimAnonymousForUser "claim-owner-user" "conflict-session"
        result <- runTestApp conn $ OnboardingRepo.claimAnonymousForUser "claim-other-user" "conflict-session"

        result `shouldSatisfy` isLeft'
