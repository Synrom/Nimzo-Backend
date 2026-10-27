{-# LANGUAGE OverloadedStrings #-}
module Routes.OnboardingTelemetrySpec (spec) where
import Test.Hspec
import TestHelpers
import Routes.OnboardingTelemetry
import Data.Aeson
import Data.Either (isLeft)
import Database.PostgreSQL.Simple
import System.Environment (setEnv, unsetEnv)

spec :: Spec
spec = describe "OnboardingTelemetry" $ do
 it "fails closed for missing webhook secret" $ withCleanDb $ \conn -> do
  unsetEnv "REVENUECAT_WEBHOOK_SECRET"
  result <- runTestApp conn $ revenueCat Nothing (object [])
  result `shouldSatisfy` isLeft
 it "authenticates webhooks, drops private fields and deduplicates deliveries" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_revenuecat_events"
  setEnv "REVENUECAT_WEBHOOK_SECRET" "test-only-secret-of-at-least-24-characters"
  let event = object ["event" .= object ["id" .= ("rc-id"::String),"type" .= ("INITIAL_PURCHASE"::String),"environment" .= ("SANDBOX"::String),"app_user_id" .= ("rc-user"::String),"subscriber_attributes" .= object ["email" .= ("private@example.org"::String)]]]
      auth = Just "Bearer test-only-secret-of-at-least-24-characters"
  _ <- runTestApp conn (revenueCat auth event) >>= expectRight
  _ <- runTestApp conn (revenueCat auth event) >>= expectRight
  rows <- query_ conn "SELECT payload ? 'subscriber_attributes' FROM onboarding_revenuecat_events" :: IO [Only Bool]
  rows `shouldBe` [Only False]
  unsetEnv "REVENUECAT_WEBHOOK_SECRET"
