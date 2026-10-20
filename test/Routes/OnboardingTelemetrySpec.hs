{-# LANGUAGE OverloadedStrings #-}
module Routes.OnboardingTelemetrySpec (spec) where
import Test.Hspec
import TestHelpers
import Routes.OnboardingTelemetry
import Data.Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft, isRight)
import Database.PostgreSQL.Simple
import System.Environment (setEnv, unsetEnv)

sample :: Value
sample = object
 [ "event_id" .= ("telemetry-test-event" :: String), "schema_version" .= (1::Int)
 , "event_name" .= ("purchase_attempt_started" :: String), "occurred_at" .= ("2026-10-19T12:00:00Z" :: String)
 , "sequence_number" .= (1::Int), "installation_id" .= ("test-install" :: String)
 , "onboarding_session_id" .= ("test-journey" :: String), "visit_id" .= ("visit" :: String)
 , "flow_version" .= ("v2" :: String), "app_version" .= ("1.0" :: String), "build_number" .= ("123" :: String)
 , "platform" .= ("android" :: String), "os_version" .= ("35" :: String)
 , "environment" .= ("sandbox" :: String), "config_version" .= ("default-v1" :: String)
 , "properties" .= object ["attempt_id" .= ("attempt-one" :: String), "product_id" .= ("monthly" :: String)] ]
replace :: Key -> Value -> Value -> Value
replace key value (Object o) = Object $ KM.insert key value o
replace _ _ other = other

spec :: Spec
spec = describe "OnboardingTelemetry" $ do
 it "accepts the shared mobile envelope and rejects nested/unknown properties" $ do
  parseEither validateEvent sample `shouldSatisfy` isRight
  parseEither validateEvent (replace "properties" (object ["email" .= ("x@example.org" :: String)]) sample) `shouldSatisfy` isLeft
  parseEither validateEvent (replace "properties" (object ["source" .= object []]) sample) `shouldSatisfy` isLeft
 it "bounds batches and sequence numbers and accepts scalar review metadata" $ do
  parseEither validateBatch (object ["events" .= replicate 51 sample]) `shouldSatisfy` isLeft
  parseEither validateEvent (replace "sequence_number" (Number (-1)) sample) `shouldSatisfy` isLeft
  parseEither validateEvent (replace "properties" (object ["invoked" .= True,"trigger" .= ("streak_complete"::String)]) sample) `shouldSatisfy` isRight
 it "validates config and rejects unsupported settings" $ do
  parseEither validateConfig safeConfig `shouldSatisfy` isRight
  parseEither validateConfig (replace "activation_path" (String "unknown") safeConfig) `shouldSatisfy` isLeft
 it "acknowledges retried IDs while retaining the first immutable payload" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_events"
  first <- runTestApp conn $ ingestEvents (object ["events" .= [sample]])
  retry <- runTestApp conn $ ingestEvents (object ["events" .= [replace "event_name" (String "purchase_failed") sample]])
  expectRight first `shouldReturn` object ["accepted_event_ids" .= ["telemetry-test-event"::String]]
  expectRight retry `shouldReturn` object ["accepted_event_ids" .= ["telemetry-test-event"::String]]
  rows <- query_ conn "SELECT payload->>'event_name' FROM onboarding_events" :: IO [Only String]
  rows `shouldBe` [Only "purchase_attempt_started"]
 it "pins journey config and applies emergency overrides" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_config_assignments"
  unsetEnv "ONBOARDING_CONFIG_JSON"
  first <- runTestApp conn $ getConfig (Just "sticky-test") Nothing Nothing
  expectRight first `shouldReturn` safeConfig
  setEnv "ONBOARDING_CONFIG_JSON" "{\"schema_version\":1,\"config_version\":\"new\",\"activation_path\":\"one_lesson\",\"minimal_questions\":true,\"featured_source\":\"instagram\",\"default_plan\":\"annual\",\"review_trigger\":\"xp_delay\",\"review_enabled\":false,\"discount_enabled\":false,\"notifications_enabled\":false,\"emergency_offer_disabled\":true}"
  second <- runTestApp conn $ getConfig (Just "sticky-test") Nothing Nothing
  expectRight second `shouldReturn` replace "review_enabled" (Bool False) (replace "emergency_offer_disabled" (Bool True) safeConfig)
  enabledJourney <- runTestApp conn $ getConfig (Just "emergency-initially-enabled") Nothing Nothing
  _ <- expectRight enabledJourney
  unsetEnv "ONBOARDING_CONFIG_JSON"
  rolledBack <- runTestApp conn $ getConfig (Just "emergency-initially-enabled") Nothing Nothing
  restored <- expectRight rolledBack
  case restored of
    Object o -> KM.lookup "emergency_offer_disabled" o `shouldBe` Just (Bool False)
    _ -> expectationFailure "Expected config object"
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
 it "reconciles sandbox billing for debug identities and reports exact versus candidate matches" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_events"
  _ <- execute_ conn "DELETE FROM onboarding_revenuecat_events"
  let attempt = replace "environment" (String "development") sample
      identity = replace "event_id" (String "identity-sandbox") $ replace "event_name" (String "identity_linked") $ replace "properties" (object ["revenuecat_app_user_id" .= ("sandbox-customer"::String)]) attempt
      confirmed = replace "event_id" (String "confirmation-sandbox") $ replace "event_name" (String "purchase_client_confirmed") $ replace "properties" (object ["attempt_id" .= ("attempt-one"::String),"transaction_id" .= ("sandbox-transaction"::String),"is_sandbox" .= True]) attempt
  _ <- runTestApp conn (ingestEvents $ object ["events" .= [attempt,identity,confirmed]]) >>= expectRight
  setEnv "REVENUECAT_WEBHOOK_SECRET" "test-only-secret-of-at-least-24-characters"
  let event = object ["event" .= object ["id" .= ("sandbox-billing-event"::String),"type" .= ("INITIAL_PURCHASE"::String),"environment" .= ("SANDBOX"::String),"app_user_id" .= ("sandbox-customer"::String),"product_id" .= ("monthly"::String),"transaction_id" .= ("sandbox-transaction"::String)]]
  _ <- runTestApp conn (revenueCat (Just "Bearer test-only-secret-of-at-least-24-characters") event) >>= expectRight
  exact <- query_ conn "SELECT server_transaction_confirmed FROM onboarding_checkout_attempts WHERE attempt_id='attempt-one'" :: IO [Only Bool]
  exact `shouldBe` [Only True]
  candidates <- query_ conn "SELECT candidate_attempt_count FROM onboarding_checkout_reconciliation WHERE event_id='sandbox-billing-event'" :: IO [Only Int]
  candidates `shouldBe` [Only 1]
  unsetEnv "REVENUECAT_WEBHOOK_SECRET"
 it "records server assignment mismatches without trusting a claimed treatment" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_events"
  _ <- execute conn "INSERT INTO experiment_assignments(onboarding_session_id,experiment_key,variant_key) VALUES (?,?,?)"
    ("test-journey"::String,"onboarding_activation_path_v1"::String,"control"::String)
  let event = replace "properties" (object ["experiment_key" .= ("onboarding_activation_path_v1"::String),"assigned_variant" .= ("one_lesson"::String)]) sample
  _ <- runTestApp conn (ingestEvents $ object ["events" .= [event]]) >>= expectRight
  rows <- query_ conn "SELECT payload->>'server_assignment_variant',payload->>'server_assignment_mismatch' FROM onboarding_events" :: IO [(String,String)]
  rows `shouldBe` [("control","true")]
 it "keeps a confirmed attempt terminal when a pending event arrives late" $ withCleanDb $ \conn -> do
  _ <- execute_ conn "DELETE FROM onboarding_events"
  let confirmed = replace "event_id" (String "terminal-confirmed") $ replace "event_name" (String "purchase_client_confirmed") $ replace "sequence_number" (Number 2) sample
      latePending = replace "event_id" (String "late-pending") $ replace "event_name" (String "purchase_pending") $ replace "sequence_number" (Number 3) sample
  _ <- runTestApp conn (ingestEvents $ object ["events" .= [sample,confirmed]]) >>= expectRight
  _ <- runTestApp conn (ingestEvents $ object ["events" .= [latePending]]) >>= expectRight
  outcomes <- query_ conn "SELECT client_outcome FROM onboarding_checkout_attempts WHERE attempt_id='attempt-one'" :: IO [Only String]
  outcomes `shouldBe` [Only "purchase_client_confirmed"]
