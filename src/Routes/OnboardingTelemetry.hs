{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.OnboardingTelemetry where

import App.AppM (AppM)
import App.Error (AppError(..))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isDigit)
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Only(..))
import Repo.Classes (execute, runQuery)
import Servant hiding (Unauthorized)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

type API =
  "onboarding" :> "events" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "onboarding" :> "config" :> QueryParam "onboarding_session_id" T.Text :> QueryParam "build_number" T.Text :> QueryParam "app_version" T.Text :> Get '[JSON] Value
  :<|> "onboarding" :> "revenuecat" :> Header "Authorization" T.Text :> ReqBody '[JSON] Value :> Post '[JSON] Value

server :: ServerT API AppM
server = ingestEvents :<|> getConfig :<|> revenueCat

-- No freeform text, emails, chess positions, receipt data or credentials.
propertyKeys :: [K.Key]
propertyKeys = map K.fromString
  [ "period_start", "period_end", "completeness"
  , "question", "chess_platform", "game_count", "classified_count", "score", "scenario_gain"
  , "opening_name", "eco", "player_color", "loss_count", "selected", "recommended", "recommendation_status"
  , "account_id", "activation_path", "assigned_variant", "assignment", "assignment_source"
  , "assisted", "attempt", "attempt_count", "attempt_id", "available_packages"
  , "billing_period", "campaign", "campaign_deck_id", "campaign_id", "card_id"
  , "completed", "config_source", "count", "currency", "deck_id"
  , "default_selection", "destination", "direction", "discount_enabled", "duration_ms"
  , "effective_config_version", "eligibility", "eligible", "emergency_override", "enabled"
  , "entitlement_active", "entitlement_id", "entry_point", "environment", "error_category"
  , "error_code", "experiment_key", "experiment_version", "expiration_at", "expires_at"
  , "fallback", "foreground_ms", "granted", "hint_count", "invoked"
  , "is_new", "is_sandbox", "is_trial", "latency_ms", "lesson_id"
  , "load_state", "mistake_count", "mode", "module_available", "next_review_at"
  , "notifications_enabled", "offer_id", "offering_id", "offering_identifier", "original_app_user_id"
  , "outcome", "package_id", "pending", "permission", "plan"
  , "policy_version", "previous_screen", "price", "price_version", "product_id"
  , "purpose", "query_category", "reason", "recalled", "rendered_policy"
  , "rendered_variant", "response", "restored", "result_count", "retry"
  , "retry_count", "retry_of", "retry_of_attempt_id", "revenuecat_app_user_id", "review_enabled"
  , "review_trigger", "route", "scheduled", "scheduled_at", "selected_plan"
  , "setup_failed", "skip_reason", "source", "stage", "status"
  , "step", "store", "storefront", "streak", "streak_increased"
  , "transaction_id", "trial_days", "trial_duration", "trial_duration_days", "trial_eligibility"
  , "trigger", "unaided", "variant", "wrong_move_count", "xp"
  ]

boundedText :: Int -> Value -> Parser T.Text
boundedText limit = withText "bounded string" $ \s ->
  if T.null (T.strip s) || T.length s > limit then fail "Invalid string length" else pure s

validateEvent :: Value -> Parser Value
validateEvent value = withObject "event" (\o -> do
  version <- o .: "schema_version" :: Parser Int
  unless (version == 1) $ fail "Unsupported event schema"
  mapM_ (\key -> o .: key >>= boundedText 128 >> pure ())
    ["event_id", "installation_id", "onboarding_session_id", "visit_id", "flow_version", "app_version", "build_number", "os_version", "config_version"]
  name <- o .: "event_name" >>= boundedText 80
  unless (T.all (\c -> isAsciiLower c || isDigit c || c == '_') name) $ fail "Invalid event name"
  _ <- o .: "occurred_at" :: Parser UTCTime
  sequenceNumber <- o .: "sequence_number" :: Parser Integer
  unless (sequenceNumber >= 0 && sequenceNumber <= 9007199254740991) $ fail "Invalid sequence"
  platform <- o .: "platform" :: Parser T.Text
  unless (platform `elem` ["android", "ios"]) $ fail "Invalid platform"
  environment <- o .: "environment" :: Parser T.Text
  unless (environment `elem` ["development", "sandbox", "production"]) $ fail "Invalid environment"
  screen <- o .:? "screen_id" :: Parser (Maybe Value)
  mapM_ (boundedText 128) screen
  properties <- o .: "properties" :: Parser Object
  unless (KM.size properties <= 40) $ fail "Too many properties"
  mapM_ (\(key, val) -> do
    unless (key `elem` propertyKeys) $ fail "Unknown property"
    case val of
      String s -> unless (T.length s <= 256) $ fail "Property too long"
      Number n -> unless (abs n <= 1e15) $ fail "Property number out of bounds"
      Bool _ -> pure ()
      Null -> pure ()
      _ -> fail "Only scalar properties are supported") (KM.toList properties)
  -- Discard unknown envelope fields, even if a client mistakenly sends PII.
  pure $ Object $ KM.filterWithKey (\k _ -> k `elem`
    ["event_id", "schema_version", "event_name", "occurred_at", "sequence_number", "installation_id", "onboarding_session_id", "visit_id", "screen_id", "flow_version", "app_version", "build_number", "platform", "os_version", "environment", "config_version", "properties"]) o) value

validateBatch :: Value -> Parser [Value]
validateBatch = withObject "batch" $ \o -> do
  events <- o .: "events" :: Parser [Value]
  unless (not (null events) && length events <= 50) $ fail "Batch must contain 1 to 50 events"
  unless (BL.length (encode events) <= 131072) $ fail "Batch exceeds 128 KiB"
  mapM validateEvent events

parseOrReject :: Parser a -> AppM a
parseOrReject parser = either (const $ throwError $ BadRequest "Invalid onboarding payload.") pure (parseEither (const parser) Null)

-- A single SQL statement atomically inserts the entire validated batch. Retries
-- acknowledge existing IDs too; their original append-only payload is retained.
ingestEvents :: Value -> AppM Value
ingestEvents value = do
  events <- parseOrReject $ validateBatch value
  _ <- execute
    "INSERT INTO onboarding_events(event_id, payload) SELECT e->>'event_id', e || jsonb_build_object('server_assignment_variant', a.variant_key, 'server_assignment_missing', jsonb_exists(e->'properties','experiment_key') AND a.variant_key IS NULL, 'server_assignment_mismatch', coalesce(e->'properties'->>'assigned_variant' <> a.variant_key, false)) FROM jsonb_array_elements(?::jsonb) e LEFT JOIN experiment_assignments a ON a.onboarding_session_id=e->>'onboarding_session_id' AND a.experiment_key=e->'properties'->>'experiment_key' ON CONFLICT(event_id) DO NOTHING"
    (Only $ encode events)
  let ids = nub [eventId | Object event <- events, Just (String eventId) <- [KM.lookup "event_id" event]]
  pure $ object ["accepted_event_ids" .= ids]

safeConfig :: Value
safeConfig = object
  [ "schema_version" .= (1 :: Int), "config_version" .= ("onboarding-v1-default" :: T.Text)
  , "activation_path" .= ("control" :: T.Text), "minimal_questions" .= False
  , "featured_source" .= ("neutral" :: T.Text), "default_plan" .= ("current" :: T.Text)
  , "review_trigger" .= ("streak_complete" :: T.Text), "review_enabled" .= True
  , "discount_enabled" .= False, "notifications_enabled" .= True, "emergency_offer_disabled" .= False
  ]

validateConfig :: Value -> Parser Value
validateConfig = withObject "config" $ \o -> do
  version <- o .: "schema_version" :: Parser Int
  unless (version == 1) $ fail "Unsupported config schema"
  _ <- o .: "config_version" >>= boundedText 128
  mapM_ (\(key, values) -> do
    value <- o .: key :: Parser T.Text
    unless (value `elem` values) $ fail "Unsupported setting")
    [ ("activation_path", ["control", "one_lesson"])
    , ("featured_source", ["neutral", "instagram", "tiktok", "youtube"])
    , ("default_plan", ["current", "monthly", "annual"])
    , ("review_trigger", ["streak_complete", "xp_delay"]) ]
  mapM_ (\key -> (o .: key :: Parser Bool) >> pure ())
    ["minimal_questions", "review_enabled", "discount_enabled", "notifications_enabled", "emergency_offer_disabled"]
  offering <- o .:? "offering_identifier" :: Parser (Maybe T.Text)
  -- Offering changes require a deployment-owned allowlist, checked in getConfig.
  mapM_ (\s -> boundedText 128 (String s)) offering
  pure $ Object $ KM.filterWithKey (\key _ -> key `elem` ("offering_identifier" : case safeConfig of Object defaults -> KM.keys defaults; _ -> [])) o

getConfig :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppM Value
getConfig sessionId build _version = do
  raw <- liftIO $ lookupEnv "ONBOARDING_CONFIG_JSON"
  allowed <- liftIO $ lookupEnv "ONBOARDING_ALLOWED_OFFERINGS"
  minimumBuild <- liftIO $ lookupEnv "ONBOARDING_MIN_BUILD_NUMBER"
  let parsed = raw >>= decodeStrict' . T.encodeUtf8 . T.pack
      validated = parsed >>= either (const Nothing) Just . parseEither validateConfig
      offeringAllowed (Object o) = case KM.lookup "offering_identifier" o of
        Nothing -> True
        Just (String s) -> s `elem` maybe [] (T.splitOn "," . T.pack) allowed
        _ -> False
      offeringAllowed _ = False
  let current = case validated of Just config | offeringAllowed config -> config; _ -> safeConfig
  let buildSupported = case minimumBuild of
        Nothing -> True
        Just raw -> case (readMaybe raw :: Maybe Integer, build >>= readMaybe . T.unpack :: Maybe Integer) of
          (Just minimumNumber, Just clientNumber) -> clientNumber >= minimumNumber
          _ -> False
  case (buildSupported, sessionId) of
    (False, _) -> pure safeConfig
    (_, Just sid) | not (T.null sid) && T.length sid <= 128 -> do
      rows <- runQuery
        "INSERT INTO onboarding_config_assignments(onboarding_session_id, config) VALUES (?, ?::jsonb) ON CONFLICT(onboarding_session_id) DO UPDATE SET onboarding_session_id=EXCLUDED.onboarding_session_id RETURNING config"
        (sid, encode current) :: AppM [Only Value]
      let pinned = case rows of Only config : _ -> config; _ -> safeConfig
          emergency = case current of Object o -> KM.lookup "emergency_offer_disabled" o == Just (Bool True); _ -> False
          reviewDisabled = case current of Object o -> KM.lookup "review_enabled" o == Just (Bool False); _ -> False
      pure $ case pinned of
        Object o -> Object $ (if reviewDisabled then KM.insert "review_enabled" (Bool False) else id) $
          KM.insert "emergency_offer_disabled" (Bool emergency) o
        _ -> pinned
    _ -> pure current

revenueCat :: Maybe T.Text -> Value -> AppM Value
revenueCat authorization value = do
  secret <- liftIO $ lookupEnv "REVENUECAT_WEBHOOK_SECRET"
  unless (maybe False (\s -> length s >= 24 && authorization == Just ("Bearer " <> T.pack s)) secret) $
    throwError $ Unauthorized "Invalid webhook authorization."
  sanitized <- parseOrReject $ withObject "webhook" (\wrapper -> do
    event <- wrapper .: "event" :: Parser Object
    _ <- event .: "id" >>= boundedText 128
    eventType <- event .: "type" >>= boundedText 80
    environment <- event .: "environment" :: Parser T.Text
    unless (environment `elem` ["SANDBOX", "PRODUCTION"]) $ fail "Unknown billing environment"
    appUser <- event .:? "app_user_id" :: Parser (Maybe Value)
    if eventType == "TRANSFER" then pure () else unless (appUser /= Nothing) $ fail "Missing app user identity"
    mapM_ (boundedText 256) appUser
    mapM_ (\key -> do
      values <- event .:? key :: Parser (Maybe [Value])
      mapM_ (\ids -> do
        unless (length ids <= 100) $ fail "Too many aliases"
        mapM_ (boundedText 256) ids) values) ["aliases", "transferred_from", "transferred_to"]
    mapM_ (\key -> do
      text <- event .:? key :: Parser (Maybe Value)
      mapM_ (boundedText 256) text) ["original_app_user_id", "product_id", "transaction_id", "original_transaction_id", "period_type", "store", "currency", "cancel_reason", "expiration_reason"]
    mapM_ (\key -> do
      number <- event .:? key :: Parser (Maybe Double)
      unless (maybe True (\n -> not (isNaN n || isInfinite n) && abs n <= 1e16) number) $ fail "Invalid financial number")
      ["purchased_at_ms", "expiration_at_ms", "event_timestamp_ms", "price", "price_in_purchased_currency", "tax_percentage", "commission_percentage", "takehome_percentage"]
    let keys = ["id", "type", "app_user_id", "original_app_user_id", "aliases", "transferred_from", "transferred_to", "product_id", "transaction_id", "original_transaction_id", "period_type", "purchased_at_ms", "expiration_at_ms", "event_timestamp_ms", "environment", "store", "currency", "price", "price_in_purchased_currency", "tax_percentage", "commission_percentage", "takehome_percentage", "cancel_reason", "expiration_reason"]
        clean = Object $ KM.filterWithKey (\key _ -> key `elem` keys) event
    unless (BL.length (encode clean) <= 32768) $ fail "Webhook exceeds limit"
    pure clean) value
  _ <- execute "INSERT INTO onboarding_revenuecat_events(event_id, payload) VALUES (?::jsonb->>'id', ?::jsonb) ON CONFLICT(event_id) DO NOTHING" (encode sanitized, encode sanitized)
  pure $ object ["accepted" .= True]
