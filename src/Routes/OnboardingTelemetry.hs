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
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Only(..))
import Repo.Classes (execute)
import Servant hiding (Unauthorized)
import System.Environment (lookupEnv)

type API =
  "onboarding" :> "revenuecat" :> Header "Authorization" T.Text :> ReqBody '[JSON] Value :> Post '[JSON] Value

server :: ServerT API AppM
server = revenueCat

boundedText :: Int -> Value -> Parser T.Text
boundedText limit = withText "bounded string" $ \s ->
  if T.null (T.strip s) || T.length s > limit then fail "Invalid string length" else pure s

parseOrReject :: Parser a -> AppM a
parseOrReject parser = either (const $ throwError $ BadRequest "Invalid onboarding payload.") pure (parseEither (const parser) Null)

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
