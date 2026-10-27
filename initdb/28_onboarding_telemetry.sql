-- Additive: apply before the telemetry-capable mobile release.
CREATE TABLE IF NOT EXISTS onboarding_revenuecat_events (
  event_id varchar(128) PRIMARY KEY,
  payload jsonb NOT NULL,
  received_at timestamptz NOT NULL DEFAULT now(),
  CHECK (payload->>'id' = event_id)
);
CREATE INDEX IF NOT EXISTS onboarding_revenuecat_identity_idx ON onboarding_revenuecat_events ((payload->>'app_user_id'));
