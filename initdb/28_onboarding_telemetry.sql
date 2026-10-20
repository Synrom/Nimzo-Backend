-- Additive: apply before the telemetry-capable mobile release.
CREATE TABLE IF NOT EXISTS onboarding_events (
  event_id varchar(128) PRIMARY KEY,
  payload jsonb NOT NULL,
  received_at timestamptz NOT NULL DEFAULT now(),
  CHECK (payload->>'event_id' = event_id),
  CHECK (payload->>'schema_version' = '1')
);
CREATE INDEX IF NOT EXISTS onboarding_events_journey_idx ON onboarding_events ((payload->>'onboarding_session_id'), received_at);
CREATE INDEX IF NOT EXISTS onboarding_events_attempt_idx ON onboarding_events ((payload->'properties'->>'attempt_id')) WHERE payload->'properties' ? 'attempt_id';
CREATE INDEX IF NOT EXISTS onboarding_events_name_idx ON onboarding_events ((payload->>'event_name'), received_at);
CREATE TABLE IF NOT EXISTS onboarding_revenuecat_events (
  event_id varchar(128) PRIMARY KEY,
  payload jsonb NOT NULL,
  received_at timestamptz NOT NULL DEFAULT now(),
  CHECK (payload->>'id' = event_id)
);
CREATE INDEX IF NOT EXISTS onboarding_revenuecat_identity_idx ON onboarding_revenuecat_events ((payload->>'app_user_id'));
-- IDs are pseudonymous, not authenticated account ownership claims. A malicious
-- anonymous client cannot obtain account access through this reporting mapping.
CREATE OR REPLACE VIEW onboarding_identity_links AS
SELECT DISTINCT payload->>'installation_id' AS installation_id,
  payload->>'onboarding_session_id' AS onboarding_session_id,
  payload->'properties'->>'revenuecat_app_user_id' AS revenuecat_app_user_id,
  CASE WHEN payload->>'environment'='development' THEN 'sandbox'
       WHEN coalesce((SELECT (outcome.payload->'properties'->>'is_sandbox')::boolean FROM onboarding_events outcome
        WHERE outcome.payload->>'onboarding_session_id'=onboarding_events.payload->>'onboarding_session_id'
         AND outcome.payload->'properties' ? 'is_sandbox' ORDER BY outcome.received_at DESC LIMIT 1),false) THEN 'sandbox'
       ELSE payload->>'environment' END AS environment
FROM onboarding_events WHERE payload->'properties'->>'revenuecat_app_user_id' IS NOT NULL;
-- Preserve transfers and aliases as separate identity links. Never overwrite a
-- customer with their latest subscription state or count an entitlement as cash.
CREATE OR REPLACE VIEW onboarding_revenuecat_identity_aliases AS
SELECT event_id, payload->>'environment' AS environment,
  payload->>'app_user_id' AS app_user_id, payload->>'app_user_id' AS alias
FROM onboarding_revenuecat_events
UNION
SELECT event_id, payload->>'environment', payload->>'app_user_id', payload->>'original_app_user_id'
FROM onboarding_revenuecat_events WHERE payload->>'original_app_user_id' IS NOT NULL
UNION
SELECT event_id, payload->>'environment', payload->>'app_user_id', alias
FROM onboarding_revenuecat_events CROSS JOIN LATERAL jsonb_array_elements_text(
  CASE WHEN jsonb_typeof(payload->'aliases')='array' THEN payload->'aliases' ELSE '[]'::jsonb END) alias
UNION
SELECT event_id, payload->>'environment', payload->>'app_user_id', alias
FROM onboarding_revenuecat_events CROSS JOIN LATERAL jsonb_array_elements_text(
  CASE WHEN jsonb_typeof(payload->'transferred_from')='array' THEN payload->'transferred_from' ELSE '[]'::jsonb END) alias
UNION
SELECT event_id, payload->>'environment', payload->>'app_user_id', alias
FROM onboarding_revenuecat_events CROSS JOIN LATERAL jsonb_array_elements_text(
  CASE WHEN jsonb_typeof(payload->'transferred_to')='array' THEN payload->'transferred_to' ELSE '[]'::jsonb END) alias;
CREATE OR REPLACE VIEW onboarding_checkout_attempts AS
WITH attempts AS (
 SELECT DISTINCT ON (payload->'properties'->>'attempt_id')
   payload->'properties'->>'attempt_id' AS attempt_id, payload,
   (payload->>'occurred_at')::timestamptz AS started_at
 FROM onboarding_events
 WHERE payload->'properties' ? 'attempt_id'
   AND payload->>'event_name' IN ('purchase_attempted','purchase_attempt_started','purchase_started','checkout_started')
 ORDER BY payload->'properties'->>'attempt_id', received_at
), outcomes AS (
 SELECT DISTINCT ON (payload->'properties'->>'attempt_id')
   payload->'properties'->>'attempt_id' AS attempt_id,
   payload->>'event_name' AS outcome, payload->'properties'->>'error_code' AS error_code,
   received_at AS outcome_at
 FROM onboarding_events
 WHERE payload->>'event_name' IN ('purchase_client_confirmed','purchase_confirmed','purchase_cancelled','purchase_failed','purchase_pending','purchase_entitlement_confirmed','checkout_confirmed','checkout_cancelled','checkout_failed','checkout_pending')
 ORDER BY payload->'properties'->>'attempt_id',
  CASE WHEN payload->>'event_name' IN ('purchase_client_confirmed','purchase_confirmed','purchase_entitlement_confirmed','checkout_confirmed') THEN 0 ELSE 1 END,
  (payload->>'occurred_at')::timestamptz DESC, (payload->>'sequence_number')::bigint DESC, received_at DESC
)
SELECT a.attempt_id, a.started_at, o.outcome_at,
  coalesce(o.outcome,'no_outcome') AS client_outcome, o.error_code,
  a.payload->>'installation_id' AS installation_id,
  a.payload->>'onboarding_session_id' AS onboarding_session_id,
  a.payload->>'platform' AS platform, a.payload->>'build_number' AS build_number,
  a.payload->>'config_version' AS config_version,
  CASE WHEN a.payload->>'environment'='development' OR EXISTS(SELECT 1 FROM onboarding_events sandbox
    WHERE sandbox.payload->'properties'->>'attempt_id'=a.attempt_id AND sandbox.payload->'properties'->>'is_sandbox'='true')
    THEN 'sandbox' ELSE a.payload->>'environment' END AS environment,
  a.payload->'properties'->>'product_id' AS product_id,
  a.payload->'properties'->>'price' AS offered_price,
  a.payload->'properties'->>'currency' AS currency,
  coalesce(a.payload->'properties'->>'trial_eligibility',a.payload->'properties'->>'eligibility') AS trial_eligibility,
  coalesce(a.payload->'properties'->>'retry_of_attempt_id',a.payload->'properties'->>'retry_of') AS retry_of_attempt_id,
  o.outcome IS NULL AND a.started_at < now()-interval '24 hours' AS unresolved_24h,
  EXISTS (
    SELECT 1 FROM onboarding_events confirmation
    JOIN onboarding_revenuecat_events r ON
      r.payload->>'transaction_id'=confirmation.payload->'properties'->>'transaction_id'
      AND lower(r.payload->>'environment')=CASE WHEN confirmation.payload->'properties'->>'is_sandbox'='true' OR a.payload->>'environment'<>'production' THEN 'sandbox' ELSE 'production' END
    WHERE confirmation.payload->'properties'->>'attempt_id'=a.attempt_id
      AND r.payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
  ) AS server_transaction_confirmed
FROM attempts a LEFT JOIN outcomes o USING(attempt_id);
CREATE TABLE IF NOT EXISTS onboarding_config_assignments (
  onboarding_session_id varchar(128) PRIMARY KEY,
  config jsonb NOT NULL,
  assigned_at timestamptz NOT NULL DEFAULT now()
);
CREATE OR REPLACE VIEW onboarding_account_links AS
SELECT DISTINCT payload->>'installation_id' AS installation_id,
 payload->>'onboarding_session_id' AS onboarding_session_id,
 payload->'properties'->>'account_id' AS account_id,
 payload->>'environment' AS environment
FROM onboarding_events WHERE payload->>'event_name'='identity_linked'
 AND payload->'properties'->>'account_id' IS NOT NULL;
-- Missing callback investigation: identity+product+time matches are candidates,
-- never exact attempt confirmations. Multiple retries can match one payment.
CREATE OR REPLACE VIEW onboarding_checkout_reconciliation AS
WITH transactions AS (
 SELECT DISTINCT ON (payload->>'transaction_id',payload->>'environment')
   event_id,payload,received_at
 FROM onboarding_revenuecat_events
 WHERE payload->>'transaction_id' IS NOT NULL
  AND payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
 ORDER BY payload->>'transaction_id',payload->>'environment',received_at
)
SELECT r.event_id,r.payload->>'transaction_id' AS transaction_id,
 r.payload->>'environment' AS environment,r.payload->>'product_id' AS product_id,
 r.received_at AS webhook_received_at,
 count(DISTINCT a.attempt_id) AS candidate_attempt_count,
 array_agg(DISTINCT a.attempt_id) FILTER(WHERE a.attempt_id IS NOT NULL) AS candidate_attempt_ids,
 bool_or(a.client_outcome IN ('purchase_client_confirmed','purchase_entitlement_confirmed','purchase_confirmed','checkout_confirmed')) AS has_client_confirmation,
 coalesce(to_timestamp((r.payload->>'purchased_at_ms')::numeric/1000), r.received_at) AS purchased_at
FROM transactions r
LEFT JOIN onboarding_revenuecat_identity_aliases alias ON alias.event_id=r.event_id
LEFT JOIN onboarding_identity_links identity ON identity.revenuecat_app_user_id=alias.alias
 AND identity.environment=lower(r.payload->>'environment')
LEFT JOIN onboarding_checkout_attempts a ON a.installation_id=identity.installation_id
 AND a.product_id=r.payload->>'product_id' AND a.environment=identity.environment
 AND a.started_at BETWEEN coalesce(to_timestamp((r.payload->>'purchased_at_ms')::numeric/1000),r.received_at)-interval '24 hours' AND coalesce(to_timestamp((r.payload->>'purchased_at_ms')::numeric/1000),r.received_at)+interval '5 minutes'
GROUP BY r.event_id,r.payload,r.received_at;
