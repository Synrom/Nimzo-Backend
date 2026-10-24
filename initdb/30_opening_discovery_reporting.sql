-- Client-observed public API attempts, never server-verified account identities.
-- Deduplicate by journey + attempt, even if a buggy client changes event IDs.
CREATE OR REPLACE VIEW onboarding_opening_attempts AS
SELECT payload->>'environment' AS environment,
       payload->>'onboarding_session_id' AS onboarding_session_id,
       payload->'properties'->>'attempt_id' AS attempt_id,
       max(payload->'properties'->>'chess_platform') AS chess_platform,
       bool_or(payload->>'event_name' = 'opening_account_attempt') AS started,
       bool_or(payload->>'event_name' = 'opening_account_result' AND payload->'properties'->>'outcome' = 'existent') AS existent,
       bool_or(payload->>'event_name' = 'opening_account_result' AND payload->'properties'->>'outcome' = 'missing') AS nonexistent,
       bool_or(payload->>'event_name' = 'opening_account_result' AND payload->'properties'->>'outcome' NOT IN ('existent','missing')) AS validation_error,
       bool_or(payload->>'event_name' = 'opening_analysis_completed') AS analysis_completed,
       bool_or(payload->>'event_name' = 'opening_analysis_failed') AS analysis_failed,
       bool_or(payload->>'event_name' IN ('opening_analysis_cancelled','opening_profile_cancelled')) AS cancelled,
       bool_or(payload->>'event_name' = 'opening_profile_viewed') AS profile_viewed,
       bool_or(payload->>'event_name' = 'opening_profile_confirmed') AS profile_confirmed,
       bool_or(payload->>'event_name' = 'opening_profile_edited') AS profile_edited,
       bool_or(payload->>'event_name' = 'opening_profile_failed') AS profile_failed
FROM onboarding_events
WHERE payload->>'event_name' LIKE 'opening_%'
  AND payload->'properties'->>'attempt_id' IS NOT NULL
GROUP BY 1,2,3;

CREATE OR REPLACE VIEW onboarding_opening_attempt_counts AS
SELECT environment, chess_platform,
       count(*) FILTER (WHERE started) AS attempts,
       count(*) FILTER (WHERE existent) AS existent_attempts,
       count(*) FILTER (WHERE nonexistent) AS nonexistent_attempts,
       count(*) FILTER (WHERE validation_error) AS validation_errors,
       count(*) FILTER (WHERE analysis_completed) AS completed_analyses,
       count(*) FILTER (WHERE analysis_failed) AS failed_analyses,
       count(*) FILTER (WHERE cancelled) AS cancellations,
       count(*) FILTER (WHERE profile_viewed) AS profiles_viewed,
       count(*) FILTER (WHERE profile_confirmed) AS profiles_confirmed,
       count(*) FILTER (WHERE profile_edited) AS profiles_edited,
       count(*) FILTER (WHERE profile_failed) AS profile_failures
FROM onboarding_opening_attempts GROUP BY 1,2;
