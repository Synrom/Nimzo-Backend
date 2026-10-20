-- Production only. Receive-time cohorts keep offline/misclocked clients from
-- rewriting cohort start. This is 7-day user success, not tap conversion.
WITH starters AS (
 SELECT payload->>'installation_id' installation_id,
   payload->>'platform' platform, min(received_at) started_at
 FROM onboarding_events WHERE payload->>'event_name'='onboarding_started'
   AND payload->>'environment'='production'
   AND NOT EXISTS(SELECT 1 FROM onboarding_identity_links test_identity
    WHERE test_identity.installation_id=onboarding_events.payload->>'installation_id' AND test_identity.environment='sandbox')
 GROUP BY 1,2
), mature AS (SELECT * FROM starters WHERE started_at <= now()-interval '7 days')
SELECT date_trunc('week',m.started_at) cohort_week,m.platform,
 count(*) starters,
 count(*) FILTER (WHERE EXISTS (
   SELECT 1 FROM onboarding_identity_links identity
   JOIN onboarding_revenuecat_identity_aliases alias ON alias.alias=identity.revenuecat_app_user_id
   JOIN onboarding_revenuecat_events r ON r.event_id=alias.event_id
   WHERE identity.installation_id=m.installation_id AND identity.environment='production'
    AND r.payload->>'environment'='PRODUCTION'
    AND r.payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
    AND coalesce((r.payload->>'price')::numeric,0)>0
    AND coalesce(to_timestamp((r.payload->>'purchased_at_ms')::numeric/1000),
      to_timestamp((r.payload->>'event_timestamp_ms')::numeric/1000)) BETWEEN m.started_at AND m.started_at+interval '7 days' 
 )) server_paid_customers_7d
FROM mature m GROUP BY 1,2 ORDER BY 1,2;

-- Transaction cash ledger: trial zeroes remain zero; refund/cancellation events
-- remain visible. RevenueCat prices do not prove bank settlement or net proceeds.
WITH transactions AS (
 SELECT DISTINCT ON (payload->>'environment',coalesce(payload->>'transaction_id',event_id),payload->>'type') *
 FROM onboarding_revenuecat_events
 ORDER BY payload->>'environment',coalesce(payload->>'transaction_id',event_id),payload->>'type',received_at
)
SELECT payload->>'environment' environment,payload->>'type' event_type,
 payload->>'period_type' period_type,payload->>'currency' currency,
 payload->>'product_id' product_id, count(*) event_count,
 sum(CASE WHEN payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
 THEN (payload->>'price_in_purchased_currency')::numeric ELSE 0 END) gross_purchase_currency
FROM transactions
WHERE payload->>'environment'='PRODUCTION'
GROUP BY 1,2,3,4,5;

-- Repeated failures and empty offerings are investigation queues; counts alone
-- must not automatically change commercial configuration.
SELECT platform,product_id,build_number,error_code,count(*) failures
FROM onboarding_checkout_attempts WHERE environment='production'
 AND started_at>=now()-interval '24 hours' AND client_outcome IN ('purchase_failed','checkout_failed')
GROUP BY 1,2,3,4 HAVING count(*)>=3;

-- Paid customers must have a positive store purchase amount, excluding trial
-- starts; use a longer mature window for trial-to-paid economics.
SELECT * FROM onboarding_checkout_reconciliation
WHERE environment='PRODUCTION' AND NOT coalesce(has_client_confirmation,false);

-- Ordered 24-hour activation and D1/D7 lesson or scheduled-card activity. A denominator is mature
-- only once its complete window has elapsed; rates retain numerator + denominator.
-- Client times outside a 24-hour skew window around receipt use receipt instead.
WITH events AS (
 SELECT payload,received_at,
 CASE WHEN (payload->>'occurred_at')::timestamptz BETWEEN received_at-interval '24 hours' AND received_at+interval '5 minutes'
      THEN (payload->>'occurred_at')::timestamptz ELSE received_at END event_at
 FROM onboarding_events WHERE payload->>'environment'='production'
 AND NOT EXISTS(SELECT 1 FROM onboarding_identity_links test_identity
  WHERE test_identity.installation_id=onboarding_events.payload->>'installation_id' AND test_identity.environment='sandbox')
), starts AS (
 SELECT DISTINCT ON(payload->>'installation_id') payload->>'installation_id' installation_id,
 payload->>'platform' platform,payload->>'config_version' config_version,event_at started_at
 FROM events WHERE payload->>'event_name'='onboarding_started'
 ORDER BY payload->>'installation_id',event_at
), metrics AS (
 SELECT s.*, EXISTS(SELECT 1 FROM events e WHERE e.payload->>'installation_id'=s.installation_id
  AND e.payload->>'event_name'='lesson_completed' AND e.event_at BETWEEN s.started_at AND s.started_at+interval '24 hours') activated_24h,
 EXISTS(SELECT 1 FROM events e WHERE e.payload->>'installation_id'=s.installation_id
  AND e.payload->>'event_name' IN ('lesson_completed','review_completed') AND e.event_at>=s.started_at+interval '1 day' AND e.event_at<s.started_at+interval '2 days') active_d1,
 EXISTS(SELECT 1 FROM events e WHERE e.payload->>'installation_id'=s.installation_id
  AND e.payload->>'event_name' IN ('lesson_completed','review_completed') AND e.event_at>=s.started_at+interval '7 days' AND e.event_at<s.started_at+interval '8 days') active_d7
 FROM starts s
)
SELECT date_trunc('week',started_at) cohort_week,platform,config_version,
 count(*) FILTER(WHERE started_at<=now()-interval '24 hours') activation_denominator,
 count(*) FILTER(WHERE started_at<=now()-interval '24 hours' AND activated_24h) activation_numerator,
 count(*) FILTER(WHERE started_at<=now()-interval '2 days') d1_denominator,
 count(*) FILTER(WHERE started_at<=now()-interval '2 days' AND active_d1) d1_numerator,
 count(*) FILTER(WHERE started_at<=now()-interval '8 days') d7_denominator,
 count(*) FILTER(WHERE started_at<=now()-interval '8 days' AND active_d7) d7_numerator
FROM metrics GROUP BY 1,2,3 ORDER BY 1,2,3;

-- Mature D7 trial and paid starts; D35/D60 gross purchase revenue by currency.
-- Server subscription purchase timestamps, not webhook delivery timestamps,
-- define outcomes. Missing timestamps are explicitly excluded from this report.
-- Trial starts stay distinct from positive-price purchases. Refund/cancellation
-- events remain separate; gross revenue is not net proceeds or settlement.
WITH starts AS (
 SELECT payload->>'installation_id' installation_id,payload->>'platform' platform,
 min(received_at) started_at FROM onboarding_events
 WHERE payload->>'event_name'='onboarding_started' AND payload->>'environment'='production'
   AND NOT EXISTS(SELECT 1 FROM onboarding_identity_links test_identity
    WHERE test_identity.installation_id=onboarding_events.payload->>'installation_id' AND test_identity.environment='sandbox')
 GROUP BY 1,2
), billing AS (
 SELECT DISTINCT ON(payload->>'transaction_id',payload->>'type') event_id,payload,
 to_timestamp((payload->>'purchased_at_ms')::numeric/1000) purchase_at
 FROM onboarding_revenuecat_events WHERE payload->>'environment'='PRODUCTION'
 AND payload->>'transaction_id' IS NOT NULL AND payload->>'purchased_at_ms' IS NOT NULL
 ORDER BY payload->>'transaction_id',payload->>'type',received_at
), links AS (
 -- Attribute each transaction once to the earliest linked starter, even if
 -- the same RevenueCat account is used on several installations.
 SELECT DISTINCT ON (b.event_id) i.installation_id,b.event_id,b.payload,b.purchase_at
 FROM billing b JOIN onboarding_revenuecat_identity_aliases alias ON alias.event_id=b.event_id
 JOIN onboarding_identity_links i ON i.revenuecat_app_user_id=alias.alias AND i.environment='production'
 JOIN starts first_start ON first_start.installation_id=i.installation_id AND first_start.started_at<=b.purchase_at
 ORDER BY b.event_id,first_start.started_at,i.installation_id
), user_metrics AS (
 SELECT s.*,EXISTS(SELECT 1 FROM links l WHERE l.installation_id=s.installation_id
  AND l.payload->>'type'='INITIAL_PURCHASE' AND l.payload->>'period_type'='TRIAL'
  AND l.purchase_at BETWEEN s.started_at AND s.started_at+interval '7 days') trial_7d,
 EXISTS(SELECT 1 FROM links l WHERE l.installation_id=s.installation_id
  AND l.payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
  AND coalesce((l.payload->>'price')::numeric,0)>0
  AND l.purchase_at BETWEEN s.started_at AND s.started_at+interval '7 days') paid_7d
 FROM starts s
), windows AS (SELECT 35 day_count UNION ALL SELECT 60), currencies AS (
 SELECT DISTINCT payload->>'currency' currency FROM billing WHERE payload->>'currency' IS NOT NULL
 UNION ALL SELECT 'unknown' WHERE NOT EXISTS(SELECT 1 FROM billing WHERE payload->>'currency' IS NOT NULL)
), mature AS (
 SELECT u.*,w.day_count,c.currency FROM user_metrics u CROSS JOIN windows w CROSS JOIN currencies c
 WHERE u.started_at<=now()-make_interval(days=>w.day_count)
)
SELECT date_trunc('week',m.started_at) cohort_week,m.platform,m.day_count,m.currency,
 count(*) denominator,count(*) FILTER(WHERE m.trial_7d) trial_7d_numerator,
 count(*) FILTER(WHERE m.paid_7d) paid_7d_numerator,
 sum(coalesce(revenue.gross,0)) gross_purchase_currency,
 count(*) FILTER(WHERE coalesce(revenue.gross,0)>0) revenue_customer_numerator,
 sum(coalesce(revenue.gross,0))/nullif(count(*),0) gross_revenue_per_starter,
 sum(coalesce(revenue.cancellations,0)) cancellation_events
FROM mature m LEFT JOIN LATERAL (
 SELECT sum(CASE WHEN l.payload->>'type' IN ('INITIAL_PURCHASE','RENEWAL','NON_RENEWING_PURCHASE')
 THEN coalesce((l.payload->>'price_in_purchased_currency')::numeric,0) ELSE 0 END) gross,
 count(*) FILTER(WHERE l.payload->>'type'='CANCELLATION') cancellations
 FROM links l WHERE l.installation_id=m.installation_id AND l.payload->>'currency'=m.currency
 AND l.purchase_at BETWEEN m.started_at AND m.started_at+make_interval(days=>m.day_count)
) revenue ON true GROUP BY 1,2,3,4 ORDER BY 1,2,3,4;
