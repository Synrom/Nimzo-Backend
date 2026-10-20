# Onboarding telemetry and checkout operations

Apply `initdb/28_onboarding_telemetry.sql` then
`initdb/29_onboarding_activation_experiment.sql` before shipping the client. It is
additive and repeatable. Existing progress and experiment routes remain intact.
The public telemetry middleware rejects bodies larger than 256 KiB before
JSON decoding, including chunked bodies. Per-peer fixed one-minute limits are
120 events requests, 60 config requests and 600 webhook requests; excess returns
429 with Retry-After: 60. Its map prunes expired entries and caps at 4,096 keys.
Legacy progress routes are unaffected. Forwarded-IP headers are not trusted;
behind a reverse proxy configure matching public per-client edge limits and a
suitable aggregate API limit for expected traffic.

New tables retain the first payload per ID; no update/delete API is exposed.
Run migrations with a migration owner and grant the API role SELECT/INSERT on
event tables. Do not grant it UPDATE/DELETE on those tables. Establish a
pseudonymous-data retention policy before production rollout.

`POST /onboarding/events` accepts `{ "events": [...] }` with 1–50 version-1
envelopes and at most 128 KiB of encoded event data. Invalid batches return 400
and write nothing. Replies contain `accepted_event_ids`, including retries.
Event IDs are global; repeated IDs retain their original payload. Envelope IDs,
versions and OS strings are bounded to 128 characters; properties are an
explicit allowlist, scalar-only, at most 40 fields and 256 characters per string.
Unknown envelope fields are discarded. The server adds the matched sticky
`server_assignment_variant`, `server_assignment_missing` and
`server_assignment_mismatch` fields, so client claims are not treated as verified
assignment and fallback exposures remain distinguishable. Unknown properties reject the batch.
The mobile outbox and server allowlists must evolve together.

Event time is device-reported and may be skewed or arrive offline; retain both
`occurred_at` and server `received_at`. Installation ID is pseudonymous; journey
ID persists until onboarding ends; visit ID changes per process launch; event
sequence supports ordering/gap diagnosis. None is a unique-person guarantee.
`identity_linked` associates installation/journey with RevenueCat app user ID and
optional account ID in reporting views. These anonymous claims never authorize
account access. Do not send an email, receipt, card data or arbitrary error text.

## Configuration

`GET /onboarding/config?onboarding_session_id=...&build_number=...&app_version=...`
returns a schema-1 config. With a journey ID, the effective config is stored once
in `onboarding_config_assignments`, so changing the deployment config affects
new cohorts. The client additionally pins local journey settings and preserves
drafts. `emergency_offer_disabled=true` and `review_enabled=false` overlay already pinned journeys. It
never changes a price or selected plan in an active purchase.

Safe defaults: control activation, full question flow, neutral library, existing
plan selection, streak-completion review, reviews and notification policy enabled, discounts disabled. No experiment allocation changes automatically. The activation experiment is
registered as `onboarding_activation_path_v1` with control weight 100 and
one_lesson weight 0; the existing bootstrap API negotiates supported variants
and retains sticky assignments. Operators must explicitly approve any later
allocation change. To test a
supported variant, set `ONBOARDING_CONFIG_JSON` to a complete validated config
with a new `config_version`, restart the API and start a new journey. Invalid
JSON/types/enums revert to safe defaults. Optional `offering_identifier` must
also appear in comma-separated `ONBOARDING_ALLOWED_OFFERINGS`. No unauthenticated
config mutation endpoint exists; use deployment-owned environment configuration.
Set `ONBOARDING_MIN_BUILD_NUMBER` to the minimum compatible numeric native build.
Older, missing or malformed build identifiers receive safe defaults without
creating a treatment assignment. This initial endpoint serves only the shipped
v1 settings; platform-specific ranges should use separate deployment config.
It does not infer support from a lexicographic marketing version. Deploy compatible app capabilities before
selecting one_lesson or changing offering. Existing clients without this endpoint
continue their previous behavior.

Record each configuration change with owner, exact before/after JSON, intended
cohort and supported builds, previous config version and rollback target. To
rollback new cohorts restore previous JSON. Existing cohorts remain pinned;
use emergency disable for a broken offer. Keep the RevenueCat trial experiment
allocation stable; config is not permission to silently redefine it.

## RevenueCat ledger

Configure RevenueCat webhook URL `/onboarding/revenuecat` and Authorization
header `Bearer <secret>`. Set `REVENUECAT_WEBHOOK_SECRET` to the same value (at
least 24 characters); missing/short configuration rejects every request.
Invalid payloads return 400; unauthorized requests return 401; valid duplicate
webhooks return success. Never expose the secret in the app. Only selected
billing/identity fields are retained; subscriber attributes and receipts are
removed. SANDBOX/PRODUCTION are explicit and retained; do not merge test revenue
with production. Keep test app users sandbox-only in RevenueCat.

`onboarding_revenuecat_events` is an append-only financial lifecycle ledger,
not a premium boolean. Trials can have zero revenue. Refund, cancellation,
expiration and transfer notifications remain separate events. `price` and
`price_in_purchased_currency` are not bank settlement/net revenue. Aliases,
original identities and transfer arrays are expanded by
`onboarding_revenuecat_identity_aliases`; identity links are not deleted on login.

`onboarding_checkout_attempts` has one row per attempt ID, the latest client
outcome, retry reference, billing/build/config metadata and a 24-hour unresolved
flag. A server confirmation requires a matching reported transaction ID; a
RevenueCat identity/offer/time resemblance is not sufficient to assign a payment
to an exact attempt. `onboarding_checkout_reconciliation` lists those candidate
links, including payments with missing client callbacks and ambiguous retries. Pending is a distinct outcome and never an immediate
failure. Restore events remain separate and do not count as new purchases.
Run `docs/reports/onboarding_cohorts.sql` for mature seven-day install cohorts,
production currency-separated gross purchase totals and repeated-code failure
queues, 24-hour activation, D1/D7 lesson or scheduled-card review activity, and mature D35/D60 gross
revenue with trial/paid numerators and cohort denominators. Billing windows use
RevenueCat purchased_at_ms; missing purchase timestamps are excluded from mature
revenue metrics. Received-time starter cohorts are retained to avoid backdating
an install when delayed client telemetry arrives. Initial trials do not prove paid conversion. Net proceeds and settlement require financial reconciliation outside these
gross-revenue queries. Refund/cancellation lifecycle events stay separately
visible and are not blindly subtracted as if every cancellation were a refund.
No automatic winner promotion or alert side effects run in the API.

## Local verification

`cabal test test:Tests --test-options='--match OnboardingTelemetry'` covers validation,
idempotent retries, config pin/override, fail-closed auth and private-field
removal. Explicit `source=neutral` on `/deck/featured` aggregates featured decks across
sources; omitting source keeps the legacy TikTok default.
`scripts/seed_onboarding_qa.sql` is an opt-in local fixture and never a production
migration. It creates one fictional public e4 e5 Nf3 lesson.

The repository test DB defaults to
`postgresql://postgres:postgres@localhost:5432/gambit_test`.

Run the complete DB-backed suite serially (`cabal test test:Tests
--test-options='--jobs=1'`): shared fixture cleanup and environment configuration
must not run concurrently. Focused telemetry tests include terminal checkout
ordering: a late pending callback cannot overwrite an earlier confirmation.

Revenue reports deduplicate by billing environment, transaction and event type.
The D35/D60 report attributes a transaction to the earliest linked onboarding
starter once, avoiding revenue duplication when an account appears on several
installations. This is an explicit reporting attribution rule, not proof of
acquisition causality. Missing identity links remain in the reconciliation queue;
revenue is reported as gross store purchase currency, not net realized proceeds.
