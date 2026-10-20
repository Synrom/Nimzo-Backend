# CRUD Backend for Nimzo

It's a normal REST-API that connects to a postgresql database.
It also implements a Watermelon compatible endpoint.

The whole backend is written in Haskell using Servant.

## Deck search migration

Before deploying the search relevance changes, apply
`initdb/27_deck_search_relevance.sql` with `psql -v ON_ERROR_STOP=1 -f`
against the application database in autocommit mode (no `--single-transaction`).
It builds two public-deck GIN expression indexes concurrently, without rewriting
the decks table. Fresh Docker databases apply it automatically.

Both search endpoints rank normalized exact titles, title prefixes, whole
phrases in titles, other title matches, then matches requiring descriptions.
Normalization ignores case, punctuation and repeated whitespace. Prefix matching
is retained for every query word. English stemming remains available in full
search; words discarded by English must also match literally in the title or
description. Downloads, rating and deck ID break relevance ties. Instant search
returns at most ten title matches; full search includes description matches.

## Streak notification worker

Apply `initdb/23_streak_live_activity_notifications.sql` and
`initdb/24_locally_scheduled_live_activities.sql` in order, then run the API
and the durable notification worker as separate processes:

```sh
cabal run App
cabal run StreakNotificationWorker
```

The worker requires `APNS_TEAM_ID`, `APNS_KEY_ID`,
`APNS_PRIVATE_KEY_PATH`, and `APNS_BUNDLE_ID`. Set
`APNS_DEFAULT_ENVIRONMENT` to `sandbox` or `production`; each stored
ActivityKit token's own environment remains authoritative for delivery.

## SQL test coverage

Run the test suite with HPC and require every SQL statement line in `src/` to
be executed:

```sh
./scripts/check_sql_coverage.py
```

The command exits non-zero and prints each uncovered source location, making
it suitable for CI. Pass `--skip-tests` to inspect the newest existing HPC
report without rerunning the suite.

## Onboarding conversion telemetry

Apply `initdb/28_onboarding_telemetry.sql` then
`initdb/29_onboarding_activation_experiment.sql` before releasing new clients. See
[configuration, RevenueCat webhooks and checkout reporting](docs/onboarding-telemetry.md)
for the endpoint contract, rollout/rollback and local verification.
