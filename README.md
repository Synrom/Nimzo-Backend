# CRUD Backend for Nimzo

It's a normal REST-API that connects to a postgresql database.
It also implements a Watermelon compatible endpoint.

The whole backend is written in Haskell using Servant.

## Streak notification worker

Apply `initdb/23_streak_live_activity_notifications.sql`, then run the API and
the durable notification worker as separate processes:

```sh
cabal run App
cabal run StreakNotificationWorker
```

The worker requires `APNS_TEAM_ID`, `APNS_KEY_ID`,
`APNS_PRIVATE_KEY_PATH`, and `APNS_BUNDLE_ID`. Set
`APNS_DEFAULT_ENVIRONMENT` to `sandbox` or `production`; each stored
ActivityKit token's own environment remains authoritative for delivery.
