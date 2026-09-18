# Streak Countdown Live Activity Backend Specification

## Goal

Nimzo should show a one-hour Live Activity countdown beginning 47 hours after
the user's last card answer. At 48 hours, the same Live Activity should change
to:

- **Nimzo misses you!**
- **Got time for studying?**

The countdown itself is rendered locally by SwiftUI. The backend is responsible
for starting the Live Activity at hour 47 and sending the ActivityKit content
update at hour 48. Replaying a card cancels the old schedule and creates a new
one.

This specification deliberately uses the same server-driven start flow on all
iOS 17.2 and newer versions. Although iOS 26 can schedule a future Live
Activity locally, using APNs push-to-start everywhere gives Nimzo one production
path and also supports iOS 17.2 through iOS 25.

Devices on iOS 17.0 and 17.1 must continue to use scheduled local notifications.
They cannot receive push-to-start Live Activities. Devices below iOS 16.2 do not
support Live Activities.

## Current implementation compatibility

Current clients explicitly register `supportsLocallyScheduledLiveActivities:
false`, including on iOS 26. Legacy clients may send true; they retain their
local-start/completion-only path. Eligibility also requires configured APNs
transport. A replay reconciles missing jobs after token upload or capability
changes, without retrying successful jobs or resurrecting superseded schedules.

Card-play requests accept optional `timingProfile`: `production` (the default)
uses +47h/+48h; `accelerated-preview` uses +15s/+25s. APNs environment selects
only the push host, independently of schedule timing.

## End-to-end protocol

```text
iOS app                         Nimzo backend                         APNs
   |                                  |                                |
   |-- register installation -------->|                                |
   |-- upload push-to-start token --->|                                |
   |                                  |                                |
   |-- card played ------------------>|                                |
   |<-- schedule id + timestamps -----|                                |
   |                                  |                                |
   |                    at +47 hours  |-- liveactivity start -------->|
   |<============================ Activity starts ====================|
   |-- upload activity update token ->|                                |
   |                                  |                                |
   |                    at +48 hours  |-- isComplete=true update ---->|
   |<========================= Same card changes =====================|
   |                                  |                                |
   |             after retention time |-- end + dismissal date ------>|
```

There are two different ActivityKit tokens:

1. A **push-to-start token** belongs to an installation and ActivityAttributes
   type. The backend sends the hour-47 `start` event to this token.
2. An **activity update token** belongs to one particular Live Activity. After
   APNs starts the activity, iOS supplies this token to the app. The app uploads
   it and the backend sends the hour-48 `update` and later `end` events to it.

These are raw ActivityKit APNs tokens. They are not the normal APNs device token
and not an FCM token.

## Public API additions

Add an authenticated `Routes.StreakNotifications` API to `SecureAPI` in
`App.API`. Every route uses the existing JWT authentication and derives the
username from `AuthenticatedUser`; the client must never submit a username.

Suggested Servant shape:

```haskell
type API =
       "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> ReqBody '[JSON] UpsertIOSInstallationRequest
         :> Put '[JSON] IOSInstallationResponse

  :<|> "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> "activitykit" :> "push-to-start-token"
         :> ReqBody '[JSON] PushToStartTokenRequest
         :> Put '[JSON] NoContent

  :<|> "notifications" :> "streak" :> "card-played"
         :> ReqBody '[JSON] CardPlayedRequest
         :> Post '[JSON] StreakScheduleResponse

  :<|> "notifications" :> "streak" :> "schedules"
         :> Capture "scheduleId" UUID
         :> "activity-token"
         :> ReqBody '[JSON] ActivityTokenRequest
         :> Put '[JSON] NoContent

  :<|> "notifications" :> "ios" :> "installations"
         :> Capture "installationId" Text
         :> DeleteNoContent
```

Use the project's current URL style if versioning is added globally later; no
separate public/unauthenticated endpoint is needed.

### 1. Register or refresh an iOS installation

```http
PUT /notifications/ios/installations/{installationId}
Authorization: Bearer <access-token>
Content-Type: application/json
```

Request:

```json
{
  "platform": "ios",
  "osVersion": "18.6",
  "appVersion": "1.4.0",
  "buildNumber": "82",
  "liveActivitiesEnabled": true,
  "apnsEnvironment": "production"
}
```

Response:

```json
{
  "installationId": "01JZ...",
  "serverTime": "2026-08-13T15:10:00Z"
}
```

Requirements:

- `installationId` is a random UUID/ULID generated once by the app and stored in
  Keychain. Do not use IDFV as the primary identifier.
- The authenticated user owns the installation. An upsert may transfer an
  installation to the newly authenticated user after logout/login, but must
  first invalidate the previous user's notification schedules.
- `apnsEnvironment` is `sandbox` for Xcode development provisioning and
  `production` for TestFlight and App Store builds. Never infer this solely from
  `appVersion`.
- If `liveActivitiesEnabled` becomes false, cancel pending Live Activity jobs
  for this installation. The frontend should keep its local-notification
  fallback enabled.
- Update `last_seen_at` on every call.

Validation:

- Maximum installation ID length: 128 characters.
- Maximum version-field length: 64 characters.
- Only accept `platform = ios` and the two known APNs environments.
- Return `401` for an invalid JWT, `400` for invalid fields, and `200` for both
  insert and update so retries are idempotent.

### 2. Upload the push-to-start token

```http
PUT /notifications/ios/installations/{installationId}/activitykit/push-to-start-token
Authorization: Bearer <access-token>
Content-Type: application/json
```

Request:

```json
{
  "token": "6f4c...lowercase-hex...",
  "attributesType": "StreakActivityAttributes",
  "apnsEnvironment": "production"
}
```

Response: `204 No Content`.

Requirements:

- The frontend obtains the token from both
  `Activity<StreakActivityAttributes>.pushToStartToken` and
  `pushToStartTokenUpdates` and calls this endpoint whenever it changes.
- Encode `Data` as lowercase hexadecimal. Accept uppercase input but normalize
  it before storage.
- Validate that the token is nonempty, even-length hexadecimal and at most 512
  bytes after decoding. Do not assume a fixed token length.
- Ensure the installation belongs to the authenticated user.
- Replacing the token must invalidate the old token in the database.
- Token upload is idempotent. Uploading the same token again only refreshes
  `token_updated_at`.
- Store the APNs environment alongside the token. Sandbox tokens must only be
  sent to the sandbox APNs host and production tokens only to production.
- Never log the full token. Log a short one-way fingerprint such as the first
  12 hex characters of SHA-256.

The app should call this endpoint at startup and whenever ActivityKit emits a
new token. The backend cannot initiate a Live Activity until it has this token.

### 3. Record card activity and replace the streak schedule

```http
POST /notifications/streak/card-played
Authorization: Bearer <access-token>
Content-Type: application/json
```

Request:

```json
{
  "eventId": "01JZ...",
  "installationId": "01JY...",
  "playedAt": "2026-08-13T15:12:31.442Z"
}
```

Response:

```json
{
  "scheduleId": "9be0707b-dccb-46eb-87ab-e004205bfdc3",
  "generation": 14,
  "lastPlayedAt": "2026-08-13T15:12:31.442Z",
  "startsAt": "2026-08-15T14:12:31.442Z",
  "completesAt": "2026-08-15T15:12:31.442Z",
  "delivery": "activitykit",
  "requiresLocalFallback": false
}
```

Behavior, in one PostgreSQL transaction:

1. Validate that `installationId` belongs to the authenticated user.
2. Deduplicate by `(username, event_id)`. If the event was already processed,
   return its existing schedule response without modifying anything.
3. Validate `playedAt`. It may be slightly in the future for clock skew, but
   reject or clamp unreasonable values. A suggested allowance is five minutes.
4. Lock the user's current streak schedule row with `SELECT ... FOR UPDATE`.
5. If `playedAt` is older than or equal to the stored `last_played_at`, treat it
   as an idempotent/out-of-order event and return the current schedule.
6. Increment the user's monotonically increasing `generation`.
7. Mark all older pending/running jobs for that user as cancelled.
8. Mark the previous schedule as superseded. If it has an active activity
   update token, enqueue an immediate `end` job for that old activity.
9. Insert the new schedule with:
   - `starts_at = played_at + interval '47 hours'`
   - `completes_at = played_at + interval '48 hours'`
10. If the installation supports push-to-start and has a valid token, insert a
    `start` job at `starts_at` and a `complete` job at `completes_at`.
11. Return the new schedule and delivery decision.

The backend should be authoritative for the calculated timestamps. The app
uses the returned values to schedule/cancel fallback notifications and to put
the same `scheduleId` and `generation` into locally created ActivityAttributes
during development or any future iOS-26-specific optimization.

If the device is iOS 17.0/17.1, Live Activities are disabled, or no
push-to-start token exists, return:

```json
{
  "delivery": "local-notifications",
  "requiresLocalFallback": true
}
```

The app then keeps the existing Notifee countdown and completion notifications.
When `delivery` is `activitykit`, the app must suppress those two notifications
to avoid duplicate user-visible reminders.

If Nimzo prefers to derive card activity from `/changes/push`, the route handler
may call the same repository transaction after a successful WatermelonDB push.
Keep this explicit endpoint as the protocol boundary initially: it is easier to
make idempotent, test, and retry, and it does not couple notification scheduling
to the shape of synchronized card records.

### 4. Upload the per-activity update token

```http
PUT /notifications/streak/schedules/{scheduleId}/activity-token
Authorization: Bearer <access-token>
Content-Type: application/json
```

Request:

```json
{
  "installationId": "01JY...",
  "activityId": "A1D604E6-...",
  "generation": 14,
  "token": "7a31...lowercase-hex...",
  "apnsEnvironment": "production"
}
```

Response: `204 No Content`.

The frontend discovers remotely started activities through
`Activity<StreakActivityAttributes>.activityUpdates` and currently active
activities through `.activities`. For each one, it observes
`activity.pushTokenUpdates` and uploads every token change.

Requirements:

- Read `scheduleId` and `generation` from the activity attributes. Do not match
  an activity merely by its start time.
- Verify that the schedule and installation belong to the JWT user and that
  `generation` matches.
- If the schedule is already superseded, store nothing and enqueue an immediate
  end request using the submitted token. Returning `204` is still appropriate.
- Upsert by `activity_id`; mark any previous token for that activity invalid.
- Validate and protect this token exactly like the push-to-start token.
- Wake the `complete` job if it was waiting for a token.
- If the token arrives after `completes_at`, enqueue the completion immediately
  rather than displaying a permanent zero countdown.

### 5. Delete an installation

```http
DELETE /notifications/ios/installations/{installationId}
Authorization: Bearer <access-token>
```

Response: `204 No Content`.

Use this on logout, account deletion, or a user disabling notification support
inside Nimzo. It must:

- Verify ownership.
- Cancel future jobs for the installation.
- Enqueue `end` for any active activities whose update tokens are still valid.
- Delete or cryptographically erase stored tokens.
- Be idempotent.

Do not delete the installation on an ordinary app restart. APNs tokens can
survive restarts and may change independently; token observers handle changes.

## PostgreSQL schema

Add a new migration after the current highest-numbered `initdb` migration. In
production, run it through the deployment migration mechanism rather than
depending on Docker's first-start initialization behavior.

The following schema is a concrete starting point; adapt naming to the existing
conventions without weakening its constraints.

```sql
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE ios_notification_installations (
  installation_id VARCHAR(128) PRIMARY KEY,
  username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,
  os_version VARCHAR(64) NOT NULL,
  app_version VARCHAR(64) NOT NULL,
  build_number VARCHAR(64) NOT NULL,
  live_activities_enabled BOOLEAN NOT NULL,
  apns_environment VARCHAR(16) NOT NULL
    CHECK (apns_environment IN ('sandbox', 'production')),
  push_to_start_token BYTEA,
  token_updated_at TIMESTAMPTZ,
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX ios_notification_installations_username_idx
  ON ios_notification_installations(username);

CREATE TABLE streak_notification_schedules (
  schedule_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL
    REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  event_id VARCHAR(128) NOT NULL,
  generation BIGINT NOT NULL,
  last_played_at TIMESTAMPTZ NOT NULL,
  starts_at TIMESTAMPTZ NOT NULL,
  completes_at TIMESTAMPTZ NOT NULL,
  status VARCHAR(24) NOT NULL
    CHECK (status IN
      ('scheduled', 'starting', 'active', 'complete', 'ending', 'ended',
       'superseded', 'failed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (username, event_id),
  UNIQUE (username, generation),
  CHECK (starts_at < completes_at)
);

CREATE UNIQUE INDEX one_current_streak_schedule_per_installation
  ON streak_notification_schedules(installation_id)
  WHERE status IN ('scheduled', 'starting', 'active', 'complete', 'ending');

CREATE TABLE streak_live_activities (
  activity_id VARCHAR(128) PRIMARY KEY,
  schedule_id UUID NOT NULL UNIQUE
    REFERENCES streak_notification_schedules(schedule_id) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL
    REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  generation BIGINT NOT NULL,
  update_token BYTEA NOT NULL,
  apns_environment VARCHAR(16) NOT NULL
    CHECK (apns_environment IN ('sandbox', 'production')),
  token_valid BOOLEAN NOT NULL DEFAULT TRUE,
  token_updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE notification_jobs (
  job_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  schedule_id UUID NOT NULL
    REFERENCES streak_notification_schedules(schedule_id) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL
    REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  generation BIGINT NOT NULL,
  job_type VARCHAR(16) NOT NULL
    CHECK (job_type IN ('start', 'complete', 'end')),
  run_at TIMESTAMPTZ NOT NULL,
  status VARCHAR(16) NOT NULL DEFAULT 'pending'
    CHECK (status IN ('pending', 'running', 'retry', 'succeeded', 'cancelled',
                      'dead')),
  attempts INTEGER NOT NULL DEFAULT 0,
  locked_at TIMESTAMPTZ,
  locked_by VARCHAR(128),
  next_attempt_at TIMESTAMPTZ,
  last_apns_status INTEGER,
  last_apns_reason VARCHAR(128),
  last_apns_id VARCHAR(64),
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (schedule_id, job_type)
);

CREATE INDEX notification_jobs_ready_idx
  ON notification_jobs(COALESCE(next_attempt_at, run_at))
  WHERE status IN ('pending', 'retry');
```

If a user can actively use several iOS devices, decide whether each installation
should receive a Live Activity. The recommended behavior is yes: create one
schedule per installation but group them under the same user/event generation.
The simplified schema above models one installation per schedule. The
`card-played` transaction should fan out schedules to all recently active,
eligible installations for that user; return the calling installation's
schedule in the HTTP response.

## ActivityAttributes and content-state contract

Backend JSON keys must exactly match Swift's default `Codable` property names.
Do not use a custom snake-case encoder for ActivityKit payloads.

Frontend attributes:

```swift
struct StreakActivityAttributes: ActivityAttributes {
  struct ContentState: Codable, Hashable {
    let endsAtTimestamp: Double
    let title: String
    let completionTitle: String
    let completionBody: String
    let isComplete: Bool
  }

  let scheduleId: String
  let generation: Int64
  let startsAtTimestamp: Double
}
```

All timestamps inside ActivityKit JSON are Unix seconds, not milliseconds. The
existing React Native bridge currently uses milliseconds internally, so convert
at the boundary.

The `attributes-type` must exactly match the runtime type name expected by the
widget extension: `StreakActivityAttributes`.

Keep the combined encoded attributes and content state comfortably below
ActivityKit's 4 KB limit.

## APNs provider implementation

Add an `App.APNS` module and an APNs configuration field to `Env`. The backend
must communicate directly with APNs over TLS/HTTP 2. FCM registration tokens
cannot address Live Activities.

Configuration, supplied through environment variables or a secret manager:

```text
APNS_TEAM_ID=<Apple developer team id>
APNS_KEY_ID=<APNs auth key id>
APNS_PRIVATE_KEY_PATH=/run/secrets/nimzo-apns-key.p8
APNS_BUNDLE_ID=<main Nimzo iOS bundle id>
APNS_DEFAULT_ENVIRONMENT=production
```

Never commit the `.p8` key, JWT, or device tokens. A `.p8` file is currently
present in the repository root; before deploying this feature, remove it from
the repository and Git history, revoke that key in the Apple Developer portal,
and create a replacement stored as a deployment secret. Treat it as compromised
even if the repository has been private.

### Provider JWT

Create an ES256 JWT with:

```json
{
  "alg": "ES256",
  "kid": "<APNS_KEY_ID>"
}
```

and claims:

```json
{
  "iss": "<APNS_TEAM_ID>",
  "iat": 1780000000
}
```

Cache and reuse the provider JWT, but refresh it before it is 60 minutes old.
A 50-minute refresh interval is reasonable. Reuse long-lived HTTP/2 connections
rather than opening one per job.

APNs hosts:

```text
sandbox:    https://api.sandbox.push.apple.com
production: https://api.push.apple.com
```

Request path:

```text
POST /3/device/{hex-encoded-activitykit-token}
```

Headers for every ActivityKit request:

```text
authorization: bearer <provider-jwt>
apns-push-type: liveactivity
apns-topic: <main-app-bundle-id>.push-type.liveactivity
apns-priority: 10
apns-expiration: <short Unix expiry appropriate to the event>
```

Use the main application bundle ID for `apns-topic`, not the widget extension's
bundle ID.

Suggested expirations:

- Start: `starts_at + 15 minutes`. A countdown that starts many hours late is
  misleading.
- Complete: `completes_at + 60 minutes`. Completion remains useful briefly, but
  should not overwrite a newer generation.
- End: `now + 24 hours`.

Generate an `apns-id` UUID for each attempt and persist both it and the ID APNs
returns. This is important for production diagnostics.

### Start payload at hour 47

Send to the installation's push-to-start token:

```json
{
  "aps": {
    "timestamp": 1780000000,
    "event": "start",
    "content-state": {
      "endsAtTimestamp": 1780003600,
      "title": "Last chance!",
      "completionTitle": "Nimzo misses you!",
      "completionBody": "Got time for studying?",
      "isComplete": false
    },
    "attributes-type": "StreakActivityAttributes",
    "attributes": {
      "scheduleId": "9be0707b-dccb-46eb-87ab-e004205bfdc3",
      "generation": 14,
      "startsAtTimestamp": 1780000000
    },
    "stale-date": 1780003600,
    "relevance-score": 100,
    "alert": {
      "title": "Last chance!",
      "body": "Your streak expires in ...",
      "sound": "default"
    }
  }
}
```

Rules:

- `timestamp` is the current server time in Unix seconds. It is ordering
  metadata, not a future delivery time.
- A push-to-start payload must contain `event = start`, `attributes-type`,
  `attributes`, and `alert`.
- Set `stale-date` to `completes_at` only as an offline/deferred-update fallback.
- On iOS 18 and newer, include `"input-push-token": 1` in `aps` so the remotely
  started activity produces an update token. For compatibility, the app must
  still observe `activityUpdates` and `pushTokenUpdates`; iOS 17.2/17.x depends
  on that path.
- Do not send a regular completion notification when the Live Activity path is
  healthy.

Because each installation has its own token and schedule, broadcast channels
are unnecessary for this feature.

### Completion payload at hour 48

Send to the activity's update token:

```json
{
  "aps": {
    "timestamp": 1780003600,
    "event": "update",
    "content-state": {
      "endsAtTimestamp": 1780003600,
      "title": "Last chance!",
      "completionTitle": "Nimzo misses you!",
      "completionBody": "Got time for studying?",
      "isComplete": true
    },
    "relevance-score": 100,
    "alert": {
      "title": "Nimzo misses you!",
      "body": "Got time for studying?",
      "sound": "default"
    }
  }
}
```

The `alert` is intentional: it makes this important transition prominent. If
product decides the transition should be silent, remove only `alert`; the
content update still works.

After success, mark the schedule `complete` and enqueue `end`. A reasonable
default is to end it 1-4 hours later with a final complete state. Do not leave an
activity active until ActivityKit's system timeout.

### End payload

Send the latest complete content state with the end event:

```json
{
  "aps": {
    "timestamp": 1780007200,
    "event": "end",
    "dismissal-date": 1780010800,
    "content-state": {
      "endsAtTimestamp": 1780003600,
      "title": "Last chance!",
      "completionTitle": "Nimzo misses you!",
      "completionBody": "Got time for studying?",
      "isComplete": true
    }
  }
}
```

When ending an old activity because the user played another card, use a
`dismissal-date` in the past so the obsolete activity disappears immediately.
For normal cleanup, use a future dismissal date of no more than four hours
after the end event.

## Durable job worker

The current application has no durable scheduler. Do not implement 47/48-hour
delays with `threadDelay`, in-memory `async`, Warp request threads, cron entries
without database state, or `BGTaskScheduler` on the phone. All of those lose
work across process restarts or deployments.

Add a separate executable, for example `StreakNotificationWorker`, to
`Gambit.cabal`. It can share `Models`, `Repo`, configuration, and `App.APNS` with
the web application. Run it as a second container/process. Both API and worker
must use independent PostgreSQL connections; moving the project to a connection
pool is recommended before adding concurrent worker operations.

Claim work atomically:

```sql
WITH candidate AS (
  SELECT job_id
  FROM notification_jobs
  WHERE status IN ('pending', 'retry')
    AND COALESCE(next_attempt_at, run_at) <= now()
  ORDER BY COALESCE(next_attempt_at, run_at)
  FOR UPDATE SKIP LOCKED
  LIMIT 20
)
UPDATE notification_jobs j
SET status = 'running',
    locked_at = now(),
    locked_by = $1,
    attempts = attempts + 1,
    updated_at = now()
FROM candidate
WHERE j.job_id = candidate.job_id
RETURNING j.*;
```

Before every APNs send, reload the schedule and installation and verify:

- The job has not been cancelled.
- The schedule generation is still current.
- The schedule is not superseded.
- The relevant token is still valid and belongs to the same installation.
- The APNs environment agrees across installation, token, and job.

This final generation check prevents a race where a worker claimed an hour-48
job just before a new card answer cancelled it.

Worker semantics by job type:

- `start`: send the start payload to the current push-to-start token. On APNs
  success, mark the schedule `starting`. Do not mark it active until the app
  uploads the per-activity update token.
- `complete`: if there is no update token yet, retry for a bounded grace period.
  If one exists, send `isComplete = true`, mark the schedule complete, and
  create the end job.
- `end`: if there is an update token, send the final state with `event = end`.
  Mark the schedule ended even if an already-invalid token returns
  `Unregistered`.

Recover jobs left `running` by a crashed worker, for example by changing them
back to `retry` when `locked_at < now() - interval '5 minutes'`.

## Retry and APNs error policy

APNs success means HTTP `200`, not guaranteed immediate presentation. Store the
response and make job handling idempotent.

Recommended handling:

| APNs result | Action |
| --- | --- |
| `200` | Mark the attempt successful. |
| `429`, `500`, `503` | Retry with exponential backoff and jitter. Honor `Retry-After` when present. |
| Network/HTTP2 timeout | Retry; reuse the same logical job but a new `apns-id` attempt value. |
| `403 ExpiredProviderToken` | Refresh the provider JWT and retry once immediately. |
| `400 BadDeviceToken` | Mark that token invalid; do not retry with it. |
| `410 Unregistered` | Mark the token invalid using APNs' timestamp; stop retrying that token. |
| Other permanent `400/403` | Mark job dead and alert/log the payload/configuration error. |

Suggested transient retry delays: 5 seconds, 30 seconds, 2 minutes, 10 minutes,
then 30 minutes, always capped by `apns-expiration` and the event's usefulness.

For a `complete` job with no activity token:

1. Retry frequently from `completes_at - 2 minutes` through
   `completes_at + 10 minutes`.
2. If the token arrives, send completion immediately.
3. If the grace period expires, mark the job failed. If Nimzo stores a normal
   APNs device token in the future, send an ordinary fallback notification.
   Otherwise rely on `stale-date`, which may transition late.

The backend cannot guarantee an exact visual transition while the device is
offline. That is an ActivityKit/APNs limitation, not a worker failure.

## Copy ownership and compatibility

For the first release, hard-code/version the Live Activity copy on the backend
to match the frontend bundle. Do not accept arbitrary title/body text from the
client; doing so would let a compromised client create arbitrary push content.

Store a `payload_version` on the schedule if the content state will evolve.
ActivityKit uses default Codable decoding, so removing/renaming required fields
can make pushes fail to decode on old app versions. Prefer additive optional
fields and select payload shape from the stored app/build version.

The initial payload version is:

```text
attributes type: StreakActivityAttributes
attributes: scheduleId, generation, startsAtTimestamp
content state: endsAtTimestamp, title, completionTitle, completionBody, isComplete
```

## Frontend changes required by this backend

The backend implementation is not sufficient without these corresponding iOS
changes:

1. Enable Push Notifications for the main app target and ensure the signed app
   has the `aps-environment` entitlement.
2. Keep `NSSupportsLiveActivities = YES`.
3. Extend `StreakActivityAttributes` with `scheduleId` and `generation`.
4. Observe and upload `pushToStartTokenUpdates` from iOS 17.2 onward.
5. Observe `Activity.activityUpdates`, all current `.activities`, and each
   activity's `pushTokenUpdates`; upload tokens through endpoint 4.
6. For any locally started/scheduled debug activity, request it with
   `pushType: .token`, not `nil`.
7. Call `card-played` with a stable `eventId` whenever a card answer is durably
   recorded.
8. Suppress the local countdown/completion notifications only when the backend
   returns `delivery = activitykit`. Retain them on iOS 17.0/17.1 and when Live
   Activities or token registration are unavailable.
9. On logout, call the installation delete endpoint before removing the access
   token, then clear local Live Activities and notification requests.

The frontend's native timer remains responsible for displaying decreasing
digits without repeated pushes. Only three ActivityKit pushes are normally
needed: start, complete, and end.

## Apple account and deployment setup

No App Store Connect feature or server callback is required. Configuration is
in Xcode and the Apple Developer portal:

1. Enable Push Notifications for the main Nimzo App ID/target.
2. Regenerate provisioning profiles after enabling the capability.
3. Create a new APNs authentication key and download the `.p8` exactly once.
4. Put the key in the production secret store and expose only its mounted path
   to the backend.
5. Deploy separate sandbox/production routing based on each stored token's
   environment.
6. Test production APNs through TestFlight; Xcode-installed development builds
   use sandbox APNs.

`NSSupportsLiveActivitiesFrequentUpdates` is not needed for one start, one
completion, and one end update.

## Security and privacy requirements

- Treat ActivityKit tokens as credentials/capability URLs. Anyone possessing a
  valid token and APNs credentials could target that activity.
- Encrypt tokens at rest if the deployment has an application-level encryption
  facility. At minimum, restrict database access and backups.
- Never expose stored tokens through GET responses.
- Never include tokens, APNs JWTs, or `.p8` contents in logs, errors, analytics,
  crash reports, or test fixtures.
- Rate-limit installation/token endpoints per user and installation.
- Enforce JWT ownership on every route and database query.
- Use constant-size validation limits before hexadecimal decoding.
- Delete tokens when the user deletes their account; foreign keys with
  `ON DELETE CASCADE` provide a final safeguard.
- APNs provider credentials must never be present in the mobile app.

## Observability

Emit structured events without raw tokens:

```text
streak_schedule_created
streak_schedule_superseded
activitykit_start_sent
activitykit_update_token_received
activitykit_complete_sent
activitykit_end_sent
activitykit_apns_retry
activitykit_apns_permanent_failure
activitykit_completion_token_timeout
```

Include `schedule_id`, `installation_id`, `generation`, job type, attempt count,
APNs environment, HTTP status, APNs reason, APNs ID, and token fingerprint.

Useful metrics and alerts:

- Start jobs due/succeeded/failed.
- Percentage of successful starts followed by an update-token upload.
- Time from start APNs success to token upload.
- Completion delivery latency relative to `completes_at`.
- APNs response counts by reason.
- Ready-job queue age and number of dead jobs.
- Alert when the oldest ready job is more than one minute late.
- Alert on `InvalidProviderToken`, `BadTopic`, or sustained APNs `5xx` errors.

## Required tests

### Repository and route tests

- Authenticated users can only modify their own installations and schedules.
- Installation and both token endpoints are idempotent.
- Invalid hex, oversized tokens, and mismatched APNs environments are rejected.
- Duplicate `eventId` returns the original result.
- Out-of-order `playedAt` cannot replace a newer schedule.
- A new card event increments generation, cancels old jobs, and creates new
  start/complete jobs atomically.
- Disabling Live Activities returns local fallback and cancels server jobs.
- iOS 17.0/17.1 returns local fallback; iOS 17.2+ with a token returns
  ActivityKit delivery.
- Uploading a token for a superseded schedule creates an immediate end job.
- A late activity token makes an overdue completion runnable immediately.

### Worker tests

- Two workers using `FOR UPDATE SKIP LOCKED` never deliver the same claimed job
  concurrently.
- A claimed job whose generation becomes stale is cancelled before APNs send.
- APNs `200`, transient failures, permanent token failures, and provider-token
  refresh follow the documented state transitions.
- A crashed `running` job is recovered after the lock timeout.
- Start, update, and end JSON exactly match Swift Codable field names and use
  seconds, not milliseconds.
- Sandbox tokens route only to sandbox and production tokens only to production.
- Completion is never sent to a push-to-start token.
- End includes the final complete content state.

### End-to-end tests

Use shortened offsets only in a dedicated non-production configuration:

```text
start after: 15 seconds
complete after: 45 seconds
end after: 2 minutes
```

Verify on a physical iOS 17.2 device, a current iOS device, and TestFlight:

1. Card activity replaces any prior schedule.
2. The activity starts while Nimzo is terminated.
3. The timer counts down without repeated backend updates.
4. The same activity changes to `isComplete = true` around zero.
5. A new card answer before zero ends the old activity and prevents its stale
   completion job from firing.
6. Disabling Live Activities preserves the local notification fallback.

Never enable accelerated offsets from a client request. Gate them with a
server-side environment setting restricted to development/staging.

## Implementation order

1. Rotate and move the exposed APNs key into secret storage.
2. Add Push Notifications entitlement/capability to the iOS app.
3. Add database migration, models, repository operations, and authenticated
   endpoints.
4. Add frontend installation and token observers/uploads.
5. Add the durable worker and APNs client using a mock APNs transport in tests.
6. Implement start jobs and verify remote starts on iOS 17.2+.
7. Implement completion and end jobs.
8. Connect `card-played`, cancellation, and local fallback selection.
9. Add metrics, dead-job alerts, staging accelerated tests, and TestFlight
   validation.
10. Restore/verify the production 47-hour, 48-hour, and cleanup offsets before
    release.

## Definition of done

The feature is ready when:

- A terminated app on iOS 17.2+ receives the countdown Live Activity at hour 47.
- Its native timer counts down without periodic pushes.
- The same activity receives `isComplete = true` at hour 48 and displays the
  completion layout.
- A later card answer reliably cancels the previous generation.
- iOS 17.0/17.1 and disabled/unregistered ActivityKit installations retain
  local-notification fallback behavior.
- Jobs survive API/worker restarts and deployments.
- APNs tokens and credentials never appear in logs or API responses.
- Production monitoring can distinguish missing tokens, APNs rejection,
  transient delays, and worker backlog.
