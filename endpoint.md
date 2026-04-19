# Anonymous Onboarding Endpoints

This backend now supports onboarding progress **before account creation** via an anonymous session id.

## Overview

- Frontend creates a stable `onboarding_session_id` (UUID/random opaque string) and stores it locally.
- Frontend calls anonymous progress endpoint while user is onboarding.
- During signup, frontend can pass the session id as a query parameter so claim happens automatically.
- After signup/login, frontend can also call the claim endpoint to attach that anonymous session to the authenticated user.
- Claim also copies onboarding fields into the existing `user_onboarding_preferences` record.

## 1) Save Anonymous Progress

- Method: `POST`
- Path: `/onboarding/anonymous/progress`
- Auth: none

### Request JSON

```json
{
  "onboarding_session_id": "f84f1d66-0b5a-4c9d-b5f6-2c9f7e7fd123",
  "last_step": "motivation",
  "stopped": true,
  "chess_level": "beginner",
  "elo": "0-800",
  "organization": "Lichess",
  "motivation": "Build a study habit",
  "study_goal": "0-5 mins"
}
```

Notes:
- `onboarding_session_id`, `last_step`, and `stopped` are required.
- Preference fields are optional and nullable.
- Endpoint is idempotent by `onboarding_session_id` (`upsert`).
- If optional fields are omitted in later calls, previously saved values are preserved.

### Response JSON

```json
{ "msg": "Successfully saved anonymous onboarding progress." }
```

## 2) Read Anonymous Progress

- Method: `GET`
- Path: `/onboarding/anonymous/progress/:onboarding_session_id`
- Auth: none

### Response JSON

```json
{
  "onboarding_session_id": "f84f1d66-0b5a-4c9d-b5f6-2c9f7e7fd123",
  "last_step": "motivation",
  "stopped": true,
  "chess_level": "beginner",
  "elo": "0-800",
  "organization": "Lichess",
  "motivation": "Build a study habit",
  "study_goal": "0-5 mins",
  "claimed_by_user": null
}
```

If unknown session id: `404`.

## 3) Claim Anonymous Session (Authenticated)

- Method: `POST`
- Path: `/user/onboarding/claim`
- Auth: required (JWT)

### Request JSON

```json
{
  "onboarding_session_id": "f84f1d66-0b5a-4c9d-b5f6-2c9f7e7fd123"
}
```

### Response JSON

```json
{ "msg": "Successfully claimed anonymous onboarding session." }
```

Claim behavior:
- Session is linked to the current authenticated user.
- Stored anonymous onboarding fields are copied to `user_onboarding_preferences`.
- If already claimed by another user, backend returns `409`.
- Claim is safe to retry for the same user/session.

## 4) Signup With Auto-Claim (Optional)

- Method: `POST`
- Path: `/user?onboarding_session_id=<session-id>`
- Auth: none (same signup endpoint as before)

Behavior:
- Creates the user account.
- If `onboarding_session_id` is present, backend claims that anonymous session for the new user and copies onboarding fields.
- If `onboarding_session_id` is absent, signup behaves as before.

## Frontend Integration Guidance

1. Generate `onboarding_session_id` when onboarding starts and persist locally.
2. On each onboarding step (or debounce), call `POST /onboarding/anonymous/progress` with latest `last_step` and any known fields.
3. If user leaves onboarding, ensure one final save with `stopped: true`.
4. Prefer auto-claim during signup by calling `POST /user?onboarding_session_id=<session-id>`.
5. If signup happened without session id, call `POST /user/onboarding/claim` once after login/signup.
6. On successful claim, clear local anonymous onboarding state.

## Suggested Step Names

Use stable strings so analytics stays consistent, e.g.:
- `chess_level`
- `elo`
- `organization`
- `motivation`
- `study_goal`
- `signup`
- `complete`
