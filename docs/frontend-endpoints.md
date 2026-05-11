# Backend Endpoint Guide (Frontend-Focused)

This document summarizes endpoints defined in `src/Routes/*`.

Base URL (local): `http://localhost:8080`

Auth model:
- `public`: no JWT required
- `secure`: JWT required (`Authorization: Bearer <access_token>`)

## Auth Routes (`src/Routes/Auth.hs`) — public

### `POST /auth`
- Purpose: login with username/email + password.
- Request (`AuthRequest`):
```json
{ "username": "alice", "password": "secret" }
```
- Response (`NewUser`):
```json
{
  "auth": {
    "access_token": "...",
    "refresh_token": "...",
    "expires": "2026-05-08T12:00:00Z"
  },
  "username": "alice",
  "premium": false,
  "xp": 10,
  "email": "alice@example.com",
  "verified": false
}
```

### `POST /auth/google`
### `POST /auth/apple`
- Purpose: social login/signup.
- Request (`SocialAuthRequest`):
```json
{ "id_token": "...", "username": "optional_name", "email": "optional_fallback@example.com" }
```
- Notes:
- `username` and `email` are optional fallback inputs.
- For first-time signup, if provider does not provide email and request has no fallback email, request fails.
- Response: `NewUser` (same shape as `/auth`).

### `POST /user?onboarding_session_id=<optional>`
- Purpose: create user (signup), optionally claim anonymous onboarding session.
- Request (`CreateUserRequest`):
```json
{ "username": "alice", "password": "secret", "email": "alice@example.com", "initialElo": 35 }
```
- Behavior:
- `initialElo` is optional.
- If provided, must be `> 0` and `<= 50`.
- Stored into `users.xp`.
- If omitted, XP defaults to `10`.
- Response: `NewUser`.

### `DELETE /user`
- Purpose: logout/expire refresh token (client-side token lifecycle endpoint).
- Request (`AuthTokenRequest`):
```json
{ "refresh_token": "..." }
```
- Response (`JsonableMsg`):
```json
{ "msg": "..." }
```

### `POST /user/reqChange`
- Purpose: request password-change flow email.
- Request (`UserEmail`):
```json
{ "email": "alice@example.com" }
```
- Response: `JsonableMsg`.

### `POST /auth/refresh`
- Purpose: exchange refresh token for new auth tokens.
- Request (`AuthTokenRequest`):
```json
{ "refresh_token": "..." }
```
- Response (`AuthTokens`):
```json
{ "access_token": "...", "refresh_token": "...", "expires": "2026-05-08T12:00:00Z" }
```

### `POST /verify`
- Purpose: verify account via verification JWT.
- Request (`Token`):
```json
{ "token": "..." }
```
- Response: `JsonableMsg`.

## Onboarding Routes (`src/Routes/Onboarding.hs`)

### Public

### `POST /onboarding/anonymous/progress`
- Purpose: save anonymous onboarding progress.
- Request (`AnonymousOnboardingProgressPayload`):
```json
{
  "onboarding_session_id": "session-123",
  "last_step": "chess-level",
  "stopped": false,
  "chess_level": "Intermediate",
  "elo": "1200",
  "organization": "Club",
  "motivation": "Improve tactics",
  "study_goal": "Daily"
}
```
- Response: `JsonableMsg`.

### `GET /onboarding/anonymous/progress/:onboarding_session_id`
- Purpose: retrieve anonymous onboarding progress.
- Response (`AnonymousOnboardingProgress`): payload above plus `claimed_by_user`.

### Secure

### `POST /user/onboarding`
- Purpose: save onboarding preferences for authenticated user.
- Request (`OnboardingPreferencesPayload`):
```json
{ "chess_level": "Intermediate", "elo": "1200", "organization": "Club", "motivation": "Improve", "study_goal": "Daily" }
```
- Response: `JsonableMsg`.

### `POST /user/onboarding/claim`
- Purpose: claim anonymous onboarding session for authenticated user.
- Request (`ClaimAnonymousOnboardingPayload`):
```json
{ "onboarding_session_id": "session-123" }
```
- Response: `JsonableMsg`.

## User Routes (`src/Routes/User.hs`) — secure

### `POST /rank`
- Purpose: leaderboard slice around a rank.
- Request (`RankQuery`):
```json
{ "rank": 100, "limit": 20, "direction": "Both" }
```
- Response (`[UserXP]`):
```json
[{ "username": "alice", "xp": 1200, "rank": 95 }]
```

### `GET /user`
- Purpose: fetch current authenticated user profile.
- Response (`PublicUser`):
```json
{ "username": "alice", "premium": false, "xp": 1200, "streak": 7, "rank": 95, "email": "alice@example.com", "verified": true }
```

### `POST /user/changepwd`
- Purpose: change password for authenticated user.
- Request (`NewPassword`):
```json
{ "pwd": "new-secret" }
```
- Response: `JsonableMsg`.

## Deck Routes (`src/Routes/Deck.hs`)

### Public

### `GET /deck/search/full?query=<q>`
- Purpose: full deck search.
- Response: `[DeckSearchResult]`.

### `GET /deck/search/instant?query=<q>`
- Purpose: instant/typeahead deck search.
- Response: `[DeckSearchResult]`.

### `GET /deck/featured?source=<optional>&limit=<optional>`
- Purpose: list featured decks.
- Response: `[DeckSearchResult]`.

### `GET /deck/search/continuations?prefix=<optional>&color=<optional>&limitDecks=<optional>&limitContinuations=<optional>`
- Purpose: opening continuation search with deck counts.
- Response (`SearchContinuationsResponse`):
```json
{
  "continuations": [{ "move": "Nf3", "nr_cards": 123 }],
  "decks": [{ "deck": { "deckId": 1, "name": "..." }, "nr_cards": 88 }]
}
```

### `POST /deck/cards`
- Purpose: paginated deck cards listing.
- Request (`CardQuery`):
```json
{ "cursor": null, "limit": 30, "deckId": 42, "prefix": "e4 e5" }
```
- Response (`PagedCards`):
```json
{ "next_cursor": "optional-cursor", "cards": [{ "moves": "e4 e5 Nf3", "title": "...", "color": "w" }] }
```

### `GET /deck/:user_deck_id/continuations?prefix=<optional>`
- Purpose: list distinct next moves for a specific deck.
- Response: `[String]`.

### Secure

### `GET /deck/:id`
- Purpose: get detailed deck info for current user context.
- Response (`DeckDetails`): includes deck metadata + `hasRated`, `userRating`.

### `POST /deck/:id/rating`
- Purpose: rate deck.
- Request (`DeckRatingRequest`):
```json
{ "rating": 4 }
```
- Response: `JsonableMsg`.

### `POST /deck/:id/image`
- Purpose: upload deck image.
- Request (`DeckImageUploadRequest`):
```json
{ "mimeType": "image/png", "base64Data": "..." }
```
- Response (`DeckImageUploadResponse`):
```json
{ "imageUrl": "https://..." }
```

### `POST /deck/:id/promotion`
### `POST /deck/featured/:id`
### `POST /deck/:id/promotion/moderate`
- Purpose: manage promotion/featured metadata (last one is moderator flow).
- Request (`DeckPromotionRequest`):
```json
{
  "featuredSource": "community",
  "featuredCardId": "optional-card-id",
  "featuredMoves": "optional moves string",
  "featuredRank": 1,
  "videoUrl": "https://..."
}
```
- Response (`DeckPromotionResponse`):
```json
{
  "deckId": 42,
  "featuredSource": "community",
  "featuredCardId": "card-123",
  "featuredRank": 1,
  "videoUrl": "https://..."
}
```

## Watermelon Sync Routes (`src/Routes/Watermelon.hs`) — secure

### `POST /changes/pull`
- Purpose: incremental sync pull.
- Request (`PullParams`):
```json
{ "lastPulledAt": 1715000000, "schemaVersion": 3, "migration": null }
```
- Response:
- For `schemaVersion >= 3`: full `ChangesResponse`.
- For older schema versions: backward-compatible response shape.

Example (`ChangesResponse`):
```json
{
  "changes": {
    "user_card_views": { "created": [], "updated": [], "deleted": [] },
    "user_deck_views": { "created": [], "updated": [], "deleted": [] }
  },
  "timestamp": 1715000123
}
```

### `POST /changes/push`
- Purpose: push local client changes.
- Request (`PushParams`):
```json
{
  "lastPulledAt": 1715000000,
  "changes": {
    "user_card_views": { "created": [], "updated": [], "deleted": [] },
    "user_deck_views": { "created": [], "updated": [], "deleted": [] }
  }
}
```
- Response (`Success`):
```json
{ "xp": 1234, "streak": 9, "msg": "Synched successfully." }
```

## Error Behavior (common)

Errors are returned as HTTP errors with message bodies from `AppError` (examples):
- `401 Unauthorized` for auth/validation/access issues.
- `404 NotFound` when resource/session is missing.
- `409 MergeConflict` on sync conflicts.

## Type Reference

For exact JSON fields, see:
- `src/App/Auth.hs`
- `src/Models/User.hs`
- `src/Models/SocialAuth.hs`
- `src/Models/Onboarding.hs`
- `src/Models/DeckDetails.hs`
- `src/Models/DeckSearch.hs`
- `src/Models/DeckImage.hs`
- `src/Models/DeckPromotion.hs`
- `src/Models/DeckRating.hs`
- `src/Models/Card.hs`
- `src/Models/Watermelon.hs`
