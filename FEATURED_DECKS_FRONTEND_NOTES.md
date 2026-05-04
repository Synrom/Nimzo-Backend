# Featured Decks: Frontend Integration Notes

This document reflects the simplified implementation.

## Core design

- No `featured_deck_lines` table is used.
- Featured card is stored directly on `decks.featured_card_id`.
- `featured_card_id` references a normal `user_card_views.id` value.

## API behavior

### List featured decks
`GET /deck/featured`
- Returns featured decks.
- Includes `featuredCardId` on each deck.
- Query params:
  - `source` optional, defaults to `tiktok` if omitted.
  - `limit` optional, clamped by backend.

### Search decks
`GET /deck/search/full`
`GET /deck/search/instant`
- If a deck is featured, search result also includes `featuredCardId`.

### Set featured metadata + featured card
You can set both at the same write endpoint:
- Existing: `POST /deck/{id}/promotion`
- Alias: `POST /deck/featured/{id}`
- Moderator path: `POST /deck/{id}/promotion/moderate`

Payload fields:
- `featuredSource` (`tiktok|instagram|youtube|x|null`)
- `featuredCardId` (`string|null`)
- `featuredRank` (`number|null`)
- `videoUrl` (`string|null`)

Rules:
- If `featuredSource` is set, `featuredCardId` is optional.
- If `featuredCardId` is provided, backend validates ownership (must belong to the deck).
- If `featuredCardId` is omitted, backend keeps the current value (which may be null).
- If `featuredSource` is `null`, backend clears `featured_card_id`.

### Get deck cards
`POST /deck/cards`

Request payload shape (current):
- `cursor` (`string|null`) pagination cursor, pass `null` for first page.
- `limit` (`number`) requested page size; backend enforces max limit.
- `deckId` (`number`) target deck id.
- `prefix` (`string|null`) optional move-prefix filter.

Response payload shape (current, unchanged):
- `next_cursor` (`string|null`)
- `cards` (`array`) with per-card fields:
  - `moves`
  - `title`
  - `color`

Important: `/deck/cards` does **not** add featured metadata in this simplified rollout.
- No `isFeatured` boolean.
- No `cardId` field addition.
- No top-level `featuredCardId` in `/deck/cards` response.

## Important compatibility note

- No `/changes/push` or `/changes/pull` behavior is changed by this feature.
- `/deck/cards` response shape stays exactly as before.
