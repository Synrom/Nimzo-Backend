# Backend Changes (So Far)

This document summarizes backend changes implemented so far for deck media and featured onboarding decks.

## 1) Deck Images (Author Upload + Public Display)

### Goal
- Allow a deck author to upload a deck image.
- Make that image publicly visible so all users can see it in library/deck views.

### API changes
- Added author-only endpoint:
  - `POST /deck/:id/image`
  - Auth required (JWT)
  - Request:
    - `mimeType` (`image/jpeg`, `image/png`, `image/webp`)
    - `base64Data` (raw base64 or data-url format)
  - Response:
    - `imageUrl`

### Business rules
- Only the author of the deck can upload/update image.
- Allowed mime types: jpeg/png/webp.
- Max size: 5MB.
- Stored file name pattern: `deck-<deckId>.<ext>`.
- URL is returned with cache-busting query parameter `?v=<timestamp>`.

### Data model changes
- Added `imageUrl` to:
  - deck base model
  - deck details model
  - deck search result model

### Repository/query changes
- Added `image_url` to deck select fields in:
  - find
  - search/full
  - search/instant
  - continuation deck list joins/grouping
- Added `saveDeckImage` repo function for validation + file write + DB update.

### Environment/config changes
- Added config values:
  - `DECK_IMAGE_DIR`
  - `DECK_IMAGE_PUBLIC_BASE`
- Added them to app environment and startup wiring.

### Database changes
- Added migration:
  - `initdb/10_deck_images.sql`
- Migration adds:
  - `decks.image_url VARCHAR(600)`

### Test support changes
- Test helper schema setup updated with `image_url`.
- Test app env updated with test values for image dir/public base.

## 2) Featured / TikTok Decks + Optional Video URL

### Goal
- Mark specific public decks as campaign decks (e.g. TikTok).
- Return them via a dedicated endpoint so onboarding/library can show them immediately.
- Optionally attach a promo video URL to those decks.

### API changes
- Added public endpoint:
  - `GET /deck/featured?source=tiktok&limit=12`
  - Returns only featured public decks.
- Added author-only endpoint:
  - `POST /deck/:id/promotion`
  - Auth required (JWT)
  - Request:
    - `featuredSource` (currently supports `tiktok` or `null`)
    - `featuredRank` (`0..100000` or `null`)
    - `videoUrl` (absolute `http(s)` URL or `null`)
  - Response:
    - `deckId`, `featuredSource`, `featuredRank`, `videoUrl`

### Business rules
- Only deck author can update promotion metadata.
- Featured source is normalized to lowercase/trimmed.
- Featured endpoint defaults:
  - source = `tiktok` (if omitted)
  - limit = `20`, bounded to `1..50`
- Featured sorting:
  - `featured_rank` ascending (nulls last),
  - then `download_count` descending,
  - then `rating_avg` descending,
  - then deck name ascending.

### Data model changes
- Added to deck models:
  - `featuredSource`
  - `featuredRank`
  - `videoUrl`
- Added new model module:
  - `Models.DeckPromotion`

### Repository/query changes
- Added `featured_source`, `featured_rank`, `video_url` to deck select fields.
- Added:
  - `listFeatured` query function.
  - `savePromotion` update function.

### Database changes
- Added migration:
  - `initdb/11_deck_promotion_metadata.sql`
- Migration adds:
  - `decks.featured_source VARCHAR(50)`
  - `decks.featured_rank INTEGER`
  - `decks.video_url VARCHAR(1000)`
  - index `decks_featured_source_rank_idx (featured_source, featured_rank, id)`

## 3) Documentation Updates

- `endpoint.md` updated with:
  - deck image upload endpoint
  - featured/tiktok endpoint
  - promotion metadata endpoint
  - migration run order reminder

## 4) Migration Order (Production/Staging)

For existing databases, apply in this order:
1. `initdb/10_deck_images.sql`
2. `initdb/11_deck_promotion_metadata.sql`

## 5) Frontend Integration Expectations

- On onboarding/library initial load:
  - call `GET /deck/featured?source=tiktok&limit=<n>`
  - render these decks first.
- Continue using normal search endpoints for broader discovery.
- Use optional fields in responses:
  - `imageUrl`
  - `featuredSource`
  - `featuredRank`
  - `videoUrl`
