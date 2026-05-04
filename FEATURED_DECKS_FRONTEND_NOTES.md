# Featured Decks: Frontend Integration Notes

This document summarizes backend changes related to social-featured decks and featured cards.

## What changed

The backend now supports linking a featured deck to a single featured card (line), and exposes that information in API responses.

### New DB concept
- New table: `featured_deck_lines`
- One row per featured deck (`deck_id` primary key)
- Maps to one `featured_card_id`
- DB trigger enforces that the `featured_card_id` belongs to the same deck

## API response changes

All changes are additive (no existing fields were removed/renamed).

### 1) `GET /deck/featured`
Deck items now include:
- `featuredCardId: string | null`

Example shape:
```json
{
  "deckId": 123,
  "name": "Sicilian Najdorf",
  "featuredSource": "tiktok",
  "featuredRank": 3,
  "featuredCardId": "fc_card_2"
}
```

### 2) `POST /deck/cards`
Each card now includes:
- `cardId: string`
- `isFeatured: boolean`

Example shape:
```json
{
  "next_cursor": "fc_card_2",
  "cards": [
    {
      "moves": "e4 c5",
      "title": "Main line",
      "color": "wh",
      "cardId": "fc_card_1",
      "isFeatured": false
    },
    {
      "moves": "e4 c5 Nf3 d6",
      "title": "Featured line",
      "color": "wh",
      "cardId": "fc_card_2",
      "isFeatured": true
    }
  ]
}
```

## Promotion write behavior (important for admin/mod tools)

Promotion payload now supports:
- `featuredSource: string | null`
- `featuredCardId: string | null`
- `featuredRank: number | null`
- `videoUrl: string | null`

Rules:
- If `featuredSource` is set, `featuredCardId` is required.
- If `featuredSource` is `null`, the deck-to-featured-card mapping is removed.
- Allowed social sources: `tiktok`, `instagram`, `youtube`, `x`.

## Frontend checklist

1. Read `featuredCardId` from `/deck/featured` response.
2. In deck-card views, use `isFeatured` (preferred) to highlight the card.
3. If you need direct matching, compare `card.cardId` with `deck.featuredCardId`.
4. For promotion UIs, enforce "featured source requires featured card" before submit.
5. Keep fallback behavior for older decks where `featuredCardId` may be `null`.

## Suggested follow-up refactor/cleanup

1. Replace subquery-based `featuredCardId` projection in search fields with a shared joined view/CTE to reduce duplication and improve readability.
2. Wrap promotion update + featured-line upsert/delete in an explicit DB transaction helper for clearer atomicity guarantees.
3. Add a small integration test around `/deck/featured` + `/deck/cards` response consistency (`featuredCardId` matches exactly one `isFeatured=true` card when present).
4. Add migration smoke script for staging rollout (`featured_source IS NOT NULL` decks missing `featured_deck_lines` should be reported).
5. Consider adding a typed enum for featured sources (Haskell side) to avoid stringly-typed propagation.
