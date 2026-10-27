# Architecture

This file documents individual source files for onboarding. It only lists files
that have actually been read in full and understood — see the note at the
bottom for what's still missing.

For the high-level module layout (App layer, Models, Repo, Routes) and overall
tech stack, see `CLAUDE.md` in the repo root first; this file goes one level
deeper into specific files.

## src/Routes/Watermelon.hs

Implements the WatermelonDB sync protocol (`/changes/pull` and `/changes/push`),
the endpoints mobile clients use to sync `user_card_views` and `user_deck_views`
while offline.

- `pullRoute` / `pullRouteVersioned` — given a `lastPulledAt` timestamp, returns
  everything created/updated/deleted since then, wrapped in a `ChangesResponse`.
  `pullRouteVersioned` additionally shapes the JSON payload to match the
  client's declared `schemaVersion` (see below).
- **Schema versioning**: clients report a `schemaVersion` integer. Helper
  predicates (`supportsDeckColor`, `supportsDeckStudyFields`, `supportsCardFen`,
  `supportsCardLikelihood`) gate which fields get serialized, so older mobile
  builds don't choke on fields they don't understand. `deckViewForSchema` /
  `cardViewForSchema` pick the right serializer function
  (`legacyUserDeckView`, `schemaV2UserDeckView`, `legacyUserCardView`,
  `schemaV4UserCardView`, `schemaV5UserCardView`, or `toJSON` for the newest
  shape). `cardChangesForSchema` filters out fen-backed cards entirely for
  clients that don't support fen.
- `pushRoute` — the client sends `lastPulledAt` plus a `Changes` changeset. It:
  1. validates the user only modifies their own `user_card_views`/`user_deck_views`
     (`validateOwnership`),
  2. rejects re-creating a deck id that was previously deleted
     (`recreatedDeletedDeckError`),
  3. validates card `fen` length (`validateCardFen`, max 92 chars),
  4. rejects "infeasible" card updates/creates — trial counts or next-review
     times that couldn't legitimately result from real study activity
     (`validateOrFailWith` + `reportInfeasibleCardUpdated`/`Created`, which just
     log details via `putStrLn` before failing),
  5. detects merge conflicts: rejects the push if any of the touched rows were
     modified server-side after the client's `lastPulledAt`
     (`neitherM [UserDeckView.modified ..., UserCardView.modified ...]`),
  6. updates the user's XP/streak via `updateUser` (only if there are updated
     card views in the batch — see `Repo/User.hs`),
  7. applies inserts/updates/deletes to `user_card_views` and `user_deck_views`,
  8. as a TODO-flagged side effect, upserts any authored decks into the public
     `decks` catalog.
- `server` — wires `pullRouteVersioned`/`pushRoute` behind JWT auth; falls back
  to `Unauthorized "No access."` for unauthenticated requests.

Key invariant: **`pushRoute` only calls `User.updateXP` (and therefore only
touches the streak) when `changes.user_card_views.updated` is non-empty** —
newly *created* card views alone do not advance the streak.

## src/Repo/User.hs

Database access for the `users` table (login/lookup, XP, streak).

- `insert` / `insertSocial` — create a user row; new users start at `xp = 10`,
  `streak` defaults to whatever the DB schema default is (observed as `0` in
  tests).
- `find` (matches username OR email), `findUsername`, `findEmail` — lookups.
- `delete` — cascades: removes the user's card views, authored decks, deck
  views, tombstone rows, then the user row itself.
- `changePwd`, `getUserID`, `verify` — auth-adjacent helpers.
- `nextStreakStampAt :: UTCTime -> User -> User` — pure function computing the
  next streak/`last_activity` state given "now" and the current user record.
  As of this session's change:
  - `streak == 0` → bump to `1` and stamp `last_activity = now` (handles a
    brand-new user's first review; without this, a fresh user reviewing
    within 12h of signup would never leave streak 0).
  - `elapsed >= 12h` since `last_activity` → increment streak by 1, stamp
    `last_activity`.
  - otherwise (< 12h) → leave streak untouched, just stamp `last_activity`.
  - **There is no longer a "lapsed streak" branch.** Previously, `elapsed >
    48h` reset the streak to `1`; that branch was removed so that
    `pushRoute` never *decreases* a streak — it only ever holds steady or
    increments. Resetting streaks after a missed deadline is intended to be
    handled by a separate cron job (not yet implemented as of this doc).
  - `nextStreakStamp` is the `IO` wrapper that supplies `getCurrentTime`.
- `updateXP :: Integer -> User -> AppM User` — applies `nextStreakStamp`, adds
  XP via `calcXp nrCards streakedUser.streak` (streak affects the XP
  multiplier — see `Repo/Xp.hs`, not yet read in full), and persists
  `streak`, `last_activity`, `xp` in one `UPDATE ... RETURNING` query.

## test/Repo/UserSpec.hs

Hspec suite for `Repo.User`, run against a real Postgres test DB via
`withCleanDb` (from `TestHelpers`, not yet read in full).

Covers `insert` (success + duplicate username/email failure), `find`,
`findUsername`, `updateXP` (XP increases, streak reaches 1 for a fresh user),
and a dedicated `nextStreakStampAt` block that exercises the pure streak
transition function directly with fixed `read "..."`-literal timestamps:
zero-streak startup, exact-12h increment, sub-12h no-op (but `last_activity`
still updates), exact-48h increment, and — after this session's change —
confirmation that streaks keep incrementing (rather than resetting) no matter
how long the gap since last activity is. Also covers password hashing
determinism/uniqueness via `hashWithSalt`.

## Gaps — not yet documented here

Read only partially (via `grep` or small excerpts) and deliberately left out
until read in full: `src/Repo/StreakNotification.hs`,
`src/Worker/StreakNotification.hs`, `src/Routes/StreakNotification.hs`,
`src/Repo/Xp.hs`, `src/Routes/Auth.hs`, `src/Routes/User.hs`,
`src/Models/User.hs`, `src/Repo/UserIdentity.hs`, `test/Routes/WatermelonSpec.hs`
(only two `it` blocks read), `test/TestHelpers.hs`, and everything else under
`src/App/`, `src/Models/`, `src/Repo/`, `src/Routes/` not named above.
