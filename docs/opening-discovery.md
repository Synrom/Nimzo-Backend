# Opening discovery onboarding

## Architecture and deployment

The mobile client calls the official public Lichess and Chess.com APIs directly.
Platform selection precedes username entry. Public profile and rapid/blitz/bullet
ratings are shown with the exact question “Is that you?” before any games are
requested. Lichess provisional ratings are marked; absent/unplayed formats show
“Unrated / no published rating”, never a fabricated zero. Chess.com stats are
fetched after its profile, serially; a stats failure never reclassifies an existent
account as missing. Confirmation is not authentication or proof of ownership.
Edit returns to username entry with a change-platform option.

The analysis window is two UTC calendar months before confirmation through the
same captured current timestamp, inclusive, clamping the day for shorter target
months. Lichess uses a single supported `since`/`until` NDJSON export with NO max
parameter; local filtering uses `createdAt` (game start). Chess.com filters its
archive list to intersecting UTC months (normally three), then filters every game
by `end_time` (game end, seconds converted to milliseconds). These platform date
semantics are explicit; no older games are substituted and no 200-game cap remains.
All archive requests are serial. Standard completed chess with moves only;
custom positions, unsupported results and nonstandard variants are excluded.
The shown count is the usable sample, not the account's total games.

The result reports the full requested date interval and `complete_public_period`
only after every required request succeeds. Any timeout/429/403/malformed body or
archive/export error discards the incomplete run and offers retry/library; it
never presents partial data as complete. Public API caches (especially Chess.com,
up to 12h) can lag; completeness means available public data, not guaranteed real-time
account history. Normal requests timeout after 45 seconds; the throttled Lichess
export allows 10 minutes. All are cancellable. RN buffers the export before parsing;
parsing yields every 100 records to keep cancellation responsive. Very large
histories may require retry or browsing without analysis rather than partial results.
Only profile HTTP404 is missing; stats/archive failures and HTTP410 are unavailable.

Lichess autocomplete uses documented `/api/player/autocomplete?term=...&names=true`
after 400ms debounce, minimum3 characters, max5 deduplicated prefix matches. Each
edit/platform/stage change aborts pending suggestions and rejects stale results.
Autocomplete errors quietly retain manual entry; they are not existence checks.
The current Chess.com Published-Data API documents no public username autocomplete;
manual entry is intentional. No scraping or undocumented endpoint is used.

Question screens reuse Elo's centered Quicksand heading, wheel Picker,
OnboardingContent/useResponsive bounds and fixed FooterSpace/Continue button.
Deck lookup/list/search use indeterminate progress because the response has no
known total. Deck card download uses actual accumulated card count and the deck's
published total; unknown or inconsistent totals remain indeterminate. No timer
fabricates a percentage. A full card-fetch bar is not a claim that local saving
has completed; its label states saving until the normal download finishes.

Backend changes extend the existing POST /onboarding/events scalar allowlist.
There is no new public analysis endpoint, external credential, or game database.
Apply `initdb/30_opening_discovery_reporting.sql` after migrations 28 and 29.
It creates views only; it does not rewrite tables. Deploy the allowlist before
shipping the client, otherwise the server correctly rejects unknown properties.
Nothing in this change has been deployed to production.

## Events and privacy

The client outbox stores events in AsyncStorage before sending them. Batches
contain at most 50 events; IDs survive retries, server acknowledgement controls
removal, and the existing INSERT ON CONFLICT contract prevents duplicates.
Retry every 15 seconds while the onboarding provider is mounted and on relaunch.
Delivery is best effort if a user deletes app data or never reconnects. The queue
is serialized to avoid concurrent read/modify/write races. Installation IDs persist
across journeys, visit IDs identify a JS process visit, and a persisted sequence
counter orders events across relaunches. Native app/build versions are reported. This is client-observed
telemetry, not cryptographic proof of an account or a server-verified identity.

- `opening_answer`: question (`acquisition_source`, `knows_opening`,
  `chess_platform`) and scalar response. Acquisition answers are also stored
  locally. No username is persisted by discovery.
- `opening_account_attempt`: unique attempt_id and chess_platform.
- `opening_account_result`: same attempt_id and outcome: existent, missing,
  rate_limit, timeout, unavailable, invalid_response. Exactly one validation
  result is emitted per completed validation. Existent remains existent even
  when the later game export fails.
- `opening_profile_viewed`, `opening_profile_confirmed`, `opening_profile_edited`,
  `opening_profile_failed`, `opening_profile_cancelled`: attempt/platform and
  bounded outcome only, never names, ratings, search terms or suggestions.
- `opening_analysis_started`: attempt/platform and exact requested UTC period.
- `opening_analysis_failed`, `opening_analysis_cancelled`: lifecycle failures.
- `opening_analysis_completed`: usable and classified sample counts, score,
  scenario_gain (nullable), recommendation_status, optional deck_id, period_start,
  period_end (ISO UTC), and completeness. Numeric scalar bounds remain 1e15;
  counts are not bounded at 200. Scenario uses the actual full-period N and S.
- `opening_analysis_group`: per-opening family and player color, sample count,
  loss count, score and selected flag. These allow comparative reports without
  retaining any game moves, game IDs, opponent names, or public usernames.
- `opening_results_viewed`, `opening_back`, `opening_skipped`,
  `opening_library_opened`, `opening_deck_selected`, `opening_deck_download`:
  navigation and normal library/download interactions; downloads distinguish
  started, completed and failed. A selected deck is not a completed download.

No usernames, raw PGNs, ratings, emails, credentials or positions are sent to
telemetry. API host receives the entered public username as required to retrieve
public data. Telemetry retention follows the existing onboarding retention policy.
Reports must filter environment; development data is not conversion evidence.

## Classification and recommendation

`src/Onboarding/discovery/openings.json` is a compact projection of the supplied
`nimzo-miner/.../all.tsv` (3618 ECO/name/PGN rows). The client builds a SAN-prefix
trie once. Classification is linear in game-prefix length, stops at the first
unknown continuation and takes the deepest matched name. PGN comments, tags,
variations, NAGs, check annotations and results are stripped. This is move-order
classification, not engine evaluation: transpositions with different move orders
can be classified differently. Variations are grouped under the name before `:`;
color is always a separate grouping key. ECO is a representative matched code,
not a claim that every game in that family shares that code.

The target is the largest loss count, tie-broken by games then name. It does NOT
claim the highest loss rate or that an opening caused the losses. Screens show
losses / games and percentages for comparison, and qualify small samples. The
recommendation searches existing decks by the exact family name, requires that
name as a word-bounded title phrase, matching player color (or both), and at least
one card. A variation deck offers one way to play that opening, not a claim that
its moves exactly reproduce every lost game. No cross-opening fallback is made.
If there is no match or search is unavailable, continue to browse/search normally.
Recommendation search has a 15-second deadline and is cancelled with analysis;
an available recommendation does not wait on the unrelated featured-deck request.
The normal Library → DeckLook → download → subsequent onboarding path is retained.

## Rating scenario (version 1)

Only shown numerically with >=20 usable games and >=3 target losses.
Let N be usable games, S total score (win=1, draw=.5, loss=0), and L target losses.
Assume half of L losses become draws: extra score = L/4.
Set p=(S+.5)/(N+1), q=(S+L/4+.5)/(N+1).
Scenario = round(400 * (log10(q/(1-q)) - log10(p/(1-p)))).
The .5 smoothing prevents infinite results in all-win/all-loss samples. Opponents
are assumed unchanged. This is an Elo-equivalent performance illustration, NOT
an estimate of guaranteed platform rating gain. Lichess uses Glicko-2 and
Chess.com also uses a rating model beyond simple Elo; the app states this caveat.
No rating or opponent strength is needed/stored for this scenario.

## Reporting queries

```sql
SELECT * FROM onboarding_opening_attempt_counts WHERE environment='production';
-- Distinguishes validation failures from truly nonexistent accounts.
SELECT payload->'properties'->>'question' question,
       payload->'properties'->>'response' answer, count(*)
FROM onboarding_events
WHERE payload->>'event_name'='opening_answer'
  AND payload->>'environment'='production' GROUP BY 1,2;
-- Exact event IDs are deduplicated; answer counts are answer events, not users.
SELECT payload->>'onboarding_session_id', payload->'properties'
FROM onboarding_events
WHERE payload->>'event_name'='opening_analysis_completed';
```

The attempt views group by environment, session and attempt ID; cancellations
can coexist with an existent account. Interrupted attempts without a terminal
event remain started only. Counts are attempts, not distinct people.

## Local verification

Use a separate local database, never a production connection. Optional
`scripts/seed_opening_discovery_qa.py` reads two existing PUBLIC decks and cards
from the production read API and copies them into `nimzo-onboarding-qa` only.
`isDownload=false` avoids reporting production downloads. No synthetic external
games are used in the public smoke check (`scripts/check-opening-public.cjs`
in the frontend). Unit tests use explicit mocks only for edge cases.

Official API references consulted:
- https://lichess.org/api#tag/Games/operation/apiGamesUser
- https://github.com/lichess-org/api/blob/master/doc/specs/tags/games/api-games-user-username.yaml
- https://support.chess.com/en/articles/9650547-what-is-the-pubapi-and-how-do-i-use-it
- https://www.chess.com/news/view/published-data-api

Iteration verification (2026-10-23): real Synrom Lichess period returned152 usable
games, Sicilian White7/14 losses; Chess.com has no archive in this period (latest
published archive June2026). Its honest zero-data flow therefore offers normal
library browsing without a personalized recommendation or numerical scenario.
Public ratings observed: Lichess rapid1953 provisional/blitz1813/bullet1640;
Chess.com rapid1025/blitz1585/bullet763. These are QA observations, not telemetry.
Official autocomplete schema checked: https://github.com/lichess-org/api/blob/master/doc/specs/tags/users/api-player-autocomplete.yaml
