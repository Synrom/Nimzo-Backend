-- Staging rollout smoke check for featured card consistency.
-- Reports decks that have featured_source set but no featured_card_id.
SELECT
  d.id AS deck_id,
  d.user_deck_id,
  d.name,
  d.author,
  d.featured_source,
  d.featured_rank,
  d.last_modified
FROM decks d
WHERE d.featured_source IS NOT NULL
  AND d.featured_card_id IS NULL
ORDER BY d.featured_source, d.id;
