-- Staging rollout smoke check for featured deck lines consistency.
-- Reports decks that have featured_source set but no linked featured_deck_lines row.
SELECT
  d.id AS deck_id,
  d.user_deck_id,
  d.name,
  d.author,
  d.featured_source,
  d.featured_rank,
  d.last_modified
FROM decks d
LEFT JOIN featured_deck_lines fdl
  ON fdl.deck_id = d.id
WHERE d.featured_source IS NOT NULL
  AND fdl.deck_id IS NULL
ORDER BY d.featured_source, d.id;
