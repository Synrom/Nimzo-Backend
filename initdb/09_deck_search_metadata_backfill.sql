BEGIN;

UPDATE user_deck_views
SET source_user_deck_id = NULL
WHERE is_author = TRUE AND source_user_deck_id IS NOT NULL;

WITH inferred AS (
  SELECT
    udv.id AS udv_id,
    (
      SELECT d.user_deck_id
      FROM decks d
      WHERE RIGHT(udv.id, LENGTH(d.user_deck_id)) = d.user_deck_id
      ORDER BY LENGTH(d.user_deck_id) DESC
      LIMIT 1
    ) AS source_user_deck_id
  FROM user_deck_views udv
  WHERE udv.is_author = FALSE
    AND COALESCE(udv.source_user_deck_id, '') = ''
)
UPDATE user_deck_views udv
SET source_user_deck_id = inferred.source_user_deck_id
FROM inferred
WHERE udv.id = inferred.udv_id
  AND inferred.source_user_deck_id IS NOT NULL;

UPDATE decks
SET
  rating_sum = 0,
  rating_count = 0,
  rating_avg = NULL,
  download_count = 0,
  starting_position = '';

UPDATE decks d
SET
  rating_sum = rs.rating_sum,
  rating_count = rs.rating_count,
  rating_avg = rs.rating_avg
FROM (
  SELECT
    dr.deck_id,
    COALESCE(SUM(dr.rating), 0)::integer AS rating_sum,
    COUNT(*)::bigint AS rating_count,
    ROUND((AVG(dr.rating))::numeric, 1)::double precision AS rating_avg
  FROM deck_ratings dr
  GROUP BY dr.deck_id
) rs
WHERE d.id = rs.deck_id;

UPDATE decks d
SET
  num_cards_total = card_stats.num_cards_total,
  starting_position = card_stats.starting_position
FROM (
  SELECT
    d2.user_deck_id,
    COUNT(ucv.id)::integer AS num_cards_total,
    common_moves_prefix(d2.user_deck_id, 6) AS starting_position
  FROM decks d2
  LEFT JOIN user_card_views ucv ON ucv.user_deck_id = d2.user_deck_id
  GROUP BY d2.user_deck_id
) card_stats
WHERE d.user_deck_id = card_stats.user_deck_id;

UPDATE decks d
SET
  download_count = dl.download_count
FROM (
  SELECT
    d2.id AS deck_id,
    COUNT(udv.id)::bigint AS download_count
  FROM decks d2
  LEFT JOIN user_deck_views udv
    ON udv.is_author = FALSE
   AND udv.source_user_deck_id = d2.user_deck_id
  GROUP BY d2.id
) dl
WHERE d.id = dl.deck_id;

COMMIT;
