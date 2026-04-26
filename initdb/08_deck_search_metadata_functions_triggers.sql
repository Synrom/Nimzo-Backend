BEGIN;

CREATE OR REPLACE FUNCTION common_moves_prefix(p_user_deck_id VARCHAR(250), p_max_plies INTEGER DEFAULT 6)
RETURNS VARCHAR
LANGUAGE SQL
STABLE
AS $$
WITH move_arrays AS (
  SELECT regexp_split_to_array(trim(ucv.moves), '[[:space:]]+') AS toks
  FROM user_card_views ucv
  WHERE ucv.user_deck_id = p_user_deck_id
    AND trim(ucv.moves) <> ''
),
bounds AS (
  SELECT
    COUNT(*) AS deck_rows,
    LEAST(COALESCE(MIN(cardinality(toks)), 0), GREATEST(p_max_plies, 0)) AS max_idx
  FROM move_arrays
),
positions AS (
  SELECT gs AS idx
  FROM bounds b
  JOIN generate_series(1, b.max_idx) gs ON TRUE
  WHERE b.deck_rows > 0
),
token_checks AS (
  SELECT
    p.idx,
    MIN(ma.toks[p.idx]) AS token,
    COUNT(DISTINCT ma.toks[p.idx]) = 1 AS all_same
  FROM positions p
  JOIN move_arrays ma ON TRUE
  GROUP BY p.idx
),
first_mismatch AS (
  SELECT COALESCE(MIN(idx), 2147483647) AS mismatch_idx
  FROM token_checks
  WHERE NOT all_same
)
SELECT COALESCE(string_agg(tc.token, ' ' ORDER BY tc.idx), '')
FROM token_checks tc
CROSS JOIN first_mismatch fm
WHERE tc.all_same
  AND tc.idx < fm.mismatch_idx;
$$;

CREATE OR REPLACE FUNCTION first_n_moves(p_moves VARCHAR, p_max_plies INTEGER DEFAULT 6)
RETURNS VARCHAR
LANGUAGE SQL
IMMUTABLE
AS $$
SELECT COALESCE(
  array_to_string(
    (regexp_split_to_array(trim(COALESCE(p_moves, '')), '[[:space:]]+'))[1:GREATEST(p_max_plies, 0)],
    ' '
  ),
  ''
);
$$;

CREATE OR REPLACE FUNCTION common_moves_prefix_two(
  p_existing_prefix VARCHAR,
  p_new_moves VARCHAR,
  p_max_plies INTEGER DEFAULT 6
)
RETURNS VARCHAR
LANGUAGE SQL
IMMUTABLE
AS $$
WITH existing_tokens AS (
  SELECT regexp_split_to_array(trim(COALESCE(p_existing_prefix, '')), '[[:space:]]+') AS toks
),
new_tokens AS (
  SELECT regexp_split_to_array(trim(COALESCE(p_new_moves, '')), '[[:space:]]+') AS toks
),
bounds AS (
  SELECT
    LEAST(
      COALESCE(cardinality(e.toks), 0),
      COALESCE(cardinality(n.toks), 0),
      GREATEST(p_max_plies, 0)
    ) AS max_idx,
    e.toks AS existing_toks,
    n.toks AS new_toks
  FROM existing_tokens e
  CROSS JOIN new_tokens n
),
positions AS (
  SELECT gs AS idx
  FROM bounds b
  JOIN generate_series(1, b.max_idx) gs ON TRUE
),
token_checks AS (
  SELECT
    p.idx,
    b.existing_toks[p.idx] AS token,
    b.existing_toks[p.idx] = b.new_toks[p.idx] AS all_same
  FROM positions p
  CROSS JOIN bounds b
),
first_mismatch AS (
  SELECT COALESCE(MIN(idx), 2147483647) AS mismatch_idx
  FROM token_checks
  WHERE NOT all_same
)
SELECT COALESCE(string_agg(tc.token, ' ' ORDER BY tc.idx), '')
FROM token_checks tc
CROSS JOIN first_mismatch fm
WHERE tc.all_same
  AND tc.idx < fm.mismatch_idx;
$$;

CREATE OR REPLACE FUNCTION refresh_deck_download_count_for_source_id(p_source_udv_id VARCHAR(250))
RETURNS VOID
LANGUAGE SQL
AS $$
UPDATE decks d
SET
  download_count = (
    SELECT COUNT(*)::bigint
    FROM user_deck_views udv
    WHERE udv.is_author = FALSE
      AND udv.source_user_deck_id = d.user_deck_id
  ),
  last_modified = CURRENT_TIMESTAMP
WHERE d.user_deck_id = p_source_udv_id;
$$;

CREATE OR REPLACE FUNCTION trg_user_deck_views_infer_source()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  inferred_source VARCHAR(250);
BEGIN
  IF NEW.is_author THEN
    NEW.source_user_deck_id := NULL;
    RETURN NEW;
  END IF;

  IF COALESCE(NEW.source_user_deck_id, '') = '' THEN
    SELECT d.user_deck_id INTO inferred_source
    FROM decks d
    WHERE RIGHT(NEW.id, LENGTH(d.user_deck_id)) = d.user_deck_id
    ORDER BY LENGTH(d.user_deck_id) DESC
    LIMIT 1;

    NEW.source_user_deck_id := inferred_source;
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS user_deck_views_infer_source_trigger ON user_deck_views;
CREATE TRIGGER user_deck_views_infer_source_trigger
BEFORE INSERT OR UPDATE OF id, is_author, source_user_deck_id ON user_deck_views
FOR EACH ROW
EXECUTE FUNCTION trg_user_deck_views_infer_source();

CREATE OR REPLACE FUNCTION trg_user_card_views_refresh_decks()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  UPDATE decks d
  SET
    num_cards_total = d.num_cards_total + 1,
    starting_position = CASE
      WHEN d.num_cards_total <= 0 THEN first_n_moves(NEW.moves, 6)
      ELSE common_moves_prefix_two(d.starting_position, NEW.moves, 6)
    END,
    last_modified = CURRENT_TIMESTAMP
  WHERE d.user_deck_id = NEW.user_deck_id;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS user_card_views_refresh_decks_trigger ON user_card_views;
CREATE TRIGGER user_card_views_refresh_decks_trigger
AFTER INSERT ON user_card_views
FOR EACH ROW
EXECUTE FUNCTION trg_user_card_views_refresh_decks();

CREATE OR REPLACE FUNCTION trg_deck_ratings_apply_delta()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'INSERT' THEN
    UPDATE decks
    SET
      rating_sum = rating_sum + NEW.rating,
      rating_count = rating_count + 1,
      rating_avg = ROUND(((rating_sum + NEW.rating)::numeric / NULLIF(rating_count + 1, 0)), 1)::double precision,
      last_modified = CURRENT_TIMESTAMP
    WHERE id = NEW.deck_id;
    RETURN NEW;
  ELSIF TG_OP = 'DELETE' THEN
    UPDATE decks
    SET
      rating_sum = GREATEST(0, rating_sum - OLD.rating),
      rating_count = GREATEST(0, rating_count - 1),
      rating_avg = CASE
        WHEN rating_count - 1 <= 0 THEN NULL
        ELSE ROUND(((rating_sum - OLD.rating)::numeric / (rating_count - 1)), 1)::double precision
      END,
      last_modified = CURRENT_TIMESTAMP
    WHERE id = OLD.deck_id;
    RETURN OLD;
  ELSE
    IF NEW.deck_id = OLD.deck_id THEN
      UPDATE decks
      SET
        rating_sum = rating_sum + NEW.rating - OLD.rating,
        rating_avg = CASE
          WHEN rating_count <= 0 THEN NULL
          ELSE ROUND(((rating_sum + NEW.rating - OLD.rating)::numeric / rating_count), 1)::double precision
        END,
        last_modified = CURRENT_TIMESTAMP
      WHERE id = NEW.deck_id;
    ELSE
      UPDATE decks
      SET
        rating_sum = GREATEST(0, rating_sum - OLD.rating),
        rating_count = GREATEST(0, rating_count - 1),
        rating_avg = CASE
          WHEN rating_count - 1 <= 0 THEN NULL
          ELSE ROUND(((rating_sum - OLD.rating)::numeric / (rating_count - 1)), 1)::double precision
        END,
        last_modified = CURRENT_TIMESTAMP
      WHERE id = OLD.deck_id;

      UPDATE decks
      SET
        rating_sum = rating_sum + NEW.rating,
        rating_count = rating_count + 1,
        rating_avg = ROUND(((rating_sum + NEW.rating)::numeric / NULLIF(rating_count + 1, 0)), 1)::double precision,
        last_modified = CURRENT_TIMESTAMP
      WHERE id = NEW.deck_id;
    END IF;
    RETURN NEW;
  END IF;
END;
$$;

DROP TRIGGER IF EXISTS deck_ratings_apply_delta_trigger ON deck_ratings;
CREATE TRIGGER deck_ratings_apply_delta_trigger
AFTER INSERT OR UPDATE OF deck_id, rating OR DELETE ON deck_ratings
FOR EACH ROW
EXECUTE FUNCTION trg_deck_ratings_apply_delta();

CREATE OR REPLACE FUNCTION trg_user_deck_views_refresh_downloads()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NOT NEW.is_author AND NEW.source_user_deck_id IS NOT NULL THEN
      PERFORM refresh_deck_download_count_for_source_id(NEW.source_user_deck_id);
    END IF;
    RETURN NEW;
  ELSIF TG_OP = 'DELETE' THEN
    IF NOT OLD.is_author AND OLD.source_user_deck_id IS NOT NULL THEN
      PERFORM refresh_deck_download_count_for_source_id(OLD.source_user_deck_id);
    END IF;
    RETURN OLD;
  ELSE
    IF NOT OLD.is_author AND OLD.source_user_deck_id IS NOT NULL THEN
      PERFORM refresh_deck_download_count_for_source_id(OLD.source_user_deck_id);
    END IF;
    IF NOT NEW.is_author AND NEW.source_user_deck_id IS NOT NULL THEN
      PERFORM refresh_deck_download_count_for_source_id(NEW.source_user_deck_id);
    END IF;
    RETURN NEW;
  END IF;
END;
$$;

DROP TRIGGER IF EXISTS user_deck_views_refresh_downloads_trigger ON user_deck_views;
CREATE TRIGGER user_deck_views_refresh_downloads_trigger
AFTER INSERT OR UPDATE OF id, is_author, source_user_deck_id OR DELETE ON user_deck_views
FOR EACH ROW
EXECUTE FUNCTION trg_user_deck_views_refresh_downloads();

CREATE OR REPLACE FUNCTION trg_decks_initialize_metadata()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  UPDATE decks d
  SET
    num_cards_total = (
      SELECT COUNT(*)::integer
      FROM user_card_views ucv
      WHERE ucv.user_deck_id = NEW.user_deck_id
    ),
    starting_position = common_moves_prefix(NEW.user_deck_id, 6),
    download_count = (
      SELECT COUNT(*)::bigint
      FROM user_deck_views udv
      WHERE udv.is_author = FALSE
        AND (
          udv.source_user_deck_id = NEW.user_deck_id
          OR (
            COALESCE(udv.source_user_deck_id, '') = ''
            AND RIGHT(udv.id, LENGTH(NEW.user_deck_id)) = NEW.user_deck_id
          )
        )
    ),
    rating_sum = COALESCE((
      SELECT SUM(dr.rating)::integer
      FROM deck_ratings dr
      WHERE dr.deck_id = NEW.id
    ), 0),
    rating_count = (
      SELECT COUNT(*)::bigint
      FROM deck_ratings dr
      WHERE dr.deck_id = NEW.id
    ),
    rating_avg = (
      SELECT ROUND(AVG(dr.rating)::numeric, 1)::double precision
      FROM deck_ratings dr
      WHERE dr.deck_id = NEW.id
    ),
    last_modified = CURRENT_TIMESTAMP
  WHERE d.id = NEW.id;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS decks_initialize_metadata_trigger ON decks;
CREATE TRIGGER decks_initialize_metadata_trigger
AFTER INSERT OR UPDATE OF user_deck_id ON decks
FOR EACH ROW
EXECUTE FUNCTION trg_decks_initialize_metadata();

COMMIT;
