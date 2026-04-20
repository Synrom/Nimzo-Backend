BEGIN;

DO $$
DECLARE
  suffix TEXT := txid_current()::text;
  author_name TEXT := 'smoke_author_' || suffix;
  rater_name TEXT := 'smoke_rater_' || suffix;
  importer_name TEXT := 'smoke_importer_' || suffix;
  source_udv_id TEXT := 'udv_smoke_' || suffix;
  import_udv_id TEXT := 'import_smoke_' || suffix;
  deck_row_id INTEGER;
  got_prefix TEXT;
  got_avg DOUBLE PRECISION;
  got_count BIGINT;
  got_downloads BIGINT;
BEGIN
  INSERT INTO users (username, password, salt, email)
  VALUES
    (author_name, 'x', 'salt123456789012', author_name || '@example.com'),
    (rater_name, 'x', 'salt123456789012', rater_name || '@example.com'),
    (importer_name, 'x', 'salt123456789012', importer_name || '@example.com');

  INSERT INTO user_deck_views (id, user_id, is_author, name, is_public, num_cards_total)
  VALUES (source_udv_id, author_name, TRUE, 'Smoke Deck', TRUE, 0);

  INSERT INTO decks (name, is_public, description, color, num_cards_total, author, user_deck_id)
  VALUES ('Smoke Deck', TRUE, 'smoke', 'b', 0, author_name, source_udv_id)
  RETURNING id INTO deck_row_id;

  INSERT INTO user_card_views (id, user_id, user_deck_id, moves, title, color, next_request)
  VALUES
    ('ucv_a_' || suffix, author_name, source_udv_id, 'e4 c5 Nf3 d6', 'A', 'b', 0),
    ('ucv_b_' || suffix, author_name, source_udv_id, 'e4 c5 Nc3 e6', 'B', 'b', 0);

  SELECT starting_position INTO got_prefix FROM decks WHERE id = deck_row_id;
  IF got_prefix <> 'e4 c5' THEN
    RAISE EXCEPTION 'starting_position mismatch: expected e4 c5, got %', got_prefix;
  END IF;

  INSERT INTO deck_ratings (deck_id, user_id, rating) VALUES (deck_row_id, rater_name, 4);
  INSERT INTO deck_ratings (deck_id, user_id, rating) VALUES (deck_row_id, author_name, 5);

  SELECT rating_avg, rating_count INTO got_avg, got_count FROM decks WHERE id = deck_row_id;
  IF got_avg <> 4.5 OR got_count <> 2 THEN
    RAISE EXCEPTION 'rating mismatch: expected avg=4.5,count=2 got avg=%,count=%', got_avg, got_count;
  END IF;

  INSERT INTO user_deck_views (id, user_id, is_author, source_user_deck_id, name, is_public, num_cards_total)
  VALUES (import_udv_id, importer_name, FALSE, source_udv_id, 'Imported Smoke Deck', FALSE, 0);

  SELECT download_count INTO got_downloads FROM decks WHERE id = deck_row_id;
  IF got_downloads <> 1 THEN
    RAISE EXCEPTION 'download_count mismatch: expected 1, got %', got_downloads;
  END IF;
END;
$$;

ROLLBACK;
