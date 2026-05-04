BEGIN;

CREATE TABLE IF NOT EXISTS featured_deck_lines (
  deck_id INTEGER PRIMARY KEY REFERENCES decks(id) ON DELETE CASCADE,
  featured_card_id VARCHAR(250) NOT NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX IF NOT EXISTS featured_deck_lines_deck_id_idx
  ON featured_deck_lines(deck_id);

CREATE INDEX IF NOT EXISTS featured_deck_lines_featured_card_id_idx
  ON featured_deck_lines(featured_card_id);

CREATE OR REPLACE FUNCTION validate_featured_deck_line_card_belongs_to_deck()
RETURNS trigger AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM decks d
    JOIN user_card_views ucv ON ucv.user_deck_id = d.user_deck_id
    WHERE d.id = NEW.deck_id
      AND ucv.id = NEW.featured_card_id
  ) THEN
    RAISE EXCEPTION 'featured card does not belong to deck';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS featured_deck_lines_validate_card_tgr ON featured_deck_lines;

CREATE TRIGGER featured_deck_lines_validate_card_tgr
BEFORE INSERT OR UPDATE ON featured_deck_lines
FOR EACH ROW
EXECUTE FUNCTION validate_featured_deck_line_card_belongs_to_deck();

COMMIT;
