BEGIN;

ALTER TABLE decks
  ADD COLUMN IF NOT EXISTS featured_card_id VARCHAR(250);

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.tables
    WHERE table_schema = 'public' AND table_name = 'featured_deck_lines'
  ) THEN
    UPDATE decks d
    SET featured_card_id = fdl.featured_card_id,
        last_modified = CURRENT_TIMESTAMP
    FROM featured_deck_lines fdl
    WHERE fdl.deck_id = d.id
      AND d.featured_card_id IS NULL;

    DROP TRIGGER IF EXISTS featured_deck_lines_validate_card_tgr ON featured_deck_lines;
    DROP TABLE IF EXISTS featured_deck_lines;
  END IF;

  DROP FUNCTION IF EXISTS validate_featured_deck_line_card_belongs_to_deck();
END $$;

COMMIT;
