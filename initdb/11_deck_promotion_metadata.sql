BEGIN;

ALTER TABLE decks
  ADD COLUMN IF NOT EXISTS featured_source VARCHAR(50),
  ADD COLUMN IF NOT EXISTS featured_rank INTEGER,
  ADD COLUMN IF NOT EXISTS video_url VARCHAR(1000);

CREATE INDEX IF NOT EXISTS decks_featured_source_rank_idx
  ON decks(featured_source, featured_rank, id);

COMMIT;
