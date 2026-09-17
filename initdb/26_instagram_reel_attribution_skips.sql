-- Persists Reels that attribute_instagram_reels_to_decks.py checked and
-- could not match to any deck, so the hourly cron run doesn't re-fetch and
-- re-match the same unmatched Reels forever. See scripts/docs/architecture.md.
CREATE TABLE IF NOT EXISTS instagram_reel_attribution_skips (
  ig_media_id VARCHAR(64) PRIMARY KEY,
  caption VARCHAR(2200),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);
