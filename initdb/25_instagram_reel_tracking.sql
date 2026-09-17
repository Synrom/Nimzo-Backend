CREATE TABLE IF NOT EXISTS deck_instagram_reels (
  id SERIAL PRIMARY KEY,
  deck_id INTEGER NOT NULL REFERENCES decks(id) ON DELETE CASCADE,
  ig_media_id VARCHAR(64) NOT NULL UNIQUE,
  note VARCHAR(250),
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX IF NOT EXISTS deck_instagram_reels_deck_id_idx ON deck_instagram_reels(deck_id);

CREATE TABLE IF NOT EXISTS instagram_reel_view_snapshots (
  id BIGSERIAL PRIMARY KEY,
  reel_id INTEGER NOT NULL REFERENCES deck_instagram_reels(id) ON DELETE CASCADE,
  views BIGINT NOT NULL,
  fetched_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS instagram_reel_view_snapshots_reel_fetched_idx
  ON instagram_reel_view_snapshots(reel_id, fetched_at DESC);

CREATE TABLE IF NOT EXISTS instagram_api_tokens (
  id INTEGER PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  access_token TEXT NOT NULL,
  token_expires_at TIMESTAMP WITH TIME ZONE,
  last_refreshed_at TIMESTAMP WITH TIME ZONE DEFAULT now()
);
