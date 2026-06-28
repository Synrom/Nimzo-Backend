CREATE TABLE IF NOT EXISTS user_explanation_views (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  user_deck_id VARCHAR(250) NOT NULL REFERENCES user_deck_views(id),
  fen VARCHAR(250) NOT NULL,
  move VARCHAR(50) NOT NULL,
  text TEXT NOT NULL,
  visualizers TEXT NOT NULL DEFAULT '{}',
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS user_explanation_views_user_deck_id_idx
  ON user_explanation_views(user_deck_id);

CREATE INDEX IF NOT EXISTS user_explanation_views_fen_idx
  ON user_explanation_views(fen);

CREATE TABLE IF NOT EXISTS deleted_explanation_views (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  deleted_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE
);
