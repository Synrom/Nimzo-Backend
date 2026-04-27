CREATE TABLE users (
  username VARCHAR(250) PRIMARY KEY,
  password VARCHAR(100) NOT NULL,
  salt VARCHAR(16) NOT NULL,
  premium BOOLEAN NOT NULL DEFAULT FALSE,
  xp INTEGER NOT NULL DEFAULT 10,
  streak INTEGER NOT NULL DEFAULT 0,
  last_activity TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  rank INTEGER NOT NULL DEFAULT 0,
  email VARCHAR(250) UNIQUE NOT NULL,
  verified BOOLEAN NOT NULL DEFAULT FALSE,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE user_identities (
  id SERIAL PRIMARY KEY,
  username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,
  provider VARCHAR(20) NOT NULL,
  provider_subject VARCHAR(255) NOT NULL,
  email VARCHAR(250),
  email_verified BOOLEAN NOT NULL DEFAULT FALSE,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (provider, provider_subject)
);

CREATE INDEX user_identities_username_idx ON user_identities(username);

CREATE TABLE user_onboarding_preferences (
  user_id VARCHAR(250) PRIMARY KEY REFERENCES users(username) ON DELETE CASCADE,
  chess_level VARCHAR(50) NOT NULL,
  elo VARCHAR(50) NOT NULL,
  organization VARCHAR(50) NOT NULL,
  motivation VARCHAR(250) NOT NULL,
  study_goal VARCHAR(50) NOT NULL,
  heard_about_us VARCHAR(50) NOT NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE anonymous_onboarding_progress (
  onboarding_session_id VARCHAR(128) PRIMARY KEY,
  last_step VARCHAR(100) NOT NULL,
  stopped BOOLEAN NOT NULL DEFAULT FALSE,
  chess_level VARCHAR(50),
  elo VARCHAR(50),
  organization VARCHAR(50),
  motivation VARCHAR(250),
  study_goal VARCHAR(50),
  heard_about_us VARCHAR(50),
  claimed_by_user VARCHAR(250) UNIQUE REFERENCES users(username) ON DELETE SET NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX anonymous_onboarding_progress_last_step_idx ON anonymous_onboarding_progress(last_step);
CREATE INDEX anonymous_onboarding_progress_last_modified_idx ON anonymous_onboarding_progress(last_modified);

CREATE TABLE user_deck_views (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  num_cards_today INTEGER NOT NULL DEFAULT 0,
  new_cards_today INTEGER NOT NULL DEFAULT 0,
  last_study_date VARCHAR(10) NOT NULL DEFAULT '',
  is_author BOOLEAN DEFAULT FALSE,
  cards_per_day INTEGER NOT NULL DEFAULT 20,
  num_cards_learnt INTEGER NOT NULL DEFAULT 0,
  name VARCHAR(250) NOT NULL,
  is_public BOOLEAN NOT NULL,
  description VARCHAR(500),
  color VARCHAR(2),
  source_user_deck_id VARCHAR(250) REFERENCES user_deck_views(id),
  num_cards_total INTEGER NOT NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE user_card_views (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  user_deck_id VARCHAR(250) NOT NULL REFERENCES user_deck_views(id),
  num_correct_trials INTEGER NOT NULL DEFAULT 0,
  next_request BIGINT NOT NULL,
  moves VARCHAR(1000) NOT NULL,
  title VARCHAR(250) NOT NULL,
  color VARCHAR(2) NOT NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE decks (
  id SERIAL PRIMARY KEY,
  name VARCHAR(250) NOT NULL,
  user_deck_id VARCHAR(250) NOT NULL UNIQUE REFERENCES user_deck_views(id),
  is_public BOOLEAN NOT NULL DEFAULT FALSE,
  description VARCHAR(500),
  color VARCHAR(2),
  image_url VARCHAR(600),
  featured_source VARCHAR(50),
  featured_rank INTEGER,
  video_url VARCHAR(1000),
  author VARCHAR(250) NOT NULL REFERENCES users(username),
  num_cards_total INTEGER NOT NULL DEFAULT 0,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE deleted_udvs (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  deleted_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE
);

CREATE TABLE deleted_ucvs (
  id VARCHAR(250) PRIMARY KEY,
  user_id VARCHAR(250) NOT NULL REFERENCES users(username),
  deleted_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE
);

ALTER TABLE decks ADD COLUMN search_vector tsvector
  GENERATED ALWAYS AS (
    setweight(to_tsvector('english', coalesce(name, '')), 'A') ||
    setweight(to_tsvector('english', coalesce(description, '')), 'B')
  ) STORED;

CREATE INDEX decks_search_vector_idx ON decks USING GIN (search_vector);

ALTER TABLE decks ADD COLUMN search_vector_name tsvector
  GENERATED ALWAYS AS (
    to_tsvector('english', name)
  ) STORED;
CREATE INDEX decks_search_vector_name_idx ON decks USING GIN (search_vector_name);

-- Supports efficient prefix search on moves for the continuations endpoint
CREATE INDEX user_card_views_deck_moves_idx ON user_card_views (user_deck_id, moves varchar_pattern_ops);
