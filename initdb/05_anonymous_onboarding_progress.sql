CREATE TABLE IF NOT EXISTS anonymous_onboarding_progress (
  onboarding_session_id VARCHAR(128) PRIMARY KEY,
  last_step VARCHAR(100) NOT NULL,
  stopped BOOLEAN NOT NULL DEFAULT FALSE,
  chess_level VARCHAR(50),
  elo VARCHAR(50),
  organization VARCHAR(50),
  motivation VARCHAR(250),
  study_goal VARCHAR(50),
  claimed_by_user VARCHAR(250) UNIQUE REFERENCES users(username) ON DELETE SET NULL,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS anonymous_onboarding_progress_last_step_idx
  ON anonymous_onboarding_progress(last_step);

CREATE INDEX IF NOT EXISTS anonymous_onboarding_progress_last_modified_idx
  ON anonymous_onboarding_progress(last_modified);
