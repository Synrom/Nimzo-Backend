CREATE TABLE IF NOT EXISTS experiments (
  experiment_key VARCHAR(100) PRIMARY KEY,
  status VARCHAR(20) NOT NULL DEFAULT 'active',
  default_variant VARCHAR(100) NOT NULL,
  respect_existing_assignments BOOLEAN NOT NULL DEFAULT TRUE,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS experiment_variants (
  experiment_key VARCHAR(100) NOT NULL REFERENCES experiments(experiment_key) ON DELETE CASCADE,
  variant_key VARCHAR(100) NOT NULL,
  weight INTEGER NOT NULL DEFAULT 0,
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (experiment_key, variant_key)
);

CREATE TABLE IF NOT EXISTS experiment_assignments (
  onboarding_session_id VARCHAR(128) NOT NULL,
  experiment_key VARCHAR(100) NOT NULL REFERENCES experiments(experiment_key) ON DELETE CASCADE,
  variant_key VARCHAR(100) NOT NULL,
  app_version VARCHAR(50),
  platform VARCHAR(20),
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (onboarding_session_id, experiment_key)
);

CREATE INDEX IF NOT EXISTS experiment_assignments_experiment_variant_idx
  ON experiment_assignments(experiment_key, variant_key);

CREATE TABLE IF NOT EXISTS experiment_events (
  id SERIAL PRIMARY KEY,
  onboarding_session_id VARCHAR(128) NOT NULL,
  experiment_key VARCHAR(100) NOT NULL,
  variant_key VARCHAR(100) NOT NULL,
  event_name VARCHAR(100) NOT NULL,
  app_version VARCHAR(50),
  platform VARCHAR(20),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS experiment_events_experiment_variant_event_idx
  ON experiment_events(experiment_key, variant_key, event_name);

CREATE INDEX IF NOT EXISTS experiment_events_session_idx
  ON experiment_events(onboarding_session_id);

INSERT INTO experiments (experiment_key, status, default_variant, respect_existing_assignments)
VALUES ('onboarding_after_notifications_v1', 'active', 'library', TRUE)
ON CONFLICT (experiment_key) DO NOTHING;

INSERT INTO experiments (experiment_key, status, default_variant, respect_existing_assignments)
VALUES ('onboarding_first_deck_segment_v1', 'active', 'show_first_deck', TRUE)
ON CONFLICT (experiment_key) DO NOTHING;

INSERT INTO experiment_variants (experiment_key, variant_key, weight)
VALUES
  ('onboarding_after_notifications_v1', 'library', 50),
  ('onboarding_after_notifications_v1', 'explore_openings', 50)
ON CONFLICT (experiment_key, variant_key) DO NOTHING;

INSERT INTO experiment_variants (experiment_key, variant_key, weight)
VALUES
  ('onboarding_first_deck_segment_v1', 'show_first_deck', 50),
  ('onboarding_first_deck_segment_v1', 'skip_to_trial', 50)
ON CONFLICT (experiment_key, variant_key) DO NOTHING;
