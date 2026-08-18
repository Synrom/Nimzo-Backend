CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS ios_notification_installations (
  installation_id VARCHAR(128) PRIMARY KEY,
  username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,
  os_version VARCHAR(64) NOT NULL,
  app_version VARCHAR(64) NOT NULL,
  build_number VARCHAR(64) NOT NULL,
  live_activities_enabled BOOLEAN NOT NULL,
  apns_environment VARCHAR(16) NOT NULL CHECK (apns_environment IN ('sandbox', 'production')),
  push_to_start_token BYTEA,
  token_updated_at TIMESTAMPTZ,
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deleted_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS ios_notification_installations_username_idx
  ON ios_notification_installations(username);

CREATE TABLE IF NOT EXISTS streak_notification_schedules (
  schedule_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  username VARCHAR(250) NOT NULL REFERENCES users(username) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  event_id VARCHAR(128) NOT NULL,
  generation BIGINT NOT NULL,
  last_played_at TIMESTAMPTZ NOT NULL,
  starts_at TIMESTAMPTZ NOT NULL,
  completes_at TIMESTAMPTZ NOT NULL,
  status VARCHAR(24) NOT NULL CHECK (status IN ('scheduled', 'starting', 'active', 'complete', 'ending', 'ended', 'superseded', 'failed')),
  payload_version INTEGER NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (starts_at < completes_at)
);
ALTER TABLE streak_notification_schedules
  DROP CONSTRAINT IF EXISTS streak_notification_schedules_username_event_id_key,
  DROP CONSTRAINT IF EXISTS streak_notification_schedules_username_generation_key,
  DROP CONSTRAINT IF EXISTS streak_schedule_event_installation_unique,
  DROP CONSTRAINT IF EXISTS streak_schedule_generation_installation_unique;
ALTER TABLE streak_notification_schedules
  ADD CONSTRAINT streak_schedule_event_installation_unique UNIQUE (username, event_id, installation_id),
  ADD CONSTRAINT streak_schedule_generation_installation_unique UNIQUE (username, generation, installation_id);
CREATE UNIQUE INDEX IF NOT EXISTS one_current_streak_schedule_per_installation
  ON streak_notification_schedules(installation_id)
  WHERE status IN ('scheduled', 'starting', 'active', 'complete', 'ending');

CREATE TABLE IF NOT EXISTS streak_live_activities (
  activity_id VARCHAR(128) PRIMARY KEY,
  schedule_id UUID NOT NULL UNIQUE REFERENCES streak_notification_schedules(schedule_id) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  generation BIGINT NOT NULL,
  update_token BYTEA NOT NULL,
  apns_environment VARCHAR(16) NOT NULL CHECK (apns_environment IN ('sandbox', 'production')),
  token_valid BOOLEAN NOT NULL DEFAULT TRUE,
  token_updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS notification_jobs (
  job_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  schedule_id UUID NOT NULL REFERENCES streak_notification_schedules(schedule_id) ON DELETE CASCADE,
  installation_id VARCHAR(128) NOT NULL REFERENCES ios_notification_installations(installation_id) ON DELETE CASCADE,
  generation BIGINT NOT NULL,
  job_type VARCHAR(16) NOT NULL CHECK (job_type IN ('start', 'complete', 'end')),
  run_at TIMESTAMPTZ NOT NULL,
  status VARCHAR(16) NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'running', 'retry', 'succeeded', 'cancelled', 'dead')),
  attempts INTEGER NOT NULL DEFAULT 0,
  locked_at TIMESTAMPTZ,
  locked_by VARCHAR(128),
  next_attempt_at TIMESTAMPTZ,
  target_token BYTEA,
  target_environment VARCHAR(16) CHECK (target_environment IN ('sandbox', 'production')),
  immediate_dismissal BOOLEAN NOT NULL DEFAULT FALSE,
  last_apns_status INTEGER,
  last_apns_reason VARCHAR(128),
  last_apns_id VARCHAR(64),
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (schedule_id, job_type)
);
CREATE INDEX IF NOT EXISTS notification_jobs_ready_idx
  ON notification_jobs(COALESCE(next_attempt_at, run_at))
  WHERE status IN ('pending', 'retry');
