ALTER TABLE ios_notification_installations
  ADD COLUMN IF NOT EXISTS supports_locally_scheduled_live_activities BOOLEAN
  NOT NULL DEFAULT FALSE;
