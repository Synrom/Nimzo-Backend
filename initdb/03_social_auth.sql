CREATE TABLE IF NOT EXISTS user_identities (
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

CREATE INDEX IF NOT EXISTS user_identities_username_idx ON user_identities(username);
