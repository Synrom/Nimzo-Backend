CREATE TABLE IF NOT EXISTS android_emails (
  id SERIAL PRIMARY KEY,
  email VARCHAR(250) NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS android_emails_email_idx ON android_emails(email);
