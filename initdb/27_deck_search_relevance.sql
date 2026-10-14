-- Apply before deploying the search code. Run with psql in autocommit mode:
-- CREATE INDEX CONCURRENTLY must not be inside a transaction.
-- Expression indexes avoid rewriting the decks table or changing old vectors.
CREATE OR REPLACE FUNCTION deck_search_normalize(value text)
RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE
AS $$
  SELECT trim(regexp_replace(lower(coalesce(value, '')), '[^[:alnum:]]+', ' ', 'g'));
$$;

CREATE INDEX CONCURRENTLY IF NOT EXISTS decks_search_title_simple_idx
  ON decks USING GIN (to_tsvector('simple', deck_search_normalize(name)))
  WHERE is_public = TRUE;

-- Keep title lexemes literal (A), description lexemes stemmed (B), and a
-- literal description copy (C) for query words discarded by English. Queries
-- constrain each word to A or B, falling back to A or C only for stopwords.
-- This lets words span title/description without silently dropping "From".
CREATE OR REPLACE FUNCTION deck_search_document(title text, description text)
RETURNS tsvector
LANGUAGE sql IMMUTABLE PARALLEL SAFE
AS $$
  SELECT setweight(to_tsvector('simple', deck_search_normalize(title)), 'A')
    || setweight(to_tsvector('english', deck_search_normalize(description)), 'B')
    || setweight(to_tsvector('simple', deck_search_normalize(description)), 'C');
$$;

CREATE INDEX CONCURRENTLY IF NOT EXISTS decks_search_document_idx
  ON decks USING GIN (deck_search_document(name, description))
  WHERE is_public = TRUE;
