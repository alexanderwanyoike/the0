-- Fix JSONB double-encoding (retry with correct cast).
-- 0004 used ::text::jsonb which is a no-op for JSONB strings.
-- Use #>> '{}' to extract the raw string value, then cast to jsonb.

UPDATE custom_bots
SET config = (config #>> '{}')::jsonb
WHERE jsonb_typeof(config) = 'string';

UPDATE bots
SET config = (config #>> '{}')::jsonb
WHERE jsonb_typeof(config) = 'string';
