-- Fix JSONB double-encoding caused by postgres.js + Drizzle both calling
-- JSON.stringify(). Rows where jsonb_typeof() = 'string' are double-encoded
-- and need one layer of unwrapping via ::text::jsonb.

UPDATE custom_bots
SET config = config::text::jsonb
WHERE jsonb_typeof(config) = 'string';

UPDATE bots
SET config = config::text::jsonb
WHERE jsonb_typeof(config) = 'string';
