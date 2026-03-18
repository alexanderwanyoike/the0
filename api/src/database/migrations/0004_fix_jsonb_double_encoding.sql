-- Fix JSONB double-encoding caused by postgres.js + Drizzle both calling
-- JSON.stringify(). Rows where jsonb_typeof() = 'string' are double-encoded.
-- Use #>> '{}' to extract the raw string value (strips JSON quotes), then
-- cast back to jsonb to get a proper object.

UPDATE custom_bots
SET config = (config #>> '{}')::jsonb
WHERE jsonb_typeof(config) = 'string';

UPDATE bots
SET config = (config #>> '{}')::jsonb
WHERE jsonb_typeof(config) = 'string';
