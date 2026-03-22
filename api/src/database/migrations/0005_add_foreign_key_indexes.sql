-- CREATE INDEX CONCURRENTLY is used to avoid taking an exclusive lock on the
-- table, which would block all writes for the duration of the index build.
--
-- IMPORTANT: CONCURRENTLY cannot run inside a transaction. Drizzle ORM's
-- migrate() runs each migration file in a transaction by default, and there
-- is no built-in directive to disable this. In production, this migration
-- should be run manually outside a transaction (e.g. via psql) or by using
-- a custom migration runner that skips the transaction wrapper for this file.
CREATE INDEX CONCURRENTLY IF NOT EXISTS "bots_user_id_idx" ON "bots" USING btree ("user_id");--> statement-breakpoint
CREATE INDEX CONCURRENTLY IF NOT EXISTS "bots_custom_bot_id_idx" ON "bots" USING btree ("custom_bot_id");--> statement-breakpoint
CREATE INDEX CONCURRENTLY IF NOT EXISTS "custom_bots_user_id_idx" ON "custom_bots" USING btree ("user_id");--> statement-breakpoint
CREATE INDEX CONCURRENTLY IF NOT EXISTS "api_keys_user_id_idx" ON "api_keys" USING btree ("user_id");
