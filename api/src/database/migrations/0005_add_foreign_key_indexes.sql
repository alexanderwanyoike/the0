-- Add indexes on foreign key columns to speed up joins and cascading deletes.
CREATE INDEX IF NOT EXISTS "bots_user_id_idx" ON "bots" USING btree ("user_id");--> statement-breakpoint
CREATE INDEX IF NOT EXISTS "bots_custom_bot_id_idx" ON "bots" USING btree ("custom_bot_id");--> statement-breakpoint
CREATE INDEX IF NOT EXISTS "custom_bots_user_id_idx" ON "custom_bots" USING btree ("user_id");--> statement-breakpoint
CREATE INDEX IF NOT EXISTS "api_keys_user_id_idx" ON "api_keys" USING btree ("user_id");
