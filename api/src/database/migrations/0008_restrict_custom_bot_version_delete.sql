ALTER TABLE "bots" DROP CONSTRAINT IF EXISTS "bots_custom_bot_id_custom_bots_id_fk";
--> statement-breakpoint
ALTER TABLE "bots" ADD CONSTRAINT "bots_custom_bot_id_custom_bots_id_fk" FOREIGN KEY ("custom_bot_id") REFERENCES "custom_bots"("id") ON DELETE restrict ON UPDATE no action;
