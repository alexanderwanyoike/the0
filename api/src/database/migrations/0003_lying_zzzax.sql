DROP TABLE IF EXISTS "backtests";--> statement-breakpoint
ALTER TABLE "custom_bots" ALTER COLUMN "status" SET DEFAULT 'active';--> statement-breakpoint
ALTER TABLE "custom_bots" DROP COLUMN IF EXISTS "review";--> statement-breakpoint
ALTER TABLE "custom_bots" DROP COLUMN IF EXISTS "marketplace";
