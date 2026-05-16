ALTER TABLE "users" ADD COLUMN IF NOT EXISTS "role" varchar(50) DEFAULT 'user' NOT NULL;
--> statement-breakpoint
UPDATE "users"
SET "role" = 'admin'
WHERE "metadata"->>'role' = 'admin';
