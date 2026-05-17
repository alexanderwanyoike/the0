ALTER TABLE "users" ADD COLUMN "role" varchar(50) DEFAULT 'user' NOT NULL CHECK ("role" IN ('admin', 'user'));
--> statement-breakpoint
ALTER TABLE "users" ADD COLUMN "session_version" integer DEFAULT 0 NOT NULL;
--> statement-breakpoint
CREATE TABLE IF NOT EXISTS "setup_locks" ("id" varchar(50) PRIMARY KEY NOT NULL, "locked_at" bigint NOT NULL);
--> statement-breakpoint
CREATE TABLE IF NOT EXISTS "admin_mutation_locks" ("id" varchar(50) PRIMARY KEY NOT NULL, "locked_at" bigint NOT NULL);
