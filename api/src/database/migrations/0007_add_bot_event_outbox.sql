CREATE TABLE IF NOT EXISTS "bot_event_outbox" (
	"id" bigserial PRIMARY KEY NOT NULL,
	"bot_id" varchar(255) NOT NULL,
	"subject" varchar(255) NOT NULL,
	"payload" text NOT NULL,
	"created_at" timestamp with time zone DEFAULT now() NOT NULL,
	"published_at" timestamp with time zone
);
--> statement-breakpoint
CREATE INDEX IF NOT EXISTS "bot_event_outbox_unpublished_idx" ON "bot_event_outbox" USING btree ("id") WHERE "published_at" IS NULL;
