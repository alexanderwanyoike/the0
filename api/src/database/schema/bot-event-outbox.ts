import { sql } from "drizzle-orm";
import {
  pgTable,
  bigserial,
  varchar,
  text,
  timestamp,
  index,
} from "drizzle-orm/pg-core";

/**
 * Bot events waiting for, or already given, a JetStream ack. A row is
 * written in the same transaction as the bot change it announces.
 *
 * Postgres only: SQLite has no migrations, so the api cannot run on it.
 */
export const botEventOutboxTable = pgTable(
  "bot_event_outbox",
  {
    // Ids follow commit order for any one bot, because a bot's row stays
    // locked from its write until commit and the event is inserted after.
    id: bigserial("id", { mode: "number" }).primaryKey(),
    // No foreign key: a delete event must outlive the bot it announces.
    botId: varchar("bot_id", { length: 255 }).notNull(),
    subject: varchar("subject", { length: 255 }).notNull(),
    // The encoded message, so the bytes published never depend on how
    // Postgres would store or reorder JSON.
    payload: text("payload").notNull(),
    createdAt: timestamp("created_at", { withTimezone: true })
      .defaultNow()
      .notNull(),
    publishedAt: timestamp("published_at", { withTimezone: true }),
  },
  (table) => ({
    unpublishedIdx: index("bot_event_outbox_unpublished_idx")
      .on(table.id)
      .where(sql`${table.publishedAt} IS NULL`),
  }),
);

export type BotEventOutboxRow = typeof botEventOutboxTable.$inferSelect;
