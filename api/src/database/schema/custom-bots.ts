import { pgTable, varchar, timestamp, jsonb } from "drizzle-orm/pg-core";
import { sqliteTable, text, integer } from "drizzle-orm/sqlite-core";
import { createId } from "@paralleldrive/cuid2";
import { usersTable, usersTableSqlite } from "./users";

// Types matching original theo-api exactly
export interface CustomBotConfig {
  name: string;
  description: string;
  version: string;
  type: "scheduled" | "realtime" | "event";
  runtime: "python3.11" | "nodejs20";
  author: string;
  entrypoints: {
    bot: string;
    backtest?: string;
  };
  schema: {
    backtest?: Record<string, any>;
    bot: Record<string, any>;
  };
  readme: string;
  metadata?: {
    categories?: string[];
    instruments?: string[];
    exchanges?: string[];
    tags?: string[];
    [key: string]: any;
  };
}

export interface MarketplaceMetadata {
  isPublished: boolean;
  publishedAt?: Date;
  price: number; // 0 for free
  description: string;
  tags: string[];
  category?: string;
  installCount: number;
  averageRating?: number; // 1-5 scale
  totalReviews: number;
  revenue: number; // Total earnings
  lastUpdated?: Date;
}

export type CustomBotStatus =
  | "approved"
  | "declined"
  | "awaiting_human_review"
  | "pending_review"
  | "published";

// PostgreSQL Custom Bots table (matches original custom-bots collection)
export const customBotsTable = pgTable("custom_bots", {
  id: varchar("id", { length: 255 })
    .primaryKey()
    .$defaultFn(() => createId()),
  userId: varchar("user_id", { length: 255 })
    .notNull()
    .references(() => usersTable.id, { onDelete: "cascade" }),
  name: varchar("name", { length: 255 }).notNull(), // revision key
  version: varchar("version", { length: 50 }).notNull(), // semver format
  config: jsonb("config").$type<CustomBotConfig>().notNull(),
  filePath: varchar("file_path", { length: 500 }), // Local file storage path
  status: varchar("status", { length: 50 })
    .default("pending_review")
    .notNull()
    .$type<CustomBotStatus>(),
  review: jsonb("review"), // Security analysis results from 0vers33r
  marketplace: jsonb("marketplace").$type<MarketplaceMetadata>(),
  createdAt: timestamp("created_at", { withTimezone: true })
    .defaultNow()
    .notNull(),
  updatedAt: timestamp("updated_at", { withTimezone: true })
    .defaultNow()
    .notNull(),
});

// SQLite Custom Bots table
export const customBotsTableSqlite = sqliteTable("custom_bots", {
  id: text("id")
    .primaryKey()
    .$defaultFn(() => createId()),
  userId: text("user_id")
    .notNull()
    .references(() => usersTableSqlite.id, { onDelete: "cascade" }),
  name: text("name").notNull(),
  version: text("version").notNull(),
  config: text("config", { mode: "json" }).$type<CustomBotConfig>().notNull(),
  filePath: text("file_path"),
  status: text("status").default("pending_review").notNull(),
  review: text("review", { mode: "json" }), // Security analysis results from 0vers33r
  marketplace: text("marketplace", {
    mode: "json",
  }).$type<MarketplaceMetadata>(),
  createdAt: integer("created_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
  updatedAt: integer("updated_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
});

export type CustomBot = typeof customBotsTable.$inferSelect;
export type NewCustomBot = typeof customBotsTable.$inferInsert;
