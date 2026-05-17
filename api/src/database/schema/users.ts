import {
  pgTable,
  varchar,
  timestamp,
  boolean,
  jsonb,
  index,
  bigint as pgBigInt,
  integer as pgInteger,
} from "drizzle-orm/pg-core";
import { sqliteTable, text, integer } from "drizzle-orm/sqlite-core";
import { createId } from "@paralleldrive/cuid2";

// PostgreSQL Users table (matches original users collection)
export const usersTable = pgTable("users", {
  id: varchar("id", { length: 255 })
    .primaryKey()
    .$defaultFn(() => createId()),
  username: varchar("username", { length: 255 }).notNull().unique(),
  email: varchar("email", { length: 255 }).notNull().unique(),
  passwordHash: varchar("password_hash", { length: 255 }).notNull(),
  firstName: varchar("first_name", { length: 255 }),
  lastName: varchar("last_name", { length: 255 }),
  role: varchar("role", { length: 50 }).default("user").notNull(),
  sessionVersion: pgInteger("session_version").default(0).notNull(),
  isActive: boolean("is_active").default(true).notNull(),
  isEmailVerified: boolean("is_email_verified").default(false).notNull(),
  lastLoginAt: timestamp("last_login_at", { withTimezone: true }),
  metadata: jsonb("metadata").$type<Record<string, unknown>>().default({}),
  createdAt: timestamp("created_at", { withTimezone: true })
    .defaultNow()
    .notNull(),
  updatedAt: timestamp("updated_at", { withTimezone: true })
    .defaultNow()
    .notNull(),
});

export const setupLocksTable = pgTable("setup_locks", {
  id: varchar("id", { length: 50 }).primaryKey(),
  lockedAt: pgBigInt("locked_at", { mode: "number" }).notNull(),
});

export const adminMutationLocksTable = pgTable("admin_mutation_locks", {
  id: varchar("id", { length: 50 }).primaryKey(),
  lockedAt: pgBigInt("locked_at", { mode: "number" }).notNull(),
});

// SQLite Users table
export const usersTableSqlite = sqliteTable("users", {
  id: text("id")
    .primaryKey()
    .$defaultFn(() => createId()),
  username: text("username").notNull().unique(),
  email: text("email").notNull().unique(),
  passwordHash: text("password_hash").notNull(),
  firstName: text("first_name"),
  lastName: text("last_name"),
  role: text("role").default("user").notNull(),
  sessionVersion: integer("session_version").default(0).notNull(),
  isActive: integer("is_active", { mode: "boolean" }).default(true).notNull(),
  isEmailVerified: integer("is_email_verified", { mode: "boolean" })
    .default(false)
    .notNull(),
  lastLoginAt: integer("last_login_at", { mode: "timestamp" }),
  metadata: text("metadata", { mode: "json" })
    .$type<Record<string, unknown>>()
    .default({}),
  createdAt: integer("created_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
  updatedAt: integer("updated_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
});

export const setupLocksTableSqlite = sqliteTable("setup_locks", {
  id: text("id").primaryKey(),
  lockedAt: integer("locked_at").notNull(),
});

export const adminMutationLocksTableSqlite = sqliteTable(
  "admin_mutation_locks",
  {
    id: text("id").primaryKey(),
    lockedAt: integer("locked_at").notNull(),
  },
);

// PostgreSQL API Keys table
export const apiKeysTable = pgTable(
  "api_keys",
  {
    id: varchar("id", { length: 255 })
      .primaryKey()
      .$defaultFn(() => createId()),
    userId: varchar("user_id", { length: 255 })
      .notNull()
      .references(() => usersTable.id, { onDelete: "cascade" }),
    name: varchar("name", { length: 255 }).notNull(),
    keyValue: varchar("key_value", { length: 255 }).notNull(),
    isActive: boolean("is_active").default(true).notNull(),
    lastUsedAt: timestamp("last_used_at", { withTimezone: true }),
    createdAt: timestamp("created_at", { withTimezone: true })
      .defaultNow()
      .notNull(),
    updatedAt: timestamp("updated_at", { withTimezone: true })
      .defaultNow()
      .notNull(),
  },
  (table) => ({
    userIdIdx: index("api_keys_user_id_idx").on(table.userId),
  }),
);

// SQLite API Keys table
export const apiKeysTableSqlite = sqliteTable("api_keys", {
  id: text("id")
    .primaryKey()
    .$defaultFn(() => createId()),
  userId: text("user_id")
    .notNull()
    .references(() => usersTableSqlite.id, { onDelete: "cascade" }),
  name: text("name").notNull(),
  keyValue: text("key_value").notNull(),
  isActive: integer("is_active", { mode: "boolean" }).default(true).notNull(),
  lastUsedAt: integer("last_used_at", { mode: "timestamp" }),
  createdAt: integer("created_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
  updatedAt: integer("updated_at", { mode: "timestamp" })
    .notNull()
    .$defaultFn(() => new Date()),
});

export type User = typeof usersTable.$inferSelect;
export type NewUser = typeof usersTable.$inferInsert;
export type SetupLock = typeof setupLocksTable.$inferSelect;
export type AdminMutationLock = typeof adminMutationLocksTable.$inferSelect;
export type ApiKey = typeof apiKeysTable.$inferSelect;
export type NewApiKey = typeof apiKeysTable.$inferInsert;
