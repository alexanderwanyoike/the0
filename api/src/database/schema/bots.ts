import { pgTable, varchar, timestamp, jsonb } from 'drizzle-orm/pg-core';
import { sqliteTable, text, integer } from 'drizzle-orm/sqlite-core';
import { createId } from '@paralleldrive/cuid2';
import { usersTable, usersTableSqlite } from './users';
import { customBotsTable, customBotsTableSqlite } from './custom-bots';

// PostgreSQL Bots table (matches original bots collection - running bot instances)
export const botsTable = pgTable('bots', {
  id: varchar('id', { length: 255 }).primaryKey().$defaultFn(() => createId()),
  userId: varchar('user_id', { length: 255 }).notNull().references(() => usersTable.id, { onDelete: 'cascade' }),
  customBotId: varchar('custom_bot_id', { length: 255 }).notNull().references(() => customBotsTable.id, { onDelete: 'cascade' }),
  name: varchar('name', { length: 255 }).notNull(),
  config: jsonb('config').$type<Record<string, any>>().notNull(), // runtime configuration
  topic: varchar('topic', { length: 255 }), // message queue topic
  createdAt: timestamp('created_at', { withTimezone: true }).defaultNow().notNull(),
  updatedAt: timestamp('updated_at', { withTimezone: true }).defaultNow().notNull(),
});

// SQLite Bots table
export const botsTableSqlite = sqliteTable('bots', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  userId: text('user_id').notNull().references(() => usersTableSqlite.id, { onDelete: 'cascade' }),
  customBotId: text('custom_bot_id').notNull().references(() => customBotsTableSqlite.id, { onDelete: 'cascade' }),
  name: text('name').notNull(),
  config: text('config', { mode: 'json' }).$type<Record<string, any>>().notNull(),
  topic: text('topic'),
  createdAt: integer('created_at', { mode: 'timestamp' }).notNull().$defaultFn(() => new Date()),
  updatedAt: integer('updated_at', { mode: 'timestamp' }).notNull().$defaultFn(() => new Date()),
});

export type Bot = typeof botsTable.$inferSelect;
export type NewBot = typeof botsTable.$inferInsert;