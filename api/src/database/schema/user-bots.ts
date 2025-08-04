import { pgTable, varchar, timestamp } from 'drizzle-orm/pg-core';
import { sqliteTable, text, integer } from 'drizzle-orm/sqlite-core';
import { createId } from '@paralleldrive/cuid2';
import { usersTable, usersTableSqlite } from './users';

// PostgreSQL User Bots table (matches original user-bots collection)
export const userBotsTable = pgTable('user_bots', {
  id: varchar('id', { length: 255 }).primaryKey().$defaultFn(() => createId()),
  userId: varchar('user_id', { length: 255 }).notNull().references(() => usersTable.id, { onDelete: 'cascade' }),
  customBotName: varchar('custom_bot_name', { length: 255 }).notNull(), // references custom bot name, not ID
  acquiredAt: timestamp('acquired_at', { withTimezone: true }).defaultNow().notNull(),
  createdAt: timestamp('created_at', { withTimezone: true }).defaultNow().notNull(),
  updatedAt: timestamp('updated_at', { withTimezone: true }).defaultNow().notNull(),
});

// SQLite User Bots table
export const userBotsTableSqlite = sqliteTable('user_bots', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  userId: text('user_id').notNull().references(() => usersTableSqlite.id, { onDelete: 'cascade' }),
  customBotName: text('custom_bot_name').notNull(),
  acquiredAt: integer('acquired_at', { mode: 'timestamp' }).notNull().$defaultFn(() => new Date()),
  createdAt: integer('created_at', { mode: 'timestamp' }).notNull().$defaultFn(() => new Date()),
  updatedAt: integer('updated_at', { mode: 'timestamp' }).notNull().$defaultFn(() => new Date()),
});

export type UserBot = typeof userBotsTable.$inferSelect;
export type NewUserBot = typeof userBotsTable.$inferInsert;