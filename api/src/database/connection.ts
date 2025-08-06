import { drizzle } from 'drizzle-orm/postgres-js';
import { drizzle as drizzleSqlite } from 'drizzle-orm/better-sqlite3';
import postgres from 'postgres';
import Database from 'better-sqlite3';
import { loadConfig, DatabaseConfig } from '../config/database.config';
import * as usersSchema from './schema/users';
import * as customBotsSchema from './schema/custom-bots';
import * as botsSchema from './schema/bots';
import * as backtestsSchema from './schema/backtests';
// Logs are stored in external storage (MinIO/S3), not in database

// Combined schema for PostgreSQL
export const pgSchema = {
  ...usersSchema,
  ...customBotsSchema,
  ...botsSchema,
  ...backtestsSchema,
};

// Combined schema for SQLite
export const sqliteSchema = {
  usersTable: usersSchema.usersTableSqlite,
  apiKeysTable: usersSchema.apiKeysTableSqlite,
  customBotsTable: customBotsSchema.customBotsTableSqlite,
  botsTable: botsSchema.botsTableSqlite,
  backtestsTable: backtestsSchema.backtestsTableSqlite,
};

// Table registry for clean access
export interface TableRegistry {
  users: typeof usersSchema.usersTable | typeof usersSchema.usersTableSqlite;
  apiKeys: typeof usersSchema.apiKeysTable | typeof usersSchema.apiKeysTableSqlite;
  customBots: typeof customBotsSchema.customBotsTable | typeof customBotsSchema.customBotsTableSqlite;
  bots: typeof botsSchema.botsTable | typeof botsSchema.botsTableSqlite;
  backtests: typeof backtestsSchema.backtestsTable | typeof backtestsSchema.backtestsTableSqlite;
}

let dbInstance: any = null;
let configCache: DatabaseConfig | null = null;
let tablesCache: TableRegistry | null = null;

export function getDatabase() {
  if (dbInstance) {
    return dbInstance;
  }

  const config = loadConfig();
  configCache = config;

  if (config.type === 'sqlite') {
    const sqlite = new Database(config.url);
    dbInstance = drizzleSqlite(sqlite, { schema: sqliteSchema });
    
    tablesCache = {
      users: usersSchema.usersTableSqlite,
      apiKeys: usersSchema.apiKeysTableSqlite,
      customBots: customBotsSchema.customBotsTableSqlite,
      bots: botsSchema.botsTableSqlite,
      backtests: backtestsSchema.backtestsTableSqlite,
    };
  } else {
    const client = postgres(config.url, {
      host: config.host,
      port: config.port,
      username: config.username,
      password: config.password,
      database: config.database,
      max: 10,
    });
    dbInstance = drizzle(client, { schema: pgSchema });
    
    tablesCache = {
      users: usersSchema.usersTable,
      apiKeys: usersSchema.apiKeysTable,
      customBots: customBotsSchema.customBotsTable,
      bots: botsSchema.botsTable,
      backtests: backtestsSchema.backtestsTable,
    };
  }

  return dbInstance;
}

export function getDatabaseConfig(): DatabaseConfig {
  return configCache || loadConfig();
}

export function getTables(): TableRegistry {
  if (!tablesCache) {
    getDatabase(); // Initialize tables cache
  }
  return tablesCache!;
}

export { pgSchema as schema };