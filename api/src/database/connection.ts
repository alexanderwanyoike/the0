import { drizzle } from "drizzle-orm/postgres-js";
import { drizzle as drizzleSqlite } from "drizzle-orm/better-sqlite3";
import postgres from "postgres";
import Database from "better-sqlite3";
import * as net from "net";
import { loadConfig, DatabaseConfig } from "../config/database.config";

// Set DB_AUTO_SELECT_FAMILY=false to disable Node 20's Happy Eyeballs (RFC 8305)
// which splits connect_timeout across all DNS results (~250ms each).
if (process.env.DB_AUTO_SELECT_FAMILY === "false") {
  net.setDefaultAutoSelectFamily(false);
}
import * as usersSchema from "./schema/users";
import * as customBotsSchema from "./schema/custom-bots";
import * as botsSchema from "./schema/bots";
// Logs are stored in external storage (MinIO/S3), not in database

// Combined schema for PostgreSQL
export const pgSchema = {
  ...usersSchema,
  ...customBotsSchema,
  ...botsSchema,
};

// Combined schema for SQLite
export const sqliteSchema = {
  usersTable: usersSchema.usersTableSqlite,
  apiKeysTable: usersSchema.apiKeysTableSqlite,
  customBotsTable: customBotsSchema.customBotsTableSqlite,
  botsTable: botsSchema.botsTableSqlite,
};

// Table registry for clean access
export interface TableRegistry {
  users: typeof usersSchema.usersTable | typeof usersSchema.usersTableSqlite;
  apiKeys:
    | typeof usersSchema.apiKeysTable
    | typeof usersSchema.apiKeysTableSqlite;
  customBots:
    | typeof customBotsSchema.customBotsTable
    | typeof customBotsSchema.customBotsTableSqlite;
  bots: typeof botsSchema.botsTable | typeof botsSchema.botsTableSqlite;
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

  if (config.type === "sqlite") {
    const sqlite = new Database(config.url);
    dbInstance = drizzleSqlite(sqlite, { schema: sqliteSchema });

    tablesCache = {
      users: usersSchema.usersTableSqlite,
      apiKeys: usersSchema.apiKeysTableSqlite,
      customBots: customBotsSchema.customBotsTableSqlite,
      bots: botsSchema.botsTableSqlite,
    };
  } else {
    const client = postgres(config.url, {
      host: config.host,
      port: config.port,
      username: config.username,
      password: config.password,
      database: config.database,
      max: config.pool.max,
      idle_timeout: config.pool.idleTimeout,
      connect_timeout: config.pool.connectTimeout,
      max_lifetime: config.pool.maxLifetime,
    });
    dbInstance = drizzle(client, { schema: pgSchema });

    tablesCache = {
      users: usersSchema.usersTable,
      apiKeys: usersSchema.apiKeysTable,
      customBots: customBotsSchema.customBotsTable,
      bots: botsSchema.botsTable,
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
