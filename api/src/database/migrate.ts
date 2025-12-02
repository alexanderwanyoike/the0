import { migrate } from "drizzle-orm/postgres-js/migrator";
import { migrate as migrateSqlite } from "drizzle-orm/better-sqlite3/migrator";
import { getDatabase, getDatabaseConfig } from "./connection";
import { join } from "path";
import pino from "pino";

const logger = pino({ name: "migrations" });

async function runMigrations() {
  const config = getDatabaseConfig();
  const db = getDatabase();
  const migrationsPath =
    process.env.MIGRATIONS_PATH || join(__dirname, "migrations");

  logger.info({ dbType: config.type, migrationsPath }, "Running migrations");

  try {
    if (config.type === "sqlite") {
      migrateSqlite(db, { migrationsFolder: migrationsPath });
      logger.info("SQLite migrations completed successfully");
    } else {
      await migrate(db, { migrationsFolder: migrationsPath });
      logger.info("PostgreSQL migrations completed successfully");
    }
  } catch (error) {
    logger.error({ err: error }, "Migration failed");
    process.exit(1);
  }
}

// Run migrations if this file is executed directly
if (require.main === module) {
  runMigrations()
    .then(() => {
      logger.info("Database migration completed");
      process.exit(0);
    })
    .catch((error) => {
      logger.error({ err: error }, "Migration script failed");
      process.exit(1);
    });
}

export { runMigrations };
