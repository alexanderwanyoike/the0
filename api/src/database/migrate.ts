import { migrate } from 'drizzle-orm/postgres-js/migrator';
import { migrate as migrateSqlite } from 'drizzle-orm/better-sqlite3/migrator';
import { getDatabase, getDatabaseConfig } from './connection';
import { join } from 'path';

async function runMigrations() {
  const config = getDatabaseConfig();
  const db = getDatabase();
  const migrationsPath = process.env.MIGRATIONS_PATH || join(__dirname, 'migrations');

  console.log(`Running migrations for ${config.type} database...`);
  console.log(`Migration path: ${migrationsPath}`);
  
  try {
    if (config.type === 'sqlite') {
      migrateSqlite(db, { migrationsFolder: migrationsPath });
      console.log('✅ SQLite migrations completed successfully');
    } else {
      await migrate(db, { migrationsFolder: migrationsPath });
      console.log('✅ PostgreSQL migrations completed successfully');
    }
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

// Run migrations if this file is executed directly
if (require.main === module) {
  runMigrations()
    .then(() => {
      console.log('🎉 Database migration completed');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error);
      process.exit(1);
    });
}

export { runMigrations };