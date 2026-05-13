import { defineConfig } from 'drizzle-kit';

// Load config directly from environment variables
const databaseUrl = process.env.DATABASE_URL;
const databaseType = process.env.DATABASE_TYPE || 'sqlite';

// Parse PostgreSQL URL if provided
let dbConfig: any = {};
if (databaseType === 'postgresql' && databaseUrl) {
  const url = new URL(databaseUrl);
  dbConfig = {
    host: url.hostname,
    port: parseInt(url.port) || 5432,
    user: url.username,
    password: url.password,
    database: url.pathname.slice(1),
  };
}

export default defineConfig(databaseType === 'sqlite'
  ? {
      schema: [
        './src/database/schema/users.ts',
        './src/database/schema/custom-bots.ts',
        './src/database/schema/bots.ts',
      ],
      out: './src/database/migrations',
      dialect: 'sqlite',
      dbCredentials: { url: databaseUrl || './data/the0.db' },
      verbose: true,
      strict: true,
    }
  : {
      schema: [
        './src/database/schema/users.ts',
        './src/database/schema/custom-bots.ts',
        './src/database/schema/bots.ts',
      ],
      out: './src/database/migrations',
      dialect: 'postgresql',
      dbCredentials: {
        url: databaseUrl || `postgresql://${dbConfig.user}:${dbConfig.password}@${dbConfig.host}:${dbConfig.port}/${dbConfig.database}`,
      },
      verbose: true,
      strict: true,
    });
