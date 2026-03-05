import { ConfigService } from "@nestjs/config";

export interface DatabaseConfig {
  type: "postgresql" | "sqlite";
  url: string;
  host?: string;
  port?: number;
  username?: string;
  password?: string;
  database?: string;
  synchronize?: boolean;
  logging?: boolean;
  pool?: {
    max: number;
    idleTimeout: number;
    connectTimeout: number;
    maxLifetime: number;
  };
}

export function loadConfig(): DatabaseConfig {
  const databaseUrl = process.env.DATABASE_URL;
  const databaseType =
    (process.env.DATABASE_TYPE as "postgresql" | "sqlite") || "sqlite";

  if (databaseType === "sqlite") {
    return {
      type: "sqlite",
      url: databaseUrl || "./data/the0.db",
      synchronize: process.env.NODE_ENV === "development",
      logging: process.env.NODE_ENV === "development",
    };
  }

  // Parse PostgreSQL URL
  if (databaseUrl) {
    const url = new URL(databaseUrl);
    return {
      type: "postgresql",
      url: databaseUrl,
      host: url.hostname,
      port: parseInt(url.port) || 5432,
      username: url.username,
      password: url.password,
      database: url.pathname.slice(1),
      synchronize: process.env.NODE_ENV === "development",
      logging: process.env.NODE_ENV === "development",
      pool: {
        max: parseInt(process.env.DB_POOL_MAX) || 10,
        idleTimeout: parseInt(process.env.DB_POOL_IDLE_TIMEOUT) || 20,
        connectTimeout: parseInt(process.env.DB_POOL_CONNECT_TIMEOUT) || 10,
        maxLifetime: parseInt(process.env.DB_POOL_MAX_LIFETIME) || 1800,
      },
    };
  }

  // Fallback to individual environment variables
  return {
    type: "postgresql",
    url: "", // Will be constructed
    host: process.env.DB_HOST || "localhost",
    port: parseInt(process.env.DB_PORT) || 5432,
    username: process.env.DB_USERNAME || "the0",
    password: process.env.DB_PASSWORD || "the0_password",
    database: process.env.DB_DATABASE || "the0_oss",
    synchronize: process.env.NODE_ENV === "development",
    logging: process.env.NODE_ENV === "development",
    pool: {
      max: parseInt(process.env.DB_POOL_MAX) || 10,
      idleTimeout: parseInt(process.env.DB_POOL_IDLE_TIMEOUT) || 20,
      connectTimeout: parseInt(process.env.DB_POOL_CONNECT_TIMEOUT) || 10,
      maxLifetime: parseInt(process.env.DB_POOL_MAX_LIFETIME) || 1800,
    },
  };
}

export function createDatabaseConfigFactory() {
  return {
    provide: "DATABASE_CONFIG",
    useFactory: (configService: ConfigService): DatabaseConfig => {
      return loadConfig();
    },
    inject: [ConfigService],
  };
}
