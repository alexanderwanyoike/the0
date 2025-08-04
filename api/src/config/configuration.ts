export default () => ({
  // Bot runtime configuration
  BOT_TYPE_API_KEY: process.env.BOT_TYPE_API_KEY || '',
  
  // Frontend configuration
  FRONTEND_URL: process.env.FRONTEND_URL || 'http://localhost:3001',
  
  // MinIO/S3 storage configuration for OSS
  MINIO_ENDPOINT: process.env.MINIO_ENDPOINT || 'localhost',
  MINIO_EXTERNAL_ENDPOINT: process.env.MINIO_EXTERNAL_ENDPOINT || process.env.MINIO_ENDPOINT || 'localhost:9000',
  MINIO_PORT: process.env.MINIO_PORT || '9000',
  MINIO_USE_SSL: process.env.MINIO_USE_SSL || 'false',
  MINIO_ACCESS_KEY: process.env.MINIO_ACCESS_KEY || 'minioadmin',
  MINIO_SECRET_KEY: process.env.MINIO_SECRET_KEY || 'minioadmin',
  
  // Storage buckets
  CUSTOM_BOTS_BUCKET: process.env.CUSTOM_BOTS_BUCKET || 'custom-bots',
  LOG_BUCKET: process.env.LOG_BUCKET || 'logs',
  BACKTEST_BUCKET: process.env.BACKTEST_BUCKET || 'backtests',
  
  // Database configuration
  DATABASE_URL: process.env.DATABASE_URL || 'sqlite:data.db',
  
  // NATS configuration
  NATS_URLs: process.env.NATS_URLS || 'nats://localhost:4222',
  
  // JWT configuration
  JWT_SECRET: process.env.JWT_SECRET || 'your-secret-key-change-in-production',
  JWT_EXPIRES_IN: process.env.JWT_EXPIRES_IN || '24h',
});
