---
title: "Data Architecture"
description: "Polyglot persistence patterns with PostgreSQL, MongoDB, NATS JetStream, and MinIO for distributed data management"
order: 30
---

# Data Architecture

## 🎯 Overview

The0 platform employs a sophisticated polyglot persistence architecture that combines multiple database technologies to optimize for different use cases while maintaining data consistency and scalability. This hybrid approach leverages PostgreSQL for relational data integrity, MongoDB for flexible document storage, NATS JetStream for real-time event streaming, and MinIO for object storage, creating a comprehensive data ecosystem that supports the complex requirements of an algorithmic trading platform.

### Key Design Principles

- **Polyglot Persistence**: Right database for right use case
- **Data Consistency**: ACID compliance where required, eventual consistency elsewhere
- **Event Sourcing**: Immutable events for audit trails and state reconstruction
- **High Availability**: Data replication, clustering, and automatic failover
- **Scalability**: Horizontal scaling with sharding and partitioning
- **Security**: End-to-end encryption, access control, and audit logging

## 🛠️ Technology Stack

| Technology | Version | Purpose | Data Type |
|------------|---------|---------|
| PostgreSQL | 15+ | Relational data, transactions, analytics |
| MongoDB | 7+ | Document storage, flexible schemas, high throughput |
| NATS JetStream | 2.10+ | Event streaming, message queuing, persistence |
| MinIO | Latest | Object storage, file uploads, backups |
| Redis | 7+ | Caching, session storage, rate limiting |

## 🏗️ Architecture & Design

### Data Flow Architecture

```mermaid
graph TB
    subgraph "Client Applications"
        WEB[Web Dashboard]
        CLI[CLI Tool]
        MOBILE[Mobile App]
        EXT_API[External APIs]
    end

    subgraph "Application Layer"
        API_SERVER[the0 API<br/>NestJS Backend]
        AI_AGENT[AI Assistant<br/>FastAPI Service]
        RUNTIME[Runtime Services<br/>Go Microservices]
    end

    subgraph "Event Streaming Layer"
        NATS_STREAM[NATS JetStream<br/>Message Broker]
        EVENT_STORE[Event Store<br/>Persistent Events]
        STREAM_PROCESSOR[Stream Processor<br/>Real-time Analytics]
    end

    subgraph "Data Storage Layer"
        POSTGRES_CLUSTER[(PostgreSQL Cluster<br/>Primary Data)]
        MONGO_CLUSTER[(MongoDB Cluster<br/>Document Storage)]
        MINIO_CLUSTER[(MinIO Cluster<br/>Object Storage)]
        REDIS_CLUSTER[(Redis Cluster<br/>Cache Layer)]
    end

    subgraph "Integration Layer"
        EXTERNAL_APIS[External Services<br/>Market Data<br/>Exchange APIs]
        DATA_PIPELINE[ETL Pipeline<br/>Data Processing<br/>Transformation]
    end
```

### Data Relationships

```mermaid
classDiagram
    class User {
        +id: String
        +email: String
        +created_at: DateTime
        +updated_at: DateTime
        +profile: UserProfile
    }

    class Bot {
        +id: String
        +user_id: String
        +name: String
        +type: BotType
        +config: BotConfiguration
        +status: BotStatus
        +created_at: DateTime
        +updated_at: DateTime
        +deployments: BotDeployment[]
    }

    class BotDeployment {
        +id: String
        +bot_id: String
        +runtime_config: RuntimeConfiguration
        +deployment_status: DeploymentStatus
        +environment: DeploymentEnvironment
        +created_at: DateTime
        +updated_at: DateTime
    }

    class Backtest {
        +id: String
        +user_id: String
        +bot_config: BotConfiguration
        +status: BacktestStatus
        +results: BacktestResults
        +created_at: DateTime
        +updated_at: DateTime
    }

    class MarketData {
        +id: String
        +symbol: String
        +price: Decimal
        +volume: Decimal
        +timestamp: DateTime
        +source: String
    }

    User "1" -- "owns" --> Bot "1"
    User "1" -- "creates" --> Backtest "1"
    Bot "1" -- "has" --> BotDeployment "1"
```

## 📡 Database Architecture

### PostgreSQL - Primary Relational Database

**Purpose**: ACID-compliant transactional data storage for user accounts, authentication, configurations, and structured business data.

**Schema Design**:
```sql
-- Users table
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    email_verified BOOLEAN DEFAULT FALSE,
    is_active BOOLEAN DEFAULT TRUE
    two_factor_enabled BOOLEAN DEFAULT FALSE,
    last_login_at TIMESTAMP WITH TIME ZONE
);

-- Bots table
CREATE TABLE bots (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id),
    name VARCHAR(255) NOT NULL,
    type VARCHAR(100) NOT NULL,
    description TEXT,
    config JSONB NOT NULL,
    status VARCHAR(50) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    version VARCHAR(50) NOT NULL,
    tags TEXT[],
    risk_level INTEGER DEFAULT 1,
    max_position_size DECIMAL(20,8),
    api_key_id UUID REFERENCES api_keys(id)
);

-- API Keys table
CREATE TABLE api_keys (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id),
    name VARCHAR(255) NOT NULL,
    key_hash VARCHAR(255) NOT NULL,
    permissions JSONB NOT NULL,
    last_used_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    expires_at TIMESTAMP WITH TIME ZONE,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);
```

**Performance Optimizations**:
```sql
-- Partitioning by user_id for user-specific queries
CREATE INDEX bots_user_created_at_idx ON bots (user_id, created_at);

-- Indexing for common queries
CREATE INDEX bots_status_idx ON bots (status);
CREATE INDEX bots_type_idx ON bots (type);

-- Partial indexes for analytical queries
CREATE INDEX bots_performance_idx ON bots (status, risk_level, created_at);
```

### MongoDB - Document Storage

**Purpose**: Flexible schema storage for runtime data, logs, analytics, and unstructured content.

**Database Design**:
```javascript
// User session data
{
  _id: ObjectId,
  userId: String,
  sessionId: String,
  createdAt: Date,
  lastActivity: Date,
  context: Object,
  active: Boolean
}

// Bot execution data
{
  _id: ObjectId,
  botId: String,
  userId: String,
  executionId: String,
  status: String,
  startTime: Date,
  endTime: Date,
  duration: Number,
  metrics: {
    trades: Number,
    profit: Number,
    drawdown: Number,
    winRate: Number
  },
  logs: [{
    timestamp: Date,
    level: String, // 'info', 'warn', 'error'
    message: String,
    metadata: Object
  }],
  trades: [{
    timestamp: Date,
    symbol: String,
    side: String, // 'buy', 'sell'
    quantity: Number,
    price: Number,
    fees: Number
  }],
  error: {
    code: String,
    message: String,
    timestamp: Date,
    stack: String
  }
}

// Backtest results
{
  _id: ObjectId,
  botId: String,
  userId: String,
  config: Object,
  results: {
    totalReturn: Number,
    maxDrawdown: Number,
    sharpeRatio: Number,
    trades: [ /* Trade objects */ ],
    equityCurve: [{ /* Price points */ }],
    metrics: Object,
    summary: String
  },
  createdAt: Date,
  completedAt: Date,
  status: String // 'running', 'completed', 'failed'
}
```

**Sharding Strategy**:
```javascript
// Shard key based on user_id for user data isolation
shardKey: { userId: user123, shard: 0-499 }

// Bot execution data sharded by botId for performance
{
  _id: ObjectId,
  botId: "bot-123",
  userId: "user123",
  shardKey: { botId: "bot-123", shard: 0-999 },
  executionData: ExecutionData
}
```

### NATS JetStream - Event Streaming

**Purpose**: Real-time event streaming, message queuing, and event sourcing for microservices communication.

**Stream Architecture**:
```yaml
# Stream configuration for different event types
streams:
  # Bot lifecycle events
  bot.lifecycle:
    subjects: ["bot.created", "bot.updated", "bot.deleted"]
    storage: file
    retention: "30d"
    ack_wait: 30s
    max_age: "72h"

  # Trading events
  trading.events:
    subjects: ["trade.executed", "trade.completed", "trade.failed"]
    storage: file
    retention: "7d"
    ack_wait: 10s
    max_age: "24h"

  # Monitoring events
  monitoring.metrics:
    subjects: ["system.health", "performance.metrics", "error.events"]
    storage: file
    retention: "14d"
    max_age: "7d"

  # Analytics events
  analytics.events:
    subjects: ["user.interaction", "bot.performance", "market.data"]
    storage: file
    retention: "90d"
    max_age: "365d"
```

**Consumer Configuration**:
```javascript
// NATS consumer with durable subscriptions
const NATS = require('nats');

class EventConsumer {
  constructor(streamName, config) {
    this.connection = nats.connect(config.natsUrl);
    this.jetstream = nc.jetstream();
    this.consumer = null;
  }

  async initialize() {
    // Create consumer
    this.consumer = await this.jetstream.consumers(streamName, {
      durable_name: `${streamName}-consumer`,
      ack_policy: nats.AckPolicy.All,
      ack_wait: 30 * 1000, // 30 seconds
      max_deliver: 100,
      deliver_policy: nats.DeliverPolicy.New,
      stream: streamName,
      config: {
        deliver_subject: `${streamName}.*`,
        ack_policy: nats.AckPolicy.All,
        max_waiting: 500,
        ack_policy: nats.AckPolicy.All,
      }
    });

    this.consumer.closed
      .then(() => {
        console.log(`Consumer connected to stream: ${streamName}`);
        this.startMessageProcessing();
      })
      .catch((err) => {
        console.error(`Failed to connect to NATS: ${err}`);
      });
  }

  startMessageProcessing() {
    this.consumer.on('message', async (err, msg) => {
      if (err) {
        console.error('Consumer error:', err);
        return;
      }

      try {
        const eventData = JSON.parse(msg.data);
        await this.processMessage(eventData);
        msg.ack();
      } catch (processError) {
        console.error('Message processing error:', processError);
        msg.nak();
      }
    });
  }
}
```

### MinIO - Object Storage

**Purpose**: S3-compatible object storage for bot code, configuration files, backtest results, logs, and large binary data.

**Bucket Structure**:
```bash
# MinIO bucket organization
the0-platform/
├── bot-code/
│   ├── user-{userId}/
│   │   ├── bots/{botId}/
│   │   └── versions/{version}/
│   └── shared/
│       └── libraries/
├── backtest-results/
│   └── user-{userId}/
│       └── {backtestId}/
├── logs/
│   ├── api/
│   ├── runtime/
│   ├── analyzer/
│   └── ai-agent/
├── backups/
│   ├── database/
│   └── minio/
└── ai-artifacts/
    └── sessions/
```

**Storage Class Configuration**:
```yaml
# Storage class definitions
storageClass:
  fast-ssd:
    name: fast-ssd
    type: StorageClass
    provisioner: kubernetes.io/no-provisioner
    parameters:
      type: kubernetes.io/no-provisioner
      encrypted: "true"
      diskType: ssd
      iops: 3000
      fsType: ext4
      volumeBindingMode: WaitForFirstConsumer

  standard:
    name: standard
    type: StorageClass
    provisioner: kubernetes.io/no-provisioner
    parameters:
      encrypted: "true"
      type: kubernetes.io/no-provisioner

  cold:
    name: cold
    type: StorageClass
    provisioner: kubernetes.io/no-provisioner
    parameters:
      encrypted: "true"
      type: kubernetes.io/no-provisioner
```

### Redis - Caching Layer

**Purpose**: High-performance in-memory caching for session storage, API rate limiting, and frequently accessed data.

**Data Patterns**:
```javascript
// Session storage with TTL expiration
const Redis = require('redis');
const client = new Redis({
  host: process.env.REDIS_HOST,
  port: process.env.REDIS_PORT,
  password: process.env.REDIS_PASSWORD,
  db: 0 // Use database 0 for caching
});

class SessionManager {
  async createSession(sessionData, ttl = 3600) {
    const sessionId = generateSessionId();
    await client.setex(
      `session:${sessionId}`,
      'data', JSON.stringify(sessionData),
      'EX', ttl // Set expiration
    );
    return sessionId;
  }

  async getSession(sessionId) {
    const data = await client.get(`session:${sessionId}`);
    return data ? JSON.parse(data) : null;
  }
}

// API rate limiting with sliding window
class RateLimiter {
  constructor(redisClient, windowSize = 60, maxRequests = 100) {
    this.redis = redisClient;
    this.windowSize = windowSize;
    this.maxRequests = maxRequests;
  }

  async isAllowed(clientId, endpoint) {
    const key = `rate_limit:${clientId}:${endpoint}`;
    const count = await this.redis.incr(key);

    // Set expiration for the window
    await this.redis.expire(key, this.windowSize);

    // Get current window count
    const windowCount = await this.redis.get(key) || 0;

    return windowCount <= this.maxRequests;
  }
}
```

## 🔧 Configuration

### Database Configuration

**PostgreSQL Configuration**:
```yaml
# postgresql.conf for performance
listen_addresses = '*'
port = 5432
max_connections = 200
shared_buffers = '256MB'
effective_cache_size = '1GB'
maintenance_work_mem = '256MB'
checkpoint_completion_target = 0.9
wal_level = replica
synchronous_commit = on
max_wal_senders = 4
work_mem = '4MB'
min_wal_size = '80MB'

# Logging
log_destination = 'stderr'
log_collector = 'jsonlog'
log_line_prefix = '%m [%p]: [%l-1] '
log_min_duration_statement = '1s'
log_min_duration_sample = '5s'
log_checkpoints = 'on'
log_connections = 'on'
log_disconnections = 'on'
log_statement = 'all'
```

**MongoDB Configuration**:
```yaml
# mongod.conf for performance
net:
  port: 27017
  bindIp: 0.0.0.0

storage:
  dbPath: /data/mongodb
  journal:
    commitIntervalMs: 100
    enabled: true
  wiredTiger:
    enabled: true
  directoryPerDB: true
    engine: "wiredTiger"

security:
  authorization: enabled
  keyFile: /etc/mongodb/keyfile

systemLog:
  quiet: false
  logAppend: true
  destination: file
  path: /var/log/mongodb/mongod.log
  logRotate: rename

processManagement:
  fork: true
  http:
    RESTInterface: false
```

### MinIO Configuration**:
```yaml
# minio.conf for distributed mode
identity:
  enabled: true
  webhook:
    notify: http://notification-service:8080/events
    enable: true
    queue_limit: 10000

policy:
  name: encryption
  default: the0-default
  rules:
    - state: enabled
      conditions:
        - 'X-Amz-Server-Side-Encryption'
      - 'X-Amz-Storage-Class: fast-ssd'
      kms:
        masterKey: the0-platform-key

api:
  enable: true
  cors:
    allowed_origins:
      - https://the0.dev
      - https://api.the0.dev

storage:
  datastorage:
    name: the0-platform-data
  private: false
    sizeLimit: 10TB
    retentionDays: 365
    lifecycle:
      - id: 30d
      - action: transition
      - storageClass: cold

logs:
    name: the0-platform-logs
    retentionDays: 90
    lifecycle:
      - id: 90d
      - action: transition
      - storageClass: standard
```

## 🚀 Development

### Local Development Setup

```bash
# 1. Database setup
cd infrastructure
docker-compose up -d postgres redis

# 2. Start NATS server
docker-compose up -d nats

# 3. Start MinIO
docker-compose up -d minio

# 4. Initialize databases
npm run db:migrate
npm run db:seed

# 5. Start services
npm run start:api
npm run start:runtime
```

### Environment Management

**Development Environment**:
```bash
# .env.development
DATABASE_URL=postgresql://postgres:the0_password@localhost:5432/the0_platform_dev
MONGODB_URI=mongodb://localhost:27017/the0_platform_dev
REDIS_URL=redis://localhost:6379
NATS_URL=nats://localhost:4222
MINIO_ENDPOINT=http://localhost:9000
MINIO_ACCESS_KEY=the0_minio_user
MINIO_SECRET_KEY=the0_minio_password
JWT_SECRET=dev_jwt_secret_key_change_me
LOG_LEVEL=debug
```

**Testing Environment**:
```bash
# .env.testing
DATABASE_URL=postgresql://postgres:test_password@test-postgres:5432/the0_platform_test
MONGODB_URI=mongodb://test-mongo:27017/the0_platform_test
REDIS_URL=redis://test-redis:6379
NATS_URL=nats://localhost:4223
MINIO_ENDPOINT=http://localhost:9001
LOG_LEVEL=debug
```

## 🧪 Testing

### Database Migration Testing

```bash
# Migration testing with multiple database versions
npm run test:db:migrate --env=postgresql:15
npm run test:db:migrate --env=postgresql:14

# Rollback testing
npm run test:db:rollback --version=v1.2.3

# Migration data consistency testing
npm run test:db:consistency
```

### Performance Testing

```bash
# Database load testing
npm run test:performance:db:load --users=1000 --bots=5000

# Event stream performance testing
npm run test:performance:nats --events=10000 --consumers=10

# Multi-service integration testing
npm run test:integration:e2e --services=all
```

## 📊 Performance & Scaling

### Database Performance

**PostgreSQL Optimizations**:
```sql
-- Connection pooling and query optimization
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS pg_stat_activity;

-- Query performance analysis
SELECT
  query,
  calls,
  total_exec_time,
  rows,
  shared_blks_hit,
  shared_blks_read,
  mean_exec_time,
  stddev_exec_time
FROM pg_stat_statements
WHERE query != '<internal>'
ORDER BY mean_exec_time DESC
LIMIT 10;

-- Index usage analysis
SELECT
  schemaname,
  tablename,
  indexname,
  idx_scan,
  idx_tup_read,
  idx_tup_fetch,
  idx_tup_return
FROM pg_stat_user_indexes
WHERE schemaname NOT IN ('pg_catalog', 'pg_toast')
ORDER BY idx_scan DESC, idx_tup_fetch DESC
LIMIT 10;
```

**MongoDB Performance**:
```javascript
// Performance monitoring with MongoDB profiler
db.setProfilingLevel(2, {sampleRate: 0.1});

// Query optimization with proper indexes
db.bots.createIndex(
  { userId: 1, createdAt: -1 },
  { status: 1 }
);

// Connection pooling with proper timeout
const client = new MongoClient(uri, {
  connectTimeoutMS: 5000,
  socketTimeoutMS: 30000,
  maxPoolSize: 10,
  minPoolSize: 2
});
```

### Scaling Strategy

**Horizontal Scaling**:
```yaml
# PostgreSQL read replicas
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata:
  name: postgres-cluster
spec:
  postgresql:
    replication:
      synchronous_commit: 'off'
      synchronous_mode: 'local'
      num_sync_replicas: 2
      synchronous_node: 'pg-primary'
    bootstrap:
      pg_hosts:
        - host: postgres-0.postgres.example.com
          primary: true
          application_name: "postgres"

# MongoDB sharding
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: mongodb-shard-1
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: mongodb
        command:
          - mongod
          - --replSet
          - mongodb-shard-1
          - --bind_ip_all
          - --port
          - "27017"
        env:
          - MONGODB_REPLICA_SET_NAME: mongodb-shard-1
          - MONGODB_REPLICA_SET_MODE: primary
          - MONGODB_ADVERTISED_HOST: mongodb-shard-1-mongodb
          - MONGODB_ADVERTISED_PORT: "27017"
```

## 🔍 Monitoring & Observability

### Comprehensive Monitoring Stack

**Prometheus Configuration**:
```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s
  external_labels:
    environment: production
    deployment: kubernetes

rule_files:
  - "/opt/prometheus/rules/*.yml"

scrape_configs:
  - job_name: 'kubernetes-apiservers'
    kubernetes_sd_configs:
    - role: endpoints
      namespaces:
        names:
          - the0-platform
    relabel_configs:
      - source_labels:
        __meta_kubernetes_service_name: "api"
      - target_label: app
      - regex: "backend"
```

**Grafana Dashboards**:
```json
{
  "dashboard": {
    "title": "the0 Platform Overview",
    "panels": [
      {
        "title": "Service Health",
        "type": "stat",
        "targets": [
          "prometheus.the0-platform",
          "nats.the0-platform"
        ],
        "gridPos": { "x": 0, "y": 0 }
      },
      {
        "title": "Database Performance",
        "type": "graph",
        "targets": ["postgres.the0-platform"],
        "panels": [
          {
            "title": "Connection Pool",
            "type": "stat",
            "targets": ["postgres.the0-platform"]
          }
        ]
      },
      {
        "title": "Application Metrics",
        "type": "stat",
        "targets": ["api.the0-platform"],
        "queries": [
          {
            "expr": "sum(rate(http_requests_total[5m]))",
            "legendFormat": "Total Requests"
          }
        ]
      }
    ]
  }
}
```

## 🛡️ Security

### Data Encryption

**Encryption at Rest**:
```yaml
# Database encryption configuration
encryption:
  default:
    key: "DATABASE_ENCRYPTION_KEY"
    algorithm: "AES-256-CBC"
    store_salt: "DATABASE_ENCRYPTION_SALT"
    iv:
      type: "PGPKEY"
      value: "DATABASE_ENCRYPTION_IV"
    data: "POSTGRES_ENCRYPTION_KEY"

# MongoDB field-level encryption
security:
  enableEncryption: true
  encryptionKeyFile: "/etc/mongodb/encryption-key"
  kmip:
    key: "MONGODB_ENCRYPTION_KEY"
    rotate: true
```

**Access Control**:
```javascript
// Row-level security policies
class DataAccessLayer {
  async getUserData(userId, requestingUserId) {
    if (userId !== requestingUserId) {
      throw new Error('Unauthorized access to user data');
    }

    return await this.userRepository.findById(userId);
  }

  // Column-level encryption for sensitive data
  encryptSensitiveData(data) {
    return {
      id: data.id,
      email: this.encrypt(data.email),
      profile: data.profile,
      encrypted_data: this.encrypt(data.sensitiveData)
    };
  }
}
```

### Backup and Recovery

**Automated Backup Strategy**:
```bash
#!/bin/bash
# Database backup script
BACKUP_DIR="/backups/$(date +%Y%m%d)"
S3_BUCKET="the0-platform-backups"

# PostgreSQL backup
pg_dump -U postgres_user -h localhost -d the0_platform \
  --format=custom --no-owner \
  --schema-only | gzip > "$BACKUP_DIR/postgres.sql.gz"

# MongoDB backup
mongodump --db the0_platform --gzip \
  --out "$BACKUP_DIR/mongodb.json.gz"

# Upload to S3
aws s3 cp "$BACKUP_DIR" "s3://$S3_BUCKET/backups/" \
  --recursive

# Cleanup old backups (keep last 30 days)
aws s3 ls "s3://$S3_BUCKET/backups/" \
  --recursive | grep '^d' | awk '{print $7}' | sort -nr \
  | awk 'NR > 30 {system("rm -rf " $7)}' | xargs -I {} aws s3 rm "s3://$S3_BUCKET/backups/"

echo "Backup completed: $BACKUP_DIR"
```

### Data Privacy Compliance

**PII Data Protection**:
```javascript
// Data masking for PII
class DataPrivacyService {
  maskSensitiveData(userData) {
    return {
      id: userData.id,
      email: this.maskEmail(userData.email),
      phone: this.maskPhone(userData.phone),
      address: this.maskAddress(userData.address),
      ip: this.maskIP(userData.ip)
    };
  }

  maskEmail(email) {
    return email.replace(/(.{2})(?=.*)/, '***$2$3***');
  }

  // GDPR right to be forgotten implementation
  async deleteUserData(userId, requestorUserId) {
    if (userId !== requestorUserId) {
      throw new Error('Unauthorized deletion request');
    }

    // Soft delete
    await this.userRepository.softDelete(userId);

    // Schedule hard deletion after 30 days
    await this.scheduleDataDeletion(userId, 30);
  }
}
```

## 🔄 Integration Points

### API Layer Integration

```typescript
// Unified data access layer
interface DataRepository {
  // User operations
  create(user: CreateUserDto): Promise<User>;
  findById(id: string): Promise<User>;
  update(id: string, data: Partial<User>): Promise<User>;
  delete(id: string): Promise<void>;

  // Bot operations
  createBot(bot: CreateBotDto): Promise<Bot>;
  updateBot(id: string, data: Partial<Bot>): Promise<Bot>;
  deleteBot(id: string): Promise<void>;

  // Analytics operations
  getUserAnalytics(userId: string): Promise<UserAnalytics>;
  getBotPerformance(botId: string): Promise<BotPerformance>;
}

// Event publishing
interface EventPublisher {
  publishBotCreated(bot: Bot): Promise<void>;
  publishBotUpdated(bot: Bot): Promise<void>;
  publishTradeExecuted(trade: Trade): Promise<void>;
  publishMetrics(metrics: AnalyticsMetrics): Promise<void>;
}
```

### Cross-Database Consistency

```typescript
// Eventual consistency between databases
class ConsistencyManager {
  async ensureConsistency(entityType: 'user', entityId: string, data: any) {
    // Update primary database
    await this.postgresRepository.updateUser(entityId, data);

    // Update MongoDB cache with event
    await this.eventPublisher.publish('user.updated', {
      entityType: 'user',
      entityId: entityId,
      updatedFields: Object.keys(data),
      timestamp: new Date()
    });

    // Verify consistency
    const postgresData = await this.postgresRepository.findById(entityId);
    const mongoData = await this.mongoCacheRepository.get(entityId);

    return this.compareDataConsistency(postgresData, mongoData);
  }

  async syncUserData(userId: string) {
    const userData = await this.postgresRepository.findById(userId);
    await this.mongoCacheRepository.set(userId, userData, { ttl: 3600 });
  }
}
```

## 🐛 Troubleshooting

### Common Issues

1. **Database Connection Failures**
   - **Symptoms**: Application errors, connection timeouts
   - **Causes**: Network issues, authentication problems, resource exhaustion
   - **Solutions**: Check connection strings, verify credentials, monitor resource usage
   - **Prevention**: Connection pooling, circuit breakers, health checks

2. **Data Inconsistency**
   - **Symptoms**: Data differences between databases, missing records
   - **Causes**: Concurrent access, partial updates, replication lag
   - **Solutions**: Implement transactions, add consistency checks, use distributed locks
   - **Prevention**: Event sourcing, idempotent operations, proper transaction boundaries

3. **Performance Degradation**
   - **Symptoms**: Slow queries, high CPU usage, memory issues
   - **Causes**: Missing indexes, unoptimized queries, connection bottlenecks
   - **Solutions**: Query optimization, proper indexing, connection pooling
   - **Prevention**: Regular performance monitoring, query analysis

### Debugging Tools

```bash
# Database connection debugging
pg_isready -h localhost -U postgres_user -d the0_platform

# MongoDB connection debugging
mongosh --eval 'db.runCommand("ping")' --host localhost

# Redis connection debugging
redis-cli -h localhost -a the0_redis_password ping

# Performance monitoring
docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}"

# Event stream debugging
nats stream list
nats stream info CONSUMER bot.lifecycle --raw

# MinIO connectivity
mc admin info local
```

## 📈 Future Roadmap

### Planned Enhancements

- **Multi-Region Deployment**: Geographic distribution for improved latency and availability
- **CQRS Implementation**: Command Query Responsibility Segregation for better scalability
- **Advanced Security**: End-to-end encryption, zero-knowledge proofs, secure enclaves
- **Machine Learning Integration**: AI-powered data analytics and anomaly detection
- **Edge Computing**: Local data processing for reduced latency

### Infrastructure Improvements

- **GitOps Integration**: Automated deployment pipelines and infrastructure as code
- **Microservices Platform**: Service mesh for advanced networking and observability
- **Serverless Components**: AWS Lambda functions for burst workloads

## 📚 Additional Resources

### Documentation

- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [MongoDB Documentation](https://docs.mongodb.com/)
- [NATS Documentation](https://docs.nats.io/)
- [MinIO Documentation](https://docs.min.io/)
- [Redis Documentation](https://redis.io/documentation/)

### Tools & Utilities

- **Database Tools**: pgAdmin, MongoDB Compass, Redis Commander
- **Migration Tools**: Flyway, Liquibase
- **Monitoring Tools**: Prometheus, Grafana, Jaeger
- **Performance Tools**: Database Benchmarking, Load Testing

### Related Services

- **API Services**: Individual service documentation for database interaction
- **Runtime Services**: Data access patterns for bot execution
- **Infrastructure**: Deployment and operations documentation

---

*Last updated: October 2024*