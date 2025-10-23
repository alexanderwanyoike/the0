---
title: "Data Architecture"
description: "Actual data architecture implementation with PostgreSQL for AI chat, MongoDB for runtime services, NATS JetStream for events, and MinIO for object storage"
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
| PostgreSQL | 15+ | AI chat sessions and artifact metadata | Relational |
| MongoDB | 7+ | Runtime bot execution data | Document storage |
| NATS JetStream | 2.10+ | Event streaming, message persistence | Events |
| MinIO | Latest | Object storage for bots and artifacts | Object storage |

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
        AI_AGENT[AI Assistant<br/>FastAPI Service<br/>Chat & Artifacts]
        RUNTIME[Runtime Services<br/>Go Microservices<br/>Bot Execution]
        SECURITY[0vers33r Security<br/>Analysis Service]
    end

    subgraph "Event Streaming Layer"
        NATS_STREAM[NATS JetStream<br/>THE0_EVENTS Stream]
        HYBRID_NATS[Hybrid NATS<br/>JetStream + Regular]
    end

    subgraph "Data Storage Layer"
        POSTGRES_AI[(PostgreSQL<br/>AI Chat Sessions)]
        MONGO_RUNTIME[(MongoDB<br/>Runtime Services<br/>3 Databases)]
        MINIO_STORAGE[(MinIO<br/>Object Storage<br/>Custom Bots & Files)]
    end

    subgraph "Integration Layer"
        EXTERNAL_APIS[External Services<br/>Market Data<br/>Exchange APIs]
        GOOGLE_AI[Google Gemini<br/>AI Model]
    end
```

### Data Relationships

```mermaid
classDiagram
    class ChatSession {
        +id: String
        +user_id: String
        +title: String
        +is_active: Boolean
        +created_at: DateTime
        +updated_at: DateTime
        +messages: ChatMessage[]
        +artifacts: Artifact[]
    }

    class ChatMessage {
        +id: Integer
        +session_id: String
        +role: String
        +content: Text
        +artifacts_created: String[]
        +timestamp: DateTime
    }

    class Artifact {
        +id: Integer
        +session_id: String
        +filename: String
        +file_path: String
        +mime_type: String
        +version: Integer
        +created_at: DateTime
    }

    class RuntimeBot {
        +id: String
        +segment_id: Integer
        +config: Map
        +custom: CustomBotVersion
        +enabled: Boolean
    }

    class CustomBotVersion {
        +version: String
        +config: APIBotConfig
        +filePath: String
        +createdAt: DateTime
        +updatedAt: DateTime
    }

    class BacktestExecution {
        +id: String
        +segment_id: Integer
        +config: Map
        +status: String
        +results: Map
    }

    class BotSchedule {
        +id: String
        +segment_id: Integer
        +schedule: Map
        +next_execution: DateTime
    }

    ChatSession "1" -- "*" ChatMessage : contains
    ChatSession "1" -- "*" Artifact : generates
    RuntimeBot "1" -- "1" CustomBotVersion : has
```

## 📡 Database Architecture

### PostgreSQL - Primary Relational Database

**Purpose**: AI agent chat sessions and artifact metadata storage for the the0-ai service using SQLAlchemy 2.0 with asyncpg driver.

**Actual Schema Design**:
```sql
-- Chat sessions table for AI conversations
CREATE TABLE chat_sessions (
    id VARCHAR(255) PRIMARY KEY,
    user_id VARCHAR(255) NOT NULL,
    title TEXT,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Chat messages table for conversation history
CREATE TABLE chat_messages (
    id SERIAL PRIMARY KEY,
    session_id VARCHAR(255) NOT NULL,
    role VARCHAR(20) NOT NULL,
    content TEXT NOT NULL,
    artifacts_created TEXT[],
    timestamp TIMESTAMP DEFAULT NOW(),
    FOREIGN KEY (session_id) REFERENCES chat_sessions(id) ON DELETE CASCADE
);

-- Artifacts table for AI-generated files
CREATE TABLE artifacts (
    id SERIAL PRIMARY KEY,
    session_id VARCHAR(255) NOT NULL,
    filename VARCHAR(255) NOT NULL,
    file_path TEXT NOT NULL,
    mime_type VARCHAR(100) DEFAULT 'text/plain',
    version INTEGER DEFAULT 1,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    FOREIGN KEY (session_id) REFERENCES chat_sessions(id) ON DELETE CASCADE,
    UNIQUE (session_id, filename)
);

-- Settings table for AI configuration
CREATE TABLE settings (
    id SERIAL PRIMARY KEY,
    key VARCHAR(255) UNIQUE NOT NULL,
    value TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);
```

**Technology Integration**:
- **SQLAlchemy 2.0**: Modern async ORM with relationship mapping
- **AsyncPG Driver**: High-performance PostgreSQL async driver
- **Google ADK Integration**: AI development kit for tool orchestration
- **Alembic Migrations**: Database version control and schema management

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

**Purpose**: Runtime services data storage with segmentation-based partitioning for bot execution, backtesting, and scheduling operations.

**Actual Database Structure**:
The MongoDB implementation spans **three separate databases** for different runtime services:

```javascript
// bot_runner database - bots collection
{
  "_id": ObjectId("..."),
  "id": "bot-123",
  "segment_id": 1,
  "config": {
    "enabled": true,
    "settings": {...}
  },
  "custom": {
    "version": "1.0.0",
    "config": {
      "name": "My Trading Bot",
      "description": "Algorithmic trading strategy",
      "runtime": "python3.11",
      "version": "1.0.0",
      "author": "Developer Name",
      "type": "trading",
      "entrypoints": {...},
      "schema": {...},
      "readme": "Bot documentation",
      "metadata": {...}
    },
    "filePath": "/path/to/bot",
    "createdAt": "2024-01-01T00:00:00Z",
    "updatedAt": "2024-01-01T00:00:00Z"
  }
}

// backtest_runner database - backtests collection
{
  "_id": ObjectId("..."),
  "id": "backtest-456",
  "segment_id": 1,
  "config": {...},
  "status": "running",
  "results": {
    "performance": {...},
    "trades": [...],
    "metrics": {...}
  }
}

// bot_scheduler database - bot_schedules collection
{
  "_id": ObjectId("..."),
  "id": "schedule-789",
  "segment_id": 1,
  "schedule": {
    "enabled": true,
    "cron": "0 9 * * 1-5",
    "timezone": "UTC"
  }
}
```

**Supporting Collections**:
```javascript
// partitions collection (across all databases)
{
  "_id": int32(1),
  "bot_count": 5,
  "max_capacity": 10
}

// counters collection for sequence generation
{
  "_id": "bot_id_sequence",
  "seq": 1000
}
```

**Segmentation Strategy**:
- **No Sharding**: Unlike documented, no MongoDB sharding is implemented
- **Application-Level Segmentation**: Services use segment_id for partitioning
- **Database Isolation**: Separate databases per service (`bot_runner`, `backtest_runner`, `bot_scheduler`)
- **Partition Management**: Dynamic partition allocation with capacity tracking

### NATS JetStream - Event Streaming

**Purpose**: Real-time event streaming and message persistence for platform microservices communication using a hybrid JetStream and regular NATS approach.

**Actual Stream Configuration**:
```typescript
// THE0_EVENTS stream configuration from API service
await this.jetStreamManager.streams.add({
  name: "THE0_EVENTS",
  subjects: [
    "custom-bot.submitted",
    "custom-bot.approved",
    "custom-bot.declined",
    "custom-bot.awaiting-human-review",
    "custom-bot.analysis-failed",
    "the0.bot.created",
    "the0.bot.updated",
    "the0.bot.deleted",
    "the0.bot-schedule.created",
    "the0.bot-schedule.updated",
    "the0.bot-schedule.deleted",
    "the0.backtest.created",
    "the0.backtest.completed",
  ],
  retention: RetentionPolicy.Limits,
  max_age: 7 * 24 * 60 * 60 * 1000000, // 7 days in nanoseconds
  storage: StorageType.File,
});
```

**Hybrid Architecture Pattern**:
- **JetStream for Critical Events**: Bot lifecycle and scheduling events use persistent storage
- **Regular NATS for High-Volume**: Backtest events use standard NATS publishing for performance
- **Selective Persistence**: Only events requiring durability and replay use JetStream
- **File-Based Storage**: Persistent events stored in file system for reliability

**Actual Consumer Implementation**:
```go
// Runtime service NATS subscriber pattern (Go implementation)
type NATSSubscriber struct {
    connection     *nats.Conn
    jetStream      jetstream.JetStream
    mongoClient    *mongo.Client
    dbName         string
    collectionName string
    maxBotsPerPartition int32
}

// Subscribe with queue group for load balancing
func (s *NATSSubscriber) Subscribe() error {
    _, err := s.connection.QueueSubscribe(
        "custom-bot.submitted",
        "bot-runner-workers", // Queue group for load balancing
        s.handleMessage,
        nats.Durable("bot-runner-durable"),
    )
    return err
}

// Message handling with MongoDB persistence
func (s *NATSSubscriber) handleMessage(msg *nats.Msg) {
    var message CustomBotEvent
    json.Unmarshal(msg.Data, &message)

    // Store in MongoDB with segmentation
    collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)
    // ... persistence logic
}
```

### MinIO - Object Storage

**Purpose**: S3-compatible object storage for custom bots, AI artifacts, and platform files with user-based segmentation.

**Actual Bucket Implementation**:
```bash
# Primary MinIO buckets in use
the0-platform/
├── custom-bots/           # Custom bot ZIP submissions
│   └── {userId}/
│       └── {botId}.zip
├── bot-code/             # Extracted bot source code
│   └── {userId}/
│       └── {botId}/
├── backtest-results/     # Backtest execution outputs
│   └── {userId}/
│       └── {backtestId}/
├── ai-artifacts/         # AI session generated files
│   └── sessions/
│       └── {sessionId}/
│           └── {filename}
└── logs/                 # Application and service logs
```

**Real Implementation Pattern**:
```python
# 0vers33r service MinIO client usage
class MinIOStorageClient:
    def __init__(self):
        self.bucket_name = os.getenv('CUSTOM_BOTS_BUCKET', 'custom-bots')
        self.client = Minio(
            endpoint=self.endpoint,
            access_key=self.access_key,
            secret_key=self.secret_key,
            secure=self.secure
        )

    def download_file(self, file_path: str) -> bytes:
        """Download with 1GB size limit for security"""
        obj_info = self.client.stat_object(self.bucket_name, file_path)
        if obj_info.size > 1 * 1024 * 1024 * 1024:
            raise ValueError(f"File too large: {obj_info.size} bytes")
        # ... download logic
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

### Redis - Not Implemented

**Status**: Redis is included as a dependency in the API service but is **not implemented** in the current architecture.

**Current Implementation**:
- **Package Dependency**: Redis v4.6.10 exists in `package.json`
- **Test Mocks**: Redis is mocked in test files only
- **No Service Configuration**: No Redis service in Docker compose
- **No Code Usage**: No actual Redis client implementation in production code

**Recommendations**:
- Implement Redis for session caching and API rate limiting
- Add Redis service to Docker compose for development
- Consider Redis for caching frequently accessed bot configurations
- Add Redis clustering for high availability in production
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

## ⚠️ Implementation Gaps & Recommendations

### Missing Components

**Database Connection Pooling**
- **Current State**: Basic database connections only
- **Impact**: Potential connection exhaustion under load
- **Recommendation**: Implement PgBouncer for PostgreSQL and connection pooling for MongoDB

**Comprehensive Backup Strategy**
- **Current State**: No automated backup configurations documented
- **Impact**: Risk of data loss in production
- **Recommendation**:
  - PostgreSQL: Implement WAL-E/WAL-G for continuous archiving
  - MongoDB: Configure regular snapshots with point-in-time recovery
  - MinIO: Cross-region replication and lifecycle policies

**Advanced Monitoring & Observability**
- **Current State**: Basic health checks only
- **Impact**: Limited visibility into performance issues
- **Recommendation**:
  - Implement Prometheus exporters for all databases
  - Add distributed tracing with Jaeger
  - Database-specific monitoring (pgBadger, MongoDB Atlas monitoring)

**Caching Layer**
- **Current State**: Redis dependency exists but not implemented
- **Impact**: Increased database load and slower response times
- **Recommendation**:
  - Implement Redis for session caching and API rate limiting
  - Add Redis Cluster for high availability
  - Consider application-level caching for frequently accessed bot configurations

### Performance Optimizations

**PostgreSQL Optimizations**
```sql
-- Recommended indexes for AI chat workloads
CREATE INDEX CONCURRENTLY idx_chat_sessions_user_active
ON chat_sessions (user_id, is_active)
WHERE is_active = true;

CREATE INDEX CONCURRENTLY idx_chat_messages_session_timestamp
ON chat_messages (session_id, timestamp DESC);

-- Partition large tables by time
CREATE TABLE chat_messages_y2024m01 PARTITION OF chat_messages
FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');
```

**MongoDB Optimizations**
```javascript
// Recommended indexes for runtime queries
db.bots.createIndex({ "segment_id": 1, "config.enabled": 1 });
db.backtests.createIndex({ "segment_id": 1, "status": 1, "created_at": -1 });
db.bot_schedules.createIndex({ "segment_id": 1, "schedule.next_execution": 1 });

// Implement read preferences for analytics
db.getMongo().setReadPref("secondaryPreferred");
```

### Security Enhancements

**Data Encryption at Rest**
- **PostgreSQL**: Enable TDE (Transparent Data Encryption)
- **MongoDB**: Enable storage engine encryption
- **MinIO**: Configure server-side encryption with AWS KMS

**Network Security**
- Implement TLS 1.3 for all database connections
- Configure private network endpoints
- Add database firewalls and access control lists

**Audit Logging**
- PostgreSQL: Enable pg_audit extension
- MongoDB: Configure audit filters for sensitive operations
- MinIO: Enable access logging and monitoring

### Scalability Improvements

**Horizontal Scaling**
```yaml
# PostgreSQL read replicas for AI chat scaling
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata:
  name: postgres-cluster
spec:
  instances: 3
  primaryUpdateStrategy: unsupervised

  # Bootstrap configuration
  bootstrap:
    initdb:
      database: the0_ai
      owner: the0_user
      secret:
        name: postgres-credentials

# MongoDB replica sets for runtime services
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: mongodb-replica
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: mongodb
        command:
        - mongod
        - --replSet
        - mongodb-rs
        - --bind_ip_all
```

**High Availability Architecture**
- Multi-AZ deployment for all databases
- Automatic failover configurations
- Data replication across geographic regions
- Disaster recovery procedures with RTO/RPO targets

## 📚 Additional Resources

### Documentation

- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [MongoDB Documentation](https://docs.mongodb.com/)
- [NATS Documentation](https://docs.nats.io/)
- [MinIO Documentation](https://docs.min.io/)
- [Redis Documentation](https://redis.io/documentation/)

### Tools & Utilities

- **Database Tools**: pgAdmin, MongoDB Compass
- **Migration Tools**: Alembic (PostgreSQL), Mongomirror (MongoDB)
- **Monitoring Tools**: Prometheus, Grafana, Jaeger
- **Performance Tools**: Database Benchmarking, Load Testing

### Related Services

- **API Services**: Individual service documentation for database interaction
- **Runtime Services**: Data access patterns for bot execution
- **Infrastructure**: Deployment and operations documentation

---

*Last updated: October 2024*