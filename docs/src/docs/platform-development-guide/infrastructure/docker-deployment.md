---
title: "Docker Deployment"
description: "Local development and container orchestration using Docker Compose with 10+ integrated services"
order: 10
---

# Docker Deployment

## 🎯 Overview

Docker deployment provides a complete containerized development environment for the0 algorithmic trading platform. Using Docker Compose, this setup orchestrates 10+ microservices including databases, message brokers, runtime engines, and AI services. It's designed for local development, testing, and can be extended for production deployments with proper configuration.

### Key Benefits

- **Complete Platform**: All services pre-configured and interconnected
- **Development Ready**: Hot reload support for frontend and API services
- **Data Persistence**: All data persisted across container restarts
- **Isolated Environment**: Clean, reproducible development setup
- **Easy Scaling**: Service scaling and resource management via Docker Compose

### Deployment Architecture

The Docker setup creates a complete microservices ecosystem:

```mermaid
graph TB
    subgraph "Docker Compose Environment"
        subgraph "Application Layer"
            API[the0-api:3000<br/>NestJS Backend]
            FRONTEND[the0-frontend:3001<br/>Next.js Web App]
            RUNTIME[the0-runtime:8080<br/>Go Execution Engine]
            ANALYZER[the0-analyzer:8000<br/>Python Security Scanner]
            AI[the0-ai:8001<br/>AI Assistant]
        end

        subgraph "Data Layer"
            POSTGRES[postgres:5432<br/>Primary Database]
            MONGODB[mongo:27017<br/>Runtime Data]
            REDIS[redis:6379<br/>Cache & Sessions]
        end

        subgraph "Infrastructure Services"
            NATS[nats:4222/8222<br/>Message Broker]
            MINIO[minio:9000/9001<br/>Object Storage]
            MONITORING[monitoring:8080<br/>Health & Metrics]
        end

        subgraph "Development Tools"
            ADMINER[adminer:8081<br/>Database Admin]
            GRAFANA[grafana:3002<br/>Dashboard & Analytics]
            PROMETHEUS[prometheus:9090<br/>Metrics Collection]
        end
    end

    %% Networks
    POSTGRES -. API
    MONGODB -. RUNTIME
    REDIS -. FRONTEND
    REDIS -. AI
    NATS -. API
    NATS -. RUNTIME
    NATS -. ANALYZER
    NATS -. AI
    MINIO -. API
    MINIO -. RUNTIME
    MINIO -. ANALYZER
    MINIO -. AI
```

## 🛠️ Technology Stack

| Technology | Version | Purpose |
|------------|---------|---------|
| Docker | 20.10+ | Containerization and orchestration |
| Docker Compose | 2.0+ | Multi-container application management |
| PostgreSQL | 15+ | Primary relational database |
| MongoDB | 7+ | Document storage for runtime data |
| NATS JetStream | 2.10+ | Message streaming and persistence |
| Redis | 7+ | Caching and session management |
| MinIO | Latest | S3-compatible object storage |
| Docker Networks | - | Service isolation and communication |
| Docker Volumes | - | Data persistence and sharing |

## 🏗️ Architecture & Design

### Service Architecture

#### Application Services

**the0-api (Port 3000)**
- **Technology**: NestJS, TypeScript, PostgreSQL
- **Purpose**: Core API server providing REST endpoints
- **Features**: JWT authentication, bot management, backtesting orchestration
- **Build**: Source mounted with hot reload for development

**the0-frontend (Port 3001)**
- **Technology**: Next.js 15, React 19, Tailwind CSS
- **Purpose**: Web dashboard and management interface
- **Features**: Server-side rendering, real-time updates via WebSocket
- **Build**: Source mounted with Turbopack for fast development

**the0-runtime (Port 8080)**
- **Technology**: Go 1.24, gRPC, NATS
- **Purpose**: Bot execution and runtime management
- **Features**: Master-worker pattern, horizontal scaling, resource isolation

**the0-analyzer (Port 8000)**
- **Technology**: Python 3.11, FastAPI, YARA
- **Purpose**: Security analysis and malware detection
- **Features**: YARA rule scanning, AI semantic analysis, event processing

**the0-ai (Port 8001)**
- **Technology**: Python 3.11, FastAPI, Google Gemini AI
- **Purpose**: AI-powered development assistance
- **Features**: Natural language interface, code generation, web browsing

#### Data Services

**PostgreSQL (Port 5432)**
- **Purpose**: Primary relational database
- **Data**: Users, bots, authentication, configurations
- **Features**: Persistent data, ACID compliance, backups

**MongoDB (Port 27017)**
- **Purpose**: Document store for runtime data
- **Data**: Bot execution state, logs, analytics
- **Features**: Flexible schema, high performance, sharding support

**Redis (Port 6379)**
- **Purpose**: In-memory data store
- **Use Cases**: Session cache, rate limiting, real-time data
- **Features**: Pub/sub, data expiration, persistence options

#### Infrastructure Services

**NATS (Ports 4222, 8222)**
- **Purpose**: Message broker and streaming platform
- **Features**: JetStream persistence, clustering support, security
- **Protocols**: TCP, WebSocket, HTTP monitoring

**MinIO (Ports 9000, 9001)**
- **Purpose**: S3-compatible object storage
- **Features**: File uploads, versioning, lifecycle policies
- **Usage**: Bot code storage, backtest results, logs

### Docker Compose Configuration

```yaml
version: '3.8'

services:
  # PostgreSQL Database
  postgres:
    image: postgres:15-alpine
    container_name: the0-postgres
    environment:
      POSTGRES_DB: the0_oss
      POSTGRES_USER: the0
      POSTGRES_PASSWORD: the0_password
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ../database/init:/docker-entrypoint-initdb.d
    ports:
      - "5432:5432"
    networks:
      - the0-network
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U the0 -d the0_oss"]
      interval: 10s
      timeout: 5s
      retries: 5

  # MongoDB for Runtime Data
  mongo:
    image: mongo:7-jammy
    container_name: the0-mongo
    environment:
      MONGO_INITDB_ROOT_USERNAME: the0_mongo_user
      MONGO_INITDB_ROOT_PASSWORD: the0_mongo_password
      MONGO_INITDB_DATABASE: bot_runner
    volumes:
      - mongo_data:/data/db
    ports:
      - "27017:27017"
    networks:
      - the0-network
    healthcheck:
      test: ["CMD-SHELL", "mongosh --eval 'db.runCommand(\"ping\")'"]
      interval: 30s
      timeout: 10s
      retries: 3

  # NATS Message Broker
  nats:
    image: nats:2.10-alpine
    container_name: the0-nats
    command: ["-js", "-m", "8222", "--store_dir", "/data"]
    volumes:
      - nats_data:/data
    ports:
      - "4222:4222"
      - "8222:8222"
    networks:
      - the0-network
    healthcheck:
      test: ["CMD-SHELL", "wget -q --spider -O /dev/null http://localhost:8222/jsz?id=stream || exit 1"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Redis Cache
  redis:
    image: redis:7-alpine
    container_name: the0-redis
    command: redis-server --appendonly yes --requirepass the0_redis_password
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"
    networks:
      - the0-network
    healthcheck:
      test: ["CMD-SHELL", "redis-cli -a the0_redis_password ping || exit 1"]
      interval: 10s
      timeout: 3s
      retries: 3

  # MinIO Object Storage
  minio:
    image: minio/minio:latest
    container_name: the0-minio
    command: server /data --console-address ":9001"
    environment:
      MINIO_ROOT_USER: the0_minio_user
      MINIO_ROOT_PASSWORD: the0_minio_password
    volumes:
      - minio_data:/data
    ports:
      - "9000:9000"
      - "9001:9001"
    networks:
      - the0-network
    healthcheck:
      test: ["CMD-SHELL", "curl -f http://localhost:9000/minio/health/live || exit 1"]
      interval: 30s
      timeout: 20s
      retries: 3

  # the0 API Service
  the0-api:
    build:
      context: ../api
      dockerfile: Dockerfile
      args:
        - --build-arg
        - BUILDKIT_INLINE_CACHE=1
    container_name: the0-api
    environment:
      NODE_ENV: development
      DATABASE_URL: postgresql://the0:the0_password@postgres:5432/the0_oss
      JWT_SECRET: your-jwt-secret-change-me
      NATS_URL: nats://nats:4222
      MINIO_ENDPOINT: http://minio:9000
      MINIO_ACCESS_KEY: the0_minio_user
      MINIO_SECRET_KEY: the0_minio_password
      REDIS_URL: redis://:the0_redis_password@redis:6379
    volumes:
      - ../api:/app
      - /app/node_modules
    ports:
      - "3000:3000"
    networks:
      - the0-network
    depends_on:
      postgres:
        condition: service_healthy
      nats:
        condition: service_healthy
      redis:
        condition: service_healthy
      minio:
        condition: service_healthy
    healthcheck:
      test: ["CMD-SHELL", "curl -f http://localhost:3000/health || exit 1"]
      interval: 30s
      timeout: 10s
      retries: 3

  # the0 Frontend Service
  the0-frontend:
    build:
      context: ../frontend
      dockerfile: Dockerfile
    container_name: the0-frontend
    environment:
      NODE_ENV: development
      NEXT_PUBLIC_API_URL: http://localhost:3000
      NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY: pk_test_your_stripe_key
    volumes:
      - ../frontend:/app
      - /app/.next
      - /app/node_modules
    ports:
      - "3001:3001"
    networks:
      - the0-network
    depends_on:
      - the0-api:
        condition: service_healthy
    healthcheck:
      test: ["CMD-SHELL", "curl -f http://localhost:3001 || exit 1"]
      interval: 30s
      timeout: 10s
      retries: 3

  # Runtime Services
  the0-runtime:
    build:
      context: ../runtime
      dockerfile: Dockerfile
    container_name: the0-runtime
    environment:
      NATS_URL: nats://nats:4222
      MONGODB_URI: mongodb://the0_mongo_user:the0_mongo_password@mongo:27017/bot_runner
      MINIO_ENDPOINT: http://minio:9000
      MINIO_ACCESS_KEY: the0_minio_user
      MINIO_SECRET_KEY: the0_minio_password
      MAX_CONCURRENT_BOTS: 10
      LOG_LEVEL: info
    volumes:
      - runtime_data:/data
    ports:
      - "8080:8080"
    networks:
      - the0-network
    depends_on:
      nats:
        condition: service_healthy
      mongo:
        condition: service_healthy
      minio:
        condition: service_healthy

  # Security Analyzer
  the0-analyzer:
    build:
      context: ../services/0vers33r
      dockerfile: Dockerfile
    container_name: the0-analyzer
    environment:
      DATABASE_URL: postgresql://the0:the0_password@postgres:5432/the0_oss
      NATS_URL: nats://nats:4222
      MINIO_ENDPOINT: http://minio:9000
      MINIO_ACCESS_KEY: the0_minio_user
      MINIO_SECRET_KEY: the0_minio_password
      GEMINI_API_KEY: your-gemini-api-key
      CUSTOM_BOTS_BUCKET: custom-bots
      MAX_CONCURRENT_ANALYSES: 5
    ports:
      - "8000:8000"
    networks:
      - the0-network
    depends_on:
      postgres:
        condition: service_healthy
      nats:
        condition: service_healthy
      minio:
        condition: service_healthy

  # AI Agent
  the0-ai:
    build:
      context: ../services/the0-ai
      dockerfile: Dockerfile
    container_name: the0-ai
    environment:
      DATABASE_URL: postgresql://the0:the0_password@postgres:5432/the0_oss
      GEMINI_API_KEY: your-gemini-api-key
      THE0_API_URL: http://the0-api:3000
      NATS_URL: nats://nats:4222
      MINIO_ENDPOINT: http://minio:9000
      MINIO_ACCESS_KEY: the0_minio_user
      MINIO_SECRET_KEY: the0_minio_password
      REDIS_URL: redis://:the0_redis_password@redis:6379
    ports:
      - "8001:8001"
    networks:
      - the0-network
    depends_on:
      postgres:
        condition: service_healthy
      nats:
        condition: service_healthy
      redis:
        condition: service_healthy
      the0-api:
        condition: service_healthy

volumes:
  postgres_data:
    driver: local
  mongo_data:
    driver: local
  nats_data:
    driver: local
  redis_data:
    driver: local
  minio_data:
    driver: local
  runtime_data:
    driver: local

networks:
  the0-network:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/16
          gateway: 172.20.0.1
```

## 🔧 Configuration

### Environment Variables

**Core Configuration**
```bash
# Database Configuration
POSTGRES_DB=the0_oss
POSTGRES_USER=the0
POSTGRES_PASSWORD=the0_password

# Authentication
JWT_SECRET=your-super-secure-jwt-secret-change-me

# External Services
GEMINI_API_KEY=your-gemini-api-key
STRIPE_PUBLISHABLE_KEY=pk_test_your_stripe_key

# NATS Configuration
NATS_URL=nats://localhost:4222
NATS_USER=nats_user
NATS_PASSWORD=nats_password

# MinIO Configuration
MINIO_ROOT_USER=the0_minio_user
MINIO_ROOT_PASSWORD=the0_minio_password
MINIO_DEFAULT_BUCKETS=custom-bots,bot-results,ai-artifacts

# Redis Configuration
REDIS_PASSWORD=the0_redis_password

# Service Configuration
NODE_ENV=development
LOG_LEVEL=info
MAX_CONCURRENT_BOTS=10
MAX_CONCURRENT_ANALYSES=5
```

### Service-Specific Settings

**API Server Configuration**
```yaml
# In docker-compose.yml for the0-api service
environment:
  DATABASE_URL: postgresql://the0:the0_password@postgres:5432/the0_oss
  JWT_SECRET: ${JWT_SECRET}
  JWT_EXPIRES_IN: 24h
  NATS_URL: nats://nats:4222
  MINIO_ENDPOINT: http://minio:9000
  MINIO_ACCESS_KEY: ${MINIO_ROOT_USER}
  MINIO_SECRET_KEY: ${MINIO_ROOT_PASSWORD}
  CORS_ORIGIN: http://localhost:3001
  RATE_LIMIT_WINDOW: 900000  # 15 minutes
  RATE_LIMIT_MAX: 100
```

**Runtime Service Configuration**
```yaml
# Runtime scaling and resource limits
environment:
  MAX_CONCURRENT_BOTS: ${MAX_CONCURRENT_BOTS:-10}
  BOT_TIMEOUT_SECONDS: 86400  # 24 hours
  MEMORY_LIMIT_MB: 512
  CPU_QUOTA: 1000
  LOG_LEVEL: ${LOG_LEVEL:-info}
  DOCKER_HOST: unix:///var/run/docker.sock
```

**Frontend Configuration**
```yaml
# Development optimizations
environment:
  NODE_ENV: development
  NEXT_PUBLIC_API_URL: http://localhost:3000
  NEXT_PUBLIC_STRIPE_PUBLISHABLE_KEY: ${STRIPE_PUBLISHABLE_KEY}
  NEXT_PUBLIC_ENVIRONMENT: development
  # Development server settings
  CHOKIDAR_USEPOLLING: true
  FAST_REFRESH: true
```

## 🚀 Development Workflow

### Quick Start

```bash
# 1. Clone the repository
git clone https://github.com/the0platform/the0.git
cd the0

# 2. Navigate to docker directory
cd docker

# 3. Copy environment template
cp .env.example .env

# 4. Configure environment variables
# Edit .env with your specific configurations

# 5. Start all services
make up
```

### Make Commands

```bash
# Start all services in development mode
make up

# Start services with fresh builds (no cache)
make up-fresh

# Start development environment with hot reload
make dev

# Build all services without starting
make build

# Start individual services
make up-api
make up-frontend
make up-runtime

# Stop all services
make down

# View logs
make logs
make logs-api
make logs-frontend

# Clean everything (remove containers, images, volumes)
make nuclear

# Run database migrations
make migrate

# Create superuser
make create-superuser

# Access service shells
make shell-api
make shell-postgres
make shell-mongo
```

### Development Optimizations

**Hot Reload Configuration**
```dockerfile
# Development Dockerfile for API
FROM node:20-alpine AS base
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production

# Development stage with source mounting
FROM node:20-alpine AS development
WORKDIR /app
COPY --from=base /app/node_modules /app/node_modules
COPY . .
RUN npm run build

# Final development image
FROM node:20-alpine AS development
WORKDIR /app
COPY --from=development /app/dist /app/dist
COPY --from=development /app/node_modules /app/node_modules
COPY . .
EXPOSE 3000
CMD ["npm", "run", "dev"]
```

**Volume Mounting Strategy**
```yaml
# Source code mounting for hot reload
services:
  the0-api:
    volumes:
      - ../api:/app                    # Source code
      - /app/node_modules              # Separate node_modules
      - api_node_modules:/app/node_modules  # Named volume for persistence

volumes:
  api_node_modules:  # Persistent node_modules volume
```

## 🧪 Testing

### Service Health Tests

```bash
# Test all service health endpoints
make health-check

# Individual service health tests
curl http://localhost:3000/health
curl http://localhost:3001/
curl http://localhost:8080/health
curl http://localhost:8000/health
curl http://localhost:8001/health
```

### Integration Testing

```python
# Automated service testing example
import asyncio
import httpx
from datetime import datetime

async def test_platform_health():
    """Test complete platform health"""

    services = {
        "api": "http://localhost:3000/health",
        "frontend": "http://localhost:3001/",
        "runtime": "http://localhost:8080/health",
        "analyzer": "http://localhost:8000/health",
        "ai": "http://localhost:8001/health"
    }

    async with httpx.AsyncClient() as client:
        tasks = [check_service(client, name, url) for name, url in services.items()]
        results = await asyncio.gather(tasks, return_exceptions=True)

    for service, result in zip(services.keys(), results):
        if isinstance(result, Exception):
            print(f"❌ {service}: FAILED - {result}")
        else:
            status = result.get("status", "unknown")
            print(f"✅ {service}: {status}")

async def check_service(client, name, url):
    try:
        response = await client.get(url, timeout=10.0)
        return {"status": "healthy", "response": response.json()}
    except Exception as e:
        return {"status": "error", "error": str(e)}

# Run tests
if __name__ == "__main__":
    asyncio.run(test_platform_health())
```

### Load Testing

```bash
# API load testing with hey
hey -z 10 -c 5 -m 30s http://localhost:3000/api/bots

# Frontend performance testing
npm run lighthouse -- --output=json --output-path=./lighthouse-results.json http://localhost:3001

# Database performance testing
docker exec the0-postgres pgbench -h localhost -U the0 -d the0_oss -c 10 -j 2 -T 60
```

## 📊 Performance & Scaling

### Resource Requirements

**Minimum Requirements**
- **CPU**: 4 cores (8+ recommended)
- **Memory**: 8GB RAM (16GB+ recommended)
- **Storage**: 50GB free space (SSD recommended)
- **Docker**: 20.10+ with Docker Compose 2.0+

**Service-Specific Resources**

| Service | CPU Min | Memory Min | Disk Min | Notes |
|---------|-----------|------------|-------|
| PostgreSQL | 1 core | 1GB | 10GB | Depends on data size |
| MongoDB | 1 core | 1GB | 20GB | For runtime data |
| NATS | 0.5 core | 512MB | 5GB | Message persistence |
| Redis | 0.5 core | 256MB | 2GB | In-memory data |
| the0-api | 2 cores | 1GB | 5GB | Node.js runtime |
| the0-frontend | 1 core | 512MB | 2GB | Build artifacts |
| the0-runtime | 2 cores | 2GB | 5GB | Go runtime |
| the0-analyzer | 2 cores | 1GB | 5GB | Python + YARA |

### Scaling Strategies

**Horizontal Scaling**
```yaml
# Scale runtime services for high throughput
docker-compose up -d --scale the0-runtime=3 --scale bot-runner-master=2

# Scale with resource limits
docker-compose up -d --scale the0-runtime=3 \
  --scale the0-analyzer=2
```

**Performance Optimizations**
```yaml
# Optimized Docker configurations
services:
  the0-api:
    # Build optimizations
    build:
      context: ../api
      args:
        BUILDKIT_INLINE_CACHE: 1
        BUILDKIT_MULTI_PLATFORM: 1
    # Runtime optimizations
    deploy:
      resources:
        limits:
          memory: 1G
          cpus: '1.0'
        reservations:
          memory: 512M
          cpus: '0.5'
    # Health checks for better container scheduling
    healthcheck:
      test: ["CMD-SHELL", "curl -f http://localhost:3000/health || exit 1"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

## 🔍 Monitoring & Observability

### Health Checks

```yaml
# Comprehensive health check configuration
services:
  the0-api:
    healthcheck:
      test: ["CMD-SHELL", "wget -q --spider -O /dev/null http://localhost:3000/health || exit 1"]
      interval: 30s          # Check every 30 seconds
      timeout: 10s           # Fail after 10 seconds
      retries: 3              # Allow 3 failures before marking unhealthy
      start_period: 60s      # Start checking after 60 seconds
    # Health check response format
    # Expected: HTTP 200 with {"status": "healthy", "checks": {...}}
```

### Logging Configuration

```yaml
# Centralized logging with structured output
services:
  the0-api:
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"
        labels: "service=api,environment=development"
    environment:
      LOG_LEVEL: info
      LOG_FORMAT: json
      LOG_CORRELATION_ID: true
```

### Metrics Collection

```yaml
# Prometheus metrics integration
services:
  prometheus:
    image: prom/prometheus:latest
    container_name: the0-prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./monitoring/prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus_data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
    networks:
      - the0-network

  grafana:
    image: grafana/grafana:latest
    container_name: the0-grafana
    ports:
      - "3002:3000"
    environment:
      GF_SECURITY_ADMIN_PASSWORD: admin
    volumes:
      - grafana_data:/var/lib/grafana
      - ./monitoring/grafana/provisioning:/etc/grafana/provisioning
    networks:
      - the0-network
```

## 🛡️ Security

### Container Security

```yaml
# Security hardening for all services
services:
  the0-api:
    # Non-root user execution
    user: "1001:1001"  # node user
    # Read-only root filesystem
    read_only: true
    # Drop all Linux capabilities
    cap_drop:
      - ALL
    # Security options
    security_opt:
      - no-new-privileges:true
      - apparmor:docker-default
    # Resource limits
    deploy:
      resources:
        limits:
          memory: 1G
          cpus: '1.0'
    # Network isolation
    networks:
      - the0-network

  postgres:
    # PostgreSQL security settings
    environment:
      POSTGRES_INITDB_ARGS: "--auth-host=scram-sha-256"
    volumes:
      - ./postgres/postgresql.conf:/etc/postgresql/postgresql.conf:ro
```

### Network Security

```yaml
# Custom bridge network with IPAM
networks:
  the0-network:
    driver: bridge
    ipam:
      driver: default
      config:
        - subnet: 172.20.0.0/16
          gateway: 172.20.0.1
    # Network policies for service isolation
    internal: false
    attachable: true
```

### Secrets Management

```yaml
# Docker secrets for sensitive data
services:
  the0-api:
    secrets:
      - jwt_secret
      - database_url
      - gemini_api_key
    environment:
      JWT_SECRET_FILE: /run/secrets/jwt_secret
      DATABASE_URL_FILE: /run/secrets/database_url
      GEMINI_API_KEY_FILE: /run/secrets/gemini_api_key

# Create secrets
echo "super-secure-jwt-secret" | docker secret create jwt_secret -
echo "postgresql://user:pass@host:5432/db" | docker secret create database_url -
echo "gemini-api-key-value" | docker secret create gemini_api_key -
```

## 🚀 Deployment

### Production Deployment

```bash
# Production deployment configuration
cd docker

# Create production override
cp docker-compose.yml docker-compose.prod.yml

# Edit production-specific settings
vim docker-compose.prod.yml

# Deploy with production configuration
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d

# Production optimizations
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d \
  --scale the0-runtime=5 \
  --scale the0-analyzer=3
```

### Multi-Environment Support

```bash
# Environment-specific configurations
environments/
├── development/
│   ├── docker-compose.yml
│   └── .env.dev
├── staging/
│   ├── docker-compose.yml
│   └── .env.staging
└── production/
    ├── docker-compose.yml
    └── .env.prod

# Deploy to specific environment
docker-compose -f environments/production/docker-compose.yml --env-file environments/production/.env.prod up -d
```

### CI/CD Integration

```yaml
# GitHub Actions workflow
name: Deploy Docker Services

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Build and Deploy
        run: |
          cd docker
          docker-compose build
          docker-compose up -d

      - name: Health Check
        run: |
          cd docker
          make health-check

      - name: Run Integration Tests
        run: |
          cd docker
          make integration-test
```

## 🔄 Integration Points

### External Service Integration

```yaml
# External API connections
services:
  the0-api:
    environment:
      # Exchange API connections
      BINANCE_API_KEY: ${BINANCE_API_KEY}
      COINBASE_API_KEY: ${COINBASE_API_KEY}
      # Market data providers
      ALPHA_VANTAGE_API_KEY: ${ALPHA_VANTAGE_API_KEY}
      # Payment processing
      STRIPE_SECRET_KEY: ${STRIPE_SECRET_KEY}
```

### Service Discovery

```yaml
# Service discovery with Docker internal DNS
services:
  the0-api:
    # Service accessible via container name
    # Other services can reach via http://the0-api:3000
    networks:
      - the0-network

  the0-frontend:
    environment:
      NEXT_PUBLIC_API_URL: http://the0-api:3000
    networks:
      - the0-network
```

### Database Integration

```yaml
# Multiple database connections
services:
  the0-api:
    environment:
      # Primary PostgreSQL connection
      DATABASE_URL: postgresql://the0:the0_password@postgres:5432/the0_oss
      # MongoDB for runtime data
      MONGODB_URI: mongodb://the0_mongo_user:the0_mongo_password@mongo:27017/bot_runner
      # Redis for caching
      REDIS_URL: redis://:the0_redis_password@redis:6379
    depends_on:
      - postgres
      - mongo
      - redis
```

## 🐛 Troubleshooting

### Common Issues

1. **Port Conflicts**
   - **Symptoms**: Services fail to start, address already in use
   - **Solutions**: Check port usage, modify docker-compose.yml ports
   - **Prevention**: Use consistent port ranges, check for conflicts

2. **Database Connection Failures**
   - **Symptoms**: API service can't connect to databases
   - **Causes**: Service startup order, network issues, authentication
   - **Solutions**: Check service dependencies, verify network, check credentials
   - **Prevention**: Proper health checks, dependency management

3. **Volume Permission Issues**
   - **Symptoms**: File system permission errors, data not persisting
   - **Causes**: Incorrect volume mounts, permission mismatches
   - **Solutions**: Check volume configurations, fix permissions
   - **Prevention**: Use consistent user IDs, proper volume configurations

4. **Memory Issues**
   - **Symptoms**: Containers OOM killed, performance degradation
   - **Causes**: Insufficient host memory, memory leaks, no limits
   - **Solutions**: Monitor memory usage, set resource limits, optimize services
   - **Prevention**: Set appropriate resource limits, monitor usage

### Debugging Commands

```bash
# Comprehensive debugging commands

# Check service status
docker-compose ps
docker-compose logs

# Inspect specific service
docker-compose exec the0-api env
docker-compose exec the0-postgres psql -U the0 -d the0_oss -c "SELECT version();"

# Access service shell
docker-compose exec the0-api sh
docker-compose exec the0-runtime sh

# Network debugging
docker network ls
docker network inspect the0_docker_the0-network

# Volume debugging
docker volume ls
docker volume inspect the0_postgres_data

# Resource monitoring
docker stats
docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}"

# System resource checking
docker system df
docker system events
```

## 📈 Future Roadmap

### Planned Enhancements

- **Kubernetes Integration**: Helm charts for production deployments
- **Docker Swarm**: Multi-node clustering capabilities
- **Service Mesh**: Istio integration for advanced networking
- **Auto-scaling**: Resource-based horizontal scaling
- **Multi-arch Support**: ARM64 and AMD64 architecture support

### Infrastructure Improvements

```yaml
# Future Docker Compose features
version: '3.9'  # Latest version with enhanced features

x-logging: &default-logging
  driver: "json-file"
  options:
    max-size: "10m"
    max-file: "3"

x-healthcheck: &default-healthcheck
  test: ["CMD-SHELL", "curl -f http://localhost:3000/health || exit 1"]
  interval: 30s
  timeout: 10s
  retries: 3

services:
  the0-api:
    <<: *default-logging
    <<: *default-healthcheck
    # Enhanced build and deployment options
    build:
      cache_from:
        - the0-base:latest
      target: production
    deploy:
      replicas: 3
      update_config:
        parallelism: 1
        delay: 10s
        failure_action: rollback
      rollback_config:
        parallelism: 1
        order: start-first
```

## 📚 Additional Resources

### Documentation

- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Documentation](https://docs.docker.com/compose/)
- [PostgreSQL Docker Guide](https://hub.docker.com/_/postgres/)
- [MongoDB Docker Guide](https://hub.docker.com/_/mongo/)
- [NATS Documentation](https://docs.nats.io/)
- [MinIO Documentation](https://docs.min.io/)

### Tools & Utilities

- **Docker Desktop**: Container management and visualization
- **Portainer**: Web-based Docker management interface
- **Lazydocker**: Terminal UI for Docker management
- **Dive**: Docker image layer analysis

### Performance Tools

- **Docker Bench**: Security and performance benchmarking
- **cAdvisor**: Container resource usage analysis
- **Prometheus**: Metrics collection and alerting
- **Grafana**: Visualization and dashboarding

### Related Services

- **Kubernetes Deployment**: Production orchestration guide
- **Platform Services**: Individual service documentation
- **API Integration**: Service communication and APIs

---

*Last updated: October 2024*