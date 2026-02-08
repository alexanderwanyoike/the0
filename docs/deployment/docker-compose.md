---
title: "Docker Compose"
description: "Deploy the0 platform with Docker Compose"
tags: ["deployment", "docker", "self-hosted"]
order: 1
---

# Docker Compose Deployment

Docker Compose provides the fastest path to a running the0 platform. The `the0` CLI manages the full stack with a single command, making it ideal for local development, evaluation, and small self-hosted deployments.

## Prerequisites

Before starting, ensure your system meets these requirements:

- **Docker** 20.10 or later with the **Compose** plugin (included with Docker Desktop)
- **the0 CLI** ([install guide](/the0-cli/installation))
- At least **4GB RAM** available for containers
- At least **20GB disk space** for images and volumes

Verify your Docker installation:

```bash
docker --version
docker compose version
```

## Quick Start

Initialize and start all services:

```bash
# Initialize (first time only — point to your repo clone)
the0 local init --repo-path /path/to/the0

# Start all services
the0 local start
```

Initial startup takes several minutes as images are built. Subsequent starts are much faster.

Once running, access the platform at:

| Service | URL | Description |
|---------|-----|-------------|
| Frontend | http://localhost:3001 | Web dashboard |
| API | http://localhost:3000 | REST API |
| Documentation | http://localhost:3002 | Platform docs |
| MinIO Console | http://localhost:9001 | Object storage admin |

MinIO credentials: `the0admin` / `the0password`

## Service Architecture

The Docker Compose configuration starts these services:

### Infrastructure Services

**PostgreSQL** (port 5432) stores user accounts, bot definitions, and configuration. The database initializes automatically with required schemas.

**MongoDB** (port 27017) stores runtime state, job queues, and execution logs. The bot runner and scheduler services use MongoDB for coordination.

**NATS** (port 4222) provides event streaming between services. JetStream is enabled for durable message delivery.

**MinIO** (ports 9000, 9001) provides S3-compatible object storage for bot code packages, execution logs, and artifacts.

### Application Services

**the0-api** (port 3000) handles REST API requests, user authentication, and bot management. It publishes events to NATS for the runtime services.

**the0-frontend** (port 3001) serves the web dashboard. It communicates with the API for all operations.

**the0-docs** (port 3002) serves the platform documentation.

**bot-runner** executes realtime bots. It uses a reconciliation loop to compare desired state (from MongoDB) with running containers and makes corrections. Bots run in isolated Docker containers with resource limits.

**bot-scheduler** manages scheduled bot execution. It monitors cron schedules from MongoDB and triggers bot executions when due. Requires NATS for receiving schedule events from the API.

## Management Commands

The CLI provides commands for common operations:

```bash
# Start all services
the0 local start

# Stop all services
the0 local stop

# Restart with fresh builds
the0 local restart

# View logs (all services)
the0 local logs

# View logs for specific service
the0 local logs api

# Follow logs in real time
the0 local logs -f api

# Check service health
the0 local status
```

For development with hot reload:

```bash
# Start with hot reload enabled for frontend and API
the0 local dev
```

## Configuration

The compose configuration is managed by the CLI at `~/.the0/compose/`. Key environment variables:

### Database Configuration

```yaml
environment:
  POSTGRES_DB: the0_oss
  POSTGRES_USER: the0
  POSTGRES_PASSWORD: the0_password  # Change in production
```

### API Configuration

```yaml
environment:
  JWT_SECRET: your-super-secret-jwt-key-change-this-in-production
  JWT_EXPIRES_IN: 24h
```

### Storage Configuration

```yaml
environment:
  MINIO_ROOT_USER: the0admin
  MINIO_ROOT_PASSWORD: the0password  # Change in production
```

### Bot Execution Limits

Worker services accept resource limit configuration:

```yaml
environment:
  BOT_MEMORY_LIMIT_MB: "512"
  BOT_CPU_SHARES: "512"
```

## Data Persistence

Data persists in Docker volumes:

- `postgres_data` - PostgreSQL database files
- `mongo_data` - MongoDB database files
- `nats_data` - NATS JetStream state
- `minio_data` - Object storage files
- `bot_runner_data` / `bot_scheduler_data` - Runtime service state

To reset all data:

```bash
the0 local stop
docker volume rm $(docker volume ls -q | grep the0)
the0 local start
```

To remove everything including volumes:

```bash
the0 local uninstall
```

## Backup and Restore

### Database Backup

```bash
# PostgreSQL backup
docker compose -f ~/.the0/compose/docker-compose.yml exec postgres pg_dump -U the0 the0_oss > backup.sql

# PostgreSQL restore
docker compose -f ~/.the0/compose/docker-compose.yml exec -T postgres psql -U the0 the0_oss < backup.sql
```

### Object Storage Backup

MinIO data can be backed up using the `mc` CLI or by copying the volume directly:

```bash
# Using docker cp
docker cp $(docker compose -f ~/.the0/compose/docker-compose.yml ps -q minio):/data ./minio-backup
```

## Service Configuration

### Reconciliation Intervals

The bot-runner and bot-scheduler services support configurable intervals:

```yaml
environment:
  # How often bot-runner reconciles bot state (default: 30s)
  RECONCILE_INTERVAL: "30s"
  # How often bot-scheduler checks for due schedules (default: 10s)
  CHECK_INTERVAL: "10s"
```

### NATS Configuration

NATS is optional for bot-runner (falls back to polling) but required for bot-scheduler:

```yaml
environment:
  NATS_URL: nats://nats:4222
```

## Troubleshooting

### Services Won't Start

Check container logs for errors:

```bash
the0 local logs api
the0 local logs bot-runner
the0 local logs bot-scheduler
```

### Memory Issues

Monitor container memory usage:

```bash
docker stats
```

Increase Docker's memory allocation in Docker Desktop settings if containers are being killed.

### Database Connection Failures

Verify database health:

```bash
the0 local status
```

### Clean Rebuild

If issues persist, perform a clean rebuild:

```bash
the0 local stop
docker system prune -a
docker volume prune
the0 local start
```

## Production Considerations

Docker Compose works for small production deployments with these adjustments:

1. **Change all default passwords** - Update PostgreSQL, MongoDB, MinIO, and JWT secrets
2. **Enable TLS** - Add a reverse proxy (nginx, traefik) for HTTPS termination
3. **Configure backups** - Schedule regular database and storage backups
4. **Set resource limits** - Add memory and CPU limits to all services
5. **Enable health monitoring** - Configure external monitoring for service health
6. **Restrict network access** - Infrastructure services should not be publicly accessible

For multi-node deployments or high availability requirements, consider [Kubernetes](./kubernetes).
