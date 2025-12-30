---
title: "Docker Compose"
description: "Deploy the0 platform with Docker Compose"
tags: ["deployment", "docker", "self-hosted"]
order: 1
---

# Docker Compose Deployment

Docker Compose provides the fastest path to a running the0 platform. A single command starts all services with development-ready defaults, making it ideal for local development, evaluation, and small self-hosted deployments.

## Prerequisites

Before starting, ensure your system meets these requirements:

- **Docker** 20.10 or later
- **Docker Compose** 2.0 or later (included with Docker Desktop)
- At least **4GB RAM** available for containers
- At least **20GB disk space** for images and volumes

Verify your Docker installation:

```bash
docker --version
docker compose version
```

## Quick Start

Navigate to the docker directory and start all services:

```bash
cd docker
make up
```

The command builds all container images and starts the services. Initial startup takes several minutes as images are built. Subsequent starts are much faster.

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

**bot-runner-master** (port 8080) coordinates realtime bot execution. It receives work requests and distributes them to workers.

**bot-runner-worker** (4 replicas) executes realtime bot code. Workers run bots in isolated Docker containers.

**bot-scheduler-master** (port 8082) manages scheduled bot execution. It monitors cron schedules and triggers executions.

**bot-scheduler-worker** (4 replicas) executes scheduled bot code. Like the runner workers, these run bots in isolated containers.

## Management Commands

The Makefile provides commands for common operations:

```bash
# Start all services
make up

# Stop all services
make down

# Restart with fresh builds
make restart

# View logs (all services)
make logs

# View logs for specific service
make logs service=the0-api
```

For development with hot reload:

```bash
# Start with hot reload enabled
make dev-up

# Stop development environment
make dev-down
```

## Configuration

The `docker-compose.yml` file contains all configuration. Key environment variables you may want to modify:

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
make down
docker volume rm $(docker volume ls -q | grep the0)
make up
```

## Backup and Restore

### Database Backup

```bash
# PostgreSQL backup
docker compose exec postgres pg_dump -U the0 the0_oss > backup.sql

# PostgreSQL restore
docker compose exec -T postgres psql -U the0 the0_oss < backup.sql
```

### Object Storage Backup

MinIO data can be backed up using the `mc` CLI or by copying the volume directly:

```bash
# Using docker cp
docker cp $(docker compose ps -q minio):/data ./minio-backup
```

## Scaling Workers

Increase worker replicas for higher throughput:

```bash
docker compose up -d --scale bot-runner-worker=8 --scale bot-scheduler-worker=8
```

Each worker can execute multiple bots concurrently. Scale based on your expected bot count and execution frequency.

## Troubleshooting

### Services Won't Start

Check container logs for errors:

```bash
docker compose logs the0-api
docker compose logs bot-runner-master
```

### Port Conflicts

If ports are already in use, modify the port mappings in `docker-compose.yml`:

```yaml
ports:
  - "3100:3000"  # Map to different host port
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
docker compose exec postgres pg_isready -U the0
docker compose exec mongo mongosh --eval "db.runCommand('ping')"
```

### Clean Rebuild

If issues persist, perform a clean rebuild:

```bash
make down
docker system prune -a
docker volume prune
make up
```

## Production Considerations

Docker Compose works for small production deployments with these adjustments:

1. **Change all default passwords** - Update PostgreSQL, MongoDB, MinIO, and JWT secrets
2. **Enable TLS** - Add a reverse proxy (nginx, traefik) for HTTPS termination
3. **Configure backups** - Schedule regular database and storage backups
4. **Set resource limits** - Add memory and CPU limits to all services
5. **Enable health monitoring** - Configure external monitoring for service health
6. **Restrict network access** - Infrastructure services should not be publicly accessible

For larger deployments, consider [Kubernetes](./kubernetes) for better orchestration and scaling capabilities.
