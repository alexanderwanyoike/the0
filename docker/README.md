# The0 OSS Docker Setup

This directory contains the Docker configuration for running The0 OSS platform with all its components.

## Architecture

The platform consists of the following services:

### Core Services
- **the0-api** (Port 3000): NestJS API server handling REST endpoints and business logic
- **the0-frontend** (Port 3001): Next.js frontend application serving the web interface
- **the0-runtime** (Port 8080): Go-based runtime engine for executing trading bots

### Infrastructure Services
- **postgres** (Port 5432): PostgreSQL database for persistent data storage
- **nats** (Ports 4222, 8222): NATS message broker with JetStream for inter-service communication
- **redis** (Port 6379): Redis cache for session management and temporary data
- **minio** (Ports 9000, 9001): MinIO S3-compatible object storage for file uploads

### Development Services (Optional)
- **adminer** (Port 8081): Database administration interface
- **nats-monitoring**: NATS connection monitoring

## Quick Start

### Prerequisites
- Docker 20.10+
- Docker Compose 2.0+
- At least 4GB RAM available for containers

### 1. Build Runtime Images

Before starting services, you need to build the runtime images used for bot execution:

```bash
cd docker
make build-images
```

This builds `the0/python311`, `the0/nodejs20`, `the0/rust-stable`, etc. These images contain the daemon binary for state/log synchronization.

**Note:** `make up` automatically builds these images, but you can build them separately if needed.

### 2. Start All Services

```bash
# Navigate to docker directory
cd docker

# Start all services in production mode
make up
```

### 3. Access the Platform

- **Frontend**: http://localhost:3001
- **API**: http://localhost:3000
- **Runtime**: http://localhost:8080
- **MinIO Console**: http://localhost:9001 (admin/password: the0admin/the0password)
- **Database Admin** (dev): http://localhost:8081

### 4. Initial Setup

The system will automatically:
- Create database schemas
- Initialize NATS JetStream
- Set up MinIO buckets
- Run database migrations

## Configuration

### Environment Variables

Key configuration options can be modified in the `docker-compose.yml` file:

#### Database Configuration
- `POSTGRES_DB`: Database name (default: the0_oss)
- `POSTGRES_USER`: Database user (default: the0)
- `POSTGRES_PASSWORD`: Database password (change in production!)

#### Storage Configuration
- `MINIO_ROOT_USER`: MinIO admin username
- `MINIO_ROOT_PASSWORD`: MinIO admin password
- `S3_BUCKET`: Default storage bucket name

#### Security Configuration
- `JWT_SECRET`: JWT signing secret (MUST change in production!)
- `JWT_EXPIRES_IN`: Token expiration time

### Custom Configuration

For production deployments, create environment-specific compose files:

```bash
# Create production override
cp docker-compose.yml docker-compose.prod.yml

# Run with override
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d
```

## Development

### Quick Development Setup

```bash
# Navigate to docker directory
cd docker

# Show all available commands
make help

# Start development environment with hot reload (always builds from fresh code)
make dev-fresh

# Start production environment with fresh code
make up-fresh

# View logs
make logs
make dev-logs    # Development logs only

# Stop environment
make down

# Nuclear option - completely fresh start (removes everything)
make nuclear
```

### Important: Fresh Code Builds

**Problem**: Docker Compose can use cached images instead of building from current code changes.

**Solution**: All code-based services include build args to prevent caching, and Makefile targets use `--build` flags.

Services that always build from fresh code:
- `the0-frontend` (Next.js with hot reload)
- `the0-api` (NestJS with hot reload)
- `the0-analyzer` (Python security scanner)
- `bot-runner-master/worker` (Go runtime)
- `backtest-runner-master/worker` (Go runtime)
- `bot-scheduler-master/worker` (Go runtime)

### Building Individual Services

```bash
# Build all services from fresh code
make build-fresh

# Build specific service with no cache
docker compose build --no-cache the0-api
docker compose build --no-cache the0-frontend  
docker compose build --no-cache bot-runner-master

# Quick commands
make up-fresh        # Start everything with fresh builds
make dev-fresh       # Start dev environment with hot reload
make nuclear         # Nuclear option - removes everything and rebuilds
```

### Viewing Logs

```bash
# View logs for all services
docker-compose logs -f

# View logs for specific service
docker-compose logs -f the0-api
docker-compose logs -f the0-runtime
docker-compose logs -f the0-frontend
```

### Development Mode

For development with hot reloading:

```bash
# Stop production containers
docker-compose down

# Start only infrastructure services
docker-compose up -d postgres nats redis minio

# Run services locally with:
# - API: cd api && yarn dev
# - Frontend: cd frontend && yarn dev  
# - Runtime: cd runtime && go run cmd/app/main.go
```

## Data Persistence

Data is persisted in Docker volumes:

- `postgres_data`: Database files
- `nats_data`: Message broker data
- `redis_data`: Cache data
- `minio_data`: Object storage files
- `runtime_data`: Runtime execution data

### Backup and Restore

```bash
# Backup database
docker-compose exec postgres pg_dump -U the0 the0_oss > backup.sql

# Restore database
docker-compose exec -T postgres psql -U the0 the0_oss < backup.sql

# Backup MinIO data
docker-compose exec minio mc mirror /data /backup/minio-data
```

## Troubleshooting

### Common Issues

1. **Port Conflicts**
   ```bash
   # Check what's using ports
   netstat -tulpn | grep :3000
   netstat -tulpn | grep :3001
   
   # Modify ports in docker-compose.yml if needed
   ```

2. **Memory Issues**
   ```bash
   # Check Docker memory usage
   docker stats
   
   # Increase Docker memory limit in Docker Desktop
   ```

3. **Database Connection Issues**
   ```bash
   # Check database health
   docker-compose exec postgres pg_isready -U the0
   
   # Reset database
   docker-compose down -v postgres
   docker-compose up -d postgres
   ```

4. **Container Build Failures**
   ```bash
   # Clean build cache
   docker system prune -a
   
   # Rebuild without cache
   docker-compose build --no-cache
   ```

### Service Health Checks

All services include health checks. Check status:

```bash
# View service health
docker-compose ps

# Check specific service logs
docker-compose logs the0-api
```

### Debugging

```bash
# Enter running container
docker-compose exec the0-api sh
docker-compose exec the0-runtime sh
docker-compose exec postgres psql -U the0 the0_oss

# Run one-off commands
docker-compose run --rm the0-api yarn db:migrate
docker-compose run --rm the0-runtime ./main -migrate
```

## Production Deployment

### Security Checklist

- [ ] Change all default passwords
- [ ] Set strong JWT_SECRET
- [ ] Configure firewall rules
- [ ] Enable TLS/SSL termination
- [ ] Set up monitoring and logging
- [ ] Configure backup strategies
- [ ] Update resource limits

### Scaling

```bash
# Scale specific services
docker-compose up -d --scale the0-runtime=3

# Use Docker Swarm for multi-node deployment
docker swarm init
docker stack deploy -c docker-compose.yml the0-stack
```

### Monitoring

Consider adding monitoring services:

```yaml
# Add to docker-compose.yml
prometheus:
  image: prom/prometheus
  ports:
    - "9090:9090"
    
grafana:
  image: grafana/grafana
  ports:
    - "3002:3000"
```

## Updates

```bash
# Pull latest images
docker-compose pull

# Update and restart
docker-compose up -d --force-recreate

# Update specific service
docker-compose up -d --force-recreate the0-api
```

## Support

For issues and questions:
- Check logs: `docker-compose logs -f [service-name]`
- Verify health: `docker-compose ps`
- Reset everything: `docker-compose down -v && docker-compose up -d`