# the0 Runtime

A unified, distributed Docker container orchestration system for the0 platform that manages bot execution, backtests, and scheduled jobs using a master-worker architecture.

## Architecture Overview

A unified runtime system for the0 platform that provides three distinct services sharing common infrastructure:
- **Bot Runner**: Executes live trading bots in isolated Docker containers
- **Backtest Runner**: Processes backtests for strategy validation
- **Bot Scheduler**: Manages scheduled bot execution with cron-based timing

All services use a master-worker architecture with Docker-based container isolation and MongoDB for persistence.

### Key Components

#### Master Node
- **gRPC Server**: Handles worker registration and streaming rebalance notifications
- **Segment Management**:
  - Simple 1:1 assignment of database segments to worker nodes
  - Handles segment rebalancing when workers join/leave
  - Discovers active segments from MongoDB partitions collection
- **Health Monitoring**:
  - Tracks worker health via 5-second heartbeat timeout
  - Automatically reassigns segments from failed workers
- **HTTP API**: Provides health check endpoint (/healthz) for Kubernetes probes
- **Autoscaling**: Native Kubernetes autoscaler that scales workers to match segment count (1 worker per segment)

#### Worker Nodes
- **Bot Execution**:
  - Executes bots/backtests in isolated Docker containers
  - Supports Python 3.11 and Node.js 20 runtimes
  - Downloads code from MinIO/S3 storage
- **Segment Processing**:
  - Scans assigned database segment for executables
  - Each worker handles exactly one segment at a time
- **Health Reporting**:
  - Sends heartbeats every 2 seconds to master
  - Resubscribes if master requests reconnection
- **Event Processing**:
  - Handles bot creation/updates/deletions via NATS
  - Manages bot lifecycle events

### Data Flow

1. **Bot Registration**:
   - Bots are created via NATS events
   - Workers process creation events and store in MongoDB
   - Bots are assigned to segments using random distribution

2. **Execution Flow**:
   ```
   Worker Node --> Scan Segments --> Find Bots --> Docker Runner --> Execute in Container --> Return Results
   ```

3. **Bot Updates**:
   - Updates handled via NATS events
   - Optimistic locking prevents concurrent modification issues
   - Automatic bot configuration updates

### Docker Execution

#### Runtime Support
- **Python 3.11**: Uses `python:3.11-slim` Docker image
- **Node.js 20**: Uses `node:20-alpine` Docker image

#### Execution Process
1. **Code Download**: Fetches bot code from MinIO/S3 (ZIP or TAR.GZ)
2. **Extraction**: Securely extracts code with directory traversal protection
3. **Script Generation**: Creates runtime-specific entrypoint scripts
4. **Container Execution**: Runs bot with mounted code and environment variables
5. **Result Collection**: Captures output, status, and execution metrics

#### Bot Configuration
```json
{
  "id": "bot-123",
  "segment_id": 1,
  "config": {
    "apiKey": "secret",
    "threshold": 0.95
  },
  "custom": {
    "version": "1.0.0",
    "gcsPath": "gs://bucket/bot.zip",
    "config": {
      "name": "Trading Bot",
      "runtime": "python3.11",
      "entrypoints": {
        "bot": "main.py",
        "backtest": "backtest.py"
      }
    }
  }
}
```

### Fault Tolerance Mechanisms

1. **Worker Failure Handling**:
   - Dead worker detection after 5 seconds of missed heartbeats
   - Automatic segment reassignment to healthy workers
   - Graceful worker shutdown with container cleanup
   - Workers automatically resubscribe on master reconnection

2. **Container Isolation**:
   - Each bot/backtest runs in isolated Docker container
   - Resource limits (CPU shares and memory)
   - Automatic cleanup of failed containers
   - File system permissions handling for cleanup

3. **Message Processing**:
   - Dead letter queues for failed message handling
   - Configurable retry policies with exponential backoff
   - Message acknowledgment tracking
   - Graceful degradation when NATS is unavailable

4. **Autoscaling Resilience**:
   - 15-second check interval with 10-second cooldown
   - Gradual scaling (+/-1 replica at a time)
   - Min/max replica bounds enforcement
   - Continues operation if autoscaler unavailable

## Technical Implementation

### Core Technologies
- **Language**: Go 1.24+
- **Database**: MongoDB (with partitions collection for segment tracking)
- **Messaging**: NATS (optional, gracefully degrades)
- **Container Runtime**: Docker
- **Container Orchestration**: Kubernetes
- **Service Communication**: gRPC with streaming
- **Cloud Storage**: MinIO/S3 compatible storage

### Key Packages

1. **core/**
   - Generic master/worker infrastructure shared by all services
   - Master: gRPC server, segment assignment, health monitoring
   - Worker: Subscription-based segment updates, heartbeat mechanism
   - Server: Rebalance logic and worker coordination

2. **docker-runner/**
   - Container lifecycle orchestration (create, start, stop, cleanup)
   - MinIO code download and extraction with security checks
   - Runtime-specific entrypoint script generation
   - Background log collection for long-running containers
   - Supports Python 3.11 and Node.js 20

3. **bot-runner/**, **backtest-runner/**, **bot-scheduler/**
   - Service-specific master/worker implementations
   - NATS subscribers for lifecycle events
   - Database-specific models and logic
   - Each service uses dedicated MongoDB database

4. **autoscaler/**
   - Native Kubernetes autoscaler (no HPA required)
   - Scales workers to match segment count
   - Gradual scaling with cooldown periods
   - Works without additional Kubernetes permissions if unavailable

### Bot Entrypoint Interface

#### Python Bots
```python
def main(id: str, config: dict):
    print(f"Running bot {id} with config {config}")
    # Bot logic here
    return {
        "status": "success",
        "message": f"Bot {id} executed successfully"
    }
```

#### Node.js Bots
```javascript
function main(id, config) {
    console.log(`Running bot ${id} with config`, config);
    // Bot logic here
    return {
        status: "success",
        message: `Bot ${id} executed successfully`
    };
}

module.exports = { main };
```

### CLI Commands

The runtime uses Cobra for a unified command-line interface:

```bash
# Bot Runner (live bot execution)
./runtime bot-runner master
./runtime bot-runner worker

# Backtest Runner (backtest processing)
./runtime backtest-runner master
./runtime backtest-runner worker
./runtime backtest-runner standalone --node=master|worker

# Bot Scheduler (cron-based scheduled execution)
./runtime bot-scheduler master
./runtime bot-scheduler worker
```

Common flags:
- `--max-segment`: Maximum segment ID (default: 16)
- `--mode`: Run mode - cluster or standalone (default: cluster)
- `--mongo-uri`: MongoDB connection string
- `--worker-id`: Unique worker identifier (auto-generated if not set)

## Deployment

### Prerequisites
- Docker and Docker Compose for local development
- Kubernetes cluster for production deployment
- MongoDB instance (with partitions collection for segment tracking)
- MinIO or S3-compatible storage (for bot code and logs)
- Optional: NATS (for bot lifecycle events, system degrades gracefully without it)
- Docker daemon accessible to worker nodes (via socket mount)

### Local Development

1. Build the service:
   ```bash
   make go-build
   ```

2. Run tests:
   ```bash
   make test
   ```

3. Start the service with Docker Compose:
   ```bash
   make up
   ```

4. Stop the service:
   ```bash
   make down
   ```

### Container Setup
- Multi-stage Docker builds
- Base image: `golang:1.24-alpine`
- Final image: `alpine:latest`
- Docker-in-Docker capability for bot execution

### Kubernetes Deployment

1. **RBAC Configuration**: Optional for autoscaler (falls back gracefully if unavailable)
2. **Docker Access**: Workers need access to Docker daemon via socket mount
3. **Storage**: Persistent volumes for temporary file storage
4. **Network**: Internal gRPC communication on service ports (50051-50053)
5. **Separate Deployments**: One deployment per service (bot-runner, backtest-runner, bot-scheduler)
6. **Master vs Worker**: Each service has separate master and worker deployments

### Environment Variables
```bash
# Core Configuration
MONGO_URI=mongodb://mongo:27017
WORKER_ID=<auto-generated from hostname-pid if not set>
MASTER_SERVICE=<service-name>-master  # e.g., bot-runner-master
MASTER_HOST=localhost  # For standalone mode

# Database (service-specific)
# Bot Runner uses: bot_runner/bots
# Backtest Runner uses: backtest_runner/backtests
# Bot Scheduler uses: bot_scheduler/bot_schedules

# NATS Messaging (optional)
NATS_URL=nats://nats:4222

# MinIO/S3 Storage
MINIO_ENDPOINT=minio:9000
MINIO_ACCESS_KEY_ID=minioadmin
MINIO_SECRET_ACCESS_KEY=minioadmin
MINIO_USE_SSL=false
MINIO_BOTS_BUCKET=bots
MINIO_LOGS_BUCKET=logs
MINIO_RESULTS_BUCKET=results

# Docker Runner
TEMP_DIR=/tmp/runtime
MEMORY_LIMIT_MB=512
CPU_SHARES=1024

# Autoscaler (optional)
NAMESPACE=default
DEPLOYMENT_NAME=runtime-worker
MIN_REPLICAS=3
MAX_REPLICAS=4

# Backtest Runner (worker-specific)
BATCH_SIZE=5
BATCH_TIMEOUT=300
BATCH_INTERVAL=10
```

### Segment Assignment Strategy

The system uses a simple 1:1 segment-to-worker assignment strategy:
- Each worker is assigned exactly one database segment at a time
- Segments are discovered from MongoDB `partitions` collection
- Only partitions with `bot_count > 0` are assigned
- When workers join/leave, unassigned segments are redistributed
- Rebalancing occurs automatically on worker connection/disconnection
- Additional rebalancing happens every 10 seconds via timer

This simple approach provides:
- Predictable segment ownership
- Fast rebalancing on worker changes
- Easy debugging and monitoring
- Native Kubernetes autoscaler matches worker count to segment count

### Security Considerations

1. **Container Isolation**: Bots run in isolated containers
2. **File System Security**: Directory traversal protection during extraction
3. **Resource Limits**: CPU and memory limits on bot containers
4. **Network Isolation**: Containers have limited network access
5. **Secret Management**: Sensitive configuration via environment variables

## Monitoring and Observability

- **Health Endpoints**: `/healthz` endpoint on port 8080 for Kubernetes liveness/readiness probes
- **Execution Metrics**: Container execution time, success/failure rates via logs
- **Resource Usage**: Container resource consumption via Docker stats
- **Log Collection**: Background log collector uploads container logs to MinIO every 30 seconds
- **Structured Logging**: Prefixed logs (MASTER/WORKER) with timestamps and context

## Scalability Considerations

1. **Horizontal Scaling**:
   - Native autoscaler automatically adds/removes workers to match segment count
   - Workers scale independently per service (bot-runner, backtest-runner, bot-scheduler)
   - 1 worker per active segment for optimal distribution
   - Gradual scaling (+/-1 replica) prevents thundering herd
   - Masters are stateless and can be scaled (though typically run as single instance)

2. **Performance Tuning**:
   - Docker image caching reduces container startup time
   - Rebalance timer interval (default: 10s) can be adjusted
   - Autoscaler check interval (15s) and cooldown (10s) are configurable
   - Batch processing for backtests (configurable batch size and timeout)

3. **Resource Management**:
   - Container resource limits (CPU shares, memory limits) prevent resource exhaustion
   - Automatic cleanup of bot directories on container stop
   - Permission fixing for root-owned files created by containers
   - Background log collection prevents disk space issues

## Development and Testing

### Running Tests

#### Using Make Targets

```bash
# Run all tests (unit + integration)
make test

# Run only unit tests
make test-unit

# Run only integration tests
make test-integration

# Run all tests with explicit labeling
make test-all
```

#### Manual Test Commands

##### Unit Tests
```bash
# Run all unit tests
go test ./...

# Run specific package tests
go test ./internal/docker-runner/ -v
go test ./internal/subscriber/ -v

# Run with coverage
go test -cover ./...
```

##### Integration Tests
Integration tests require Docker to be running and will pull Docker images during execution.

```bash
# Run integration tests (requires Docker)
go test -tags=integration ./internal/docker-runner/ -v

# Run integration tests with timeout
go test -tags=integration -timeout=10m ./internal/docker-runner/ -v

# Skip integration tests in short mode
go test -short ./...
```

**Prerequisites for Integration Tests:**
- Docker daemon must be running
- Internet connection (to pull Docker images)
- Sufficient disk space for Docker images (~200MB for python:3.11-slim and node:20-alpine)

### Test Coverage
- **Docker Runner**: Runtime validation, script generation, file extraction
- **Subscriber**: Bot lifecycle events, database operations
- **Server**: Worker management, segment distribution
- **Security**: Directory traversal protection, input validation