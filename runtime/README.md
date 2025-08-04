# Bot Runner

A distributed, fault-tolerant Docker container orchestration system for the theo platform that manages and executes bots using a master-worker architecture with on-demand execution.

## Architecture Overview

Bot Runner has been transformed from a cron-based scheduler to a Docker container orchestrator while maintaining the distributed architecture. The system now executes bots in isolated Docker containers rather than scheduling them via cron expressions.

### Key Components

#### Master Node
- **Leader Election**: Uses Kubernetes leader election pattern to ensure single master operation
- **Segment Management**: 
  - Dynamically assigns database segments to worker nodes using consistent hashing
  - Handles segment rebalancing when workers join/leave
  - Uses configurable virtual nodes per worker for optimal load distribution
- **Health Monitoring**: 
  - Tracks worker health via 5-second heartbeat intervals
  - Automatically reassigns segments from failed workers
- **gRPC Server**: Handles worker registration and communication

#### Worker Nodes
- **Bot Execution**: 
  - Executes bots in isolated Docker containers
  - Supports Python 3.11 and Node.js 20 runtimes
  - Downloads bot code from Google Cloud Storage
- **Segment Processing**: 
  - Scans assigned database segments for bots
  - Supports concurrent processing with configurable delay
- **Health Reporting**: 
  - Sends heartbeats every 2 seconds to master
  - Automatically reconnects on master failover
- **Event Processing**:
  - Handles bot creation/updates/deletions via Pub/Sub
  - Manages bot lifecycle events

### Data Flow

1. **Bot Registration**:
   - Bots are created via Pub/Sub events
   - Workers process creation events and store in MongoDB
   - Bots are assigned to segments using random distribution

2. **Execution Flow**:
   ```
   Worker Node --> Scan Segments --> Find Bots --> Docker Runner --> Execute in Container --> Return Results
   ```

3. **Bot Updates**:
   - Updates handled via Pub/Sub events
   - Optimistic locking prevents concurrent modification issues
   - Automatic bot configuration updates

### Docker Execution

#### Runtime Support
- **Python 3.11**: Uses `python:3.11-slim` Docker image
- **Node.js 20**: Uses `node:20-alpine` Docker image

#### Execution Process
1. **Code Download**: Fetches bot code from GCS (ZIP or TAR.GZ)
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

1. **Leader Election**:
   - Kubernetes-based leader election
   - 15-second lease duration
   - Automatic failover with 10-second renewal deadline

2. **Worker Failure Handling**:
   - Dead worker detection after 5 seconds of missed heartbeats
   - Automatic segment redistribution
   - Graceful worker shutdown and cleanup

3. **Container Isolation**:
   - Each bot runs in isolated Docker container
   - Resource limits and timeouts
   - Automatic cleanup of failed containers

4. **Message Processing**:
   - Dead letter queues for failed message handling
   - Configurable retry policies
   - Message acknowledgment tracking

## Technical Implementation

### Core Technologies
- **Language**: Go 1.24+
- **Database**: MongoDB
- **Messaging**: Google Cloud Pub/Sub
- **Container Runtime**: Docker
- **Container Orchestration**: Kubernetes
- **Service Communication**: gRPC
- **Cloud Storage**: Google Cloud Storage

### Key Packages

1. **docker-runner/**
   - Handles Docker container execution
   - Manages GCS file downloads and extraction
   - Creates runtime-specific entrypoint scripts
   - Supports Python 3.11 and Node.js 20

2. **server/**
   - Contains master/worker node implementation
   - Manages leader election
   - Handles gRPC communication
   - Orchestrates bot processing

3. **subscriber/**
   - Processes Pub/Sub messages for bot lifecycle
   - Manages dead letter queues
   - Handles bot CRUD operations

4. **model/**
   - Defines Bot data structure
   - Bot configuration and metadata
   - Runtime and entrypoint specifications

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

### Configuration Options

```go
type BotDockerRunnerOptions struct {
    GCPProjectID string
    Logger       util.Logger
    TempDir      string
}

type SubscriberOptions struct {
    ProjectID      string
    DBName         string
    CollectionName string
    MaxRetries     int
    MaxSegments    int
    Logger         util.Logger
}
```

## Deployment

### Prerequisites
- Docker and Docker Compose for local development
- Kubernetes cluster for production deployment
- MongoDB instance
- Google Cloud Platform account (for Pub/Sub and Storage)
- Docker daemon accessible to worker nodes

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

1. **RBAC Configuration**: Required for leader election
2. **Docker Access**: Workers need access to Docker daemon
3. **Storage**: Persistent volumes for temporary file storage
4. **Network**: Internal communication for gRPC

### Environment Variables
```bash
# Core Configuration
POD_NAME=<auto-populated in Kubernetes>
MONGO_URI=mongodb://mongo:27017/bot_runner
GCP_PROJECT_ID=your-project-id
GCP_PROJECT_NUMBER=123456789

# Database
DB_NAME=bot_scheduler
COLLECTION_NAME=bots

# Processing
MAX_RETRIES=3
MAX_SEGMENT=64
PROCESSING_DELAY_IN_SECONDS=5

# Docker Runner
TEMP_DIR=/tmp/runtime

# Segment Assignment
VIRTUAL_NODES_PER_WORKER=128
```

### Consistent Hashing Configuration

The system uses consistent hashing to distribute database segments across worker nodes for optimal load balancing. This ensures even distribution of workload even when workers join or leave the cluster.

#### Virtual Nodes Configuration
- **Environment Variable**: `VIRTUAL_NODES_PER_WORKER`
- **Default Value**: 128 virtual nodes per worker
- **Purpose**: Higher virtual node counts provide better load distribution

#### Recommended Settings
- **Small deployments (2-5 workers)**: 128-256 virtual nodes per worker
- **Medium deployments (5-20 workers)**: 64-128 virtual nodes per worker (default)
- **Large deployments (20+ workers)**: 32-64 virtual nodes per worker

#### Benefits
- **Better Load Distribution**: More uniform segment assignment across workers
- **Improved Scalability**: Maintains good distribution as worker count changes
- **Flexible Configuration**: Tunable based on deployment size and requirements

### Security Considerations

1. **Container Isolation**: Bots run in isolated containers
2. **File System Security**: Directory traversal protection during extraction
3. **Resource Limits**: CPU and memory limits on bot containers
4. **Network Isolation**: Containers have limited network access
5. **Secret Management**: Sensitive configuration via environment variables

## Monitoring and Observability

- **Health Endpoints**: Health check endpoints for Kubernetes
- **Execution Metrics**: Bot execution time, success/failure rates
- **Resource Usage**: Container resource consumption tracking
- **Structured Logging**: JSON-formatted logs with correlation IDs

## Scalability Considerations

1. **Horizontal Scaling**:
   - Add worker nodes to handle more bots
   - Automatic segment rebalancing
   - Independent container execution

2. **Performance Tuning**:
   - Adjustable processing delays
   - Concurrent bot execution
   - Docker image caching

3. **Resource Management**:
   - Container resource limits
   - Temporary file cleanup
   - Memory usage optimization

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