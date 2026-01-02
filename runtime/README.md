# the0 Runtime

A unified Docker container orchestration system for the0 platform that manages bot execution and scheduled jobs.

## Overview

The runtime provides two deployment modes:

- **Docker Mode**: Single-process services for local development and small deployments
  - `bot-runner`: Executes live trading bots
  - `bot-scheduler`: Manages cron-based scheduled bot execution
- **Kubernetes Mode**: Controller-based deployment for production scale
  - `controller`: K8s-native Pod/CronJob management

## Architecture

### Docker Mode (Recommended for Development)

Docker mode uses a simplified single-process architecture with a reconciliation loop:

```
┌─────────────────────────────────────────────────────────┐
│                      BotService                          │
│                                                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │    NATS     │  │   Service   │  │  DockerRunner   │  │
│  │ (optional)  │──│    State    │──│                 │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
│                                                          │
│  Events:              State:              Actions:       │
│  - bot.created        map[botID]*Bot      - Start        │
│  - bot.updated        map[botID]contID    - Stop         │
│  - bot.deleted                            - Restart      │
└─────────────────────────────────────────────────────────┘
```

**How it works:**
1. BotService queries MongoDB for enabled bots (desired state)
2. Lists running containers from Docker (actual state)
3. Reconciles: starts missing bots, stops extra containers
4. Detects config changes and restarts as needed
5. Repeats every 30 seconds (configurable)

### Kubernetes Mode (Production Scale)

For deployments exceeding ~1000 bots, use Kubernetes mode with the controller:

```
┌─────────────────────────────────────────────────────────┐
│                    Controller Manager                    │
│  ┌─────────────────┐      ┌─────────────────┐          │
│  │  Bot Controller │      │Schedule Controller│         │
│  └────────┬────────┘      └────────┬────────┘          │
│           │                        │                    │
│           ▼                        ▼                    │
│      ┌─────────┐              ┌─────────┐              │
│      │  Pods   │              │CronJobs │              │
│      └─────────┘              └─────────┘              │
└─────────────────────────────────────────────────────────┘
```

## Quick Start

### Using Docker Compose

```bash
# Start the platform (includes runtime services)
cd docker
make up

# View logs
docker compose logs -f bot-runner
docker compose logs -f bot-scheduler
```

### Running Locally

```bash
# Build
go build -o build/runtime ./cmd/app

# Run bot-runner (live bots)
./build/runtime bot-runner --mongo-uri mongodb://localhost:27017

# Run bot-scheduler (scheduled bots, requires NATS)
./build/runtime bot-scheduler \
  --mongo-uri mongodb://localhost:27017 \
  --nats-url nats://localhost:4222

# Run controller (Kubernetes mode)
./build/runtime controller --namespace the0
```

## CLI Commands

### Bot Runner

Manages live trading bot execution.

```bash
./runtime bot-runner [flags]
```

| Flag | Default | Description |
|------|---------|-------------|
| `--mongo-uri` | `mongodb://localhost:27017` | MongoDB connection URI |
| `--nats-url` | (empty) | NATS URL (optional, enables instant updates) |
| `--reconcile-interval` | `30s` | How often to reconcile bot state |

**NATS is optional**: Without NATS, the service polls MongoDB every reconcile interval. With NATS, bot changes are applied immediately.

### Bot Scheduler

Manages cron-based scheduled bot execution.

```bash
./runtime bot-scheduler [flags]
```

| Flag | Default | Description |
|------|---------|-------------|
| `--mongo-uri` | `mongodb://localhost:27017` | MongoDB connection URI |
| `--nats-url` | (required) | NATS URL for API communication |
| `--check-interval` | `10s` | How often to check for due schedules |

**NATS is required** for bot-scheduler to receive schedule events from the API.

### Controller (Kubernetes Mode)

Manages bots as Kubernetes Pods and CronJobs.

```bash
./runtime controller [flags]
```

| Flag | Default | Description |
|------|---------|-------------|
| `--namespace` | `the0` | Kubernetes namespace for bot pods |
| `--reconcile-interval` | `30s` | How often to reconcile state |
| `--minio-endpoint` | `minio:9000` | MinIO endpoint for bot code |
| `--minio-bucket` | `the0-custom-bots` | MinIO bucket for bot code |

### Version

```bash
./runtime version
```

## Environment Variables

### Required for All Modes

```bash
MONGO_URI=mongodb://localhost:27017    # MongoDB connection
```

### Optional for bot-runner

```bash
NATS_URL=nats://localhost:4222         # NATS for real-time events (optional)
```

### Required for bot-scheduler

```bash
NATS_URL=nats://localhost:4222         # NATS for API communication (required)
```

### Required for Docker Mode (Bot Execution)

```bash
MINIO_ENDPOINT=localhost:9000          # MinIO endpoint
MINIO_ACCESS_KEY=the0admin             # MinIO access key
MINIO_SECRET_KEY=the0password          # MinIO secret key
```

### Required for Controller Mode

```bash
NAMESPACE=the0                         # K8s namespace
MINIO_ENDPOINT=minio:9000              # MinIO endpoint (in-cluster)
MINIO_ACCESS_KEY=the0admin
MINIO_SECRET_KEY=the0password
MINIO_BUCKET=the0-custom-bots
```

## When to Use Each Mode

| Scenario | Recommended Mode |
|----------|-----------------|
| Local development | Docker Mode (`bot-runner`) |
| Small self-hosted (<100 bots) | Docker Mode |
| Medium deployments (<1000 bots) | Docker Mode |
| Large deployments (>1000 bots) | Kubernetes Mode (`controller`) |
| High availability requirements | Kubernetes Mode |

**Docker Mode** is simpler to operate and debug. A single process manages all containers on the host.

**Kubernetes Mode** leverages K8s for scheduling, health checks, and automatic restarts. Each bot becomes its own Pod.

## Supported Runtimes

| Runtime | Docker Image | Entrypoint |
|---------|-------------|------------|
| Python 3.11 | `python:3.11-slim` | `main.py` |
| Node.js 20 | `node:20-alpine` | `main.js` |
| Rust | `rust:1-slim` | Binary executable |
| C++ | `gcc:13` | Binary executable |
| C# | `mcr.microsoft.com/dotnet/runtime:8.0` | DLL |
| Scala | `sbtscala/scala-sbt:eclipse-temurin-21` | JAR |
| Haskell | `haskell:9.6` | Binary executable |

## Bot Interface

### Python

```python
def main(id: str, config: dict):
    """Bot entry point"""
    print(f"Running bot {id}")
    # Your trading logic here
    return {
        "status": "success",
        "message": f"Bot {id} executed successfully"
    }
```

### Node.js

```javascript
function main(id, config) {
    console.log(`Running bot ${id}`);
    // Your trading logic here
    return {
        status: "success",
        message: `Bot ${id} executed successfully`
    };
}

module.exports = { main };
```

## Building

```bash
# Build binary
go build -o build/runtime ./cmd/app

# Run tests
go test ./...

# Run tests with verbose output
go test -v ./internal/docker/...
```

## Testing

```bash
# All tests
go test ./...

# Skip integration tests
go test -short ./...

# Specific package
go test -v ./internal/docker/

# With coverage
go test -cover ./...
```

Integration tests require Docker to be running and will pull images during execution.

## Capacity

| Metric | Docker Mode |
|--------|-------------|
| Bots managed | 10,000+ |
| Memory overhead | ~100MB for 1000 bots |
| Goroutines | 1 per bot + collectors |
| Docker container limit | ~1000 per host |

For more than 1000 bots per host, use Kubernetes mode.

## Security

- **Container Isolation**: Each bot runs in an isolated Docker container
- **Resource Limits**: CPU and memory limits per container
- **Code Validation**: YARA rule scanning for uploaded bot code
- **File System Protection**: Directory traversal prevention during extraction
- **Secret Management**: Configuration passed via environment variables

## Project Structure

```
runtime/
├── cmd/app/                    # CLI entry point
│   └── main.go                 # Cobra commands
├── internal/
│   ├── docker/                 # Docker mode implementation
│   │   ├── service.go          # BotService (live bots)
│   │   ├── schedule_service.go # ScheduleService (cron bots)
│   │   ├── state.go            # In-memory state management
│   │   ├── runner.go           # DockerRunner interface
│   │   └── ...                 # Container orchestration
│   ├── k8s/                    # Kubernetes mode
│   │   ├── controller/         # Bot and schedule controllers
│   │   ├── podgen/             # Pod spec generation
│   │   └── health/             # Health server
│   ├── model/                  # Shared data models
│   ├── runtime/                # Runtime configuration
│   └── entrypoints/            # Script generation
├── Makefile
├── go.mod
└── README.md
```

See [CLAUDE.md](./CLAUDE.md) for detailed developer documentation.
