# the0 Runtime - Development Guide

## Project Overview

**the0 Runtime** is a unified Docker container orchestration system providing two modes:
- **Docker Mode**: Single-process services for local/small deployments
  - `bot-runner`: Live trading bot execution
  - `bot-scheduler`: Cron-based scheduled bot execution
- **Kubernetes Mode**: Controller-based deployment for scale
  - `controller`: K8s-native Pod/CronJob management

## Directory Structure

```
runtime/
├── cmd/app/                        # Application entry point
│   └── main.go                     # Cobra CLI with service commands
│
├── internal/                       # Internal packages (not importable externally)
│   ├── docker/                     # Docker container orchestration
│   │   ├── service.go             # BotService - single-process bot manager
│   │   ├── state.go               # In-memory state management
│   │   ├── runner.go              # DockerRunner interface & implementation
│   │   ├── container_orchestrator.go  # Docker API operations
│   │   ├── container_builder.go   # Container config builder
│   │   ├── code_manager.go        # MinIO code download/extraction
│   │   ├── script_manager.go      # Entrypoint script generation
│   │   ├── log_collector.go       # Background log collection
│   │   ├── config.go              # Environment-based configuration
│   │   ├── bot-runner/
│   │   │   └── subscriber/        # NATS subscriber for bot lifecycle
│   │   └── bot-scheduler/
│   │       └── subscriber/        # NATS subscriber for schedule lifecycle
│   │
│   ├── model/                     # Shared data models
│   │   ├── executable.go          # Generic executable configuration
│   │   ├── bot.go                 # Bot data model
│   │   └── botschedule.go         # Bot schedule data model
│   │
│   ├── runtime/                   # Shared runtime configuration
│   │   ├── images.go              # Runtime-to-Docker-image mappings
│   │   ├── runtimes.go            # Runtime validation
│   │   ├── resources.go           # Default resource limits
│   │   └── buckets.go             # MinIO bucket constants
│   │
│   ├── entrypoints/               # Entrypoint script generation
│   │   ├── generator.go           # Unified entrypoint generator
│   │   ├── python311.go           # Python entrypoint
│   │   ├── nodejs20.go            # Node.js entrypoint
│   │   └── ...                    # Other runtimes
│   │
│   ├── k8s/                       # Kubernetes controller
│   │   ├── controller/            # Bot and schedule controllers
│   │   ├── podgen/                # Pod specification generation
│   │   ├── detect/                # Runtime mode detection
│   │   └── health/                # Health server for K8s probes
│   │
│   ├── util/                      # Utility functions
│   │   ├── logger.go              # Logging utilities
│   │   ├── retry.go               # Retry logic with exponential backoff
│   │   ├── validators.go          # Input validation
│   │   └── cron.go                # Cron expression handling
│   │
│   ├── minio-logger/              # MinIO log persistence
│   │   └── minio_logger.go        # Log uploader
│   │
│   └── constants/                 # Shared constants
│       └── database.go            # Database names, ports, collections
│
├── build/                         # Build output
│   └── runtime                    # Compiled binary
│
├── Makefile                       # Build automation
├── go.mod                         # Go module dependencies
├── README.md                      # User-facing documentation
└── CLAUDE.md                      # This file - developer guide
```

## Architecture

### Docker Mode (Single-Process)

The Docker mode uses a simplified single-process architecture:

```
┌─────────────────────────────────────────────────────────┐
│                      BotService                          │
│                                                         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐ │
│  │    NATS     │  │   Service   │  │  DockerRunner   │ │
│  │ (optional)  │──│    State    │──│   (existing)    │ │
│  └─────────────┘  └─────────────┘  └─────────────────┘ │
│                                                         │
│  Events:              State:              Actions:      │
│  - bot.created        map[botID]*Bot      - Start       │
│  - bot.updated        map[botID]contID    - Stop        │
│  - bot.deleted                            - Restart     │
└─────────────────────────────────────────────────────────┘
```

**Key Components:**
- `BotService` (`internal/docker/service.go`): Main service orchestrator
- `ServiceState` (`internal/docker/state.go`): Thread-safe in-memory state
- `DockerRunner` (`internal/docker/runner.go`): Container lifecycle management

**Reconciliation Loop:**
1. Query MongoDB for enabled bots
2. List running containers from Docker
3. Start missing bots, stop extra containers
4. Detect config changes and restart as needed

### Kubernetes Mode

The K8s mode uses a controller pattern with Kubernetes as the orchestrator:

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

## CLI Commands

```bash
# Docker mode - live bot execution
./runtime bot-runner --mongo-uri mongodb://localhost:27017

# Docker mode - scheduled bot execution (requires NATS)
./runtime bot-scheduler --mongo-uri mongodb://localhost:27017 --nats-url nats://localhost:4222

# Kubernetes mode
./runtime controller --namespace the0 --reconcile-interval 30s

# Version
./runtime version
```

## Environment Variables

### Required for All Modes
```bash
MONGO_URI=mongodb://localhost:27017    # MongoDB connection
```

### Optional for bot-runner
```bash
NATS_URL=nats://localhost:4222         # NATS for real-time events (optional, enables instant updates)
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

## Key Patterns

### 1. Reconciliation Pattern

The BotService uses a reconciliation loop to converge actual state with desired state:

```go
func (s *BotService) reconcile() {
    // 1. Get desired state from MongoDB
    desiredBots, _ := s.getDesiredBots(ctx)

    // 2. Get actual state from Docker
    actualContainers, _ := s.runner.ListManagedContainers(ctx, -1)

    // 3. Reconcile: start missing, stop extras, restart changed
    s.performReconciliation(ctx, desiredBots, actualContainers)
}
```

### 2. Component Delegation

DockerRunner delegates to specialized components:

```go
type dockerRunner struct {
    orchestrator  ContainerOrchestrator  // Docker API operations
    codeManager   CodeManager            // MinIO download/extract
    scriptManager ScriptManager          // Entrypoint generation
    logCollector  LogCollector           // Background log collection
    minioLogger   MinIOLogger            // Log persistence
}
```

### 3. Options Pattern

Components are configured with options structs:

```go
type DockerRunnerOptions struct {
    Logger util.Logger  // Optional, defaults to DefaultLogger
}

func NewDockerRunner(options DockerRunnerOptions) (*dockerRunner, error)
```

### 4. Interface-First Design

Key interfaces for testability:
- `DockerRunner` - Container execution
- `ContainerOrchestrator` - Docker operations
- `CodeManager` - Code fetching
- `ScriptManager` - Script generation
- `LogCollector` - Log collection

### 5. Graceful Degradation

Services operate without optional dependencies:
- NATS: System polls MongoDB if NATS unavailable
- Real-time events: Falls back to 30s reconciliation interval

## Working with NATS

**Subject Naming:**
```
the0.{entity}.{action}

Examples:
- the0.bot.created
- the0.bot.updated
- the0.bot.deleted
- the0.bot-schedule.created
- the0.bot-schedule.updated
- the0.bot-schedule.deleted
```

**Subscribers:**
- `internal/docker/bot-runner/subscriber/` - Persists bot events to MongoDB
- `internal/docker/bot-scheduler/subscriber/` - Persists schedule events to MongoDB

## Development Workflow

### Test-First Development

**Always write or update tests before making code changes.** This ensures:
- The change is properly validated
- Regression prevention
- Clear documentation of expected behavior

```bash
# 1. Write/update the test first
# 2. Run the test to see it fail (for new features) or pass (for existing behavior)
go test -v ./path/to/package -run TestName

# 3. Make the code change
# 4. Run tests again to verify
go test ./...
```

### Building

```bash
go build -o build/runtime ./cmd/app
```

### Running Locally

```bash
# Start with MongoDB (docker mode)
./build/runtime bot-runner --mongo-uri mongodb://localhost:27017

# With NATS for real-time events
./build/runtime bot-runner --mongo-uri mongodb://localhost:27017 --nats-url nats://localhost:4222
```

### Testing

```bash
go test ./...              # All tests
go test -short ./...       # Skip integration tests
go test -v ./internal/docker/  # Specific package
```

### Formatting

```bash
go fmt ./...
```

## Adding Docker Runtime Support

To add a new runtime (e.g., `ruby3.2`):

1. **Create entrypoint factory:**
   ```go
   // internal/entrypoints/ruby32.go
   type Ruby32EntrypointFactory struct{}

   func (f *Ruby32EntrypointFactory) GenerateEntrypoint(exec model.Executable) (string, error) {
       return fmt.Sprintf(`#!/bin/bash
   ruby %s
   `, exec.EntrypointFiles[exec.Entrypoint]), nil
   }
   ```

2. **Register in runtime package:**
   ```go
   // internal/runtime/images.go
   var RuntimeImages = map[string]string{
       "ruby3.2": "ruby:3.2-alpine",
   }
   ```

3. **Update script manager:**
   ```go
   // internal/docker/script_manager.go
   func (sm *scriptManager) getFactory(runtime string) EntrypointFactory {
       switch runtime {
       case "ruby3.2":
           return &Ruby32EntrypointFactory{}
       }
   }
   ```

## Important Files Reference

### Core Services
- `cmd/app/main.go` - CLI entry point
- `internal/docker/service.go` - BotService (live bot execution)
- `internal/docker/schedule_service.go` - ScheduleService (cron-based execution)
- `internal/docker/state.go` - Service state management

### Docker Orchestration
- `internal/docker/runner.go` - DockerRunner interface & implementation
- `internal/docker/container_orchestrator.go` - Docker API wrapper
- `internal/docker/container_builder.go` - Container configuration

### Kubernetes
- `internal/k8s/controller/manager.go` - Controller manager
- `internal/k8s/podgen/` - Pod spec generation

### Data Models
- `internal/model/executable.go` - Generic executable
- `internal/model/bot.go` - Bot model
- `internal/model/botschedule.go` - Schedule model

### Constants
- `internal/constants/database.go` - Database names and ports

## Tips for Working with This Codebase

1. **Start with service.go**: Understand the reconciliation loop
2. **Follow the interfaces**: Interface definitions show component contracts
3. **Check constants/database.go**: Find database names and collections
4. **Use existing tests as examples**: See runner_test.go for patterns
5. **Trace from CLI**: Start at `cmd/app/main.go` to understand initialization
6. **Test with Docker**: Many features require actual Docker
7. **Check environment variables**: Many behaviors configurable via env vars

## Capacity

| Metric | Single Process |
|--------|----------------|
| Bots managed | 10,000+ |
| Memory overhead | ~100MB for 1000 bots |
| Goroutines | 1 per bot + collectors |
| Docker limit | ~1000 containers/host |

**If you need more than 1000 bots: use Kubernetes mode.**

---

**Last Updated:** 2026-01-02
**Maintainer:** the0 team
