name: "the0 Runtime PRP Template v3 - Master-Worker Architecture"
description: |

## Purpose
Template optimized for AI agents to implement features in the0 Runtime's distributed Docker orchestration system with master-worker architecture, ensuring proper validation and working code through iterative refinement.

## Core Principles
1. **Context is King**: Include ALL necessary documentation, examples, and caveats
2. **Validation Loops**: Provide executable tests the AI can run and fix
3. **Information Dense**: Use keywords and patterns from the codebase
4. **Progressive Success**: Start simple, validate, then enhance
5. **Global Rules**: Follow all rules in CLAUDE.md and README.md
6. **Architecture First**: Understand master-worker, segments, and Docker patterns before coding

---

## Goal
[What needs to be built - be specific about the end state and which service(s) are affected: bot-runner, backtest-runner, bot-scheduler, or core infrastructure]

## Why
- [Business value and user impact]
- [Integration with existing services]
- [Problems this solves and for whom]
- [Impact on distributed system behavior]

## What
[User-visible behavior, technical requirements, and system-level changes]

### Success Criteria
- [ ] [Specific measurable outcomes]
- [ ] [Tests pass: unit and integration]
- [ ] [Build succeeds: `make go-build`]
- [ ] [Docker containers work correctly]

## All Needed Context

### Project Architecture Overview
```yaml
System: the0 Runtime - Unified Docker Container Orchestration
Architecture: Master-Worker with gRPC streaming
Services:
  - bot-runner: Live trading bot execution (port 50051)
  - backtest-runner: Strategy backtest processing (port 50052)
  - bot-scheduler: Cron-based scheduled execution (port 50053)

Core Technologies:
  - Language: Go 1.24+
  - Database: MongoDB (with partitions collection)
  - Messaging: NATS (optional, graceful degradation)
  - Containers: Docker
  - Communication: gRPC with streaming
  - Storage: MinIO/S3
  - Orchestration: Kubernetes with native autoscaler

Key Patterns:
  - 1:1 segment-to-worker assignment
  - Heartbeat-based health monitoring (2s worker → 5s timeout)
  - Long-running vs terminating container modes
  - Component delegation (orchestrator, code manager, script manager, log collector)
  - Factory pattern for service-specific dependencies
```

### Documentation & References
```yaml
# MUST READ - Include these in your context window

# Project Documentation
- file: CLAUDE.md
  why: Global coding rules, architecture patterns, implementation guidelines
  critical: READ THIS FIRST - contains all project conventions

- file: README.md
  why: User-facing documentation, environment variables, deployment guide

- file: pb/worker.proto
  why: gRPC service definition for master-worker communication

# Core Infrastructure (READ if modifying master-worker)
- file: internal/core/master.go
  why: Generic master implementation (gRPC, HTTP, autoscaler)
  line: 15 (Master struct)

- file: internal/core/worker.go
  why: Generic worker implementation (subscription, heartbeat)
  line: 25 (Worker struct)

- file: internal/core/server.go
  why: gRPC server with segment assignment and rebalancing
  line: 36 (WorkerServiceGrpcServer struct)
  line: 270 (Rebalance method - critical for understanding segment assignment)

# Docker Orchestration (READ if working with containers)
- file: internal/docker-runner/docker_runner.go
  why: Main DockerRunner interface and implementation
  line: 28 (DockerRunner interface)
  line: 88 (dockerRunner struct)
  line: 362 (StartContainer - long-running vs terminating modes)

- file: internal/docker-runner/container_orchestrator.go
  why: Docker API operations wrapper

- file: internal/docker-runner/docker_runner_config.go
  why: Environment-based configuration with defaults

- file: internal/docker-runner/script_manager.go
  why: Runtime-specific entrypoint generation

# Service Implementations (READ if modifying specific service)
- file: internal/bot-runner/server/worker.go
  why: Bot execution worker implementation

- file: internal/backtest-runner/server/worker.go
  why: Backtest execution worker (batch processing)

- file: internal/bot-scheduler/server/worker.go
  why: Scheduled bot worker (cron-based)

# Data Models
- file: internal/model/executable.go
  why: Generic executable configuration

- file: internal/bot-runner/model/bot.go
  why: Bot data model with versioning

- file: internal/constants/database.go
  why: Database names, ports, collection names

# Utilities
- file: internal/util/logger.go
  why: Logging utilities (LogMaster, LogWorker)

- file: internal/util/retry.go
  why: Retry logic with exponential backoff

# External Documentation
- url: https://pkg.go.dev/google.golang.org/grpc
  why: gRPC patterns, streaming, error handling

- url: https://docs.docker.com/engine/api/sdk/
  why: Docker API patterns and best practices

- url: https://pkg.go.dev/go.mongodb.org/mongo-driver
  why: MongoDB operations, context handling, BSON

- url: https://docs.nats.io/using-nats/developer
  why: NATS messaging patterns (if working with subscribers)

- url: https://pkg.go.dev/github.com/spf13/cobra
  why: CLI framework for cmd/app/main.go

- url: https://min.io/docs/minio/linux/developers/go/minio-go.html
  why: MinIO client operations (if working with storage)
```

### Current Codebase Structure
```bash
runtime/
├── cmd/app/
│   └── main.go                    # Cobra CLI entry point
├── internal/
│   ├── core/                      # Shared master-worker infrastructure
│   │   ├── master.go             # Generic master
│   │   ├── worker.go             # Generic worker
│   │   └── server.go             # gRPC server (segment assignment)
│   ├── bot-runner/               # Live bot execution service
│   │   ├── server/{master,worker}.go
│   │   ├── subscriber/           # NATS subscriber
│   │   └── model/bot.go
│   ├── backtest-runner/          # Backtest processing service
│   │   ├── server/{master,worker}.go
│   │   ├── subscriber/           # NATS subscriber
│   │   ├── publisher/            # NATS publisher
│   │   └── model/backtest.go
│   ├── bot-scheduler/            # Scheduled execution service
│   │   ├── server/{master,worker}.go
│   │   ├── subscriber/           # NATS subscriber
│   │   └── model/bot-schedule.go
│   ├── docker-runner/            # Docker orchestration (shared)
│   │   ├── docker_runner.go
│   │   ├── container_orchestrator.go
│   │   ├── container_builder.go
│   │   ├── code_manager.go
│   │   ├── script_manager.go
│   │   ├── log_collector.go
│   │   └── entrypoints/
│   ├── model/                    # Shared models
│   ├── util/                     # Utilities
│   ├── autoscaler/               # K8s autoscaler
│   ├── metrics/
│   ├── minio-logger/
│   └── constants/
├── pb/                           # Protocol Buffers
│   ├── worker.proto
│   ├── worker.pb.go
│   └── worker_grpc.pb.go
├── Makefile
├── go.mod
├── README.md
└── CLAUDE.md
```

### Desired Codebase Tree with Files to Be Added
```bash
# List new files/directories that will be created
# Example:
internal/new-feature/
├── feature.go
├── feature_test.go
└── factory.go
```

### Known Gotchas & Library Quirks
```go
// CRITICAL GOTCHAS for the0 Runtime

// 1. gRPC Streaming
// Streaming subscriptions must handle context cancellation properly
// Workers MUST cancel old subscription before starting new one
// See: internal/core/worker.go:180 (Subscribe method)

// 2. Docker API
// Container removal requires stopping first (unless AutoRemove=true)
// Root-owned files in containers need permission fixing before cleanup
// See: internal/docker-runner/docker_runner.go:207 (fixDirectoryPermissions)

// 3. MongoDB Segments
// Segments come from partitions collection where bot_count > 0
// Workers assigned exactly ONE segment at a time
// See: internal/core/server.go:248 (getAvailableSegments)

// 4. NATS
// System must operate without NATS (graceful degradation)
// Subjects follow pattern: the0.{entity}.{action}
// See: internal/bot-runner/subscriber/subscriber.go:22

// 5. MinIO
// Must use environment variables for configuration
// Buckets: bots (code), logs, results (backtests)
// See: internal/docker-runner/docker_runner_config.go:30

// 6. Concurrency
// State mutations require mutex protection
// Use RWMutex for read-heavy operations
// Buffered channels for notifications (size 1): make(chan struct{}, 1)
// See: internal/core/server.go:36 (WorkerServiceGrpcServer struct)

// 7. Context Propagation
// ALWAYS use context.Context for Docker operations
// Container execution must respect context cancellation
// See: internal/docker-runner/docker_runner.go:362 (StartContainer)

// 8. Environment Configuration
// Required vars must error if missing
// Optional vars must have sensible defaults
// See: internal/docker-runner/docker_runner_config.go:30 (LoadConfigFromEnv)

// 9. Logging
// Masters use util.LogMaster()
// Workers use util.LogWorker()
// Include context in logs (worker_id, segment, container_id)

// 10. Factory Pattern
// Services use factories to inject dependencies into core.Worker
// Implement both DockerRunnerFactory and SubscriberFactory
// See: internal/bot-runner/server/worker.go (factory implementations)
```

## Implementation Blueprint

### Data Models and Structure
Create core data models ensuring type safety and consistency.
```go
// Examples for the0 Runtime:
// - MongoDB BSON models with bson tags
// - Executable configurations
// - gRPC request/response structs
// - NATS event structures
// - Container configuration builders
// - Factory interfaces

// Example pattern from actual codebase:
type Executable struct {
    ID              string
    Runtime         string                    // "python3.11" or "nodejs20"
    Entrypoint      string                    // "bot" or "backtest"
    EntrypointFiles map[string]string
    Config          map[string]interface{}
    FilePath        string                    // MinIO path
    IsLongRunning   bool                      // true for bots, false for backtests
    PersistResults  bool
    Segment         int32                     // Worker segment for labeling
}

// Factory pattern example:
type DockerRunnerFactory interface {
    Create() (dockerrunner.DockerRunner, error)
}
```

### List of Tasks (in order)

```yaml
Task 1: [Task description]
CREATE/MODIFY [file path]:
  - MIRROR pattern from: [existing similar file]
  - ADD [specific functionality]
  - IMPLEMENT [interface or behavior]
  - PRESERVE [existing patterns]
  - KEEP [consistency with existing code]
  validation: |
    go build -o build/runtime ./cmd/app
    go test -v ./path/to/package

Task 2: [Task description]
CREATE [file path]:
  - MIRROR pattern from: [existing file]
  - IMPLEMENT [specific interface]
  - USE [specific library/pattern]
  - PRESERVE [coding conventions]
  validation: |
    go test -v ./...

Task 3: Update Master/Worker (if needed)
MODIFY internal/[service]/server/master.go:
  - ADD new functionality to master
  - PRESERVE existing initialization patterns
  - KEEP gRPC server patterns consistent
  validation: |
    go build ./...
    go test -v ./internal/core/...

Task 4: Add NATS Subscriber (if needed)
MODIFY internal/[service]/subscriber/subscriber.go:
  - ADD new event handler
  - FOLLOW existing subject naming: the0.{entity}.{action}
  - PRESERVE error handling patterns
  validation: |
    go test -v ./internal/[service]/subscriber/...

Task 5: Update Configuration
MODIFY internal/docker-runner/docker_runner_config.go (or similar):
  - ADD environment variable with default
  - VALIDATE configuration in LoadConfigFromEnv
  - PRESERVE existing pattern
  validation: |
    go test -v ./internal/docker-runner/...

Task 6: Add CLI Command (if needed)
MODIFY cmd/app/main.go:
  - ADD new command/subcommand
  - PRESERVE Cobra patterns
  - KEEP consistent help text
  validation: |
    go build -o build/runtime ./cmd/app
    ./build/runtime --help

...
```

### Per-Task Pseudocode

```go
// Task-specific pseudocode with CRITICAL details

// ============================================
// Example: Adding a new service worker feature
// ============================================

// 1. Create Factory
type NewFeatureDockerRunnerFactory struct{}

func (f *NewFeatureDockerRunnerFactory) Create() (dockerrunner.DockerRunner, error) {
    // PATTERN: Always use DockerRunnerOptions with logger
    return dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{
        Logger: &util.DefaultLogger{},
    })
}

// 2. Implement Worker
func NewWorker(
    ctx context.Context,
    workerId string,
    mongoUri string,
    dbName string,
    collectionName string,
    masterAddress string,
    dockerRunnerFactory DockerRunnerFactory,
    subscriberFactory SubscriberFactory,
    mongoClient *mongo.Client,
) (*core.Worker, error) {
    // PATTERN: Use core.NewWorker with factories
    worker, err := core.NewWorker(ctx, workerId, mongoUri, dbName, collectionName, masterAddress)
    if err != nil {
        return nil, fmt.Errorf("failed to create worker: %w", err)
    }

    // CRITICAL: Initialize DockerRunner
    dockerRunner, err := dockerRunnerFactory.Create()
    if err != nil {
        return nil, fmt.Errorf("failed to create docker runner: %w", err)
    }

    // CRITICAL: Initialize subscriber with NATS
    subscriber, err := subscriberFactory.Create()
    if err != nil {
        // GOTCHA: NATS is optional, log warning but don't fail
        util.LogWorker("Warning: Failed to create subscriber: %v", err)
    }

    return worker, nil
}

// 3. Add Segment Processing Logic
func (w *Worker) ProcessSegment(ctx context.Context, segment int32) error {
    // PATTERN: Query MongoDB for executables in segment
    collection := w.MongoClient.Database(w.DBName).Collection(w.CollectionName)

    filter := bson.M{"segment_id": segment}
    cursor, err := collection.Find(ctx, filter)
    if err != nil {
        return fmt.Errorf("failed to query segment %d: %w", segment, err)
    }
    defer cursor.Close(ctx)

    // PATTERN: Process each executable
    for cursor.Next(ctx) {
        var exec model.Executable
        if err := cursor.Decode(&exec); err != nil {
            util.LogWorker("Failed to decode executable: %v", err)
            continue
        }

        // CRITICAL: Use DockerRunner to execute
        result, err := w.dockerRunner.StartContainer(ctx, exec)
        if err != nil {
            util.LogWorker("Failed to start container for %s: %v", exec.ID, err)
            continue
        }

        util.LogWorker("Started container %s for executable %s", result.ContainerID, exec.ID)
    }

    return nil
}

// ============================================
// Example: Adding a new Docker runtime
// ============================================

// 1. Create Entrypoint Factory
type Rust132EntrypointFactory struct{}

func (f *Rust132EntrypointFactory) GenerateEntrypoint(exec model.Executable) (string, error) {
    // PATTERN: Generate shell script that runs the executable
    script := fmt.Sprintf(`#!/bin/bash
set -e

# Load configuration as environment variables
export CONFIG='%s'

# Execute the Rust binary
cargo run --release --bin %s
`, toJSON(exec.Config), exec.EntrypointFiles[exec.Entrypoint])

    return script, nil
}

// 2. Register Runtime
// MODIFY: internal/docker-runner/docker_runner.go

func (r *dockerRunner) isValidRuntime(runtime string) bool {
    validRuntimes := []string{"python3.11", "nodejs20", "rust1.32"}
    for _, valid := range validRuntimes {
        if runtime == valid {
            return true
        }
    }
    return false
}

func (r *dockerRunner) getDockerImage(runtime string) string {
    switch runtime {
    case "python3.11":
        return "python:3.11-slim"
    case "nodejs20":
        return "node:20-alpine"
    case "rust1.32":
        return "rust:1.32-alpine"
    default:
        return "python:3.11-slim" // fallback
    }
}

// 3. Update Script Manager
// MODIFY: internal/docker-runner/script_manager.go

func (sm *scriptManager) getFactory(runtime string) EntrypointFactory {
    switch runtime {
    case "python3.11":
        return &Python311EntrypointFactory{}
    case "nodejs20":
        return &NodeJS20EntrypointFactory{}
    case "rust1.32":
        return &Rust132EntrypointFactory{}
    default:
        return &Python311EntrypointFactory{} // fallback
    }
}

// ============================================
// Example: Adding NATS Event Handler
// ============================================

const (
    SubjectNewFeature = "the0.feature.created"
)

type FeatureEvent struct {
    Type      string          `json:"type"`
    EntityID  string          `json:"entity_id"`
    Data      FeatureEventData `json:"data"`
    Timestamp string          `json:"timestamp"`
}

func (s *Subscriber) handleFeatureCreated(msg *nats.Msg) {
    // PATTERN: Parse event
    var event FeatureEvent
    if err := json.Unmarshal(msg.Data, &event); err != nil {
        s.logger.Info("Failed to unmarshal event", "error", err.Error())
        return
    }

    // PATTERN: Process in MongoDB
    ctx := context.Background()
    collection := s.mongoClient.Database(s.dbName).Collection(s.collectionName)

    // CRITICAL: Use upsert to handle duplicates
    filter := bson.M{"id": event.EntityID}
    update := bson.M{
        "$set": bson.M{
            "id":         event.EntityID,
            "data":       event.Data,
            "updated_at": time.Now(),
        },
    }

    opts := mongoOptions.Update().SetUpsert(true)
    _, err := collection.UpdateOne(ctx, filter, update, opts)
    if err != nil {
        s.logger.Info("Failed to update feature", "error", err.Error())
        return
    }

    // CRITICAL: Acknowledge message
    msg.Ack()
}

func (s *Subscriber) Start(ctx context.Context) error {
    // PATTERN: Subscribe to subjects
    _, err := s.natsConn.Subscribe(SubjectNewFeature, s.handleFeatureCreated)
    if err != nil {
        return fmt.Errorf("failed to subscribe: %w", err)
    }

    <-ctx.Done()
    return nil
}
```

## Validation Loop

### Level 1: Syntax & Build
```bash
# Run these FIRST - fix any errors before proceeding
make go-build                    # Full build with proto generation
go fmt ./...                     # Format code
go vet ./...                     # Static analysis
go mod tidy                      # Clean dependencies

# Expected: No errors. If errors, READ the error and fix.
```

### Level 2: Unit Tests
```go
// Example test patterns for the0 Runtime

// ============================================
// Testing Worker Factory
// ============================================
func TestDockerRunnerFactory_Create(t *testing.T) {
    factory := &WorkerDockerRunnerFactory{}

    runner, err := factory.Create()

    assert.NoError(t, err)
    assert.NotNil(t, runner)
}

// ============================================
// Testing Segment Processing
// ============================================
func TestWorker_ProcessSegment(t *testing.T) {
    // Setup MongoDB test container or mock
    mongoClient := setupTestMongo(t)
    defer cleanupTestMongo(t, mongoClient)

    // Insert test data
    ctx := context.Background()
    collection := mongoClient.Database("test_db").Collection("test_collection")

    testExecutable := model.Executable{
        ID:            "test-bot",
        Runtime:       "python3.11",
        Entrypoint:    "bot",
        Segment:       0,
        IsLongRunning: true,
    }

    _, err := collection.InsertOne(ctx, testExecutable)
    require.NoError(t, err)

    // Create worker with mock DockerRunner
    mockRunner := &MockDockerRunner{
        StartContainerFunc: func(ctx context.Context, exec model.Executable) (*dockerrunner.ExecutionResult, error) {
            return &dockerrunner.ExecutionResult{
                Status:      "running",
                ContainerID: "test-container-id",
            }, nil
        },
    }

    worker := &Worker{
        MongoClient: mongoClient,
        DBName:      "test_db",
        CollectionName: "test_collection",
        dockerRunner: mockRunner,
    }

    // Execute
    err = worker.ProcessSegment(ctx, 0)

    // Assert
    assert.NoError(t, err)
}

// ============================================
// Testing gRPC Server
// ============================================
func TestWorkerServiceGrpcServer_Heartbeat(t *testing.T) {
    server := &core.WorkerServiceGrpcServer{
        workers: make(map[string]*core.Worker),
    }

    // First connect worker
    ctx := context.Background()
    server.onConnected("test-worker")

    // Send heartbeat
    request := &pb.Request{
        Data: `{"workerId": "test-worker"}`,
    }

    response, err := server.Heartbeat(ctx, request)

    assert.NoError(t, err)
    assert.Equal(t, "ok", response.Data)
}

// ============================================
// Testing NATS Subscriber
// ============================================
func TestSubscriber_HandleBotCreated(t *testing.T) {
    // Setup test MongoDB
    mongoClient := setupTestMongo(t)
    defer cleanupTestMongo(t, mongoClient)

    subscriber := &Subscriber{
        mongoClient:    mongoClient,
        dbName:         "test_db",
        collectionName: "bots",
        logger:         &util.DefaultLogger{},
    }

    // Create test event
    event := BotEvent{
        Type:     "created",
        EntityID: "test-bot-123",
        Data: BotEventData{
            ID:     "test-bot-123",
            Config: map[string]interface{}{"key": "value"},
        },
    }

    eventData, err := json.Marshal(event)
    require.NoError(t, err)

    msg := &nats.Msg{
        Data: eventData,
    }

    // Execute
    subscriber.handleBotCreated(msg)

    // Verify bot was created in MongoDB
    ctx := context.Background()
    collection := mongoClient.Database("test_db").Collection("bots")

    var result model.Bot
    err = collection.FindOne(ctx, bson.M{"id": "test-bot-123"}).Decode(&result)

    assert.NoError(t, err)
    assert.Equal(t, "test-bot-123", result.ID)
}
```

```bash
# Run and iterate until passing:
make test                        # All tests
go test -v ./...                 # Verbose output
go test -race ./...              # Check for race conditions
go test -cover ./...             # Check test coverage
go test -short ./...             # Skip integration tests

# For specific package:
go test -v ./internal/docker-runner/

# If failing: Read error, understand root cause, fix code, re-run
```

### Level 3: Integration Tests
```go
// Integration tests for the0 Runtime require Docker

//go:build integration

func TestIntegration_RealDocker_PythonBot(t *testing.T) {
    if testing.Short() {
        t.Skip("Skipping integration test in short mode")
    }

    // Setup
    ctx := context.Background()
    runner, err := dockerrunner.NewDockerRunner(dockerrunner.DockerRunnerOptions{})
    require.NoError(t, err)
    defer runner.Close()

    // Create test executable
    executable := model.Executable{
        ID:              "integration-test-bot",
        Runtime:         "python3.11",
        Entrypoint:      "bot",
        EntrypointFiles: map[string]string{"bot": "main.py"},
        Config:          map[string]interface{}{"test": true},
        FilePath:        "path/to/test-bot.zip",
        IsLongRunning:   false,
        Segment:         0,
    }

    // Execute
    result, err := runner.StartContainer(ctx, executable)

    // Assert
    assert.NoError(t, err)
    assert.NotNil(t, result)
    assert.Equal(t, "success", result.Status)
    assert.Equal(t, 0, result.ExitCode)
}

func TestIntegration_MasterWorker_SegmentAssignment(t *testing.T) {
    if testing.Short() {
        t.Skip("Skipping integration test")
    }

    // Start master
    mongoUri := "mongodb://localhost:27017"
    address := ":50099" // Test port

    master, err := core.NewMaster(mongoUri, "test_db", "test_collection", address)
    require.NoError(t, err)

    go master.Start()
    defer master.Stop()

    time.Sleep(1 * time.Second) // Wait for master to start

    // Start worker
    ctx := context.Background()
    worker, err := core.NewWorker(ctx, "test-worker-1", mongoUri, "test_db", "test_collection", "localhost:50099")
    require.NoError(t, err)

    go worker.Start()
    defer worker.Stop()

    // Wait for assignment
    time.Sleep(2 * time.Second)

    // Verify worker has segment
    status := worker.GetStatus()
    assert.NotEqual(t, int32(-1), status["segment"])
}
```

```bash
# Run integration tests (requires Docker):
go test -v ./internal/docker-runner/
go test -tags=integration -v ./...

# Expected: Docker images pulled, containers created/destroyed, tests pass
```

## Final Validation Checklist
- [ ] All tests pass: `make test` or `go test ./...`
- [ ] No build errors: `make go-build`
- [ ] No race conditions: `go test -race ./...`
- [ ] Integration tests pass (if applicable): `go test -v ./internal/docker-runner/`
- [ ] Code formatted: `go fmt ./...`
- [ ] Proto regenerated (if changed): `make build-proto`
- [ ] Environment variables documented
- [ ] Error messages are clear and actionable
- [ ] Logging uses appropriate prefixes (LogMaster/LogWorker)
- [ ] Context cancellation handled properly
- [ ] Resources cleaned up (containers, directories, connections)

---

## Anti-Patterns to Avoid

### General Go Anti-Patterns
- ❌ Don't ignore errors - always handle them explicitly
- ❌ Don't use global variables for state
- ❌ Don't skip context propagation in long-running operations
- ❌ Don't hardcode configuration values
- ❌ Don't use panic for regular error cases (only fatal startup issues)

### the0 Runtime Specific Anti-Patterns
- ❌ Don't skip mutex protection when modifying shared state
- ❌ Don't forget to cancel old subscriptions before starting new ones
- ❌ Don't assume NATS is always available (graceful degradation required)
- ❌ Don't forget to clean up Docker containers on worker shutdown
- ❌ Don't query segments without checking `bot_count > 0`
- ❌ Don't create workers without implementing both factories
- ❌ Don't modify core/ without understanding impact on all services
- ❌ Don't hardcode ports - use constants/database.go
- ❌ Don't forget AutoRemove flag differences (long-running vs terminating)
- ❌ Don't skip permission fixing when cleaning up container directories
- ❌ Don't use blocking operations in gRPC streaming without select/context
- ❌ Don't forget to defer resource cleanup (Close, RemoveAll, Disconnect)
- ❌ Don't create unbuffered channels for notifications (use size 1)
- ❌ Don't skip environment variable defaults

## PR Message

When done, write a PR message summarizing the feature, changes made, validation steps, and known issues. Use the following template:

```markdown
## Feature: [Feature Name]
[Brief description of the feature and its purpose in the distributed system]

## Background
[Context on why this feature is needed and how it fits into the master-worker architecture]

## Changes Made
### Core Infrastructure
- [Changes to internal/core/]

### Service Changes
- [Changes to bot-runner/backtest-runner/bot-scheduler]

### Docker Orchestration
- [Changes to docker-runner/]

### Database/Storage
- [MongoDB schema changes, MinIO bucket changes]

### Configuration
- [New environment variables added]

## Architecture Impact
- [Impact on segment assignment]
- [Impact on worker scaling]
- [Impact on container lifecycle]
- [Impact on message flow]

## Validation Steps
- [ ] Unit tests pass: `go test ./...`
- [ ] Integration tests pass: `go test -v ./internal/docker-runner/`
- [ ] Build succeeds: `make go-build`
- [ ] Manual testing: [describe manual test scenarios]
- [ ] No race conditions: `go test -race ./...`

## Known Issues/Limitations
[List any known issues or limitations of the implementation]

## Follow-up Tasks
[List any follow-up tasks or future improvements]

## Breaking Changes
[List any breaking changes to APIs, configs, or behavior]
```

Save as: `PRPs/[feature-name]/PR.md`
