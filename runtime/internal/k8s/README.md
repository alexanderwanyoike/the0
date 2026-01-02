# Kubernetes Mode Package

The `internal/k8s` package provides a Kubernetes-native controller for managing bots at scale. Each bot becomes its own Pod, leveraging K8s for scheduling, health checks, and automatic restarts.

## Overview

Use Kubernetes mode when:
- Running more than ~1000 bots per host
- High availability is required
- You need K8s-native observability and management

## Architecture

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

**How it works:**
1. Controller Manager queries MongoDB for enabled bots/schedules (desired state)
2. Queries Kubernetes API for existing Pods/CronJobs (actual state)
3. Reconciles: creates missing Pods, deletes extra Pods, updates changed configs
4. Repeats every `ReconcileInterval` (default: 30s)

## Components

### Controller Manager

**File:** `controller/manager.go`

Coordinates multiple K8s controllers and their dependencies.

```go
type ManagerConfig struct {
    Namespace         string        // K8s namespace for bot pods
    ReconcileInterval time.Duration // Default: 30s
    MinIOEndpoint     string        // For bot code download
    MinIOAccessKey    string
    MinIOSecretKey    string
    MinIOBucket       string
}
```

**Managed Controllers:**
- `BotController` - Manages live bot Pods
- `BotScheduleController` - Manages scheduled bot CronJobs
- `K8sLogCollector` - Collects logs from bot Pods

### Bot Controller

**File:** `controller/bot_controller.go`

Manages live trading bots as Kubernetes Pods.

**Reconciliation Logic:**
1. List enabled bots from MongoDB
2. List existing Pods with `managed-by: the0-bot-controller` label
3. Create Pods for new bots
4. Delete Pods for removed bots
5. Update Pods with config changes (delete + recreate)

**Pod Lifecycle:**
- Pods are created with bot code mounted via init container
- Bot code downloaded from MinIO to ephemeral volume
- Pod runs until explicitly deleted or bot disabled

### Schedule Controller

**File:** `controller/botschedule_controller.go`

Manages scheduled bots as Kubernetes CronJobs.

**How it works:**
1. Schedules stored in MongoDB with cron expressions
2. Controller creates CronJobs matching schedule definitions
3. K8s handles cron scheduling and Job creation
4. Jobs run to completion and are cleaned up

### PodGen (Pod Generator)

**File:** `podgen/pod_generator.go`

Generates Pod specifications for bots.

**Generated Pod Structure:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: bot-{botID}
  namespace: the0
  labels:
    app: the0-bot
    bot-id: {botID}
    managed-by: the0-bot-controller
spec:
  initContainers:
    - name: code-downloader
      # Downloads bot code from MinIO
  containers:
    - name: bot
      image: {runtime-image}
      command: ["/entrypoint.sh"]
      resources:
        limits:
          memory: "512Mi"
          cpu: "500m"
      env:
        - name: BOT_ID
        - name: BOT_CONFIG
```

### Health Server

**File:** `health/server.go`

Provides health endpoints for K8s probes.

**Endpoints:**
- `/healthz` - Liveness probe (always returns 200 if server running)
- `/readyz` - Readiness probe (returns 200 when controllers are ready)

### Runtime Mode Detection

**File:** `detect/runtime_mode.go`

Detects whether running inside Kubernetes cluster.

**Detection Methods:**
- Checks `KUBERNETES_SERVICE_HOST` environment variable
- Checks for service account token at `/var/run/secrets/kubernetes.io/serviceaccount/token`

## Configuration

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `NAMESPACE` | No | `the0` | K8s namespace for bot pods |
| `MONGO_URI` | Yes | - | MongoDB connection string |
| `NATS_URL` | No | - | NATS URL for events (recommended) |
| `MINIO_ENDPOINT` | Yes | - | MinIO endpoint for bot code |
| `MINIO_ACCESS_KEY` | Yes | - | MinIO access key |
| `MINIO_SECRET_KEY` | Yes | - | MinIO secret key |
| `MINIO_BUCKET` | No | `the0-custom-bots` | MinIO bucket name |

### CLI Usage

```bash
./runtime controller \
  --namespace the0 \
  --reconcile-interval 30s \
  --minio-endpoint minio:9000 \
  --minio-bucket the0-custom-bots
```

## RBAC Requirements

The controller needs permissions to manage Pods and CronJobs in its namespace:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: the0-controller
  namespace: the0
rules:
  - apiGroups: [""]
    resources: ["pods", "pods/log"]
    verbs: ["get", "list", "watch", "create", "delete"]
  - apiGroups: ["batch"]
    resources: ["cronjobs", "jobs"]
    verbs: ["get", "list", "watch", "create", "delete", "update"]
```

## Directory Structure

```
internal/k8s/
├── controller/
│   ├── manager.go              # Controller manager
│   ├── bot_controller.go       # Bot Pod management
│   ├── botschedule_controller.go # Schedule CronJob management
│   ├── log_collector.go        # Pod log collection
│   ├── mongo_repository.go     # MongoDB data access
│   └── *_test.go               # Tests
├── podgen/
│   ├── pod_generator.go        # Pod spec generation
│   └── pod_generator_test.go   # Tests
├── health/
│   └── server.go               # Health probe endpoints
├── detect/
│   └── runtime_mode.go         # K8s environment detection
└── README.md                   # This file
```

## Testing

```bash
# Run all k8s tests
go test -v ./internal/k8s/...

# Run specific controller tests
go test -v ./internal/k8s/controller/

# Run podgen tests
go test -v ./internal/k8s/podgen/
```

## Comparison with Docker Mode

| Feature | Docker Mode | Kubernetes Mode |
|---------|-------------|-----------------|
| Process model | Single process | Controller + Pods |
| Bot isolation | Docker containers | K8s Pods |
| Scaling | Manual | K8s native |
| Health checks | Application level | K8s probes |
| Log collection | Background service | K8s native + collector |
| Recommended for | <1000 bots | >1000 bots |
| Complexity | Simple | More complex |
