# Docker Mode

The Docker mode provides single-process services for running bots in containers. This is the recommended deployment for local development and small-to-medium deployments.

## Architecture

Docker mode uses a **unified execute architecture** where each bot container is self-managing. The runtime services (bot-runner and bot-scheduler) only handle container lifecycle - starting, stopping, and restarting containers. All the complexity of code download, state persistence, and log streaming is delegated to the `runtime execute` command running inside each container.

```mermaid
flowchart LR
    NATS["NATS Events"] --> BotSvc & SchedSvc
    HTTP["HTTP :9477"] --> QS

    subgraph Services
        BotSvc["BotService"]
        SchedSvc["ScheduleService"]
        QS["query-server"]
    end

    BotSvc & SchedSvc <--> MongoDB[(MongoDB)]
    QS <--> MongoDB
    BotSvc & SchedSvc --> Docker["Docker Engine"]

    subgraph Container["Bot Container"]
        Execute["runtime execute"]
        Sync["daemon sync<br/>(subprocess)"]
        QuerySvc["query server<br/>(subprocess :9476)"]
    end

    subgraph Ephemeral["Ephemeral Query Container"]
        EphBot["runtime execute<br/>(query mode)"]
    end

    Docker --> Container
    Sync --> MinIO[(MinIO)]
    QS -.->|realtime| QuerySvc
    QS -.->|scheduled| Ephemeral
```

## How It Works

### The Reconciliation Loop

Both BotService and ScheduleService use a reconciliation pattern. Every 30 seconds (configurable), the service queries MongoDB for the desired state - which bots should be running - and compares it against the actual state from Docker. If a bot should be running but isn't, the service starts it. If a container is running but the bot was disabled, the service stops it. If the configuration changed, the service restarts the container.

NATS integration is optional but recommended. Without NATS, changes only take effect at the next reconciliation interval. With NATS, the service receives events immediately and can act on them right away.

### Container Execution

When a container starts, it runs `runtime execute` which handles everything. First, it downloads the bot code and any existing state from MinIO. Then it starts `daemon sync` as a subprocess for state/log persistence. For realtime bots with a query entrypoint, it also starts a query server subprocess on port 9476. Finally, it executes the bot process directly.

```mermaid
flowchart TD
    A[Container Start] --> B[runtime execute]
    B --> C[Download code from MinIO]
    C --> D[Download state from MinIO]
    D --> E[Start daemon sync subprocess]
    E --> F{Query entrypoint?}
    F -->|Yes| G[Start query server subprocess]
    F -->|No| H[Execute bot process]
    G --> H
    H --> I{Bot exits?}
    I -->|Scheduled| J[Write done file]
    I -->|Crash| K[Signal cleanup]
    J --> L[Final sync]
    K --> L
    L --> M[Container exits]
```

### State Persistence

The daemon sync process periodically (every 60s) computes a hash of the state directory and uploads changes to MinIO when detected. Logs are also synced periodically.

For scheduled bots, the daemon also watches for a "done" file. When the bot completes and writes its exit code to this file, the daemon performs a final sync and then exits, allowing the container to terminate cleanly.

## Services

**BotService** manages live trading bots - containers that run continuously until stopped. The service monitors container health and restarts failed containers through the reconciliation loop.

**ScheduleService** manages cron-based scheduled bots. It evaluates cron expressions to determine when bots should run, then starts containers that execute once and terminate.

## Simplification from Previous Architecture

The previous architecture had multiple components in the runner: CodeManager for downloading code, ScriptManager for generating entrypoints, LogCollector for streaming logs, StateManager for persistence, and StateCollector for background sync. Each component added complexity and the Docker and K8s implementations diverged.

The current architecture consolidates all of this into the `runtime execute` command which handles code download, state management, and subprocess orchestration. In Docker mode, `execute` spawns `daemon sync` as a subprocess. In K8s mode, these run as separate containers. The runner is now just container lifecycle management - about 40% less code with better separation of concerns.

## Query Execution

Docker mode supports querying bots for computed data without affecting state.

### Realtime Bot Queries

For running bots with a query entrypoint, `runtime execute` starts a query server subprocess on port 9476. Queries are proxied to this server:

1. QueryServer (port 9477) receives HTTP request
2. QueryResolver finds the bot and its container
3. QueryHandler proxies to container IP:9476 (the query server subprocess)
4. Response returned (~10-50ms latency)

### Scheduled Bot Queries

For scheduled bots (not currently running), queries spawn ephemeral containers:

1. QueryServer receives request
2. QueryResolver finds the bot configuration
3. QueryHandler builds executable with `QUERY_PATH` and `QUERY_PARAMS` env vars
4. DockerRunner starts ephemeral container
5. Container runs query mode, returns JSON result
6. Container terminates (~1-3s latency)

## Service State

The services maintain in-memory state to track running containers:

- **RunningBot**: Tracks each bot's container ID, status, start time, and restart count
- **ServiceState**: Thread-safe map of botID → RunningBot with metrics
- **Statuses**: "starting", "running", "stopping", "failed"

This enables the reconciliation loop to compare desired state (MongoDB) with actual state (running containers) and take corrective action.
