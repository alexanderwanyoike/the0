---
title: "Platform Deployment"
description: "Deploy the0 platform for development and production"
order: 5
---

# Platform Deployment

the0 platform consists of multiple services that work together to provide bot development, deployment, and execution capabilities. This section covers how to deploy the platform itself, whether for local development, self-hosted production, or cloud environments.

## Architecture Overview

A complete the0 deployment includes:

**Application Services**:
- **API Server** (NestJS/TypeScript) - REST API handling authentication, bot management, and orchestration
- **Frontend** (Next.js/React) - Web dashboard for bot management and monitoring
- **Documentation** (VitePress) - Platform documentation site
- **Bot Runner** (Go) - Single-process service for executing realtime bots
- **Bot Scheduler** (Go) - Single-process service for executing scheduled bots

**Infrastructure Services**:
- **PostgreSQL** - Primary database for users, bots, and configuration
- **MongoDB** - Runtime state and execution tracking
- **NATS with JetStream** - Event streaming and inter-service communication
- **MinIO** - S3-compatible storage for bot code, logs, and artifacts

The runtime services use a **reconciliation loop** pattern: they periodically compare desired state (from MongoDB) with actual state (running containers) and make corrections. Bots run in isolated Docker containers with resource limits.

## Deployment Options

### Docker Compose (Recommended)

Docker Compose provides the simplest and most tested path to running the0. A single `make up` command starts all services with sensible defaults. This approach works well for:

- Local development
- Small teams and evaluation
- Self-hosted production (up to ~1000 bots)

See [Docker Compose Deployment](./docker-compose) for setup instructions.

### Kubernetes (Production Scale)

For large deployments with more than 1000 bots, use Kubernetes mode. The runtime controller manages bots as individual Pods, leveraging K8s for scheduling, health checks, and automatic restarts.

See [Kubernetes Deployment](./kubernetes) for setup instructions.

## When to Use Each Mode

| Scenario | Recommended Deployment |
|----------|----------------------|
| Local development | Docker Compose |
| Small team (<10 users) | Docker Compose |
| Medium deployment (<1000 bots) | Docker Compose |
| Large deployment (>1000 bots) | Kubernetes |
| High availability required | Kubernetes |

## Hardware Requirements

The platform's resource requirements depend on the number of bots and execution frequency. As a baseline:

**Development (Docker Compose)**:
- 4 CPU cores
- 8GB RAM
- 20GB disk space

**Production (Docker Compose)**:
- 8+ CPU cores
- 16GB+ RAM
- 100GB+ disk space

**Production (Kubernetes)**:
- 8+ CPU cores across nodes
- 16GB+ RAM
- 100GB+ disk space for persistent volumes

## Service Communication

Services communicate through multiple channels:

- **HTTP/REST** - External API access and frontend-to-API communication
- **NATS JetStream** - Event-driven communication between API and runtime services
- **Direct Database** - Services access PostgreSQL and MongoDB directly for their data

The API server acts as the central coordinator, receiving requests from the frontend and CLI, then publishing events to NATS for the runtime services to process.

## Security Considerations

Before deploying to production:

1. **Change default credentials** - All services ship with development passwords that must be changed
2. **Configure JWT secrets** - Generate strong secrets for API authentication
3. **Enable TLS** - Use HTTPS for all external endpoints
4. **Network isolation** - Infrastructure services should not be publicly accessible
5. **Resource limits** - Configure appropriate limits to prevent runaway bots

See the deployment-specific pages for detailed security configuration.
