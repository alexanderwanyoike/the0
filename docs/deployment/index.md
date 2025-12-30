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
- **Bot Runner** (Go) - Master-worker architecture for executing realtime bots
- **Bot Scheduler** (Go) - Master-worker architecture for executing scheduled bots

**Infrastructure Services**:
- **PostgreSQL** - Primary database for users, bots, and configuration
- **MongoDB** - Runtime state, job queues, and execution logs
- **NATS with JetStream** - Event streaming and inter-service communication
- **MinIO** - S3-compatible storage for bot code, logs, and artifacts

The platform uses a master-worker pattern for bot execution. Each runtime service has a master node that coordinates work distribution and multiple worker nodes that actually execute bot code. Workers run bot code in isolated Docker containers with resource limits.

## Deployment Options

### Docker Compose (Recommended)

Docker Compose provides the simplest and most tested path to running the0. A single `make up` command starts all services with sensible defaults. This approach works well for local development, small teams, evaluation, and self-hosted production.

See [Docker Compose Deployment](./docker-compose) for setup instructions.

### Kubernetes (Experimental)

Kubernetes deployment via Helm charts provides orchestration with automatic restarts, health checks, and scaling. The platform includes a Helm chart that mirrors the Docker Compose configuration. Note that this deployment path has been tested previously but may require updates.

See [Kubernetes Deployment](./kubernetes) for setup instructions.

## Hardware Requirements

The platform's resource requirements depend on the number of bots and execution frequency. As a baseline:

**Development (Docker Compose)**:
- 4 CPU cores
- 8GB RAM
- 20GB disk space

**Production (Kubernetes)**:
- 8+ CPU cores across nodes
- 16GB+ RAM
- 100GB+ disk space for persistent volumes
- Worker nodes with Docker socket access for bot execution

## Service Communication

Services communicate through multiple channels:

- **HTTP/REST** - External API access and frontend-to-API communication
- **gRPC** - Master-worker coordination within runtime services
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
