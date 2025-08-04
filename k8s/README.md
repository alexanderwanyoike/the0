# the0 Kubernetes Deployment

This directory contains Helm charts and configuration for deploying the0 platform on Kubernetes, providing the same functionality as the docker-compose setup with production-ready orchestration.

## Quick Start

### Minikube (Local Development)
```bash
# Single command - deploy and show service URLs
make minikube-up

# Services automatically available at minikube URLs:
# - Frontend: http://192.168.49.2:30XXX
# - API: http://192.168.49.2:30XXX  
# - MinIO Console: http://192.168.49.2:30XXX
# - Bot Runner: http://192.168.49.2:30XXX
# - Backtest Runner: http://192.168.49.2:30XXX
# - Bot Scheduler: http://192.168.49.2:30XXX

# No additional setup required - works immediately!
```

### Production Cluster
```bash
# Deploy with external infrastructure
make deploy

# Or deploy with included infrastructure for dev clusters
make dev
```

## Prerequisites

### Required Tools
- **Docker** - For building local images
- **kubectl** - Kubernetes CLI tool ([install guide](https://kubernetes.io/docs/tasks/tools/))
- **Helm 3.0+** - Kubernetes package manager ([install guide](https://helm.sh/docs/intro/install/))
- **minikube** - For local development ([install guide](https://minikube.sigs.k8s.io/docs/start/))

### Minikube Requirements
- Minimum 4GB RAM and 2 CPUs allocated to minikube
- 20GB+ disk space
- Docker driver (recommended)

## Project Structure

```
k8s/
├── Chart.yaml              # Helm chart metadata
├── values.yaml             # Default configuration values
├── Makefile                # Deployment commands
├── templates/              # Kubernetes manifests
│   ├── postgres.yaml       # PostgreSQL database
│   ├── mongo.yaml          # MongoDB database  
│   ├── nats.yaml           # NATS message broker
│   ├── minio.yaml          # MinIO S3-compatible storage
│   ├── the0-api.yaml       # Main API service
│   ├── the0-frontend.yaml  # Frontend web application
│   ├── the0-analyzer.yaml  # Security analyzer service
│   ├── bot-runner.yaml     # Bot execution service
│   ├── backtest-runner.yaml # Backtesting service
│   ├── bot-scheduler.yaml  # Bot scheduling service
│   ├── minikube-services.yaml # LoadBalancer services for minikube
│   └── _helpers.tpl        # Helm template helpers
└── README.md              # This file
```

### Architecture Overview

**Infrastructure Services** (can be disabled for production):
- **PostgreSQL** - Main application database
- **MongoDB** - Bot runtime data and metrics
- **NATS with JetStream** - Message broker for service communication
- **MinIO** - S3-compatible object storage for logs and assets

**Application Services**:
- **the0-api** - REST API server (NestJS/TypeScript)
- **the0-frontend** - Web interface (Next.js/React)
- **the0-analyzer** - Security analysis service (Python)
- **bot-runner** - Bot execution runtime (Go)
- **backtest-runner** - Backtesting engine (Go)  
- **bot-scheduler** - Bot scheduling service (Go)

## Installation Methods

### 1. Minikube (Recommended for Development)

Deploy with the same endpoints as docker-compose:

```bash
# Single command - builds images, deploys, and starts tunnels
make minikube
```

**Endpoints (identical to docker-compose):**
- Frontend: http://localhost:3001
- API: http://localhost:3000
- MinIO Console: http://localhost:9001
- Bot Runner API: http://localhost:8080
- Backtest Runner API: http://localhost:8081
- Bot Scheduler API: http://localhost:8082

The command will:
1. Start minikube (if not running)
2. Build all Docker images in minikube environment  
3. Deploy all services with Helm
4. Automatically start port forwarding for localhost access
5. Keep running until you press Ctrl+C

### 2. Production Cluster with External Infrastructure

For production deployments where you provide your own databases and services:

```bash
# Deploy application services only
make deploy

# Configure external services in values.yaml:
# postgresql.enabled: false
# mongodb.enabled: false  
# nats.enabled: false
# minio.enabled: false
```

### 3. Development Cluster with Included Infrastructure

For development clusters where you want the full stack:

```bash
# Deploy with included databases and services
make dev
```

### 4. Manual Helm Deployment

For custom configurations:

```bash
# Install with custom values
helm install the0 . --namespace the0 --create-namespace \
  --set global.imagePullPolicy=Always \
  --set postgresql.enabled=true

# Upgrade existing deployment
helm upgrade the0 . --namespace the0

# Uninstall
helm uninstall the0 --namespace the0
```

## Configuration

### Key Configuration Options

Edit `values.yaml` to customize the deployment:

```yaml
# Global settings
global:
  imagePullPolicy: Never  # Never (local), Always (registry)
  storageClass: ""        # Leave empty for default

# Infrastructure (set to false for external services)
postgresql:
  enabled: true
mongodb:
  enabled: true
nats:
  enabled: true
minio:
  enabled: true

# Minikube LoadBalancer services (for localhost endpoints)
minikube:
  enabled: true  # Set to false for production

# Service configuration
service:
  type: ClusterIP  # ClusterIP (default), NodePort, LoadBalancer
```

### Environment Variables

All services use environment variables that match the docker-compose configuration exactly. These are defined in the `env` sections of each service in `values.yaml`.

### Resource Limits

Default resource limits are conservative. Adjust in `values.yaml` based on your cluster capacity:

```yaml
the0Api:
  resources:
    requests:
      memory: 512Mi
      cpu: 200m
    limits:
      memory: 1Gi
      cpu: 1000m
```

## Management Commands

```bash
# Check deployment status
make status

# View logs from all services
make logs

# Show service URLs
make services

# Remove deployment
make uninstall

# Remove deployment and namespace
make clean

# Check prerequisites
make check-deps
```

## Networking

### Minikube
- Uses LoadBalancer services accessible via minikube service URLs
- `make minikube-up` shows all service URLs automatically
- No tunnels or port forwarding needed - works immediately
- Example: `http://192.168.49.2:30587` for frontend

### Production Clusters
- Uses ClusterIP services by default for internal communication
- LoadBalancer services get external IPs from cloud providers
- Configure ingress in `values.yaml` for custom domain access

## Troubleshooting

### Common Issues

**Images not found:**
```bash
# For minikube, ensure images are built in minikube's Docker
eval $(minikube docker-env)
docker build -t the0-api:latest ../api
```

**Can't access services:**
```bash
# Check service URLs with
make services
# Use the minikube URLs shown (no tunnel needed)
```

**Pods stuck in ImagePullBackOff:**
```bash
# Check image pull policy
kubectl get pods -n the0
kubectl describe pod <pod-name> -n the0
```

**Service connectivity issues:**
```bash
# Check service endpoints
kubectl get endpoints -n the0
kubectl get svc -n the0
```

### Debug Commands

```bash
# View all resources
kubectl get all -n the0

# Describe failed pods
kubectl get pods -n the0 | grep -v Running
kubectl describe pod <pod-name> -n the0

# Check events
kubectl get events -n the0 --sort-by='.lastTimestamp'

# Test service connectivity
kubectl run debug --image=busybox -it --rm --restart=Never -- sh
# Inside pod: nslookup the0-api.the0.svc.cluster.local
```

## Development Workflow

1. **Local Development**: Run `make minikube-up` - automatically builds, deploys, and shows service URLs
2. **Code Changes**: Run `make minikube-restart` to rebuild and redeploy
3. **Testing**: Use `make logs`, `make status`, and `make services` to monitor
4. **Production**: Use `make deploy` with external infrastructure

## Comparison with Docker Compose

| Feature | Docker Compose | Kubernetes |
|---------|----------------|------------|
| **Command** | `docker-compose up` | `make minikube-up` |
| **Endpoints** | localhost:3000/3001 | minikube URLs (auto-shown) |
| **Setup** | Manual | Fully automated |
| **Infrastructure** | Included | Optional (configurable) |
| **Scaling** | Manual | Automatic + manual |
| **Health checks** | Basic | Advanced (liveness/readiness) |
| **Service discovery** | Container names | DNS-based |
| **Persistence** | Docker volumes | PersistentVolumes |
| **Production ready** | No | Yes |

The Kubernetes deployment provides **similar developer experience** to docker-compose with a single command, while adding production-ready features like automatic restarts, health checks, horizontal scaling, and deployment flexibility. Service URLs are automatically displayed after deployment.