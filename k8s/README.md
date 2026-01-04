# the0 Kubernetes Deployment

This directory contains Helm charts and configuration for deploying the0 platform on Kubernetes, providing the same functionality as the docker-compose setup with production-ready orchestration.

## Quick Start

### Minikube (Local Development)

**Starting from absolute zero (minikube not running):**

```bash
# Single command - starts minikube, builds images, deploys everything
make minikube-up

# Services will be available at:
# - Frontend:    http://the0.local:30001
# - API:         http://api.the0.local:30000  
# - MinIO Console: http://minio.the0.local:30002

# Set up local domain names (required once)
make setup-hosts

# That's it! No tunnel required.
```

**If you already have minikube running:**
```bash
# Just deploy (faster, skips minikube start)
make minikube-up
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

The `make minikube-up` command will check for these tools and provide install links if missing:

- **Docker** - For building local images ([install guide](https://docs.docker.com/get-docker/))
- **kubectl** - Kubernetes CLI tool ([install guide](https://kubernetes.io/docs/tasks/tools/))
- **Helm 3.0+** - Kubernetes package manager ([install guide](https://helm.sh/docs/intro/install/))
- **minikube** - For local development ([install guide](https://minikube.sigs.k8s.io/docs/start/))

### Minikube Requirements
- Minimum 4GB RAM and 4 CPUs allocated to minikube
- 20GB+ disk space
- Docker driver (recommended)

### Runtime Images

Bot execution requires runtime images (`the0/python311`, `the0/nodejs20`, etc.) to be available. For minikube:

```bash
cd docker/images
make minikube-build-all
```

This builds the images directly into minikube's cache - no registry required.

**Don't have these installed?** Run `make minikube-up` anyway - it will tell you exactly what to install!

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
│   ├── bot-runner.yaml     # Bot execution service (master + workers)
│   ├── bot-scheduler.yaml  # Bot scheduling service (master + workers)
│   ├── external-services.yaml # NodePort services for .local access
│   ├── ingress.yaml        # Ingress configuration (optional)
│   └── _helpers.tpl        # Helm template helpers
└── README.md              # This file
```

### Architecture Overview

**Infrastructure Services** (can be disabled for production):
- **PostgreSQL** - Main application database (port 5432)
- **MongoDB** - Bot runtime data and metrics (port 27017)
- **NATS with JetStream** - Message broker for service communication (port 4222)
- **MinIO** - S3-compatible object storage for logs and assets (port 9000, console 9001)

**Application Services**:
- **the0-api** - REST API server (NestJS/TypeScript) - port 3000
- **the0-frontend** - Web interface (Next.js/React) - port 3000
- **the0-analyzer** - Security analysis service (Python) - background service
- **bot-runner** - Bot execution runtime (Go) - HTTP port 8080, gRPC port 50051
- **bot-scheduler** - Bot scheduling service (Go) - HTTP port 8080, gRPC port 50053

**External Access** (via NodePort):
- Frontend: NodePort 30001 → the0.local:30001
- API: NodePort 30000 → api.the0.local:30000  
- MinIO Console: NodePort 30002 → minio.the0.local:30002

## Installation Methods

### 1. Minikube (Recommended for Development)

Deploy with NodePort services for easy access:

```bash
# Build runtime images into minikube (required for bot execution)
cd docker/images
make minikube-build-all

# Then deploy
cd ../../k8s
make minikube-up
```

Or if `make minikube-up` handles image building automatically, just run:

```bash
make minikube-up
```

**Endpoints (via .local domains):**
- Frontend: http://the0.local:30001
- API: http://api.the0.local:30000
- MinIO Console: http://minio.the0.local:30002

**Note:** Runtime services (bot-runner, bot-scheduler) are internal services accessed via the API.

The command will:
1. Check prerequisites (minikube, docker, helm, kubectl)
2. Start minikube (if not running) with 4GB RAM and 4 CPUs
3. Build all Docker images in minikube environment  
4. Deploy all services with Helm using NodePort
5. Show service URLs for access

**Required setup step:**
```bash
# Add .local domains to /etc/hosts (required once)
make setup-hosts
```

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

### 3. Manual Helm Deployment

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

# Pause minikube (saves resources, preserves everything)
make minikube-pause

# Resume paused minikube
make minikube-resume

# Stop minikube (saves more resources than pause)
make minikube-stop

# Start stopped minikube
make minikube-start

# Remove deployment but keep minikube
make minikube-down

# Remove deployment and namespace
make clean

# Check prerequisites
make check-deps
```

## Networking

### Minikube
- Uses NodePort services with fixed ports for predictable access
- External services available via .local domains:
  - Frontend: the0.local:30001 (NodePort 30001)
  - API: api.the0.local:30000 (NodePort 30000) 
  - MinIO Console: minio.the0.local:30002 (NodePort 30002)
- Requires `/etc/hosts` setup via `make setup-hosts`
- No tunnels or port forwarding needed

### Production Clusters
- Uses ClusterIP services by default for internal communication
- External access via ingress controllers or LoadBalancer services
- Configure ingress in `values.yaml` for custom domain access
- Set `externalServices.enabled: false` to disable NodePort services

## Troubleshooting

### Starting from Zero Issues

**"❌ kubectl not configured or cluster unreachable"**
- This should no longer happen with the updated Makefile
- If you see this, minikube failed to start - check Docker is running

**"❌ Minikube is required"**
```bash
# Install minikube first
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube
```

**"❌ Docker is required"**
```bash
# Install Docker first
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER
# Log out and back in
```

**Network issues / Docker Hub timeouts:**
```bash
# Wait for network to improve and try again
make minikube-up

# Or check your internet connection and Docker daemon
docker run hello-world
```

**Minikube won't start:**
```bash
# Check Docker is running
sudo systemctl start docker

# Reset minikube if needed
minikube delete
minikube start --driver=docker
```

### Common Issues

**Images not found:**
```bash
# For minikube, ensure images are built in minikube's Docker
eval $(minikube docker-env)
docker build -t the0-api:latest ../api
```

**Can't access services (domains don't resolve):**
```bash
# Run the hosts setup command
make setup-hosts

# Or manually add to /etc/hosts:
echo "$(minikube ip) the0.local api.the0.local minio.the0.local" | sudo tee -a /etc/hosts
```

**Pods stuck in ImagePullBackOff:**
```bash
# Check image pull policy
kubectl get pods -n the0
kubectl describe pod <pod-name> -n the0
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

## Kubernetes-Native Controller Mode

The0 supports two runtime modes for bot execution:

### Docker Mode (Default)
The traditional master-worker architecture using Docker-in-Docker:
- Workers run bot containers inside Kubernetes pods
- Requires privileged pods with Docker socket access
- Good for development and testing

```bash
make minikube-up  # Uses Docker mode by default
```

### Controller Mode (Kubernetes-Native)
A Kubernetes-native approach where the controller manages bots directly:
- Each bot runs as its own Kubernetes Pod
- Scheduled bots use native Kubernetes CronJobs
- Kaniko builds bot images automatically (no Docker-in-Docker)
- No privileged containers required
- Better security and Kubernetes integration

```bash
make minikube-controller  # Deploys in controller mode
```

### Controller Mode Benefits

| Feature | Docker Mode | Controller Mode |
|---------|-------------|-----------------|
| **Privileged Pods** | Required | Not required |
| **Bot Isolation** | Docker containers | K8s Pods |
| **Scheduled Bots** | Cron in worker | K8s CronJobs |
| **Image Building** | Docker in worker | Kaniko Jobs |
| **Resource Limits** | Docker limits | K8s ResourceQuota |
| **Monitoring** | Docker logs | kubectl logs |
| **Scaling** | Add workers | K8s handles it |

### Controller Mode Commands

```bash
# Deploy in controller mode
make minikube-controller

# View controller logs
make logs-controller

# Enable registry addon (for bot images)
make enable-registry
```

### How Controller Mode Works

1. **Bot Controller** reads enabled bots from MongoDB
2. For each bot, it ensures a matching Pod exists
3. If the bot image doesn't exist, Kaniko builds it automatically
4. Config changes trigger Pod recreation
5. Deleted bots have their Pods removed

**Schedule Controller** does the same for scheduled bots using CronJobs.

### Configuration

Enable controller mode in `values.yaml`:

```yaml
# Disable Docker mode
botRunner:
  enabled: false
botScheduler:
  enabled: false

# Enable controller mode
botController:
  enabled: true
  imageBuilder:
    enabled: true
    registry: "localhost:5000"  # Minikube registry
```

## Comparison with Docker Compose

| Feature | Docker Compose | Kubernetes |
|---------|----------------|------------|
| **Command** | `make up` | `make minikube-up` |
| **Endpoints** | localhost:3000/3001 | the0.local:30001, api.the0.local:30000 |
| **Setup** | Single command | Single command + hosts setup |
| **Infrastructure** | Included | Included (configurable) |
| **Scaling** | Manual | Automatic + manual |
| **Health checks** | Basic | Advanced (liveness/readiness) |
| **Service discovery** | Container names | DNS-based |
| **Persistence** | Docker volumes | PersistentVolumes |
| **Production ready** | Development only | Yes |
| **Resource limits** | Manual | Automatic |
| **Restart policies** | Basic | Advanced |

The Kubernetes deployment provides **similar developer experience** to docker-compose with predictable endpoints and single-command deployment, while adding production-ready features like automatic restarts, health checks, horizontal scaling, and deployment flexibility.