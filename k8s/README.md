[![Artifact Hub](https://img.shields.io/endpoint?url=https://artifacthub.io/badge/repository/the0)](https://artifacthub.io/packages/search?repo=the0)

# the0 Kubernetes Deployment

This directory contains Helm charts and configuration for deploying the0 platform on Kubernetes, providing the same functionality as the docker-compose setup with production-ready orchestration.

## Quick Start

### Install from Helm Repository

The chart is available from the public Helm repository:

```bash
helm repo add the0 https://alexanderwanyoike.github.io/the0
helm repo update
```

Do not install it into a production cluster without a real values file. the0
needs operator-managed PostgreSQL, MongoDB, S3-compatible object storage, JWT
signing configuration, and a deployment-managed root admin. The chart can run
NATS in the cluster, or you can point it at an external NATS service. Use a
Secret workflow such as Sealed Secrets or External Secrets.

See [Kubernetes Deployment](../docs/deployment/kubernetes.md) for the full
production guide and [Root Admin Configuration](../docs/deployment/admin-bootstrap.md)
for the root admin behavior.

### Minikube (Local Development)

Use the docs guide for the tested minikube walkthrough. It uses direct
`minikube`, `docker`, `kubectl`, and `helm` commands, and runs PostgreSQL,
MongoDB, NATS, and MinIO inside minikube:

[Kubernetes Minikube Quick Start](../docs/deployment/kubernetes.md#quick-start-with-minikube)

### Production Cluster

Use the docs guide for production. You provide PostgreSQL, MongoDB, and
S3-compatible object storage, then install the chart with your values and
Secrets:

[Kubernetes Production Deployment](../docs/deployment/kubernetes.md#production-deployment)

## Prerequisites

### Required Tools

The docs walkthrough uses these tools directly:

- **Docker** - For building local images ([install guide](https://docs.docker.com/get-docker/))
- **kubectl** - Kubernetes CLI tool ([install guide](https://kubernetes.io/docs/tasks/tools/))
- **Helm 3.0+** - Kubernetes package manager ([install guide](https://helm.sh/docs/intro/install/))
- **minikube** - For local development ([install guide](https://minikube.sigs.k8s.io/docs/start/))

### Minikube Requirements
- Minimum 4GB RAM and 4 CPUs allocated to minikube
- 20GB+ disk space
- Docker driver (recommended)

### Runtime Image

Bot execution in Kubernetes mode uses the runtime image configured by
`botController.runtimeImage`. The minikube guide builds that image directly
inside minikube's Docker daemon:

```bash
eval "$(minikube docker-env)"
docker build -t runtime:latest ../runtime
```

For production, publish the runtime image to a registry your cluster can pull
from and set `botController.runtimeImage` or `botController.image.repository`
in your values file.

**Don't have these installed?** Install them first, then follow the docs guide.

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
│   ├── the0-docs.yaml      # Documentation site
│   ├── bot-controller.yaml # Kubernetes-native bot controller
│   ├── gc-cronjob.yaml     # Bot garbage collection CronJob
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
- **the0-docs** - Documentation site - port 8080
- **bot-controller** - Kubernetes-native bot and schedule controller - port 9477

**External Access** (via NodePort):
- Frontend: NodePort 30001 → the0.local:30001
- API: NodePort 30000 → api.the0.local:30000  
- MinIO Console: NodePort 30002 → minio.the0.local:30002

## Installation Methods

### 1. Minikube (Recommended for Development)

Use the docs guide for a tested minikube walkthrough with chart-managed
PostgreSQL, MongoDB, NATS, and MinIO:

[Kubernetes Minikube Quick Start](../docs/deployment/kubernetes.md#quick-start-with-minikube)

**Endpoints (via .local domains):**
- Frontend: http://the0.local:30001
- API: http://api.the0.local:30000
- MinIO Console: http://minio.the0.local:30002

**Note:** The runtime controller is an internal service accessed via the API.

The docs walkthrough starts minikube, builds local images in minikube's Docker
daemon, creates the root admin Secret, deploys the chart with Helm, and shows
the checks to run with `kubectl`.

**Required setup step:**
Follow the `/etc/hosts` step in the [Kubernetes Minikube Quick Start](../docs/deployment/kubernetes.md#quick-start-with-minikube).

### 2. Production Cluster with External Infrastructure

For production deployments where you provide your own databases and services,
follow [Kubernetes Production Deployment](../docs/deployment/kubernetes.md#production-deployment).

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

Set the deployment-managed root admin email and provide the password from a Secret:

```yaml
the0Api:
  env:
    THE0_ADMIN_EMAIL: "admin@example.com"
  extraEnv:
    - name: THE0_ADMIN_PASSWORD
      valueFrom:
        secretKeyRef:
          name: the0-root-admin
          key: password
```

`the0Api.env` remains a string map for simple values. `the0Api.extraEnv` accepts full Kubernetes `EnvVar` entries for `secretKeyRef`, including secrets created by Sealed Secrets. Keep `THE0_ADMIN_PASSWORD` configured as the root admin password source; rotating the Secret and rolling out the API rotates the root admin password. See `docs/deployment/admin-bootstrap.md` for the full root admin flow and last-admin protection.

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
kubectl get deploy -n the0
kubectl get pods -n the0

# View service URLs
kubectl get svc -n the0
minikube service list

# View logs
kubectl logs -n the0 deploy/the0-api
kubectl logs -n the0 deploy/the0-bot-controller

# Pause minikube (saves resources, preserves everything)
minikube pause

# Resume paused minikube
minikube unpause

# Stop minikube (saves more resources than pause)
minikube stop

# Start stopped minikube
minikube start --driver=docker

# Remove deployment but keep minikube and the namespace
helm uninstall the0 -n the0

# Remove deployment and namespace
helm uninstall the0 -n the0 --ignore-not-found
kubectl delete namespace the0

# Full local cleanup
minikube delete
```

## Networking

### Minikube
- Uses NodePort services with fixed ports for predictable access
- External services available via .local domains:
  - Frontend: the0.local:30001 (NodePort 30001)
  - API: api.the0.local:30000 (NodePort 30000) 
  - MinIO Console: minio.the0.local:30002 (NodePort 30002)
- Requires `/etc/hosts` entries for the minikube IP. Follow the
  [docs step](../docs/deployment/kubernetes.md#quick-start-with-minikube).
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
# Check your internet connection and Docker daemon
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
# Manually add to /etc/hosts:
MINIKUBE_IP="$(minikube ip)"
sudo tee -a /etc/hosts >/dev/null <<EOF
$MINIKUBE_IP the0.local
$MINIKUBE_IP api.the0.local
$MINIKUBE_IP minio.the0.local
$MINIKUBE_IP docs.the0.local
EOF
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

1. **Local Development**: Follow the [minikube guide](../docs/deployment/kubernetes.md#quick-start-with-minikube) to build images, deploy with Helm, and verify with `kubectl`.
2. **Code Changes**: Rebuild the changed image inside minikube's Docker daemon, then run `helm upgrade --install` with the same values and `--set` flags from the guide.
3. **Testing**: Use `kubectl get pods`, `kubectl logs`, `kubectl get svc`, and the HTTP health checks from the docs.
4. **Production**: Follow the [production guide](../docs/deployment/kubernetes.md#production-deployment) with external infrastructure and Secret-backed credentials.

## Kubernetes-Native Controller Mode

Kubernetes deployments use the runtime controller by default. The controller
manages bots directly:

- Each bot runs as its own Kubernetes Pod
- Scheduled bots use native Kubernetes CronJobs
- Kaniko builds bot images automatically (no Docker-in-Docker)
- No privileged containers required
- Better security and Kubernetes integration

The chart enables controller mode through `botController.enabled`.

### Controller Mode Benefits

| Feature | Kubernetes Controller |
|---------|-----------------------|
| **Bot Isolation** | K8s Pods |
| **Scheduled Bots** | K8s CronJobs |
| **Image Building** | Kaniko Jobs |
| **Resource Limits** | K8s ResourceQuota |
| **Monitoring** | `kubectl logs` |
| **Scaling** | Kubernetes handles scheduling |

### Controller Mode Commands

Deploy controller mode with the full `helm upgrade --install` command in the
[minikube guide](../docs/deployment/kubernetes.md#quick-start-with-minikube).

```bash
# View controller logs
kubectl logs -n the0 deploy/the0-bot-controller

# Enable the minikube registry addon if your values use it
minikube addons enable registry
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

## Flux Configuration

If using Flux for GitOps, set a short reconciliation interval on the HelmRepository to pick up new chart versions quickly:

```yaml
apiVersion: source.toolkit.fluxcd.io/v1beta2
kind: HelmRepository
metadata:
  name: the0
  namespace: flux-system
spec:
  type: "oci"
  interval: 5m
  url: oci://ghcr.io/alexanderwanyoike/the0/charts
```

All deployment pod templates include a `checksum/chart-version` annotation that changes with each chart version. This ensures Kubernetes detects a diff and triggers a rolling update whenever Flux reconciles a new chart version, preventing stale pods from running after an upgrade.

## Comparison with Docker Compose

| Feature | Docker Compose | Kubernetes |
|---------|----------------|------------|
| **Command** | `the0 local start` | `minikube` + `kubectl` + `helm` |
| **Endpoints** | localhost:3000/3001 | the0.local:30001, api.the0.local:30000 |
| **Setup** | CLI init + start | Root admin secret + values.yaml + hosts setup |
| **Infrastructure** | Included | Included (configurable) |
| **Scaling** | Manual | Automatic + manual |
| **Health checks** | Basic | Advanced (liveness/readiness) |
| **Service discovery** | Container names | DNS-based |
| **Persistence** | Docker volumes | PersistentVolumes |
| **Production ready** | Development only | Yes |
| **Resource limits** | Manual | Automatic |
| **Restart policies** | Basic | Advanced |

The Kubernetes deployment provides predictable endpoints for local development,
while adding production-ready features like automatic restarts, health checks,
horizontal scaling, and deployment flexibility.
