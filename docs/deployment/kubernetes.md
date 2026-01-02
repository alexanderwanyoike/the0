---
title: "Kubernetes"
description: "Deploy the0 platform on Kubernetes with Helm"
tags: ["deployment", "kubernetes", "helm", "production"]
order: 2
---

# Kubernetes Deployment

::: warning Experimental
The Kubernetes deployment has been tested previously but may require updates. If you encounter issues, please report them on [GitHub](https://github.com/alexanderwanyoike/the0/issues). For a fully tested deployment path, use [Docker Compose](./docker-compose).
:::

Kubernetes deployment provides production-ready orchestration for the0 platform. Use Kubernetes mode when:

- Deploying across multiple nodes
- High availability is required
- You need K8s-native observability and management

The runtime uses a **controller pattern** in Kubernetes mode: each bot becomes its own Pod, and the controller manages the lifecycle by comparing desired state (MongoDB) with actual state (Pods) and making corrections.

## Prerequisites

### Required Tools

- **Docker** - For building container images ([install guide](https://docs.docker.com/get-docker/))
- **kubectl** - Kubernetes CLI ([install guide](https://kubernetes.io/docs/tasks/tools/))
- **Helm 3.0+** - Kubernetes package manager ([install guide](https://helm.sh/docs/intro/install/))
- **minikube** (local development) - Local Kubernetes cluster ([install guide](https://minikube.sigs.k8s.io/docs/start/))

### Cluster Requirements

**Minikube (development)**:
- 4GB RAM minimum
- 4 CPU cores
- 20GB+ disk space

**Production clusters**:
- 8+ CPU cores across nodes
- 16GB+ RAM
- 100GB+ disk for persistent volumes
- Worker nodes with Docker socket access

## Quick Start with Minikube

For local development, minikube provides the simplest path to a running cluster:

```bash
cd k8s

# Single command - starts minikube, builds images, deploys everything
make minikube-up
```

The command performs these steps:

1. Checks prerequisites (minikube, docker, helm, kubectl)
2. Starts minikube with appropriate resources
3. Builds all Docker images in minikube's environment
4. Deploys all services via Helm
5. Shows service URLs

After deployment, configure local DNS:

```bash
make setup-hosts
```

This adds entries to `/etc/hosts` for the `.local` domains.

### Minikube Endpoints

| Service | URL | Description |
|---------|-----|-------------|
| Frontend | http://the0.local:30001 | Web dashboard |
| API | http://api.the0.local:30000 | REST API |
| MinIO Console | http://minio.the0.local:30002 | Object storage admin |
| Documentation | http://docs.the0.local:30004 | Platform docs |

## Production Deployment

For production clusters, deploy with external infrastructure:

```bash
cd k8s

# Deploy with default values
make deploy

# Or deploy with custom values
helm install the0 . --namespace the0 --create-namespace \
  --values production-values.yaml
```

### Using External Infrastructure

In production, use managed services for databases and storage. Disable internal infrastructure in `values.yaml`:

```yaml
postgresql:
  enabled: false
mongodb:
  enabled: false
nats:
  enabled: false
minio:
  enabled: false
```

Configure connection strings for external services:

```yaml
the0Api:
  env:
    DATABASE_URL: "postgresql://user:pass@your-rds-instance:5432/the0"
    NATS_URLS: "nats://your-nats-cluster:4222"
    MINIO_ENDPOINT: "your-s3-endpoint"
    # ... other configuration
```

## Helm Configuration

The chart is configured through `values.yaml`. Key sections:

### Global Settings

```yaml
global:
  imagePullPolicy: Never  # Never (local), Always (registry), IfNotPresent
  storageClass: ""        # Leave empty for default, or specify class name
```

### Infrastructure Services

Each infrastructure service can be enabled/disabled and configured:

```yaml
postgresql:
  enabled: true
  image: postgres:15-alpine
  port: 5432
  persistence:
    enabled: true
    size: 10Gi
  resources:
    requests:
      memory: 256Mi
      cpu: 100m
    limits:
      memory: 512Mi
      cpu: 500m
```

Similar blocks exist for MongoDB, NATS, and MinIO.

### Application Services

Application services have environment variables matching the Docker Compose configuration:

```yaml
the0Api:
  enabled: true
  replicas: 1
  image:
    repository: the0-api
    tag: latest
  port: 3000
  env:
    DATABASE_URL: "postgresql://the0:the0_password@the0-postgres:5432/the0_oss"
    JWT_SECRET: "your-jwt-secret-key"
    # ... other environment variables
  resources:
    requests:
      memory: 512Mi
      cpu: 200m
    limits:
      memory: 1Gi
      cpu: 1000m
```

### Runtime Controller Configuration

In Kubernetes mode, the runtime runs as a controller that manages bots as Pods:

```yaml
runtimeController:
  enabled: true
  image:
    repository: runtime
    tag: latest
  replicas: 1
  resources:
    requests:
      memory: 256Mi
      cpu: 100m
    limits:
      memory: 512Mi
      cpu: 500m
  env:
    NAMESPACE: the0
    RECONCILE_INTERVAL: "30s"
    MINIO_ENDPOINT: "the0-minio:9000"
```

The controller creates a Pod for each enabled bot. Bot Pods run with resource limits and are automatically restarted by Kubernetes if they fail.

### External Access

For minikube, NodePort services provide external access:

```yaml
externalServices:
  enabled: true
  nodePort:
    frontend: 30001
    api: 30000
    minio: 30002
    docs: 30004
```

For production clusters, use Ingress instead:

```yaml
ingress:
  enabled: true
  className: nginx
  hosts:
    - host: the0.yourdomain.com
      paths:
        - path: /
          service: the0-frontend
```

## Management Commands

The Makefile provides commands for cluster management:

```bash
# Check deployment status
make status

# View service URLs
make services

# View logs from all services
make logs

# Pause minikube (preserves state)
make minikube-pause

# Resume paused minikube
make minikube-resume

# Stop minikube
make minikube-stop

# Remove deployment, keep cluster
make minikube-down

# Full cleanup
make clean
```

## Scaling

### Bot Scaling

In Kubernetes mode, each bot runs as its own Pod. The controller automatically creates and removes Pods based on the enabled bots in MongoDB. There's no need to manually scale workers - Kubernetes handles scheduling across nodes.

### API Scaling

Scale the API service by adjusting replica counts:

```yaml
the0Api:
  replicas: 3
```

Apply changes:

```bash
helm upgrade the0 . --namespace the0
```

### Vertical Scaling

Adjust resource limits based on workload:

```yaml
the0Api:
  resources:
    requests:
      memory: 1Gi
      cpu: 500m
    limits:
      memory: 2Gi
      cpu: 2000m
```

## Health Checks

All services include liveness and readiness probes. The Helm chart configures these automatically based on each service's health endpoints.

API and frontend services expose HTTP health endpoints. Runtime services expose `/healthz` endpoints for Kubernetes to monitor.

## Networking

### Internal Communication

Services communicate internally via Kubernetes DNS:

- `the0-api.the0.svc.cluster.local:3000` - API service
- `the0-postgres.the0.svc.cluster.local:5432` - PostgreSQL
- `the0-nats.the0.svc.cluster.local:4222` - NATS

### External Access

**Minikube**: Uses NodePort services with fixed ports, accessed via `.local` domains after configuring `/etc/hosts`.

**Production**: Use an Ingress controller (nginx, traefik, AWS ALB) for external access. Configure TLS termination at the Ingress level.

## Troubleshooting

### Pods Not Starting

Check pod status and events:

```bash
kubectl get pods -n the0
kubectl describe pod <pod-name> -n the0
```

### Image Pull Failures

For local images, ensure minikube's Docker environment was used:

```bash
eval $(minikube docker-env)
docker images | grep the0
```

### Service Connectivity

Test internal DNS resolution:

```bash
kubectl run debug --image=busybox -it --rm --restart=Never -n the0 -- sh
# Inside pod:
nslookup the0-api.the0.svc.cluster.local
```

### View Events

```bash
kubectl get events -n the0 --sort-by='.lastTimestamp'
```

### Debug Pods

```bash
kubectl exec -it <pod-name> -n the0 -- sh
kubectl logs <pod-name> -n the0
kubectl logs <pod-name> -n the0 --previous  # Previous container logs
```

## Docker Compose vs Kubernetes

| Feature | Docker Compose | Kubernetes |
|---------|----------------|------------|
| Setup command | `make up` | `make minikube-up` |
| Bot model | Containers on single host | Pod per bot |
| Recommended for | Single-host deployments | Multi-node, HA |
| Infrastructure | Included | Included (configurable) |
| Bot scaling | Single-process reconciliation | Controller + Pods |
| Health checks | Application level | Liveness + readiness probes |
| Service discovery | Container names | DNS-based |
| Persistence | Docker volumes | PersistentVolumes |
| Complexity | Simple | More complex |

Use Docker Compose for development and single-host production. Use Kubernetes when you need multi-node scaling or high availability.
