# Bot Runner Deployment Guide

## Quick Start

### 1. Build and Push Docker Image
```bash
# Build the image
docker build -t runtime:latest .

# Tag for your registry (replace with your registry)
docker tag runtime:latest gcr.io/YOUR_PROJECT/runtime:latest

# Push to registry
docker push gcr.io/YOUR_PROJECT/runtime:latest
```

### 2. Update Configuration
Edit `deploy/runtime.yaml` and update these values:

```yaml
# Line 75: Update with your actual image
image: gcr.io/YOUR_PROJECT/runtime:latest

# Line 90-95: Update MongoDB connection
MONGO_URI: "mongodb://your-mongodb:27017"
DB_NAME: "your_database"
COLLECTION_NAME: "your_collection"

# Line 98: Adjust for your workload
MAX_SEGMENT: "64"  # Total number of segments
```

### 3. Deploy to Kubernetes
```bash
# Deploy everything
kubectl apply -f deploy/runtime.yaml

# Check deployment status
kubectl get pods -n runtime
kubectl get deployments -n runtime
```

### 4. Verify Native Autoscaler
```bash
# Check logs for autoscaler activity
kubectl logs -f deployment/runtime -n runtime | grep -i scaling

# Check metrics endpoint (if enabled)
kubectl port-forward service/runtime-service 8080:8080 -n runtime
curl http://localhost:8080/metrics/segments
```

## Configuration Options

### Required Environment Variables
| Variable | Description | Example |
|----------|-------------|---------|
| `POD_NAME` | Pod identifier (auto-set) | `runtime-abc123` |
| `NAMESPACE` | Kubernetes namespace (auto-set) | `runtime` |
| `MONGO_URI` | MongoDB connection string | `mongodb://mongo:27017` |
| `MAX_SEGMENT` | Total number of segments | `64` |

### Optional Environment Variables
| Variable | Default | Description |
|----------|---------|-------------|
| `ENABLE_METRICS_ENDPOINT` | `true` | Enable HTTP metrics at `/metrics/segments` |
| `LEASE_LOCK_NAME` | `runtime` | Leader election lock name |
| `DB_NAME` | `bot_scheduler` | MongoDB database name |
| `COLLECTION_NAME` | `bots` | MongoDB collection name |

### Scaling Configuration
The autoscaler uses these defaults (can be modified in code):
- **Min Replicas**: 3
- **Max Replicas**: 40
- **Check Interval**: 15 seconds
- **Cooldown Period**: 30 seconds

## Architecture Overview

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   MongoDB       │    │   Bot Runner     │    │  Docker Engine  │
│   (Segments)    │◄──►│   (Master +      │◄──►│   (Bot Exec)    │
│                 │    │    Workers)      │    │                 │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                               │
                               ▼
                       ┌──────────────────┐
                       │  Kubernetes API  │
                       │  (Auto Scaling)  │
                       └──────────────────┘
```

## Scaling Behavior

### Perfect 1:1 Scaling
```
10 active segments → 11 pods (10 workers + 1 master)
15 active segments → 16 pods (15 workers + 1 master)
5 active segments  → 6 pods (5 workers + 1 master)
```

### Scaling Events
- **Scale Up**: When `segment_backlog > 0` (unassigned segments)
- **Scale Down**: When segments are removed/completed
- **Worker Death**: Kubernetes restarts pod, autoscaler handles orphaned segments

## Monitoring

### Check Autoscaler Status
```bash
# View scaling decisions
kubectl logs deployment/runtime -n runtime | grep "Scaling from"

# Check current metrics
kubectl exec deployment/runtime -n runtime -- curl localhost:8080/metrics/segments

# Monitor deployment changes
kubectl get deployment runtime -n runtime -w
```

### Key Metrics
```json
{
  "total_segments": 10,      // Total segments in system
  "assigned_segments": 8,    // Segments assigned to workers
  "orphaned_segments": 0,    // Segments from dead workers
  "available_workers": 8,    // Active worker pods
  "segment_backlog": 2       // Unassigned segments (triggers scaling)
}
```

## Troubleshooting

### Autoscaler Not Working
```bash
# Check RBAC permissions
kubectl auth can-i update deployments --as=system:serviceaccount:runtime:runtime

# Check leader election
kubectl logs deployment/runtime -n runtime | grep -i leader

# Check autoscaler initialization
kubectl logs deployment/runtime -n runtime | grep -i autoscaler
```

### No Scaling Activity
```bash
# Check segment metrics
curl http://runtime-service.runtime:8080/metrics/segments

# Verify cooldown period
kubectl logs deployment/runtime -n runtime | grep cooldown

# Check for errors
kubectl logs deployment/runtime -n runtime | grep -i error
```

### Pods Not Starting
```bash
# Check pod status
kubectl describe pods -n runtime

# Check service account permissions
kubectl get serviceaccount runtime -n runtime -o yaml

# Check resource limits
kubectl top pods -n runtime
```

## Production Considerations

### Security
- [ ] Use non-root containers when possible
- [ ] Network policies for pod-to-pod communication
- [ ] Secret management for MongoDB credentials
- [ ] RBAC least-privilege principle

### Performance
- [ ] Adjust resource requests/limits based on workload
- [ ] Monitor autoscaler performance in production
- [ ] Consider node affinity for Docker-in-Docker workloads

### High Availability
- [ ] Deploy across multiple availability zones
- [ ] Use persistent volumes for any stateful components
- [ ] Monitor leader election stability

## Uninstall
```bash
kubectl delete -f deploy/runtime.yaml
```

This removes all resources including the namespace, deployments, and RBAC configurations.