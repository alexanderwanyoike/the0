# Bot Runner Deployment

## ⚡ Quick Deploy

```bash
# 1. Update image and MongoDB config in runtime.yaml
# 2. Deploy everything
kubectl apply -f runtime.yaml

# 3. Check status
kubectl get pods -n runtime
```

## 📁 Files

- **`runtime.yaml`** - Complete deployment configuration (everything you need!)
- **`DEPLOYMENT_GUIDE.md`** - Detailed deployment instructions and troubleshooting

## 🎯 What You Get

✅ **Native Autoscaler** - Zero external dependencies  
✅ **Leader Election** - High availability master-worker coordination  
✅ **Single-Segment Architecture** - Perfect 1:1 scaling  
✅ **Health Checks** - Kubernetes readiness/liveness probes  
✅ **RBAC** - Minimal required permissions  
✅ **Production Ready** - Resource limits, security contexts  

## 🔧 Configuration

**Required Changes:**
1. **Image**: Update `image: runtime:latest` with your registry
2. **MongoDB**: Update `MONGO_URI` in ConfigMap
3. **Segments**: Set `MAX_SEGMENT` for your workload

**Everything else works out of the box!**

## 📊 Monitoring

```bash
# Watch autoscaler in action
kubectl logs -f deployment/runtime -n runtime | grep Scaling

# Check metrics
kubectl port-forward svc/runtime-service 8080:8080 -n runtime
curl localhost:8080/metrics/segments
```

## ❓ Need Help?

See **`DEPLOYMENT_GUIDE.md`** for detailed instructions and troubleshooting.