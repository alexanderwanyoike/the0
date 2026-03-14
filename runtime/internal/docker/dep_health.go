package docker

import (
	"context"
	"time"

	"runtime/internal/k8s/health"
	"runtime/internal/runtime/storage"
	"runtime/internal/util"

	"github.com/minio/minio-go/v7"
	"go.mongodb.org/mongo-driver/mongo"
)

// newDependencyCheckerFromEnv creates a DependencyChecker with a MinIO client
// loaded from environment. If MinIO config is missing, the checker treats
// MinIO as permanently unhealthy (safe default).
func newDependencyCheckerFromEnv(mongoClient *mongo.Client, logger util.Logger) *DependencyChecker {
	var minioClient *minio.Client
	minioBucket := "custom-bots"

	cfg, err := storage.LoadConfigFromEnv()
	if err != nil {
		logger.Info("MinIO config not available for health checks: %v", err)
	} else {
		mc, err := storage.NewMinioClient(cfg)
		if err != nil {
			logger.Error("Failed to create MinIO client for health checks: %v", err)
		} else {
			minioClient = mc
			minioBucket = cfg.CodeBucket
		}
	}

	return NewDependencyChecker(mongoClient, minioClient, minioBucket, logger)
}

// probeDepHealth performs a synchronous health check and updates the
// readiness probe on the health server. Returns whether deps are healthy.
func probeDepHealth(ctx context.Context, dc *DependencyChecker, hs *health.Server) bool {
	mongoOK, minioOK := dc.CheckHealth(ctx)
	dc.SetLastResult(mongoOK, minioOK)
	depsHealthy := mongoOK && minioOK
	if hs != nil {
		hs.SetReady(depsHealthy)
	}
	return depsHealthy
}

// runDepHealthLoop periodically checks dependency health and updates the
// readiness probe. Liveness is never touched — only readiness is driven by
// dependency state.
func runDepHealthLoop(ctx context.Context, interval time.Duration, dc *DependencyChecker, hs *health.Server, logger util.Logger, done func()) {
	defer done()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			mongoOK, minioOK := dc.CheckHealth(ctx)
			dc.SetLastResult(mongoOK, minioOK)
			if !mongoOK || !minioOK {
				logger.Error("Dependencies unhealthy (mongo=%v, minio=%v)", mongoOK, minioOK)
				if hs != nil {
					hs.SetReady(false)
				}
			} else {
				if hs != nil {
					hs.SetReady(true)
				}
			}
		case <-ctx.Done():
			return
		}
	}
}
