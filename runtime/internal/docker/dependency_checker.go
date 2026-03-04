package docker

import (
	"context"
	"sync"
	"sync/atomic"
	"time"

	"runtime/internal/util"

	"github.com/minio/minio-go/v7"
	"go.mongodb.org/mongo-driver/mongo"
)

// DependencyChecker verifies that MinIO and MongoDB are reachable
// before the reconciler acts. It caches the last result so the
// reconciliation loop can read health without network calls.
type DependencyChecker struct {
	mongoClient *mongo.Client
	minioClient *minio.Client
	minioBucket string
	logger      util.Logger

	lastMongoOK atomic.Bool
	lastMinioOK atomic.Bool
	mu          sync.Mutex
}

// NewDependencyChecker creates a DependencyChecker.
// Both clients may be nil — a nil client is treated as permanently unhealthy.
func NewDependencyChecker(mongoClient *mongo.Client, minioClient *minio.Client, minioBucket string, logger util.Logger) *DependencyChecker {
	dc := &DependencyChecker{
		mongoClient: mongoClient,
		minioClient: minioClient,
		minioBucket: minioBucket,
		logger:      logger,
	}
	// Start optimistic: assume healthy until first check proves otherwise.
	// This avoids blocking the first reconciliation cycle if deps are actually up.
	dc.lastMongoOK.Store(true)
	dc.lastMinioOK.Store(true)
	return dc
}

// CheckHealth performs the actual network calls to verify dependencies.
// Returns (mongoOK, minioOK). Thread-safe but serialises checks.
func (dc *DependencyChecker) CheckHealth(ctx context.Context) (mongoOK, minioOK bool) {
	dc.mu.Lock()
	defer dc.mu.Unlock()

	mongoOK = dc.checkMongo(ctx)
	minioOK = dc.checkMinio(ctx)
	return mongoOK, minioOK
}

// IsHealthy returns the cached result of the last CheckHealth call.
// This is cheap to call from the reconciliation hot path.
func (dc *DependencyChecker) IsHealthy() bool {
	return dc.lastMongoOK.Load() && dc.lastMinioOK.Load()
}

// SetLastResult updates the cached health state.
func (dc *DependencyChecker) SetLastResult(mongoOK, minioOK bool) {
	dc.lastMongoOK.Store(mongoOK)
	dc.lastMinioOK.Store(minioOK)
}

func (dc *DependencyChecker) checkMongo(ctx context.Context) bool {
	if dc.mongoClient == nil {
		return false
	}
	checkCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	if err := dc.mongoClient.Ping(checkCtx, nil); err != nil {
		dc.logger.Error("MongoDB health check failed: %v", err)
		return false
	}
	return true
}

func (dc *DependencyChecker) checkMinio(ctx context.Context) bool {
	if dc.minioClient == nil {
		return false
	}
	checkCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	if _, err := dc.minioClient.BucketExists(checkCtx, dc.minioBucket); err != nil {
		dc.logger.Error("MinIO health check failed: %v", err)
		return false
	}
	return true
}
