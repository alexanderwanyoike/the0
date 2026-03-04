package docker

import (
	"context"
	"testing"

	"runtime/internal/util"

	"github.com/stretchr/testify/assert"
)

func TestDependencyChecker_NilClients(t *testing.T) {
	dc := NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})

	mongoOK, minioOK := dc.CheckHealth(context.Background())
	assert.False(t, mongoOK, "nil mongo client should be unhealthy")
	assert.False(t, minioOK, "nil minio client should be unhealthy")
}

func TestDependencyChecker_IsHealthy_CachedResult(t *testing.T) {
	dc := NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})

	// Default is optimistic (true)
	assert.True(t, dc.IsHealthy())

	// Set both unhealthy
	dc.SetLastResult(false, false)
	assert.False(t, dc.IsHealthy())

	// Mongo OK but MinIO not
	dc.SetLastResult(true, false)
	assert.False(t, dc.IsHealthy())

	// MinIO OK but Mongo not
	dc.SetLastResult(false, true)
	assert.False(t, dc.IsHealthy())

	// Both OK
	dc.SetLastResult(true, true)
	assert.True(t, dc.IsHealthy())
}

func TestDependencyChecker_CheckHealth_UpdatesNothing(t *testing.T) {
	// CheckHealth returns results but does NOT update the cached state.
	// The caller must call SetLastResult explicitly.
	dc := NewDependencyChecker(nil, nil, "test-bucket", &util.NopLogger{})

	// Cached state starts optimistic
	assert.True(t, dc.IsHealthy())

	// CheckHealth with nil clients returns false, false
	mongoOK, minioOK := dc.CheckHealth(context.Background())
	assert.False(t, mongoOK)
	assert.False(t, minioOK)

	// But cached state is still true because SetLastResult wasn't called
	assert.True(t, dc.IsHealthy())

	// Now update cached state
	dc.SetLastResult(mongoOK, minioOK)
	assert.False(t, dc.IsHealthy())
}
