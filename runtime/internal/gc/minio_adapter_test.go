package gc

import (
	"context"
	"errors"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// fakeBucketAPI simulates a bucket that can be created by a concurrent
// consumer between the existence check and the create call.
type fakeBucketAPI struct {
	exists         bool
	makeErr        error
	existsAfterErr bool // bucket becomes visible after MakeBucket fails
	makeCalls      int
}

func (f *fakeBucketAPI) BucketExists(ctx context.Context, bucket string) (bool, error) {
	return f.exists, nil
}

func (f *fakeBucketAPI) MakeBucket(ctx context.Context, bucket string, opts minio.MakeBucketOptions) error {
	f.makeCalls++
	if f.makeErr != nil {
		f.exists = f.existsAfterErr
		return f.makeErr
	}
	f.exists = true
	return nil
}

func TestEnsureBucket_CreatesMissingBucket(t *testing.T) {
	api := &fakeBucketAPI{exists: false}

	require.NoError(t, ensureBucket(context.Background(), api, "bot-logs"))
	assert.Equal(t, 1, api.makeCalls)
	assert.True(t, api.exists)
}

func TestEnsureBucket_SkipsCreateWhenBucketExists(t *testing.T) {
	api := &fakeBucketAPI{exists: true}

	require.NoError(t, ensureBucket(context.Background(), api, "bot-logs"))
	assert.Equal(t, 0, api.makeCalls)
}

func TestEnsureBucket_ToleratesConcurrentCreation(t *testing.T) {
	// Another consumer creates the bucket between BucketExists and MakeBucket;
	// the create fails but the bucket is there, which is all that matters.
	api := &fakeBucketAPI{
		exists:         false,
		makeErr:        errors.New("Your previous request to create the named bucket succeeded and you already own it."),
		existsAfterErr: true,
	}

	require.NoError(t, ensureBucket(context.Background(), api, "bot-logs"))
}

func TestEnsureBucket_ReturnsErrorWhenCreateFailsAndBucketMissing(t *testing.T) {
	api := &fakeBucketAPI{
		exists:  false,
		makeErr: errors.New("access denied"),
	}

	err := ensureBucket(context.Background(), api, "bot-logs")
	require.Error(t, err)
	assert.Contains(t, err.Error(), "access denied")
}
