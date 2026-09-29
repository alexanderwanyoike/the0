package s3test

import (
	"bytes"
	"context"
	"io"
	"testing"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestStartT_ServesBucketAndObjectOperations(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping integration test")
	}

	server := StartT(t)
	ctx := context.Background()

	// A client built from the exported values, the way services under test
	// build theirs, must be able to use the store.
	client, err := minio.New(server.Endpoint, &minio.Options{
		Creds: credentials.NewStaticV4(AccessKey, SecretKey, ""),
	})
	require.NoError(t, err)

	require.NoError(t, client.MakeBucket(ctx, "s3test", minio.MakeBucketOptions{}))
	_, err = client.PutObject(ctx, "s3test", "hello.txt", bytes.NewReader([]byte("hello")), 5, minio.PutObjectOptions{})
	require.NoError(t, err)

	object, err := server.Client.GetObject(ctx, "s3test", "hello.txt", minio.GetObjectOptions{})
	require.NoError(t, err)
	defer object.Close()
	body, err := io.ReadAll(object)
	require.NoError(t, err)
	assert.Equal(t, "hello", string(body))
}
