package s3test

import (
	"bytes"
	"context"
	"io"
	"net"
	"testing"
	"time"

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

func TestWaitForS3_GivesUpOnAStalledStore(t *testing.T) {
	// Accepts connections and never answers, so each S3 call blocks.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	require.NoError(t, err)
	defer listener.Close()
	go func() {
		for {
			conn, err := listener.Accept()
			if err != nil {
				return
			}
			defer conn.Close()
		}
	}()

	err = waitWithin(t, 5*time.Second, func() error {
		return waitForS3(context.Background(), clientFor(t, listener.Addr().String()), 300*time.Millisecond)
	})
	assert.Error(t, err)
}

func TestWaitForS3_StopsWhenTheCallerCancels(t *testing.T) {
	// Nothing listens here, so every attempt fails fast and the loop retries.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	require.NoError(t, err)
	addr := listener.Addr().String()
	require.NoError(t, listener.Close())

	ctx, cancel := context.WithCancel(context.Background())
	time.AfterFunc(100*time.Millisecond, cancel)

	err = waitWithin(t, 5*time.Second, func() error {
		return waitForS3(ctx, clientFor(t, addr), time.Minute)
	})
	assert.ErrorIs(t, err, context.Canceled)
}

func clientFor(t *testing.T, endpoint string) *minio.Client {
	t.Helper()
	client, err := minio.New(endpoint, &minio.Options{Creds: credentials.NewStaticV4(AccessKey, SecretKey, "")})
	require.NoError(t, err)
	return client
}

// waitWithin fails the test if fn has not returned within limit.
func waitWithin(t *testing.T, limit time.Duration, fn func() error) error {
	t.Helper()
	done := make(chan error, 1)
	go func() { done <- fn() }()
	select {
	case err := <-done:
		return err
	case <-time.After(limit):
		t.Fatalf("did not return within %s", limit)
		return nil
	}
}
