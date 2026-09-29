// Package s3test runs the bundled S3-compatible object store in a container
// for integration tests. The image and its startup live only here, so moving
// the platform to a different store is a one-file change for the tests (#342).
package s3test

import (
	"context"
	"fmt"
	"net"
	"testing"
	"time"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

const (
	image = "pgsty/silo:RELEASE.2026-09-16T00-00-00Z"

	AccessKey = "the0testaccess"
	SecretKey = "the0testsecretkey"
)

// Server is a running store.
type Server struct {
	// Endpoint is host:port as seen from the test process.
	Endpoint string
	// Port is the mapped host port, for reaching the store from other
	// containers via host.docker.internal.
	Port   string
	Client *minio.Client

	container testcontainers.Container
}

// Start starts a store and returns once it serves S3 requests.
func Start(ctx context.Context) (*Server, error) {
	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: testcontainers.ContainerRequest{
			Image:        image,
			ExposedPorts: []string{"9000/tcp"},
			Env: map[string]string{
				"MINIO_ROOT_USER":     AccessKey,
				"MINIO_ROOT_PASSWORD": SecretKey,
			},
			Cmd:        []string{"server", "/data"},
			WaitingFor: wait.ForListeningPort("9000/tcp"),
		},
		Started: true,
	})
	if err != nil {
		return nil, fmt.Errorf("start store container: %w", err)
	}

	server, err := newServer(ctx, container)
	if err != nil {
		_ = container.Terminate(context.Background())
		return nil, err
	}
	return server, nil
}

// StartT starts a store that is terminated when the test finishes.
func StartT(t testing.TB) *Server {
	t.Helper()

	server, err := Start(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := server.Terminate(context.Background()); err != nil {
			t.Errorf("terminate store container: %v", err)
		}
	})
	return server
}

// Terminate stops and removes the store container.
func (s *Server) Terminate(ctx context.Context) error {
	return s.container.Terminate(ctx)
}

func newServer(ctx context.Context, container testcontainers.Container) (*Server, error) {
	host, err := container.Host(ctx)
	if err != nil {
		return nil, fmt.Errorf("store container host: %w", err)
	}
	port, err := container.MappedPort(ctx, "9000/tcp")
	if err != nil {
		return nil, fmt.Errorf("store container port: %w", err)
	}

	endpoint := net.JoinHostPort(host, port.Port())
	client, err := minio.New(endpoint, &minio.Options{
		Creds: credentials.NewStaticV4(AccessKey, SecretKey, ""),
	})
	if err != nil {
		return nil, fmt.Errorf("store client: %w", err)
	}
	if err := waitForS3(ctx, client, 30*time.Second); err != nil {
		return nil, err
	}

	return &Server{Endpoint: endpoint, Port: port.Port(), Client: client, container: container}, nil
}

// waitForS3 polls an authenticated S3 call rather than a health endpoint:
// health paths differ between stores, and MinIO reports live before bucket
// operations succeed.
func waitForS3(ctx context.Context, client *minio.Client, timeout time.Duration) error {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	for {
		_, err := client.ListBuckets(ctx)
		if err == nil {
			return nil
		}
		select {
		case <-ctx.Done():
			return fmt.Errorf("store not serving S3 within %s: %w (last error: %v)", timeout, ctx.Err(), err)
		case <-time.After(200 * time.Millisecond):
		}
	}
}
