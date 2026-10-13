// Package mongotest runs MongoDB in a container for integration tests, at
// the version the Helm chart and Docker Compose run.
package mongotest

import (
	"context"
	"fmt"
	"sync/atomic"
	"testing"
	"time"

	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

const image = "mongo:7-jammy"

// Server is a running MongoDB.
type Server struct {
	URI    string
	Client *mongo.Client

	container testcontainers.Container
}

var databases atomic.Int64

// Start starts MongoDB and returns once it answers a ping.
func Start(ctx context.Context) (*Server, error) {
	container, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: testcontainers.ContainerRequest{
			Image:        image,
			ExposedPorts: []string{"27017/tcp"},
			WaitingFor:   wait.ForListeningPort("27017/tcp").WithStartupTimeout(60 * time.Second),
		},
		Started: true,
	})
	if err != nil {
		return nil, fmt.Errorf("start mongo container: %w", err)
	}
	server, err := newServer(ctx, container)
	if err != nil {
		_ = container.Terminate(context.Background())
		return nil, err
	}
	return server, nil
}

// StartT starts MongoDB that is terminated when the test finishes.
func StartT(t testing.TB) *Server {
	t.Helper()
	server, err := Start(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := server.Terminate(context.Background()); err != nil {
			t.Errorf("terminate mongo container: %v", err)
		}
	})
	return server
}

// Database is an empty database no other test uses.
func (s *Server) Database() *mongo.Database {
	return s.Client.Database(fmt.Sprintf("test_%d", databases.Add(1)))
}

// Terminate disconnects and removes the container.
func (s *Server) Terminate(ctx context.Context) error {
	_ = s.Client.Disconnect(ctx)
	return s.container.Terminate(ctx)
}

func newServer(ctx context.Context, container testcontainers.Container) (*Server, error) {
	host, err := container.Host(ctx)
	if err != nil {
		return nil, fmt.Errorf("mongo container host: %w", err)
	}
	port, err := container.MappedPort(ctx, "27017/tcp")
	if err != nil {
		return nil, fmt.Errorf("mongo container port: %w", err)
	}
	uri := fmt.Sprintf("mongodb://%s:%s", host, port.Port())

	connectCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	client, err := mongo.Connect(connectCtx, options.Client().ApplyURI(uri))
	if err != nil {
		return nil, fmt.Errorf("mongo client: %w", err)
	}
	if err := client.Ping(connectCtx, nil); err != nil {
		_ = client.Disconnect(context.Background())
		return nil, fmt.Errorf("mongo not answering: %w", err)
	}
	return &Server{URI: uri, Client: client, container: container}, nil
}
