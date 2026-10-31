package internal

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"io"
	"reflect"
	"strings"
	"testing"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/docker/docker/api/types/network"
	ocispec "github.com/opencontainers/image-spec/specs-go/v1"
)

// fakeDocker records what a build asks of Docker and answers from its fields.
type fakeDocker struct {
	localTags []string
	exitCode  int64
	output    string
	createErr error

	pulled    []string
	created   []*container.Config
	hosts     []*container.HostConfig
	names     []string
	started   []string
	removed   []string
	closed    bool
	pingErr   error
	logsCalls int
}

// frame wraps text in Docker's multiplexed log header (stdout stream).
func frame(text string) []byte {
	header := make([]byte, 8)
	header[0] = 1
	binary.BigEndian.PutUint32(header[4:], uint32(len(text)))
	return append(header, text...)
}

func (f *fakeDocker) Ping(context.Context) (types.Ping, error) { return types.Ping{}, f.pingErr }

func (f *fakeDocker) ImageList(context.Context, image.ListOptions) ([]image.Summary, error) {
	return []image.Summary{{RepoTags: f.localTags}}, nil
}

func (f *fakeDocker) ImagePull(_ context.Context, ref string, _ image.PullOptions) (io.ReadCloser, error) {
	f.pulled = append(f.pulled, ref)
	return io.NopCloser(strings.NewReader("pull progress")), nil
}

func (f *fakeDocker) ContainerCreate(_ context.Context, config *container.Config, host *container.HostConfig, _ *network.NetworkingConfig, _ *ocispec.Platform, name string) (container.CreateResponse, error) {
	if f.createErr != nil {
		return container.CreateResponse{}, f.createErr
	}
	f.created = append(f.created, config)
	f.hosts = append(f.hosts, host)
	f.names = append(f.names, name)
	return container.CreateResponse{ID: "ctr-1"}, nil
}

func (f *fakeDocker) ContainerStart(_ context.Context, id string, _ container.StartOptions) error {
	f.started = append(f.started, id)
	return nil
}

func (f *fakeDocker) ContainerLogs(context.Context, string, container.LogsOptions) (io.ReadCloser, error) {
	f.logsCalls++
	return io.NopCloser(bytes.NewReader(frame(f.output))), nil
}

func (f *fakeDocker) ContainerWait(context.Context, string, container.WaitCondition) (<-chan container.WaitResponse, <-chan error) {
	status := make(chan container.WaitResponse, 1)
	status <- container.WaitResponse{StatusCode: f.exitCode}
	return status, make(chan error)
}

func (f *fakeDocker) ContainerRemove(_ context.Context, id string, _ container.RemoveOptions) error {
	f.removed = append(f.removed, id)
	return nil
}

func (f *fakeDocker) Close() error {
	f.closed = true
	return nil
}

func rustStep() BuildStep {
	return BuildStep{
		NamePrefix:      "rust-",
		Image:           "rust:latest",
		Cmd:             []string{"sh", "-c", "cargo build --release 2>&1"},
		WorkingDir:      "/project",
		Env:             []string{"GITHUB_TOKEN=t"},
		User:            "1000:1000",
		Binds:           []string{"/src:/project"},
		FailureHeadline: "Rust build",
		FailedCommand:   "cargo build",
	}
}

func TestDockerRunner_EnsureImage(t *testing.T) {
	t.Run("pulls an image no local tag matches", func(t *testing.T) {
		docker := &fakeDocker{localTags: []string{"python:3.11-slim"}}
		if err := (&dockerRunner{api: docker}).EnsureImage("rust:latest"); err != nil {
			t.Fatal(err)
		}
		if !reflect.DeepEqual(docker.pulled, []string{"rust:latest"}) {
			t.Errorf("pulled %v, want [rust:latest]", docker.pulled)
		}
	})

	t.Run("does not pull an image that is already local", func(t *testing.T) {
		docker := &fakeDocker{localTags: []string{"rust:latest"}}
		if err := (&dockerRunner{api: docker}).EnsureImage("rust:latest"); err != nil {
			t.Fatal(err)
		}
		if len(docker.pulled) != 0 {
			t.Errorf("pulled %v, want nothing", docker.pulled)
		}
	})
}

func TestDockerRunner_Run(t *testing.T) {
	t.Run("runs the step in a container built from it", func(t *testing.T) {
		docker := &fakeDocker{output: "Compiling bot\n"}

		id, err := (&dockerRunner{api: docker}).Run(rustStep())

		if err != nil {
			t.Fatalf("Run() error = %v", err)
		}
		if id != "ctr-1" {
			t.Errorf("container = %q, want ctr-1", id)
		}
		want := &container.Config{
			Image:      "rust:latest",
			Cmd:        []string{"sh", "-c", "cargo build --release 2>&1"},
			WorkingDir: "/project",
			Env:        []string{"GITHUB_TOKEN=t"},
			User:       "1000:1000",
		}
		if len(docker.created) != 1 || !reflect.DeepEqual(docker.created[0], want) {
			t.Errorf("created %+v, want %+v", docker.created, want)
		}
		if !reflect.DeepEqual(docker.hosts[0].Binds, []string{"/src:/project"}) || docker.hosts[0].AutoRemove {
			t.Errorf("host config = %+v, want the step's binds and no auto-remove", docker.hosts[0])
		}
		if !strings.HasPrefix(docker.names[0], "the0-vendor-rust-") {
			t.Errorf("container name = %q, want the0-vendor-rust-<time>", docker.names[0])
		}
		if !reflect.DeepEqual(docker.started, []string{"ctr-1"}) {
			t.Errorf("started %v, want [ctr-1]", docker.started)
		}
	})

	t.Run("reports a non-zero exit with the step's command", func(t *testing.T) {
		docker := &fakeDocker{exitCode: 101, output: "error[E0425]\n"}

		id, err := (&dockerRunner{api: docker}).Run(rustStep())

		if err == nil || err.Error() != "cargo build failed with exit code 101" {
			t.Errorf("error = %v, want cargo build failed with exit code 101", err)
		}
		if id != "ctr-1" {
			t.Errorf("container = %q, want ctr-1", id)
		}
		if docker.logsCalls != 2 {
			t.Errorf("read the logs %d times, want 2 (stream, then the full log)", docker.logsCalls)
		}
	})

	t.Run("returns the engine's error when the container cannot be created", func(t *testing.T) {
		createErr := errors.New("no such image")
		docker := &fakeDocker{createErr: createErr}

		_, err := (&dockerRunner{api: docker}).Run(rustStep())

		if !errors.Is(err, createErr) {
			t.Errorf("error = %v, want %v", err, createErr)
		}
	})
}

func TestCleanDockerLogOutput(t *testing.T) {
	raw := append(frame("first line\n"), frame("second line\n")...)
	if got, want := cleanDockerLogOutput(raw), "first line\nsecond line\n"; got != want {
		t.Errorf("cleanDockerLogOutput() = %q, want %q", got, want)
	}
}
