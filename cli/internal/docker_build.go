package internal

import (
	"context"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/docker/docker/api/types/network"
	"github.com/docker/docker/client"
	"github.com/fatih/color"
	ocispec "github.com/opencontainers/image-spec/specs-go/v1"
)

// dockerAPI is the part of the Docker client that builds use.
// *client.Client satisfies it.
type dockerAPI interface {
	Ping(ctx context.Context) (types.Ping, error)
	ImageList(ctx context.Context, options image.ListOptions) ([]image.Summary, error)
	ImagePull(ctx context.Context, ref string, options image.PullOptions) (io.ReadCloser, error)
	ContainerCreate(ctx context.Context, config *container.Config, hostConfig *container.HostConfig, networkingConfig *network.NetworkingConfig, platform *ocispec.Platform, containerName string) (container.CreateResponse, error)
	ContainerStart(ctx context.Context, containerID string, options container.StartOptions) error
	ContainerLogs(ctx context.Context, containerID string, options container.LogsOptions) (io.ReadCloser, error)
	ContainerWait(ctx context.Context, containerID string, condition container.WaitCondition) (<-chan container.WaitResponse, <-chan error)
	ContainerRemove(ctx context.Context, containerID string, options container.RemoveOptions) error
	Close() error
}

// BuildStep is one build or install command, run in a throwaway container
// with the project (or part of it) mounted.
type BuildStep struct {
	// NamePrefix tells the step's containers apart, e.g. "rust-".
	NamePrefix string
	Image      string
	Cmd        []string
	WorkingDir string
	Env        []string
	User       string
	Binds      []string
	// LogColor prints the streamed output; nil means blue.
	LogColor *color.Color
	// FailureHeadline and FailedCommand name the step in the messages for
	// a non-zero exit, e.g. "Rust build" and "cargo build".
	FailureHeadline string
	FailedCommand   string
}

// BuildRunner runs build steps in containers. The CLI runs them in Docker;
// tests pass a fake.
type BuildRunner interface {
	// Ping reports whether the container engine answers.
	Ping() error
	// EnsureImage pulls image unless a local image has that tag.
	EnsureImage(image string) error
	// Run runs step to completion, streaming its output.
	Run(step BuildStep) (containerID string, err error)
	// Remove deletes a container Run created.
	Remove(containerID string)
	Close() error
}

type dockerRunner struct {
	api dockerAPI
}

// NewDockerRunner connects to the Docker engine the environment points at.
func NewDockerRunner() (BuildRunner, error) {
	cli, err := client.NewClientWithOpts(client.FromEnv, client.WithAPIVersionNegotiation())
	if err != nil {
		return nil, fmt.Errorf("failed to create Docker client: %v", err)
	}
	return &dockerRunner{api: cli}, nil
}

func (r *dockerRunner) Ping() error {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	_, err := r.api.Ping(ctx)
	return err
}

func (r *dockerRunner) Close() error {
	return r.api.Close()
}

func (r *dockerRunner) EnsureImage(ref string) error {
	ctx := context.Background()

	images, err := r.api.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == ref {
				return nil
			}
		}
	}

	color.New(color.FgBlue).Printf("Pulling Docker image: %s...\n", ref)

	reader, err := r.api.ImagePull(ctx, ref, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	// The pull progress is noise next to the build's own output.
	io.Copy(io.Discard, reader)
	return nil
}

func (r *dockerRunner) Run(step BuildStep) (string, error) {
	ctx := context.Background()

	name := fmt.Sprintf("%s%s%d", vendorContainerName, step.NamePrefix, time.Now().Unix())
	config := &container.Config{
		Image:      step.Image,
		Cmd:        step.Cmd,
		WorkingDir: step.WorkingDir,
		Env:        step.Env,
		User:       step.User,
	}
	hostConfig := &container.HostConfig{
		Binds:      step.Binds,
		AutoRemove: false,
	}

	resp, err := r.api.ContainerCreate(ctx, config, hostConfig, nil, nil, name)
	if err != nil {
		return "", err
	}

	if err := r.api.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	logReader, err := r.api.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

	logColor := step.LogColor
	if logColor == nil {
		logColor = color.New(color.FgBlue)
	}
	logDone := make(chan struct{})
	go func() {
		defer close(logDone)
		defer logReader.Close()

		buffer := make([]byte, 1024)
		for {
			n, err := logReader.Read(buffer)
			if err != nil {
				if err != io.EOF {
					logColor.Printf("Log read error: %v\n", err)
				}
				return
			}
			if n > 0 {
				if output := cleanDockerLogOutput(buffer[:n]); output != "" {
					logColor.Printf("  %s", output)
				}
			}
		}
	}()

	statusCh, errCh := r.api.ContainerWait(ctx, resp.ID, container.WaitConditionNotRunning)
	select {
	case err := <-errCh:
		if err != nil {
			return resp.ID, err
		}
	case status := <-statusCh:
		<-logDone
		if status.StatusCode != 0 {
			logs, _ := r.containerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("%s failed with exit code %d\n", step.FailureHeadline, status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("%s failed with exit code %d", step.FailedCommand, status.StatusCode)
		}
	}

	return resp.ID, nil
}

func (r *dockerRunner) Remove(containerID string) {
	r.api.ContainerRemove(context.Background(), containerID, container.RemoveOptions{Force: true})
}

// containerLogs reads everything a finished container wrote.
func (r *dockerRunner) containerLogs(containerID string) (string, error) {
	reader, err := r.api.ContainerLogs(context.Background(), containerID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
	})
	if err != nil {
		return "", err
	}
	defer reader.Close()

	logs, err := io.ReadAll(reader)
	if err != nil {
		return "", err
	}
	return string(logs), nil
}

// cleanDockerLogOutput removes Docker log headers from raw log data
func cleanDockerLogOutput(logData []byte) string {
	var output strings.Builder
	i := 0

	for i < len(logData) {
		// Docker log format: 8-byte header + message
		// Header: [stream_type][0][0][0][size_bytes_3][size_bytes_2][size_bytes_1][size_bytes_0]
		if i+8 > len(logData) {
			// Not enough bytes for header, append remaining
			output.Write(logData[i:])
			break
		}

		// Extract message size from header
		size := int(logData[i+4])<<24 | int(logData[i+5])<<16 | int(logData[i+6])<<8 | int(logData[i+7])

		// Skip header
		i += 8

		// Extract message
		if i+size <= len(logData) {
			output.Write(logData[i : i+size])
			i += size
		} else {
			// Invalid size, append remaining
			output.Write(logData[i:])
			break
		}
	}

	return output.String()
}
