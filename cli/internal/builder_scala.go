package internal

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/user"
	"path/filepath"
	"strings"
	"time"

	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/image"
	"github.com/fatih/color"
)

const scala3Image = "sbtscala/scala-sbt:eclipse-temurin-17.0.15_6_1.11.7_3.3.7"

// ScalaBuilder handles Scala 3 project detection and building.
type ScalaBuilder struct{}

func init() {
	RegisterBuilder(&ScalaBuilder{})
}

// Name returns the language name for logging.
func (b *ScalaBuilder) Name() string {
	return "Scala 3"
}

// DockerImage returns the Docker image for Scala builds.
func (b *ScalaBuilder) DockerImage() string {
	return scala3Image
}

// Detect checks if build.sbt exists in the project.
func (b *ScalaBuilder) Detect(projectPath string) bool {
	buildSbt := filepath.Join(projectPath, "build.sbt")
	if _, err := os.Stat(buildSbt); err == nil {
		return true
	}
	return false
}

// ShouldBuild checks if Scala project needs building.
func (b *ScalaBuilder) ShouldBuild(projectPath string) (bool, error) {
	if !b.Detect(projectPath) {
		return false, nil
	}
	return true, nil
}

// Build performs the Docker-based Scala build.
func (b *ScalaBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building Scala project...")

	// Ensure project has assembly plugin
	if err := b.ensureAssemblyPlugin(vm.projectPath); err != nil {
		return fmt.Errorf("failed to setup assembly plugin: %v", err)
	}

	// Pull Scala image if needed
	if err := b.pullImage(vm); err != nil {
		return fmt.Errorf("failed to pull Scala image: %v", err)
	}

	// Run build container
	containerID, err := b.runBuildContainer(vm)
	if err != nil {
		return fmt.Errorf("failed to run Scala build container: %v", err)
	}
	defer vm.cleanupContainer(containerID)

	// Verify output was created
	binaryPath := b.FindBinary(vm.projectPath)
	if binaryPath == "" {
		return fmt.Errorf("Scala build did not produce output in target/scala-*/")
	}

	// Get binary info
	binaryInfo, err := os.Stat(binaryPath)
	if err != nil {
		return fmt.Errorf("failed to stat JAR: %v", err)
	}

	sizeStr := vm.formatFileSize(binaryInfo.Size())
	green.Printf("v Scala project built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	return nil
}

// FindBinary finds the built assembly JAR in target/scala-*/.
func (b *ScalaBuilder) FindBinary(projectPath string) string {
	targetDir := filepath.Join(projectPath, "target")

	// Look for scala-* directories
	entries, err := os.ReadDir(targetDir)
	if err != nil {
		return ""
	}

	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}
		if !strings.HasPrefix(entry.Name(), "scala-") {
			continue
		}

		scalaDir := filepath.Join(targetDir, entry.Name())
		files, err := os.ReadDir(scalaDir)
		if err != nil {
			continue
		}

		// Look for assembly JAR
		for _, file := range files {
			if file.IsDir() {
				continue
			}
			name := file.Name()
			if strings.HasSuffix(name, "-assembly.jar") || strings.Contains(name, "-assembly") && strings.HasSuffix(name, ".jar") {
				return filepath.Join(scalaDir, name)
			}
		}
	}

	return ""
}

// ensureAssemblyPlugin ensures the project has sbt-assembly plugin configured.
func (b *ScalaBuilder) ensureAssemblyPlugin(projectPath string) error {
	projectDir := filepath.Join(projectPath, "project")
	pluginsFile := filepath.Join(projectDir, "plugins.sbt")

	// Check if plugins.sbt exists
	if _, err := os.Stat(pluginsFile); err == nil {
		// File exists, check if it has assembly plugin
		content, err := os.ReadFile(pluginsFile)
		if err != nil {
			return err
		}
		if strings.Contains(string(content), "sbt-assembly") {
			return nil // Already configured
		}
		// Append assembly plugin
		f, err := os.OpenFile(pluginsFile, os.O_APPEND|os.O_WRONLY, 0644)
		if err != nil {
			return err
		}
		defer f.Close()
		_, err = f.WriteString("\naddSbtPlugin(\"com.eed3si9n\" % \"sbt-assembly\" % \"2.1.5\")\n")
		return err
	}

	// Create project directory if needed
	if err := os.MkdirAll(projectDir, 0755); err != nil {
		return err
	}

	// Create plugins.sbt with assembly plugin
	content := `addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
`
	return os.WriteFile(pluginsFile, []byte(content), 0644)
}

// pullImage pulls the Scala image if it doesn't exist locally.
func (b *ScalaBuilder) pullImage(vm *VendorManager) error {
	ctx := context.Background()

	images, err := vm.dockerClient.ImageList(ctx, image.ListOptions{})
	if err != nil {
		return err
	}

	imageExists := false
	for _, img := range images {
		for _, tag := range img.RepoTags {
			if tag == scala3Image {
				imageExists = true
				break
			}
		}
		if imageExists {
			break
		}
	}

	if imageExists {
		return nil
	}

	blue := color.New(color.FgBlue)
	blue.Printf("Pulling Docker image: %s...\n", scala3Image)

	reader, err := vm.dockerClient.ImagePull(ctx, scala3Image, image.PullOptions{})
	if err != nil {
		return err
	}
	defer reader.Close()

	io.Copy(io.Discard, reader)
	return nil
}

// runBuildContainer creates and runs a container for Scala building.
func (b *ScalaBuilder) runBuildContainer(vm *VendorManager) (string, error) {
	ctx := context.Background()

	ctrName := fmt.Sprintf("%sscala-%d", vendorContainerName, time.Now().Unix())

	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return "", fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Get current user info for ownership
	currentUser, err := user.Current()
	var uid, gid string
	if err == nil {
		uid = currentUser.Uid
		gid = currentUser.Gid
	} else {
		uid = "1000"
		gid = "1000"
	}

	// Build command
	buildCmd := "sbt assembly 2>&1"

	config := &container.Config{
		Image: scala3Image,
		Cmd: []string{
			"bash", "-c",
			buildCmd,
		},
		WorkingDir: "/project",
		Env:        getScalaBuildEnvVars(),
		User:       fmt.Sprintf("%s:%s", uid, gid),
	}

	hostConfig := &container.HostConfig{
		Binds: []string{
			fmt.Sprintf("%s:/project", absProjectPath),
		},
		AutoRemove: false,
	}

	resp, err := vm.dockerClient.ContainerCreate(ctx, config, hostConfig, nil, nil, ctrName)
	if err != nil {
		return "", err
	}

	if err := vm.dockerClient.ContainerStart(ctx, resp.ID, container.StartOptions{}); err != nil {
		return "", err
	}

	// Stream logs
	logReader, err := vm.dockerClient.ContainerLogs(ctx, resp.ID, container.LogsOptions{
		ShowStdout: true,
		ShowStderr: true,
		Follow:     true,
		Timestamps: false,
	})
	if err != nil {
		return resp.ID, fmt.Errorf("failed to get container logs: %v", err)
	}

	logDone := make(chan struct{})
	go func() {
		defer close(logDone)
		defer logReader.Close()

		blue := color.New(color.FgBlue)
		buffer := make([]byte, 1024)
		for {
			n, err := logReader.Read(buffer)
			if err != nil {
				if err != io.EOF {
					blue.Printf("Log read error: %v\n", err)
				}
				break
			}
			if n > 0 {
				output := vm.cleanDockerLogOutput(buffer[:n])
				if output != "" {
					blue.Printf("  %s", output)
				}
			}
		}
	}()

	// Wait for container to finish
	statusCh, errCh := vm.dockerClient.ContainerWait(ctx, resp.ID, container.WaitConditionNotRunning)
	select {
	case err := <-errCh:
		if err != nil {
			return resp.ID, err
		}
	case status := <-statusCh:
		<-logDone
		if status.StatusCode != 0 {
			logs, _ := vm.getContainerLogs(resp.ID)
			red := color.New(color.FgRed)
			red.Printf("Scala build failed with exit code %d\n", status.StatusCode)
			if logs != "" {
				red.Printf("Full error log:\n%s\n", logs)
			}
			return resp.ID, fmt.Errorf("sbt assembly failed with exit code %d", status.StatusCode)
		}
	}

	return resp.ID, nil
}

// getScalaBuildEnvVars returns environment variables for Scala build.
func getScalaBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string
	if secrets.GitHubToken != "" {
		// Configure SBT to use GitHub token for private packages
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}
	return envVars
}

// Legacy functions for backwards compatibility

// IsScalaProject checks if the project is a Scala project.
func IsScalaProject(projectPath string) bool {
	builder := &ScalaBuilder{}
	return builder.Detect(projectPath)
}

// ShouldBuildScala checks if Scala project needs building.
func ShouldBuildScala(projectPath string) (bool, error) {
	builder := &ScalaBuilder{}
	return builder.ShouldBuild(projectPath)
}

// BuildScalaIfNeeded builds Scala project only if needed and Docker is available.
func BuildScalaIfNeeded(projectPath string) error {
	builder := &ScalaBuilder{}

	shouldBuild, err := builder.ShouldBuild(projectPath)
	if err != nil {
		return err
	}

	if !shouldBuild {
		return nil
	}

	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	blue.Println("Scala project detected, building...")

	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for Scala build: %v\n", err)
		red.Println("Solution: Install Docker or pre-build with 'sbt assembly'")
		return fmt.Errorf("docker unavailable for Scala build")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker or pre-build with 'sbt assembly'")
		return fmt.Errorf("docker daemon not running for Scala build")
	}

	return builder.Build(vm)
}
