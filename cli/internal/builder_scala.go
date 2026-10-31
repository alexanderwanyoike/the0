package internal

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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
	if err := vm.runner.EnsureImage(scala3Image); err != nil {
		return fmt.Errorf("failed to pull Scala image: %v", err)
	}

	// Run build container
	if err := b.runBuildContainer(vm); err != nil {
		return fmt.Errorf("failed to run Scala build container: %v", err)
	}

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

// runBuildContainer creates and runs a container for Scala building.
func (b *ScalaBuilder) runBuildContainer(vm *VendorManager) error {
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Build command
	buildCmd := "sbt assembly 2>&1"

	uid, gid := hostUserIDs()
	return vm.runner.Run(BuildStep{
		NamePrefix:      "scala-",
		Image:           scala3Image,
		Cmd:             []string{"bash", "-c", buildCmd},
		WorkingDir:      "/project",
		Env:             getScalaBuildEnvVars(),
		User:            fmt.Sprintf("%s:%s", uid, gid),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: "Scala build",
		FailedCommand:   "sbt assembly",
	})
}

// getScalaBuildEnvVars returns environment variables for Scala build.
func getScalaBuildEnvVars() []string {
	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return nil
	}

	var envVars []string

	// Pass GitHub token for private git dependencies
	if secrets.GitHubToken != "" {
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}

	// Pass Maven credentials for private Maven/Ivy repositories
	if secrets.MavenUser != "" {
		envVars = append(envVars, fmt.Sprintf("MAVEN_USER=%s", secrets.MavenUser))
	}
	if secrets.MavenToken != "" {
		envVars = append(envVars, fmt.Sprintf("MAVEN_TOKEN=%s", secrets.MavenToken))
	}

	return envVars
}
