package internal

import (
	"fmt"

	"github.com/fatih/color"
)

// LanguageBuilder handles detection and building for a specific compiled language.
// Each language (Rust, C#, etc.) implements this interface.
type LanguageBuilder interface {
	// Name returns the language name for logging (e.g., "Rust", "C#/.NET")
	Name() string

	// Detect checks if this language's project exists in the given path.
	// Returns true if the project manifest file exists (e.g., Cargo.toml, *.csproj).
	Detect(projectPath string) bool

	// ShouldBuild checks if building is needed for the project.
	// Returns true if the project should be built.
	ShouldBuild(projectPath string) (bool, error)

	// Build performs the Docker-based build using the VendorManager.
	// Returns an error if the build fails.
	Build(vm *VendorManager) error

	// FindBinary locates the built binary/output in the project.
	// Returns empty string if not found.
	FindBinary(projectPath string) string

	// DockerImage returns the Docker image to use for building.
	DockerImage() string
}

// builderRegistry holds all registered language builders
var builderRegistry = []LanguageBuilder{}

// RegisterBuilder adds a language builder to the registry.
// Called from init() functions in each builder implementation.
func RegisterBuilder(b LanguageBuilder) {
	builderRegistry = append(builderRegistry, b)
}

// GetBuilders returns all registered language builders.
func GetBuilders() []LanguageBuilder {
	return builderRegistry
}

// BuildCompiledLanguagesIfNeeded iterates through all registered builders
// and builds any detected projects.
func BuildCompiledLanguagesIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	for _, builder := range GetBuilders() {
		shouldBuild, err := builder.ShouldBuild(projectPath)
		if err != nil {
			return err
		}

		if !shouldBuild {
			continue
		}

		blue.Printf("%s project detected, building...\n", builder.Name())

		// Check Docker availability
		if err := CheckDockerInstalled(); err != nil {
			red.Printf("Docker required for %s build: %v\n", builder.Name(), err)
			red.Printf("Solution: Install Docker or pre-build the project\n")
			return fmt.Errorf("docker unavailable for %s build", builder.Name())
		}

		vm, err := NewVendorManager(projectPath)
		if err != nil {
			return fmt.Errorf("failed to initialize vendor manager: %v", err)
		}
		defer vm.Close()

		if err := vm.CheckDockerRunning(); err != nil {
			red.Printf("Docker daemon not running: %v\n", err)
			red.Printf("Solution: Start Docker or pre-build the project\n")
			return fmt.Errorf("docker daemon not running for %s build", builder.Name())
		}

		if err := builder.Build(vm); err != nil {
			return fmt.Errorf("%s build failed: %v", builder.Name(), err)
		}
	}

	return nil
}
