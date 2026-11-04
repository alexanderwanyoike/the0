package internal

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/fatih/color"
)

const haskellImage = "haskell:9.6"

// HaskellBuilder handles Haskell project detection and building.
type HaskellBuilder struct{}

func init() {
	RegisterBuilder(&HaskellBuilder{})
}

// Name returns the language name for logging.
func (b *HaskellBuilder) Name() string {
	return "Haskell"
}

// DockerImage returns the Docker image for Haskell builds.
func (b *HaskellBuilder) DockerImage() string {
	return haskellImage
}

// Detect checks if any .cabal file exists in the project.
func (b *HaskellBuilder) Detect(projectPath string) bool {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.cabal"))
	if err != nil {
		return false
	}
	return len(files) > 0
}

// ShouldBuild checks if Haskell project needs building.
func (b *HaskellBuilder) ShouldBuild(projectPath string) (bool, error) {
	files, err := filepath.Glob(filepath.Join(projectPath, "*.cabal"))
	if err != nil {
		return false, err
	}
	if len(files) == 0 {
		return false, nil
	}
	return true, nil
}

// Build performs the Docker-based Haskell build.
func (b *HaskellBuilder) Build(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building Haskell project...")

	// Pull Haskell image if needed
	if err := vm.runner.EnsureImage(haskellImage); err != nil {
		return fmt.Errorf("failed to pull Haskell image: %v", err)
	}

	// Run build container
	if err := b.runBuildContainer(vm); err != nil {
		return fmt.Errorf("failed to run Haskell build container: %v", err)
	}

	// Verify binaries were created in bin/
	binaries := b.FindAllBinaries(vm.projectPath)
	if len(binaries) == 0 {
		return fmt.Errorf("haskell build did not produce any binaries in bin/")
	}

	// Report all built binaries
	for _, binaryPath := range binaries {
		binaryInfo, err := os.Stat(binaryPath)
		if err != nil {
			continue
		}
		sizeStr := vm.formatFileSize(binaryInfo.Size())
		green.Printf("v Haskell binary built: %s (%s)\n", filepath.Base(binaryPath), sizeStr)
	}
	return nil
}

// FindBinary finds the first built binary in bin/.
// After build, binaries are copied to bin/ for simplified paths.
func (b *HaskellBuilder) FindBinary(projectPath string) string {
	binaries := b.FindAllBinaries(projectPath)
	if len(binaries) > 0 {
		return binaries[0]
	}
	return ""
}

// FindAllBinaries finds all built binaries in bin/.
func (b *HaskellBuilder) FindAllBinaries(projectPath string) []string {
	binDir := filepath.Join(projectPath, "bin")

	entries, err := os.ReadDir(binDir)
	if err != nil {
		return nil
	}

	var binaries []string
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		// Check if it's executable
		info, err := entry.Info()
		if err != nil {
			continue
		}
		if info.Mode()&0111 != 0 {
			binaries = append(binaries, filepath.Join(binDir, entry.Name()))
		}
	}
	return binaries
}

// runBuildContainer creates and runs a container for Haskell building.
func (b *HaskellBuilder) runBuildContainer(vm *VendorManager) error {
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return fmt.Errorf("failed to get absolute project path: %v", err)
	}

	// Build command: build with optimizations, then copy all executables to bin/
	// This provides simple, predictable paths like bin/my-bot instead of
	// dist-newstyle/build/x86_64-linux/ghc-9.6.7/pkg-1.0.0/x/my-bot/opt/build/my-bot/my-bot
	buildCmd := `set -e
cabal update
cabal build --enable-optimization=2

# Create bin directory
mkdir -p bin

# Find and copy all executables from opt/build directories
# These are the optimized binaries built by cabal
find dist-newstyle -path "*/opt/build/*/*" -type f -executable 2>/dev/null | while read exe; do
    name=$(basename "$exe")
    # Skip autogen files and other non-binary files
    case "$name" in
        *.hi|*.o|*.dyn_*|Paths_*) continue ;;
    esac
    cp "$exe" bin/
    echo "Copied $name to bin/"
done

# Verify at least one binary was copied
if [ -z "$(ls -A bin/ 2>/dev/null)" ]; then
    echo "Warning: No executables found in dist-newstyle opt/build directories"
    exit 1
fi
`

	uid, gid := hostUserIDs()
	return vm.runner.Run(BuildStep{
		NamePrefix:      "haskell-",
		Image:           haskellImage,
		Cmd:             []string{"sh", "-c", buildCmd},
		WorkingDir:      "/project",
		Env:             getHaskellBuildEnvVars(),
		User:            fmt.Sprintf("%s:%s", uid, gid),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: "Haskell build",
		FailedCommand:   "cabal build",
	})
}

// getHaskellBuildEnvVars returns environment variables for Haskell build.
func getHaskellBuildEnvVars() []string {
	envVars := []string{
		"HOME=/tmp",
		"CABAL_DIR=/tmp/.cabal",
	}

	secrets, err := LoadBuildSecrets()
	if err != nil || secrets == nil {
		return envVars
	}

	if secrets.GitHubToken != "" {
		// Configure git to use GitHub token for private dependencies
		envVars = append(envVars, fmt.Sprintf("GITHUB_TOKEN=%s", secrets.GitHubToken))
	}
	return envVars
}
