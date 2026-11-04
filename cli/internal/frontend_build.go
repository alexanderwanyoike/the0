package internal

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/fatih/color"
)

// The dashboard bundle runs in a browser, so its build Node is independent of
// the bot's runtime.
const frontendNodeImage = "node:22-slim"

// Only a missing package.json skips the build. Any other stat error still
// builds, so the problem surfaces instead of a deploy without its bundle.
func hasFrontend(projectPath string) bool {
	_, err := os.Stat(filepath.Join(projectPath, "frontend", "package.json"))
	return !os.IsNotExist(err)
}

func buildFrontend(vm *VendorManager) error {
	blue := color.New(color.FgBlue)
	green := color.New(color.FgGreen)

	blue.Println("Building frontend bundle...")

	// Step 0: Clean dist directory to ensure fresh build
	distPath := filepath.Join(vm.projectPath, "frontend", "dist")
	os.RemoveAll(distPath)

	// Step 1: Pull Node image if needed
	if err := vm.runner.EnsureImage(frontendNodeImage); err != nil {
		return fmt.Errorf("failed to pull Node image: %v", err)
	}

	// Step 2: Run frontend build container
	if err := runFrontendBuildContainer(vm); err != nil {
		return fmt.Errorf("failed to run frontend build container: %v", err)
	}

	// Step 3: Verify bundle was created
	bundlePath := filepath.Join(vm.projectPath, "frontend", "dist", "bundle.js")
	if _, err := os.Stat(bundlePath); os.IsNotExist(err) {
		return fmt.Errorf("frontend build did not produce dist/bundle.js")
	}

	// Get bundle size
	bundleInfo, _ := os.Stat(bundlePath)
	sizeStr := vm.formatFileSize(bundleInfo.Size())

	green.Printf("v Frontend bundle built (%s)\n", sizeStr)
	return nil
}

func runFrontendBuildContainer(vm *VendorManager) error {
	// Prepare absolute paths for volume mounting
	absProjectPath, err := filepath.Abs(vm.projectPath)
	if err != nil {
		return fmt.Errorf("failed to get absolute project path: %v", err)
	}

	uid, gid := hostUserIDs()

	// Configure npm auth for GitHub Packages if GITHUB_TOKEN is set. The SDKs
	// now live on the public npm registry, but bots created before the move
	// may still have a project .npmrc pinning the scope to GitHub Packages,
	// and their builds only work with this auth in place.
	npmAuthSetup := `if [ -n "$GITHUB_TOKEN" ]; then echo "//npm.pkg.github.com/:_authToken=$GITHUB_TOKEN" >> ~/.npmrc; fi`

	// Build command: setup auth, install deps, run build script
	// The build script in package.json should output ESM with external react
	buildCmd := fmt.Sprintf(
		"%s && cd /project/frontend && npm install && npm run build; STATUS=$?; chown -R %s:%s /project/frontend/dist /project/frontend/node_modules 2>/dev/null || true; exit $STATUS",
		npmAuthSetup, uid, gid,
	)

	return vm.runner.Run(BuildStep{
		NamePrefix:      "frontend-",
		Image:           frontendNodeImage,
		Cmd:             []string{"sh", "-c", buildCmd},
		WorkingDir:      "/project/frontend",
		Env:             getBuildEnvVars(),
		Binds:           []string{fmt.Sprintf("%s:/project", absProjectPath)},
		FailureHeadline: "Frontend build",
		FailedCommand:   "frontend build",
	})
}

// BuildFrontendIfNeeded builds the bundle whenever the project has a
// frontend, even if dist/bundle.js already exists, so a deploy never ships a
// bundle older than its source.
func BuildFrontendIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	if !hasFrontend(projectPath) {
		return nil
	}

	blue.Println("Frontend detected, building bundle...")

	// Check Docker availability
	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for frontend build: %v\n", err)
		red.Println("Solution: Install Docker and try again: the frontend is built in Docker on every deploy")
		return fmt.Errorf("docker unavailable for frontend build")
	}

	runner, err := NewDockerRunner()
	if err != nil {
		return fmt.Errorf("failed to initialize vendor manager: %v", err)
	}
	vm := NewVendorManager(projectPath, runner)
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Solution: Start Docker and try again")
		return fmt.Errorf("docker daemon not running for frontend build")
	}

	if err := buildFrontend(vm); err != nil {
		return fmt.Errorf("frontend build failed: %v", err)
	}

	return nil
}
