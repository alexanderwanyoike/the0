package internal

import (
	"fmt"

	"github.com/fatih/color"
)

// DependencyVendor handles detection and installation of dependencies for a specific language/tool.
// Each vendor (Python, Node.js, Frontend) implements this interface.
type DependencyVendor interface {
	// Name returns the vendor name for logging (e.g., "Python", "Node.js", "Frontend")
	Name() string

	// Detect checks if this vendor applies to the given project path.
	// Returns true if the dependency file exists (e.g., requirements.txt, package.json).
	Detect(projectPath string) bool

	// ShouldVendor checks if vendoring is needed.
	// Returns true if dependencies should be installed/rebuilt.
	ShouldVendor(projectPath string) (bool, error)

	// Vendor performs the Docker-based dependency installation.
	// Returns an error if vendoring fails.
	Vendor(vm *VendorManager) error

	// DockerImage returns the Docker image to use for vendoring.
	DockerImage() string
}

// vendorRegistry holds all registered dependency vendors
var vendorRegistry = []DependencyVendor{}

// RegisterVendor adds a dependency vendor to the registry.
// Called from init() functions in each vendor implementation.
func RegisterVendor(v DependencyVendor) {
	vendorRegistry = append(vendorRegistry, v)
}

// GetVendors returns all registered dependency vendors.
func GetVendors() []DependencyVendor {
	return vendorRegistry
}

// VendorDependenciesIfNeeded iterates through all registered vendors
// and installs dependencies for any detected projects.
func VendorDependenciesIfNeeded(projectPath string) error {
	blue := color.New(color.FgBlue)
	red := color.New(color.FgRed)

	// Collect vendors that need to run
	var vendorsToRun []DependencyVendor
	for _, vendor := range GetVendors() {
		shouldVendor, err := vendor.ShouldVendor(projectPath)
		if err != nil {
			return err
		}
		if shouldVendor {
			vendorsToRun = append(vendorsToRun, vendor)
		}
	}

	if len(vendorsToRun) == 0 {
		return nil
	}

	// Check Docker availability once for all vendors
	if err := CheckDockerInstalled(); err != nil {
		red.Printf("Docker required for dependency installation: %v\n", err)
		red.Println("Cannot deploy bot without installed dependencies")
		red.Println("Solution: Install Docker and try again")
		return fmt.Errorf("docker unavailable but dependencies detected - installation required")
	}

	vm, err := NewVendorManager(projectPath)
	if err != nil {
		red.Printf("Failed to initialize dependency installer: %v\n", err)
		red.Println("Cannot deploy bot without installed dependencies")
		return fmt.Errorf("vendor manager initialization failed: %v", err)
	}
	defer vm.Close()

	if err := vm.CheckDockerRunning(); err != nil {
		red.Printf("Docker daemon not running: %v\n", err)
		red.Println("Cannot install dependencies without Docker")
		red.Println("Solution: Start Docker daemon and try again")
		return fmt.Errorf("docker daemon not running but dependencies detected - installation required")
	}

	// Run each vendor
	for _, vendor := range vendorsToRun {
		blue.Printf("%s dependencies detected\n", vendor.Name())
		if err := vendor.Vendor(vm); err != nil {
			red.Printf("%s dependency installation failed: %v\n", vendor.Name(), err)
			red.Println("Cannot deploy bot without successfully installed dependencies")
			return fmt.Errorf("%s vendoring failed: %v", vendor.Name(), err)
		}
	}

	return nil
}
