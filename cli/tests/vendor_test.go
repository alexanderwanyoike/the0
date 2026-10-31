package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"the0/cmd"
	"the0/internal"
)

func TestCheckDockerInstalled(t *testing.T) {
	// This test will pass if Docker is installed, skip if not available
	err := internal.CheckDockerInstalled()
	if err != nil {
		t.Skipf("Docker not installed: %v", err)
	}
}

func TestCheckRequirementsFile(t *testing.T) {
	tests := []struct {
		name           string
		setupFunc      func(string) error
		projectPath    string
		expectedExists bool
	}{
		{
			name: "requirements.txt exists",
			setupFunc: func(dir string) error {
				return os.WriteFile(filepath.Join(dir, "requirements.txt"), []byte("requests==2.25.1\n"), 0644)
			},
			expectedExists: true,
		},
		{
			name: "requirements.txt does not exist",
			setupFunc: func(dir string) error {
				return nil // Don't create the file
			},
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary directory
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			// Setup test case
			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			exists := (&internal.PythonVendor{}).Detect(tmpDir)
			if exists != tt.expectedExists {
				t.Errorf("Expected requirements file exists = %v, got %v", tt.expectedExists, exists)
			}
		})
	}
}

func TestShouldPerformVendoring(t *testing.T) {
	tests := []struct {
		name         string
		setupFunc    func(string) error
		shouldVendor bool
		expectError  bool
	}{
		{
			name: "no requirements.txt",
			setupFunc: func(dir string) error {
				return nil // Don't create requirements.txt
			},
			shouldVendor: false,
		},
		{
			name: "requirements.txt exists, no vendor directory",
			setupFunc: func(dir string) error {
				return os.WriteFile(filepath.Join(dir, "requirements.txt"), []byte("requests==2.25.1\n"), 0644)
			},
			shouldVendor: true,
		},
		{
			name: "requirements.txt exists, vendor directory is older",
			setupFunc: func(dir string) error {
				// Create vendor directory first (older)
				vendorDir := filepath.Join(dir, "vendor")
				if err := os.MkdirAll(vendorDir, 0755); err != nil {
					return err
				}

				// Sleep briefly then create requirements.txt (newer)
				time.Sleep(10 * time.Millisecond)
				return os.WriteFile(filepath.Join(dir, "requirements.txt"), []byte("requests==2.25.1\n"), 0644)
			},
			shouldVendor: true,
		},
		{
			name: "requirements.txt exists, vendor directory is newer",
			setupFunc: func(dir string) error {
				// Create requirements.txt first (older)
				if err := os.WriteFile(filepath.Join(dir, "requirements.txt"), []byte("requests==2.25.1\n"), 0644); err != nil {
					return err
				}

				// Sleep briefly then create vendor directory (newer)
				time.Sleep(10 * time.Millisecond)
				vendorDir := filepath.Join(dir, "vendor")
				return os.MkdirAll(vendorDir, 0755)
			},
			shouldVendor: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary directory
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			// Setup test case
			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			// Test
			shouldVendor, err := (&internal.PythonVendor{}).ShouldVendor(tmpDir)
			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
				if shouldVendor != tt.shouldVendor {
					t.Errorf("Expected shouldVendor = %v, got %v", tt.shouldVendor, shouldVendor)
				}
			}
		})
	}
}

func TestPerformVendoringIfNeeded_NoRequirements(t *testing.T) {
	// Test when no requirements.txt exists
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Should not error and should not attempt vendoring
	err = internal.PerformVendoringIfNeeded(tmpDir, "")
	if err != nil {
		t.Errorf("Unexpected error when no requirements.txt exists: %v", err)
	}

	// Vendor directory should not be created
	vendorPath := filepath.Join(tmpDir, "vendor")
	if _, err := os.Stat(vendorPath); !os.IsNotExist(err) {
		t.Error("Vendor directory should not exist when no requirements.txt")
	}
}

func TestPerformVendoringIfNeeded_DockerNotAvailable(t *testing.T) {
	// Test that vendoring fails hard when Docker is not available but dependencies exist

	// Skip this test if Docker is actually available
	if internal.CheckDockerInstalled() == nil {
		t.Skip("Docker is available - test requires Docker to be unavailable")
	}

	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create requirements.txt
	requirementsContent := "requests==2.25.1\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "requirements.txt"), []byte(requirementsContent), 0644); err != nil {
		t.Fatalf("Failed to create requirements.txt: %v", err)
	}

	// Should error when Docker is not available but dependencies are detected
	err = internal.PerformVendoringIfNeeded(tmpDir, "")
	if err == nil {
		t.Error("Expected error when Docker unavailable but dependencies detected")
	}

	// Verify error message contains expected text
	expectedMsg := "docker unavailable but dependencies detected"
	if !strings.Contains(err.Error(), expectedMsg) {
		t.Errorf("Expected error message to contain '%s', got: %v", expectedMsg, err)
	}
}

func TestPerformVendoringIfNeeded_LeavesFrontendToFrontendStep(t *testing.T) {
	// Deploy runs dependency vendoring and then BuildFrontendIfNeeded, so the
	// frontend is built once only if vendoring skips it. With no docker on
	// PATH, a step that tries to build fails fast, which shows which step
	// builds without running a container.
	t.Setenv("PATH", t.TempDir())

	tmpDir := t.TempDir()
	frontendDir := filepath.Join(tmpDir, "frontend")
	if err := os.MkdirAll(frontendDir, 0755); err != nil {
		t.Fatalf("Failed to create frontend directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(frontendDir, "package.json"), []byte(`{"name": "frontend"}`), 0644); err != nil {
		t.Fatalf("Failed to create frontend/package.json: %v", err)
	}

	if err := internal.PerformVendoringIfNeeded(tmpDir, ""); err != nil {
		t.Errorf("Expected dependency vendoring not to build the frontend, got: %v", err)
	}

	if err := internal.BuildFrontendIfNeeded(tmpDir); err == nil {
		t.Error("Expected the frontend step to build the frontend")
	}
}

func TestCustomBotDeployCommand_WithVendoring(t *testing.T) {
	// Test that the deploy command includes vendoring step
	cmd := cmd.NewDeployCmd()

	// Verify command exists
	if cmd == nil {
		t.Fatal("Deploy command should not be nil")
	}

	// Verify command has correct usage
	expectedUsage := "deploy"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	// Note: We can't easily test the actual vendoring integration without
	// setting up a full bot project environment, but we can verify the
	// command structure
}

func TestVendorDirectoryCreation(t *testing.T) {
	// Test vendor directory creation logic
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vendorPath := filepath.Join(tmpDir, "vendor")

	// Create existing vendor directory with some content
	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		t.Fatalf("Failed to create vendor dir: %v", err)
	}

	testFile := filepath.Join(vendorPath, "test.txt")
	if err := os.WriteFile(testFile, []byte("old content"), 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	// Simulate vendor directory cleanup (part of vendoring process)
	if err := os.RemoveAll(vendorPath); err != nil {
		t.Errorf("Failed to clean vendor directory: %v", err)
	}

	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		t.Errorf("Failed to create new vendor directory: %v", err)
	}

	// Verify directory exists and is empty
	entries, err := os.ReadDir(vendorPath)
	if err != nil {
		t.Errorf("Failed to read vendor directory: %v", err)
	}

	if len(entries) != 0 {
		t.Errorf("Expected empty vendor directory, got %d entries", len(entries))
	}
}

// JavaScript/Node.js Vendoring Tests

func TestCheckPackageJsonFile(t *testing.T) {
	tests := []struct {
		name           string
		setupFunc      func(string) error
		expectedExists bool
	}{
		{
			name: "package.json exists",
			setupFunc: func(dir string) error {
				packageJson := `{
					"name": "test-bot",
					"version": "1.0.0",
					"dependencies": {
						"express": "^4.18.0"
					}
				}`
				return os.WriteFile(filepath.Join(dir, "package.json"), []byte(packageJson), 0644)
			},
			expectedExists: true,
		},
		{
			name: "package.json does not exist",
			setupFunc: func(dir string) error {
				return nil // Don't create the file
			},
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary directory
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			// Setup test case
			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			exists := (&internal.NodeVendor{}).Detect(tmpDir)
			if exists != tt.expectedExists {
				t.Errorf("Expected package.json exists = %v, got %v", tt.expectedExists, exists)
			}
		})
	}
}

func TestShouldPerformNodeVendoring(t *testing.T) {
	tests := []struct {
		name         string
		setupFunc    func(string) error
		shouldVendor bool
		expectError  bool
	}{
		{
			name: "no package.json",
			setupFunc: func(dir string) error {
				return nil // Don't create package.json
			},
			shouldVendor: false,
		},
		{
			name: "package.json exists, no node_modules directory",
			setupFunc: func(dir string) error {
				packageJson := `{"name": "test-bot", "dependencies": {"express": "^4.18.0"}}`
				return os.WriteFile(filepath.Join(dir, "package.json"), []byte(packageJson), 0644)
			},
			shouldVendor: true,
		},
		{
			name: "package.json exists, node_modules directory is older",
			setupFunc: func(dir string) error {
				// Create node_modules directory first (older)
				nodeModulesDir := filepath.Join(dir, "node_modules")
				if err := os.MkdirAll(nodeModulesDir, 0755); err != nil {
					return err
				}

				// Sleep briefly then create package.json (newer)
				time.Sleep(10 * time.Millisecond)
				packageJson := `{"name": "test-bot", "dependencies": {"express": "^4.18.0"}}`
				return os.WriteFile(filepath.Join(dir, "package.json"), []byte(packageJson), 0644)
			},
			shouldVendor: true,
		},
		{
			name: "package.json exists, node_modules directory is newer",
			setupFunc: func(dir string) error {
				// Create package.json first (older)
				packageJson := `{"name": "test-bot", "dependencies": {"express": "^4.18.0"}}`
				if err := os.WriteFile(filepath.Join(dir, "package.json"), []byte(packageJson), 0644); err != nil {
					return err
				}

				// Sleep briefly then create node_modules directory (newer)
				time.Sleep(10 * time.Millisecond)
				nodeModulesDir := filepath.Join(dir, "node_modules")
				return os.MkdirAll(nodeModulesDir, 0755)
			},
			shouldVendor: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create temporary directory
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			// Setup test case
			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			// Test
			shouldVendor, err := (&internal.NodeVendor{}).ShouldVendor(tmpDir)
			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
				if shouldVendor != tt.shouldVendor {
					t.Errorf("Expected shouldVendor = %v, got %v", tt.shouldVendor, shouldVendor)
				}
			}
		})
	}
}

func TestCleanupVendoring(t *testing.T) {
	// Test cleanup functionality
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create vendor directory with some content
	vendorPath := filepath.Join(tmpDir, "vendor")
	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		t.Fatalf("Failed to create vendor dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(vendorPath, "test.py"), []byte("# test"), 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	// Create node_modules directory with some content
	nodeModulesPath := filepath.Join(tmpDir, "node_modules")
	if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
		t.Fatalf("Failed to create node_modules dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(nodeModulesPath, "package.json"), []byte("{}"), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	// Create some bot ZIP files
	zipFile1 := filepath.Join(tmpDir, "bot_12345.zip")
	zipFile2 := filepath.Join(tmpDir, "bot_67890.zip")
	if err := os.WriteFile(zipFile1, []byte("zip content"), 0644); err != nil {
		t.Fatalf("Failed to create zip file 1: %v", err)
	}
	if err := os.WriteFile(zipFile2, []byte("zip content"), 0644); err != nil {
		t.Fatalf("Failed to create zip file 2: %v", err)
	}

	// Perform cleanup
	internal.CleanupVendoring(tmpDir)

	// Verify all files and directories are removed
	if _, err := os.Stat(vendorPath); !os.IsNotExist(err) {
		t.Error("Vendor directory should be removed")
	}

	if _, err := os.Stat(nodeModulesPath); !os.IsNotExist(err) {
		t.Error("node_modules directory should be removed")
	}

	if _, err := os.Stat(zipFile1); !os.IsNotExist(err) {
		t.Error("Bot ZIP file 1 should be removed")
	}

	if _, err := os.Stat(zipFile2); !os.IsNotExist(err) {
		t.Error("Bot ZIP file 2 should be removed")
	}
}
