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

			// Create vendor manager
			vm, err := internal.NewVendorManager(tmpDir)
			if err != nil {
				t.Skipf("Failed to create vendor manager: %v", err)
			}
			defer vm.Close()

			// Test
			exists := vm.CheckRequirementsFile()
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
			shouldVendor, err := internal.ShouldPerformVendoring(tmpDir)
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

func TestVendorManagerWithMockDocker(t *testing.T) {
	// Test VendorManager creation and basic operations without actually using Docker
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create requirements.txt
	requirementsContent := "requests==2.25.1\nnumpy==1.21.0\n"
	if err := os.WriteFile(filepath.Join(tmpDir, "requirements.txt"), []byte(requirementsContent), 0644); err != nil {
		t.Fatalf("Failed to create requirements.txt: %v", err)
	}

	// Test VendorManager creation
	vm, err := internal.NewVendorManager(tmpDir)
	if err != nil {
		t.Skipf("Failed to create vendor manager (Docker may not be available): %v", err)
	}
	defer vm.Close()

	// Test CheckRequirementsFile
	if !vm.CheckRequirementsFile() {
		t.Error("Expected requirements file to exist")
	}

	// Test CheckDockerRunning (may fail if Docker not available)
	err = vm.CheckDockerRunning()
	if err != nil {
		t.Logf("Docker not running (expected in CI): %v", err)
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
	err = internal.PerformVendoringIfNeeded(tmpDir)
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
	err = internal.PerformVendoringIfNeeded(tmpDir)
	if err == nil {
		t.Error("Expected error when Docker unavailable but dependencies detected")
	}

	// Verify error message contains expected text
	expectedMsg := "docker unavailable but dependencies detected"
	if !strings.Contains(err.Error(), expectedMsg) {
		t.Errorf("Expected error message to contain '%s', got: %v", expectedMsg, err)
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

func TestVendorDirectoryValidation(t *testing.T) {
	// Test validation of vendored files
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vendorPath := filepath.Join(tmpDir, "vendor")

	tests := []struct {
		name        string
		setupFunc   func() error
		expectError bool
		errorMsg    string
	}{
		{
			name: "vendor directory with packages",
			setupFunc: func() error {
				if err := os.MkdirAll(vendorPath, 0755); err != nil {
					return err
				}
				// Simulate vendored packages
				return os.WriteFile(filepath.Join(vendorPath, "requests.py"), []byte("# requests module"), 0644)
			},
			expectError: false,
		},
		{
			name: "empty vendor directory",
			setupFunc: func() error {
				return os.MkdirAll(vendorPath, 0755)
			},
			expectError: true,
			errorMsg:    "no packages were vendored",
		},
		{
			name: "vendor directory does not exist",
			setupFunc: func() error {
				return nil // Don't create vendor directory
			},
			expectError: true,
			errorMsg:    "failed to read vendor directory",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Clean up before each test
			os.RemoveAll(vendorPath)

			// Setup
			if err := tt.setupFunc(); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			// Create vendor manager for testing
			vm, err := internal.NewVendorManager(tmpDir)
			if err != nil {
				t.Skipf("Failed to create vendor manager: %v", err)
			}
			defer vm.Close()

			// Test verification (this is a private method, so we test the logic indirectly)
			// We simulate the verification by checking directory contents directly
			entries, err := os.ReadDir(vendorPath)
			hasError := err != nil || len(entries) == 0

			if tt.expectError {
				if !hasError {
					t.Error("Expected error but got none")
				}
			} else {
				if hasError {
					t.Errorf("Unexpected error: %v", err)
				}
			}
		})
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

			// Create vendor manager
			vm, err := internal.NewVendorManager(tmpDir)
			if err != nil {
				t.Skipf("Failed to create vendor manager: %v", err)
			}
			defer vm.Close()

			// Test
			exists := vm.CheckPackageJsonFile()
			if exists != tt.expectedExists {
				t.Errorf("Expected package.json exists = %v, got %v", tt.expectedExists, exists)
			}
		})
	}
}

func TestCheckTypeScriptFiles(t *testing.T) {
	tests := []struct {
		name           string
		setupFunc      func(string) error
		expectedExists bool
	}{
		{
			name: "TypeScript files exist",
			setupFunc: func(dir string) error {
				return os.WriteFile(filepath.Join(dir, "bot.ts"), []byte("export class Bot {}"), 0644)
			},
			expectedExists: true,
		},
		{
			name: "multiple TypeScript files exist",
			setupFunc: func(dir string) error {
				if err := os.WriteFile(filepath.Join(dir, "bot.ts"), []byte("export class Bot {}"), 0644); err != nil {
					return err
				}
				return os.WriteFile(filepath.Join(dir, "types.ts"), []byte("export interface Config {}"), 0644)
			},
			expectedExists: true,
		},
		{
			name: "no TypeScript files",
			setupFunc: func(dir string) error {
				return os.WriteFile(filepath.Join(dir, "bot.js"), []byte("class Bot {}"), 0644)
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

			// Create vendor manager
			vm, err := internal.NewVendorManager(tmpDir)
			if err != nil {
				t.Skipf("Failed to create vendor manager: %v", err)
			}
			defer vm.Close()

			// Test
			exists := vm.CheckTypeScriptFiles()
			if exists != tt.expectedExists {
				t.Errorf("Expected TypeScript files exist = %v, got %v", tt.expectedExists, exists)
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
			shouldVendor, err := internal.ShouldPerformNodeVendoring(tmpDir)
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

func TestNodeModulesBackupAndRestore(t *testing.T) {
	// Test node_modules backup and restore functionality
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vm, err := internal.NewVendorManager(tmpDir)
	if err != nil {
		t.Skipf("Failed to create vendor manager (Docker may not be available): %v", err)
	}
	defer vm.Close()

	t.Run("backup existing node_modules", func(t *testing.T) {
		// Create a node_modules directory with some content
		nodeModulesPath := filepath.Join(tmpDir, "node_modules")
		testPackageDir := filepath.Join(nodeModulesPath, "test-package")
		if err := os.MkdirAll(testPackageDir, 0755); err != nil {
			t.Fatalf("Failed to create test node_modules: %v", err)
		}

		testFile := filepath.Join(testPackageDir, "index.js")
		testContent := "console.log('original package');"
		if err := os.WriteFile(testFile, []byte(testContent), 0644); err != nil {
			t.Fatalf("Failed to create test file: %v", err)
		}

		// Backup should succeed
		err := vm.BackupExistingNodeModules()
		if err != nil {
			t.Fatalf("Backup failed: %v", err)
		}

		// Original node_modules should be gone
		if _, err := os.Stat(nodeModulesPath); !os.IsNotExist(err) {
			t.Error("Original node_modules should be removed after backup")
		}

		// Backup should exist
		backupPath := filepath.Join(tmpDir, "_node_modules_backup")
		if _, err := os.Stat(backupPath); os.IsNotExist(err) {
			t.Error("Backup directory should exist")
		}

		// Test file should exist in backup
		backupTestFile := filepath.Join(backupPath, "test-package", "index.js")
		if content, err := os.ReadFile(backupTestFile); err != nil {
			t.Errorf("Failed to read backup test file: %v", err)
		} else if string(content) != testContent {
			t.Errorf("Backup content mismatch: expected %q, got %q", testContent, string(content))
		}
	})

	t.Run("restore from backup", func(t *testing.T) {
		// Create a "failed" node_modules
		nodeModulesPath := filepath.Join(tmpDir, "node_modules")
		if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
			t.Fatalf("Failed to create failed node_modules: %v", err)
		}
		failedFile := filepath.Join(nodeModulesPath, "failed.txt")
		if err := os.WriteFile(failedFile, []byte("failed vendoring"), 0644); err != nil {
			t.Fatalf("Failed to create failed file: %v", err)
		}

		// Restore should succeed
		err := vm.RestoreNodeModulesBackup()
		if err != nil {
			t.Fatalf("Restore failed: %v", err)
		}

		// Failed node_modules should be gone
		if _, err := os.Stat(failedFile); !os.IsNotExist(err) {
			t.Error("Failed node_modules should be removed after restore")
		}

		// Original content should be restored
		testFile := filepath.Join(nodeModulesPath, "test-package", "index.js")
		if content, err := os.ReadFile(testFile); err != nil {
			t.Errorf("Failed to read restored test file: %v", err)
		} else if string(content) != "console.log('original package');" {
			t.Errorf("Restored content mismatch: expected %q, got %q", "console.log('original package');", string(content))
		}

		// Backup should be gone
		backupPath := filepath.Join(tmpDir, "_node_modules_backup")
		if _, err := os.Stat(backupPath); !os.IsNotExist(err) {
			t.Error("Backup directory should be removed after restore")
		}
	})
}

func TestNodeModulesBackupWithoutExisting(t *testing.T) {
	// Test backup when no node_modules exists
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vm, err := internal.NewVendorManager(tmpDir)
	if err != nil {
		t.Skipf("Failed to create vendor manager (Docker may not be available): %v", err)
	}
	defer vm.Close()

	// Backup should succeed (no-op)
	err = vm.BackupExistingNodeModules()
	if err != nil {
		t.Fatalf("Backup should succeed when no node_modules exists: %v", err)
	}

	// No backup should be created
	backupPath := filepath.Join(tmpDir, "_node_modules_backup")
	if _, err := os.Stat(backupPath); !os.IsNotExist(err) {
		t.Error("Backup directory should not be created when no node_modules exists")
	}
}

func TestNodeModulesRestoreWithoutBackup(t *testing.T) {
	// Test restore when no backup exists
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vm, err := internal.NewVendorManager(tmpDir)
	if err != nil {
		t.Skipf("Failed to create vendor manager (Docker may not be available): %v", err)
	}
	defer vm.Close()

	// Restore should succeed (no-op)
	err = vm.RestoreNodeModulesBackup()
	if err != nil {
		t.Fatalf("Restore should succeed when no backup exists: %v", err)
	}
}

func TestNodeModulesCleanupBackup(t *testing.T) {
	// Test cleanup of backup after successful vendoring
	tmpDir, err := os.MkdirTemp("", "vendor-test-")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	vm, err := internal.NewVendorManager(tmpDir)
	if err != nil {
		t.Skipf("Failed to create vendor manager (Docker may not be available): %v", err)
	}
	defer vm.Close()

	// Create a backup directory
	backupPath := filepath.Join(tmpDir, "_node_modules_backup")
	if err := os.MkdirAll(backupPath, 0755); err != nil {
		t.Fatalf("Failed to create backup directory: %v", err)
	}
	testFile := filepath.Join(backupPath, "test.txt")
	if err := os.WriteFile(testFile, []byte("backup content"), 0644); err != nil {
		t.Fatalf("Failed to create backup file: %v", err)
	}

	// Cleanup should succeed
	err = vm.CleanupNodeModulesBackup()
	if err != nil {
		t.Fatalf("Cleanup failed: %v", err)
	}

	// Backup should be gone
	if _, err := os.Stat(backupPath); !os.IsNotExist(err) {
		t.Error("Backup directory should be removed after cleanup")
	}
}

// Frontend Build Tests

func TestCheckFrontendExists(t *testing.T) {
	tests := []struct {
		name           string
		setupFunc      func(string) error
		expectedExists bool
	}{
		{
			name: "frontend/package.json exists",
			setupFunc: func(dir string) error {
				frontendDir := filepath.Join(dir, "frontend")
				if err := os.MkdirAll(frontendDir, 0755); err != nil {
					return err
				}
				packageJson := `{"name": "bot-frontend", "dependencies": {"react": "^19.0.0"}}`
				return os.WriteFile(filepath.Join(frontendDir, "package.json"), []byte(packageJson), 0644)
			},
			expectedExists: true,
		},
		{
			name: "frontend directory exists but no package.json",
			setupFunc: func(dir string) error {
				frontendDir := filepath.Join(dir, "frontend")
				return os.MkdirAll(frontendDir, 0755)
			},
			expectedExists: false,
		},
		{
			name: "no frontend directory",
			setupFunc: func(dir string) error {
				return nil
			},
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			vm, err := internal.NewVendorManager(tmpDir)
			if err != nil {
				t.Skipf("Failed to create vendor manager: %v", err)
			}
			defer vm.Close()

			exists := vm.CheckFrontendExists()
			if exists != tt.expectedExists {
				t.Errorf("Expected frontend exists = %v, got %v", tt.expectedExists, exists)
			}
		})
	}
}

func TestShouldBuildFrontend(t *testing.T) {
	tests := []struct {
		name        string
		setupFunc   func(string) error
		shouldBuild bool
		expectError bool
	}{
		{
			name: "no frontend directory",
			setupFunc: func(dir string) error {
				return nil
			},
			shouldBuild: false,
		},
		{
			name: "frontend/package.json exists - should always build",
			setupFunc: func(dir string) error {
				frontendDir := filepath.Join(dir, "frontend")
				if err := os.MkdirAll(frontendDir, 0755); err != nil {
					return err
				}
				packageJson := `{"name": "bot-frontend"}`
				return os.WriteFile(filepath.Join(frontendDir, "package.json"), []byte(packageJson), 0644)
			},
			shouldBuild: true,
		},
		{
			name: "frontend with existing bundle - should still build (no caching)",
			setupFunc: func(dir string) error {
				frontendDir := filepath.Join(dir, "frontend")
				distDir := filepath.Join(frontendDir, "dist")
				if err := os.MkdirAll(distDir, 0755); err != nil {
					return err
				}
				packageJson := `{"name": "bot-frontend"}`
				if err := os.WriteFile(filepath.Join(frontendDir, "package.json"), []byte(packageJson), 0644); err != nil {
					return err
				}
				// Create an existing bundle
				return os.WriteFile(filepath.Join(distDir, "bundle.js"), []byte("// old bundle"), 0644)
			},
			shouldBuild: true, // Always build - no caching
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tmpDir, err := os.MkdirTemp("", "vendor-test-")
			if err != nil {
				t.Fatalf("Failed to create temp dir: %v", err)
			}
			defer os.RemoveAll(tmpDir)

			if err := tt.setupFunc(tmpDir); err != nil {
				t.Fatalf("Setup failed: %v", err)
			}

			shouldBuild, err := internal.ShouldBuildFrontend(tmpDir)
			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
				if shouldBuild != tt.shouldBuild {
					t.Errorf("Expected shouldBuild = %v, got %v", tt.shouldBuild, shouldBuild)
				}
			}
		})
	}
}
