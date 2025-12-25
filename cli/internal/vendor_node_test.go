package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestNodeVendor_Name(t *testing.T) {
	v := &NodeVendor{}
	if v.Name() != "JavaScript" {
		t.Errorf("Expected 'JavaScript', got '%s'", v.Name())
	}
}

func TestNodeVendor_DockerImage(t *testing.T) {
	v := &NodeVendor{}
	if v.DockerImage() != "node:20-slim" {
		t.Errorf("Expected 'node:20-slim', got '%s'", v.DockerImage())
	}
}

func TestNodeVendor_Detect_WithPackageJson(t *testing.T) {
	tmpDir := t.TempDir()
	packageJsonPath := filepath.Join(tmpDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "test"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	v := &NodeVendor{}
	if !v.Detect(tmpDir) {
		t.Error("Expected Detect to return true when package.json exists")
	}
}

func TestNodeVendor_Detect_WithoutPackageJson(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	if v.Detect(tmpDir) {
		t.Error("Expected Detect to return false when package.json doesn't exist")
	}
}

func TestNodeVendor_ShouldVendor_NoPackageJson(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldVendor {
		t.Error("Expected ShouldVendor to return false when no package.json")
	}
}

func TestNodeVendor_ShouldVendor_WithPackageJson(t *testing.T) {
	tmpDir := t.TempDir()
	packageJsonPath := filepath.Join(tmpDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "test"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	v := &NodeVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !shouldVendor {
		t.Error("Expected ShouldVendor to return true when package.json exists")
	}
}

func TestNodeVendor_ShouldVendor_NodeModulesNewer(t *testing.T) {
	tmpDir := t.TempDir()

	// Create package.json with older timestamp
	packageJsonPath := filepath.Join(tmpDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "test"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}
	// Set package.json to be older
	oldTime := time.Now().Add(-time.Hour)
	if err := os.Chtimes(packageJsonPath, oldTime, oldTime); err != nil {
		t.Fatalf("Failed to set package.json time: %v", err)
	}

	// Create node_modules directory with newer timestamp
	nodeModulesPath := filepath.Join(tmpDir, "node_modules")
	if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
		t.Fatalf("Failed to create node_modules directory: %v", err)
	}

	v := &NodeVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldVendor {
		t.Error("Expected ShouldVendor to return false when node_modules is newer than package.json")
	}
}

func TestNodeVendor_HasTypeScriptFiles_NoFiles(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	if v.hasTypeScriptFiles(tmpDir) {
		t.Error("Expected hasTypeScriptFiles to return false when no .ts files")
	}
}

func TestNodeVendor_HasTypeScriptFiles_WithFiles(t *testing.T) {
	tmpDir := t.TempDir()
	tsPath := filepath.Join(tmpDir, "index.ts")
	if err := os.WriteFile(tsPath, []byte("const x: number = 1;"), 0644); err != nil {
		t.Fatalf("Failed to create .ts file: %v", err)
	}

	v := &NodeVendor{}
	if !v.hasTypeScriptFiles(tmpDir) {
		t.Error("Expected hasTypeScriptFiles to return true when .ts files exist")
	}
}

func TestNodeVendor_GetInstallCommand_Production(t *testing.T) {
	v := &NodeVendor{}
	cmd := v.getInstallCommand(false)

	if !strings.Contains(cmd, "npm install --production") {
		t.Error("Expected command to contain 'npm install --production'")
	}

	if !strings.Contains(cmd, "chown") {
		t.Error("Expected command to contain chown for proper ownership")
	}
}

func TestNodeVendor_GetInstallCommand_TypeScript(t *testing.T) {
	v := &NodeVendor{}
	cmd := v.getInstallCommand(true)

	if !strings.Contains(cmd, "npm install && npm run build") {
		t.Error("Expected command to contain 'npm install && npm run build' for TypeScript")
	}

	if strings.Contains(cmd, "--production") {
		t.Error("Did not expect --production flag for TypeScript builds")
	}
}

func TestNodeVendor_BackupRestore(t *testing.T) {
	tmpDir := t.TempDir()

	// Create node_modules with a test file
	nodeModulesPath := filepath.Join(tmpDir, "node_modules")
	if err := os.MkdirAll(nodeModulesPath, 0755); err != nil {
		t.Fatalf("Failed to create node_modules: %v", err)
	}
	testFile := filepath.Join(nodeModulesPath, "test.txt")
	if err := os.WriteFile(testFile, []byte("test"), 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	v := &NodeVendor{}

	// Test backup
	if err := v.backupExistingNodeModules(tmpDir); err != nil {
		t.Fatalf("Failed to backup node_modules: %v", err)
	}

	// Verify backup exists
	backupPath := filepath.Join(tmpDir, "_node_modules_backup")
	if _, err := os.Stat(backupPath); os.IsNotExist(err) {
		t.Error("Expected backup directory to exist")
	}

	// Verify original is gone
	if _, err := os.Stat(nodeModulesPath); !os.IsNotExist(err) {
		t.Error("Expected original node_modules to be removed")
	}

	// Test restore
	if err := v.restoreNodeModulesBackup(tmpDir); err != nil {
		t.Fatalf("Failed to restore node_modules: %v", err)
	}

	// Verify node_modules is restored
	if _, err := os.Stat(nodeModulesPath); os.IsNotExist(err) {
		t.Error("Expected node_modules to be restored")
	}

	// Verify test file exists
	if _, err := os.Stat(testFile); os.IsNotExist(err) {
		t.Error("Expected test file to be restored")
	}
}

func TestNodeVendor_CleanupBackup(t *testing.T) {
	tmpDir := t.TempDir()

	// Create backup directory
	backupPath := filepath.Join(tmpDir, "_node_modules_backup")
	if err := os.MkdirAll(backupPath, 0755); err != nil {
		t.Fatalf("Failed to create backup directory: %v", err)
	}

	v := &NodeVendor{}
	if err := v.cleanupNodeModulesBackup(tmpDir); err != nil {
		t.Fatalf("Failed to cleanup backup: %v", err)
	}

	// Verify backup is removed
	if _, err := os.Stat(backupPath); !os.IsNotExist(err) {
		t.Error("Expected backup directory to be removed")
	}
}

func TestNodeVendor_BackupNoExisting(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	// Should not error when no node_modules exists
	if err := v.backupExistingNodeModules(tmpDir); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
}

func TestNodeVendor_RestoreNoBackup(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	// Should not error when no backup exists
	if err := v.restoreNodeModulesBackup(tmpDir); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
}

func TestNodeVendor_CleanupNoBackup(t *testing.T) {
	tmpDir := t.TempDir()

	v := &NodeVendor{}
	// Should not error when no backup exists
	if err := v.cleanupNodeModulesBackup(tmpDir); err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
}
