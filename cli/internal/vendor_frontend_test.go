package internal

import (
	"os"
	"path/filepath"
	"testing"
)

func TestFrontendVendor_Name(t *testing.T) {
	v := &FrontendVendor{}
	if v.Name() != "Frontend" {
		t.Errorf("Expected 'Frontend', got '%s'", v.Name())
	}
}

func TestFrontendVendor_DockerImage(t *testing.T) {
	v := &FrontendVendor{}
	// Frontend uses the same Node image
	if v.DockerImage() != "node:20-slim" {
		t.Errorf("Expected 'node:20-slim', got '%s'", v.DockerImage())
	}
}

func TestFrontendVendor_Detect_WithFrontendPackageJson(t *testing.T) {
	tmpDir := t.TempDir()
	frontendDir := filepath.Join(tmpDir, "frontend")
	if err := os.MkdirAll(frontendDir, 0755); err != nil {
		t.Fatalf("Failed to create frontend directory: %v", err)
	}
	packageJsonPath := filepath.Join(frontendDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "frontend"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	v := &FrontendVendor{}
	if !v.Detect(tmpDir) {
		t.Error("Expected Detect to return true when frontend/package.json exists")
	}
}

func TestFrontendVendor_Detect_WithoutFrontend(t *testing.T) {
	tmpDir := t.TempDir()

	v := &FrontendVendor{}
	if v.Detect(tmpDir) {
		t.Error("Expected Detect to return false when frontend/package.json doesn't exist")
	}
}

func TestFrontendVendor_Detect_RootPackageJsonOnly(t *testing.T) {
	tmpDir := t.TempDir()
	// Create package.json in root (not in frontend/)
	packageJsonPath := filepath.Join(tmpDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "root"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	v := &FrontendVendor{}
	if v.Detect(tmpDir) {
		t.Error("Expected Detect to return false when only root package.json exists (no frontend/)")
	}
}

func TestFrontendVendor_ShouldVendor_NoFrontend(t *testing.T) {
	tmpDir := t.TempDir()

	v := &FrontendVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldVendor {
		t.Error("Expected ShouldVendor to return false when no frontend/package.json")
	}
}

func TestFrontendVendor_ShouldVendor_WithFrontend(t *testing.T) {
	tmpDir := t.TempDir()
	frontendDir := filepath.Join(tmpDir, "frontend")
	if err := os.MkdirAll(frontendDir, 0755); err != nil {
		t.Fatalf("Failed to create frontend directory: %v", err)
	}
	packageJsonPath := filepath.Join(frontendDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "frontend"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	v := &FrontendVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !shouldVendor {
		t.Error("Expected ShouldVendor to return true when frontend/package.json exists")
	}
}

func TestFrontendVendor_ShouldVendor_AlwaysBuilds(t *testing.T) {
	tmpDir := t.TempDir()
	frontendDir := filepath.Join(tmpDir, "frontend")
	if err := os.MkdirAll(frontendDir, 0755); err != nil {
		t.Fatalf("Failed to create frontend directory: %v", err)
	}
	packageJsonPath := filepath.Join(frontendDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "frontend"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	// Create dist directory (simulating a previous build)
	distDir := filepath.Join(frontendDir, "dist")
	if err := os.MkdirAll(distDir, 0755); err != nil {
		t.Fatalf("Failed to create dist directory: %v", err)
	}
	bundlePath := filepath.Join(distDir, "bundle.js")
	if err := os.WriteFile(bundlePath, []byte("// old bundle"), 0644); err != nil {
		t.Fatalf("Failed to create bundle.js: %v", err)
	}

	v := &FrontendVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	// Frontend always builds when detected - doesn't check cache
	if !shouldVendor {
		t.Error("Expected ShouldVendor to return true even with existing dist/bundle.js")
	}
}

func TestBuildFrontendIfNeeded_NoFrontend(t *testing.T) {
	tmpDir := t.TempDir()

	// Should return nil when no frontend exists
	err := BuildFrontendIfNeeded(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error when no frontend: %v", err)
	}
}

func TestShouldBuildFrontend_LegacyFunction(t *testing.T) {
	tmpDir := t.TempDir()

	// Test without frontend
	shouldBuild, err := ShouldBuildFrontend(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldBuild {
		t.Error("Expected ShouldBuildFrontend to return false when no frontend")
	}

	// Create frontend
	frontendDir := filepath.Join(tmpDir, "frontend")
	if err := os.MkdirAll(frontendDir, 0755); err != nil {
		t.Fatalf("Failed to create frontend directory: %v", err)
	}
	packageJsonPath := filepath.Join(frontendDir, "package.json")
	if err := os.WriteFile(packageJsonPath, []byte(`{"name": "frontend"}`), 0644); err != nil {
		t.Fatalf("Failed to create package.json: %v", err)
	}

	// Test with frontend
	shouldBuild, err = ShouldBuildFrontend(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !shouldBuild {
		t.Error("Expected ShouldBuildFrontend to return true when frontend exists")
	}
}
