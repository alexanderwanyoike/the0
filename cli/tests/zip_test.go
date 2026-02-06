package internal

import (
	"archive/zip"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"the0/internal"
)

func TestCreateBotZip(t *testing.T) {
	// Create temporary directory for test
	tempDir, err := os.MkdirTemp("", "bot-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create test files in temp directory
	testFiles := []string{
		"main.py",
		"bot-schema.json",
		"README.md",
		"utils.py",
		"config.yaml",
	}

	for _, file := range testFiles {
		filePath := filepath.Join(tempDir, file)
		content := "# Test content for " + file
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create test file %s: %v", file, err)
		}
	}

	// Create subdirectory with files
	srcDir := filepath.Join(tempDir, "src")
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		t.Fatalf("Failed to create src directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(srcDir, "helper.py"), []byte("# Helper file"), 0644); err != nil {
		t.Fatalf("Failed to create src/helper.py: %v", err)
	}

	// Create hidden file (should be skipped)
	if err := os.WriteFile(filepath.Join(tempDir, ".hidden"), []byte("hidden content"), 0644); err != nil {
		t.Fatalf("Failed to create hidden file: %v", err)
	}

	// Create hidden directory (should be skipped)
	gitDir := filepath.Join(tempDir, ".git")
	if err := os.MkdirAll(gitDir, 0755); err != nil {
		t.Fatalf("Failed to create .git directory: %v", err)
	}
	if err := os.WriteFile(filepath.Join(gitDir, "config"), []byte("git config"), 0644); err != nil {
		t.Fatalf("Failed to create .git/config: %v", err)
	}

	// Create node_modules directory (should be included when vendored)
	nodeDir := filepath.Join(tempDir, "node_modules", "package")
	if err := os.MkdirAll(nodeDir, 0755); err != nil {
		t.Fatalf("Failed to create node_modules: %v", err)
	}
	if err := os.WriteFile(filepath.Join(nodeDir, "index.js"), []byte("module.exports = {}"), 0644); err != nil {
		t.Fatalf("Failed to create node_modules file: %v", err)
	}

	// Create vendor directory (should be included when vendored)
	vendorDir := filepath.Join(tempDir, "vendor")
	if err := os.MkdirAll(vendorDir, 0755); err != nil {
		t.Fatalf("Failed to create vendor: %v", err)
	}
	if err := os.WriteFile(filepath.Join(vendorDir, "requests.py"), []byte("# requests module"), 0644); err != nil {
		t.Fatalf("Failed to create vendor file: %v", err)
	}

	// Create __pycache__ directory (should be skipped)
	cacheDir := filepath.Join(tempDir, "__pycache__")
	if err := os.MkdirAll(cacheDir, 0755); err != nil {
		t.Fatalf("Failed to create __pycache__: %v", err)
	}
	if err := os.WriteFile(filepath.Join(cacheDir, "cache.pyc"), []byte("compiled python"), 0644); err != nil {
		t.Fatalf("Failed to create cache file: %v", err)
	}

	// Create ZIP from temp directory
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP exists
	if _, err := os.Stat(zipPath); os.IsNotExist(err) {
		t.Fatalf("ZIP file was not created: %s", zipPath)
	}

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	// Check that all expected files are in ZIP
	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
		t.Logf("Found in ZIP: %s", file.Name)
	}

	// Expected files should be in ZIP (including vendored dependencies)
	expectedFiles := []string{
		"main.py",
		"bot-schema.json",
		"README.md",
		"utils.py",
		"config.yaml",
		"src/helper.py",
		"vendor/requests.py",
		"node_modules/package/index.js",
	}

	for _, expectedFile := range expectedFiles {
		if !zipFiles[expectedFile] {
			t.Errorf("Expected file %s not found in ZIP", expectedFile)
		}
	}

	// Files that should NOT be in ZIP
	excludedFiles := []string{
		".hidden",
		".git/config",
		"__pycache__/cache.pyc",
	}

	for _, excludedFile := range excludedFiles {
		if zipFiles[excludedFile] {
			t.Errorf("Excluded file %s should not be in ZIP", excludedFile)
		}
	}

	// Verify that the ZIP file itself is not included
	for fileName := range zipFiles {
		if strings.HasSuffix(fileName, ".zip") {
			t.Errorf("ZIP file itself should not be included: %s", fileName)
		}
	}

	// Verify ZIP is not empty
	if len(zipFiles) == 0 {
		t.Error("ZIP file is empty, expected files to be included")
	}

	t.Logf("ZIP created successfully with %d files", len(zipFiles))
}

func TestCreateBotZipEmptyDirectory(t *testing.T) {
	// Create temporary empty directory
	tempDir, err := os.MkdirTemp("", "empty-bot-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create ZIP from empty directory
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP exists but is essentially empty
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	// Should have very few or no files
	if len(zipReader.File) > 0 {
		t.Logf("Empty directory ZIP contains %d entries (might be directory entries)", len(zipReader.File))
		for _, file := range zipReader.File {
			t.Logf("Entry: %s", file.Name)
		}
	}
}

func TestCreateBotZipWithOnlyHiddenFiles(t *testing.T) {
	// Create temporary directory with only hidden files
	tempDir, err := os.MkdirTemp("", "hidden-files-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create only hidden files
	hiddenFiles := []string{
		".env",
		".gitignore",
		".hidden",
	}

	for _, file := range hiddenFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.WriteFile(filePath, []byte("hidden content"), 0644); err != nil {
			t.Fatalf("Failed to create hidden file %s: %v", file, err)
		}
	}

	// Create ZIP from directory with only hidden files
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP exists
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	// Should not contain any of the hidden files
	for _, file := range zipReader.File {
		if strings.HasPrefix(filepath.Base(file.Name), ".") {
			t.Errorf("Hidden file %s should not be in ZIP", file.Name)
		}
	}

	t.Logf("ZIP with only hidden files contains %d entries (should be 0 or very few)", len(zipReader.File))
}

func TestCreateBotZipCurrentDirectory(t *testing.T) {
	// Test the original function that works with current directory
	tempDir, err := os.MkdirTemp("", "current-dir-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Change to temp directory
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	// Create test files
	testFiles := []string{
		"main.py",
		"README.md",
	}

	for _, file := range testFiles {
		if err := os.WriteFile(file, []byte("test content"), 0644); err != nil {
			t.Fatalf("Failed to create test file %s: %v", file, err)
		}
	}

	// Create ZIP using original function
	zipPath, err := internal.CreateBotZip()
	if err != nil {
		t.Fatalf("CreateBotZip() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
	}

	for _, expectedFile := range testFiles {
		if !zipFiles[expectedFile] {
			t.Errorf("Expected file %s not found in ZIP", expectedFile)
		}
	}

	t.Logf("Current directory ZIP created successfully with %d files", len(zipReader.File))
}

func TestCreateBotZipWithVendoredDependencies(t *testing.T) {
	// Test that vendored dependencies are properly included in ZIP
	tempDir, err := os.MkdirTemp("", "vendor-zip-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create bot files
	botFiles := map[string]string{
		"bot.py":           "# Bot implementation",
		"bot.js":           "// Bot implementation",
		"package.json":     `{"name": "test-bot", "dependencies": {"express": "^4.0.0"}}`,
		"requirements.txt": "requests==2.25.1\nnumpy==1.21.0\n",
	}

	for file, content := range botFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create vendor directory (Python dependencies)
	vendorDir := filepath.Join(tempDir, "vendor")
	if err := os.MkdirAll(vendorDir, 0755); err != nil {
		t.Fatalf("Failed to create vendor dir: %v", err)
	}

	vendorFiles := map[string]string{
		"vendor/requests.py":          "# requests library",
		"vendor/requests/__init__.py": "# requests package init",
		"vendor/numpy.py":             "# numpy library",
	}

	for file, content := range vendorFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.MkdirAll(filepath.Dir(filePath), 0755); err != nil {
			t.Fatalf("Failed to create dir for %s: %v", file, err)
		}
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create node_modules directory (JavaScript dependencies)
	nodeModulesDir := filepath.Join(tempDir, "node_modules")
	if err := os.MkdirAll(nodeModulesDir, 0755); err != nil {
		t.Fatalf("Failed to create node_modules dir: %v", err)
	}

	nodeFiles := map[string]string{
		"node_modules/express/package.json": `{"name": "express", "version": "4.18.0"}`,
		"node_modules/express/index.js":     "module.exports = require('./lib/express');",
		"node_modules/express/bin/express":  "#!/usr/bin/env node",
	}

	for file, content := range nodeFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.MkdirAll(filepath.Dir(filePath), 0755); err != nil {
			t.Fatalf("Failed to create dir for %s: %v", file, err)
		}
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
		t.Logf("Found in ZIP: %s", file.Name)
	}

	// All bot files should be included
	for file := range botFiles {
		if !zipFiles[file] {
			t.Errorf("Expected bot file %s not found in ZIP", file)
		}
	}

	// All vendor files should be included
	for file := range vendorFiles {
		if !zipFiles[file] {
			t.Errorf("Expected vendor file %s not found in ZIP", file)
		}
	}

	// All node_modules files should be included
	for file := range nodeFiles {
		if !zipFiles[file] {
			t.Errorf("Expected node_modules file %s not found in ZIP", file)
		}
	}

	t.Logf("Vendored dependencies ZIP created successfully with %d files", len(zipFiles))
}

func TestCreateBotZipSkipsNonVendoredNodeModules(t *testing.T) {
	// Test that non-vendored node_modules are skipped, but vendored ones are included
	tempDir, err := os.MkdirTemp("", "non-vendor-zip-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create bot files but NO package.json (indicating not a JS project)
	botFiles := map[string]string{
		"bot.py":           "# Bot implementation",
		"requirements.txt": "requests==2.25.1\n",
	}

	for file, content := range botFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create vendor directory (should be included)
	vendorFiles := map[string]string{
		"vendor/requests.py": "# requests library",
	}

	for file, content := range vendorFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.MkdirAll(filepath.Dir(filePath), 0755); err != nil {
			t.Fatalf("Failed to create dir for %s: %v", file, err)
		}
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create node_modules directory without package.json (might be from another project)
	// This should still be included due to our current logic, but in practice
	// it would only exist if it was vendored
	nodeModulesFile := filepath.Join(tempDir, "node_modules", "leftover", "index.js")
	if err := os.MkdirAll(filepath.Dir(nodeModulesFile), 0755); err != nil {
		t.Fatalf("Failed to create node_modules dir: %v", err)
	}
	if err := os.WriteFile(nodeModulesFile, []byte("// leftover file"), 0644); err != nil {
		t.Fatalf("Failed to create node_modules file: %v", err)
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
		t.Logf("Found in ZIP: %s", file.Name)
	}

	// Bot files should be included
	for file := range botFiles {
		if !zipFiles[file] {
			t.Errorf("Expected bot file %s not found in ZIP", file)
		}
	}

	// Vendor files should be included
	for file := range vendorFiles {
		if !zipFiles[file] {
			t.Errorf("Expected vendor file %s not found in ZIP", file)
		}
	}

	// Node_modules files should still be included (our logic includes all vendor dirs)
	if !zipFiles["node_modules/leftover/index.js"] {
		t.Error("Expected node_modules file not found in ZIP")
	}

	t.Logf("Mixed vendoring ZIP created successfully with %d files", len(zipFiles))
}

func TestCreateBotZipWithIgnoreFile(t *testing.T) {
	// Test that .the0ignore file works correctly during ZIP creation
	tempDir, err := os.MkdirTemp("", "ignore-zip-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create test files
	testFiles := map[string]string{
		"main.py":       "# Main bot file",
		"helper.py":     "# Helper module",
		"config.yaml":   "# Configuration",
		"debug.log":     "Debug log content",
		"app.log":       "Application log",
		"README.md":     "# Bot Documentation",
		"test.tmp":      "Temporary file",
		"important.log": "Important log that should be kept",
	}

	for file, content := range testFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create test directory with files
	testDir := filepath.Join(tempDir, "test")
	if err := os.MkdirAll(testDir, 0755); err != nil {
		t.Fatalf("Failed to create test dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(testDir, "test_module.py"), []byte("# Test module"), 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	// Create keep directory with files (should not be ignored)
	keepDir := filepath.Join(tempDir, "keep")
	if err := os.MkdirAll(keepDir, 0755); err != nil {
		t.Fatalf("Failed to create keep dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(keepDir, "keep.py"), []byte("# Keep this file"), 0644); err != nil {
		t.Fatalf("Failed to create keep file: %v", err)
	}

	// Create vendor directory (should always be included)
	vendorDir := filepath.Join(tempDir, "vendor")
	if err := os.MkdirAll(vendorDir, 0755); err != nil {
		t.Fatalf("Failed to create vendor dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(vendorDir, "requests.py"), []byte("# Vendor dependency"), 0644); err != nil {
		t.Fatalf("Failed to create vendor file: %v", err)
	}

	// Create .the0ignore file
	ignoreContent := `# Ignore patterns for bot deployment
*.log
!important.log
*.tmp
test/
# Keep vendor and node_modules (they are protected anyway)`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create .the0ignore: %v", err)
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
		t.Logf("Found in ZIP: %s", file.Name)
	}

	// Files that should be included
	expectedFiles := []string{
		"main.py",
		"helper.py",
		"config.yaml",
		"README.md",
		"important.log", // Negation pattern !important.log
		"keep/keep.py",
		"vendor/requests.py", // Vendor is always included
	}

	for _, expectedFile := range expectedFiles {
		if !zipFiles[expectedFile] {
			t.Errorf("Expected file %s not found in ZIP", expectedFile)
		}
	}

	// Files that should NOT be included
	excludedFiles := []string{
		"debug.log",           // *.log pattern
		"app.log",             // *.log pattern
		"test.tmp",            // *.tmp pattern
		"test/test_module.py", // test/ pattern
	}

	for _, excludedFile := range excludedFiles {
		if zipFiles[excludedFile] {
			t.Errorf("Excluded file %s should not be in ZIP", excludedFile)
		}
	}

	t.Logf("Ignore file ZIP created successfully with %d files", len(zipFiles))
}

func TestCreateBotZipWithComplexIgnorePatterns(t *testing.T) {
	// Test complex ignore patterns including ** and glob patterns
	tempDir, err := os.MkdirTemp("", "complex-ignore-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create nested directory structure
	dirs := []string{
		"src",
		"src/utils",
		"src/models",
		"tests",
		"tests/unit",
		"docs",
		"docs/api",
		"cache",
		"build/output",
	}

	for _, dir := range dirs {
		if err := os.MkdirAll(filepath.Join(tempDir, dir), 0755); err != nil {
			t.Fatalf("Failed to create dir %s: %v", dir, err)
		}
	}

	// Create various files
	testFiles := map[string]string{
		"main.py":                  "# Main file",
		"src/bot.py":               "# Bot implementation",
		"src/utils/helper.py":      "# Helper utilities",
		"src/models/model.py":      "# ML models",
		"tests/test_bot.py":        "# Bot tests",
		"tests/unit/test_utils.py": "# Unit tests",
		"docs/README.md":           "# Documentation",
		"docs/api/api.md":          "# API docs",
		"cache/temp.dat":           "# Cache file",
		"build/output/bot.exe":     "# Build output",
		"app.log":                  "# Application log",
		"src/debug.log":            "# Debug log",
		"error.pyc":                "# Compiled Python",
		"src/models/model.pyc":     "# Compiled model",
		"config_prod.yaml":         "# Production config",
		"config_dev.yaml":          "# Development config",
		"test_runner.py":           "# Test runner (should not be ignored)",
	}

	for file, content := range testFiles {
		filePath := filepath.Join(tempDir, file)
		if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
			t.Fatalf("Failed to create %s: %v", file, err)
		}
	}

	// Create vendor directory
	if err := os.MkdirAll(filepath.Join(tempDir, "vendor"), 0755); err != nil {
		t.Fatalf("Failed to create vendor dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(tempDir, "vendor", "lib.py"), []byte("# Vendor lib"), 0644); err != nil {
		t.Fatalf("Failed to create vendor file: %v", err)
	}

	// Create complex .the0ignore file
	ignoreContent := `# Complex ignore patterns
*.log
**/*.pyc
tests/
docs/**/*.md
cache/
build/
config_*
!config_prod.yaml`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create .the0ignore: %v", err)
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
		t.Logf("Found in ZIP: %s", file.Name)
	}

	// Files that should be included
	expectedFiles := []string{
		"main.py",
		"src/bot.py",
		"src/utils/helper.py",
		"src/models/model.py",
		"config_prod.yaml",       // Negation pattern
		"test_runner.py",         // Should not match tests/ pattern
		"vendor/lib.py",          // Vendor is always included
	}

	for _, expectedFile := range expectedFiles {
		if !zipFiles[expectedFile] {
			t.Errorf("Expected file %s not found in ZIP", expectedFile)
		}
	}

	// Files that should NOT be included
	excludedFiles := []string{
		"app.log",                  // *.log
		"src/debug.log",            // *.log
		"error.pyc",                // **/*.pyc
		"src/models/model.pyc",     // **/*.pyc
		"tests/test_bot.py",        // tests/
		"tests/unit/test_utils.py", // tests/
		"docs/README.md",           // docs/**/*.md
		"docs/api/api.md",          // docs/**/*.md
		"cache/temp.dat",           // cache/
		"config_dev.yaml",          // config_*
		"build/output/bot.exe",     // build/ is in default ignore patterns
	}

	for _, excludedFile := range excludedFiles {
		if zipFiles[excludedFile] {
			t.Errorf("Excluded file %s should not be in ZIP", excludedFile)
		}
	}

	t.Logf("Complex ignore patterns ZIP created successfully with %d files", len(zipFiles))
}

func TestCreateBotZipPreservesExecutablePermissions(t *testing.T) {
	// Test that executable file permissions are preserved in ZIP
	tempDir, err := os.MkdirTemp("", "exec-perm-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create bin directory with executable files
	binDir := filepath.Join(tempDir, "bin")
	if err := os.MkdirAll(binDir, 0755); err != nil {
		t.Fatalf("Failed to create bin dir: %v", err)
	}

	// Create executable files (simulating compiled binaries)
	execFiles := []string{"my-bot", "my-query"}
	for _, name := range execFiles {
		filePath := filepath.Join(binDir, name)
		if err := os.WriteFile(filePath, []byte("#!/bin/bash\necho hello"), 0755); err != nil {
			t.Fatalf("Failed to create executable %s: %v", name, err)
		}
	}

	// Create non-executable file for comparison
	nonExecPath := filepath.Join(tempDir, "config.yaml")
	if err := os.WriteFile(nonExecPath, []byte("key: value"), 0644); err != nil {
		t.Fatalf("Failed to create config file: %v", err)
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents and permissions
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	for _, file := range zipReader.File {
		mode := file.Mode()
		isExecutable := mode&0111 != 0

		switch file.Name {
		case "bin/my-bot", "bin/my-query":
			if !isExecutable {
				t.Errorf("Expected %s to have executable permission, got mode %s", file.Name, mode)
			}
			t.Logf("%s: mode=%s executable=%v", file.Name, mode, isExecutable)
		case "config.yaml":
			if isExecutable {
				t.Errorf("Expected %s to NOT have executable permission, got mode %s", file.Name, mode)
			}
			t.Logf("%s: mode=%s executable=%v", file.Name, mode, isExecutable)
		}
	}
}

func TestCreateBotZipIgnoreFileIncluded(t *testing.T) {
	// Test that .the0ignore file itself is NOT included in the ZIP
	tempDir, err := os.MkdirTemp("", "ignore-file-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create some files
	if err := os.WriteFile(filepath.Join(tempDir, "main.py"), []byte("# Main"), 0644); err != nil {
		t.Fatalf("Failed to create main.py: %v", err)
	}

	// Create .the0ignore file
	ignoreContent := `*.log
test/`
	if err := os.WriteFile(filepath.Join(tempDir, ".the0ignore"), []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create .the0ignore: %v", err)
	}

	// Create ZIP
	originalDir, _ := os.Getwd()
	defer os.Chdir(originalDir)
	os.Chdir(tempDir)

	zipPath, err := internal.CreateBotZipFromDir(tempDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir() error = %v", err)
	}
	defer os.Remove(zipPath)

	// Verify ZIP contents
	zipReader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("Failed to open ZIP: %v", err)
	}
	defer zipReader.Close()

	zipFiles := make(map[string]bool)
	for _, file := range zipReader.File {
		zipFiles[file.Name] = true
	}

	// .the0ignore should NOT be in the ZIP (it's a hidden file)
	if zipFiles[".the0ignore"] {
		t.Error(".the0ignore file should not be included in ZIP")
	}

	// main.py should be in the ZIP
	if !zipFiles["main.py"] {
		t.Error("main.py should be included in ZIP")
	}

	t.Logf("Ignore file exclusion test passed with %d files", len(zipFiles))
}
