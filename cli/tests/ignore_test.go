package internal

import (
	"os"
	"path/filepath"
	"testing"
	"the0/internal"
)

func TestIgnoreParser_LoadIgnoreFile(t *testing.T) {
	// Create temporary directory
	tempDir, err := os.MkdirTemp("", "ignore-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create .the0ignore file
	ignoreContent := `# Test ignore file
*.log
*.tmp
test/
!important.log
**/*.pyc
node_modules/
!vendor/
.DS_Store
build/
dist/`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	// Test parser
	parser := internal.NewIgnoreParser()
	err = parser.LoadIgnoreFile(ignoreFile)
	if err != nil {
		t.Fatalf("LoadIgnoreFile() error = %v", err)
	}

	// Test that patterns were loaded correctly
	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "*.log should be ignored"},
		{"debug.tmp", false, true, "*.tmp should be ignored"},
		{"important.log", false, false, "!important.log should not be ignored (negation)"},
		{"test", true, true, "test/ directory should be ignored"},
		{"test/file.py", false, true, "files in test/ should be ignored"},
		{"src/module.pyc", false, true, "**/*.pyc should be ignored"},
		{"build", true, false, "build/ directory should not be ignored (protected for compiled languages)"},
		{"main.py", false, false, "main.py should not be ignored"},
		{".DS_Store", false, true, ".DS_Store should be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestIgnoreParser_NonExistentFile(t *testing.T) {
	parser := internal.NewIgnoreParser()
	err := parser.LoadIgnoreFile("/nonexistent/.the0ignore")

	// Should not error for non-existent file
	if err != nil {
		t.Errorf("LoadIgnoreFile() for non-existent file should not error, got: %v", err)
	}
}

func TestIgnoreParser_VendorDirectoryProtection(t *testing.T) {
	parser := internal.NewIgnoreParser()

	// Add patterns that would normally ignore vendor
	parser.AddDefaultPatterns()

	// Vendor directories should never be ignored
	testCases := []struct {
		path  string
		isDir bool
		desc  string
	}{
		{"vendor", true, "vendor directory"},
		{"vendor/requests.py", false, "file in vendor"},
		{"vendor/subdir/file.py", false, "file in vendor subdirectory"},
		{"node_modules", true, "node_modules directory"},
		{"node_modules/express/index.js", false, "file in node_modules"},
		{"node_modules/express/lib/router.js", false, "nested file in node_modules"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result {
			t.Errorf("IsIgnored(%q, %v) = %v, want false (%s should never be ignored)", tc.path, tc.isDir, result, tc.desc)
		}
	}
}

func TestIgnoreParser_DefaultPatterns(t *testing.T) {
	parser := internal.NewIgnoreParser()
	parser.AddDefaultPatterns()

	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "*.log should be ignored by default"},
		{"debug.tmp", false, true, "*.tmp should be ignored by default"},
		{"cache.temp", false, true, "*.temp should be ignored by default"},
		{"test", true, true, "test/ should be ignored by default"},
		{"tests", true, true, "tests/ should be ignored by default"},
		{"__tests__", true, true, "__tests__/ should be ignored by default"},
		{".DS_Store", false, true, ".DS_Store should be ignored by default"},
		{"Thumbs.db", false, true, "Thumbs.db should be ignored by default"},
		{"module.pyc", false, true, "*.pyc should be ignored by default"},
		{"compiled.pyo", false, true, "*.pyo should be ignored by default"},
		{"__pycache__", true, true, "__pycache__/ should be ignored by default"},
		{".git", true, true, ".git/ should be ignored by default"},
		{"build", true, false, "build/ should not be ignored (protected for compiled languages)"},
		{"dist", true, true, "dist/ should be ignored by default"},
		{"main.py", false, false, "main.py should not be ignored"},
		{"src", true, false, "src/ should not be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestIgnoreParser_GlobPatterns(t *testing.T) {
	parser := internal.NewIgnoreParser()

	// Create temporary ignore file with glob patterns
	tempDir, err := os.MkdirTemp("", "glob-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	ignoreContent := `*.log
test_*.py
*_backup
config_*
**/*.tmp
**/*_cache/`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	if err := parser.LoadIgnoreFile(ignoreFile); err != nil {
		t.Fatalf("LoadIgnoreFile() error = %v", err)
	}

	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "*.log pattern"},
		{"test_module.py", false, true, "test_*.py pattern"},
		{"data_backup", false, true, "*_backup pattern"},
		{"config_prod", false, true, "config_* pattern"},
		{"src/temp.tmp", false, true, "**/*.tmp pattern"},
		{"build/test_cache", true, false, "build/ is protected, so cache pattern doesn't apply"},
		{"main.py", false, false, "should not match any pattern"},
		{"test.py", false, false, "should not match test_*.py"},
		{"backup", false, false, "should not match *_backup"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestIgnoreParser_NegationPatterns(t *testing.T) {
	parser := internal.NewIgnoreParser()

	// Create temporary ignore file with negation patterns
	tempDir, err := os.MkdirTemp("", "negation-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	ignoreContent := `*.log
!important.log
test/
!test/keep.py
**/*.tmp
!config/*.tmp`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	if err := parser.LoadIgnoreFile(ignoreFile); err != nil {
		t.Fatalf("LoadIgnoreFile() error = %v", err)
	}

	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "*.log should be ignored"},
		{"important.log", false, false, "!important.log should not be ignored"},
		{"test", true, true, "test/ should be ignored"},
		{"test/keep.py", false, false, "!test/keep.py should not be ignored"},
		{"test/other.py", false, true, "other files in test/ should be ignored"},
		{"src/file.tmp", false, true, "**/*.tmp should be ignored"},
		{"config/app.tmp", false, false, "!config/*.tmp should not be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestIgnoreParser_DoubleStarPatterns(t *testing.T) {
	parser := internal.NewIgnoreParser()

	// Create temporary ignore file with ** patterns
	tempDir, err := os.MkdirTemp("", "doublestar-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	ignoreContent := `**/cache/
**/*.log
docs/**/*.md
**/node_modules/**`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	if err := parser.LoadIgnoreFile(ignoreFile); err != nil {
		t.Fatalf("LoadIgnoreFile() error = %v", err)
	}

	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"cache", true, true, "**/cache/ should match cache dir"},
		{"build/cache", true, false, "build/ is protected, so cache pattern doesn't apply"},
		{"src/deep/cache", true, true, "**/cache/ should match deeply nested cache dir"},
		{"app.log", false, true, "**/*.log should match top-level log"},
		{"src/app.log", false, true, "**/*.log should match nested log"},
		{"build/debug/app.log", false, false, "build/ is protected, so log pattern doesn't apply"},
		{"docs/api/readme.md", false, true, "docs/**/*.md should match"},
		{"docs/guide/install.md", false, true, "docs/**/*.md should match nested"},
		{"src/readme.md", false, false, "docs/**/*.md should not match outside docs"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestCreateIgnoreParserForDir(t *testing.T) {
	// Create temporary directory
	tempDir, err := os.MkdirTemp("", "parser-for-dir-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create .the0ignore file
	ignoreContent := `*.log
test/`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	// Create parser for directory
	parser, err := internal.CreateIgnoreParserForDir(tempDir)
	if err != nil {
		t.Fatalf("CreateIgnoreParserForDir() error = %v", err)
	}

	// Should have both default patterns and custom patterns
	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "custom *.log pattern"},
		{"test", true, true, "custom test/ pattern"},
		{"build", true, false, "build/ should not be ignored (protected for compiled languages)"},
		{"main.py", false, false, "should not be ignored"},
		{"vendor", true, false, "vendor should never be ignored"},
		{"node_modules", true, false, "node_modules should never be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestCreateIgnoreParserForDir_NoIgnoreFile(t *testing.T) {
	// Create temporary directory without .the0ignore file
	tempDir, err := os.MkdirTemp("", "no-ignore-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create parser for directory
	parser, err := internal.CreateIgnoreParserForDir(tempDir)
	if err != nil {
		t.Fatalf("CreateIgnoreParserForDir() error = %v", err)
	}

	// Should only have default patterns
	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"build", true, false, "build/ should not be ignored (protected for compiled languages)"},
		{"__pycache__", true, true, "default __pycache__/ pattern"},
		{"app.log", false, true, "default *.log pattern"},
		{"main.py", false, false, "should not be ignored"},
		{"vendor", true, false, "vendor should never be ignored"},
		{"node_modules", true, false, "node_modules should never be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}

func TestIgnoreParser_CommentsAndEmptyLines(t *testing.T) {
	parser := internal.NewIgnoreParser()

	// Create temporary ignore file with comments and empty lines
	tempDir, err := os.MkdirTemp("", "comments-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	ignoreContent := `# This is a comment
*.log

# Another comment
test/
   # Indented comment
*.tmp

# Final comment`

	ignoreFile := filepath.Join(tempDir, ".the0ignore")
	if err := os.WriteFile(ignoreFile, []byte(ignoreContent), 0644); err != nil {
		t.Fatalf("Failed to create ignore file: %v", err)
	}

	if err := parser.LoadIgnoreFile(ignoreFile); err != nil {
		t.Fatalf("LoadIgnoreFile() error = %v", err)
	}

	// Should ignore the patterns but not the comments
	testCases := []struct {
		path     string
		isDir    bool
		expected bool
		desc     string
	}{
		{"app.log", false, true, "*.log should be ignored"},
		{"test", true, true, "test/ should be ignored"},
		{"file.tmp", false, true, "*.tmp should be ignored"},
		{"main.py", false, false, "should not be ignored"},
	}

	for _, tc := range testCases {
		result := parser.IsIgnored(tc.path, tc.isDir)
		if result != tc.expected {
			t.Errorf("IsIgnored(%q, %v) = %v, want %v (%s)", tc.path, tc.isDir, result, tc.expected, tc.desc)
		}
	}
}
