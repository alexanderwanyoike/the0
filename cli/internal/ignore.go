package internal

import (
	"bufio"
	"os"
	"path/filepath"
	"strings"
)

// IgnorePattern represents a single ignore pattern with its properties
type IgnorePattern struct {
	Pattern     string
	IsNegation  bool
	IsDirectory bool
}

// IgnoreParser handles parsing and matching of .the0ignore files
type IgnoreParser struct {
	patterns []IgnorePattern
}

// NewIgnoreParser creates a new ignore parser
func NewIgnoreParser() *IgnoreParser {
	return &IgnoreParser{
		patterns: make([]IgnorePattern, 0),
	}
}

// LoadIgnoreFile loads patterns from a .the0ignore file
func (p *IgnoreParser) LoadIgnoreFile(path string) error {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			// No ignore file is not an error
			return nil
		}
		return err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())

		// Skip empty lines and comments
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		pattern := IgnorePattern{
			Pattern: line,
		}

		// Check for negation
		if strings.HasPrefix(line, "!") {
			pattern.IsNegation = true
			pattern.Pattern = line[1:]
		}

		// Check for directory-only pattern
		if strings.HasSuffix(pattern.Pattern, "/") {
			pattern.IsDirectory = true
			pattern.Pattern = strings.TrimSuffix(pattern.Pattern, "/")
		}

		p.patterns = append(p.patterns, pattern)
	}

	return scanner.Err()
}

// AddDefaultPatterns adds default ignore patterns
func (p *IgnoreParser) AddDefaultPatterns() {
	// Note: build/ and dist/ are NOT included by default because they often contain
	// compiled binaries needed for deployment (C++, Rust, Scala, etc.).
	// Projects can add them to .the0ignore if they want to exclude them.
	defaults := []string{
		"*.log", "*.tmp", "*.temp",
		"test/", "tests/", "__tests__/",
		".DS_Store", "Thumbs.db",
		"*.pyc", "*.pyo",
		"__pycache__/",
		".git/",
	}

	for _, pattern := range defaults {
		ignorePattern := IgnorePattern{
			Pattern: pattern,
		}

		if strings.HasSuffix(pattern, "/") {
			ignorePattern.IsDirectory = true
			ignorePattern.Pattern = strings.TrimSuffix(pattern, "/")
		}

		p.patterns = append(p.patterns, ignorePattern)
	}
}

// IsIgnored checks if a file path should be ignored
func (p *IgnoreParser) IsIgnored(relPath string, isDir bool) bool {
	// Never ignore these protected directories (they are needed for deployment)
	// - vendor/node_modules: dependencies (Python/Node packages)
	// - frontend/dist: built frontend assets
	// Note: build/, target/, bin/, dist-newstyle/ are NOT protected because they
	// often contain large build artifacts. Users should use .the0ignore to exclude
	// unwanted files and only include the final binaries.
	protectedDirs := []string{"vendor", "node_modules", "frontend/dist"}
	for _, dir := range protectedDirs {
		if relPath == dir || strings.HasPrefix(relPath, dir+"/") {
			return false
		}
	}

	ignored := false

	// Process patterns in order
	for _, pattern := range p.patterns {
		matched := p.matchPattern(pattern.Pattern, relPath, isDir)

		if matched {
			if pattern.IsNegation {
				ignored = false
			} else {
				ignored = true
			}
		}
	}

	return ignored
}

// matchPattern performs pattern matching similar to gitignore
func (p *IgnoreParser) matchPattern(pattern, path string, isDir bool) bool {
	// Handle ** patterns first (match any depth)
	if strings.Contains(pattern, "**") {
		return p.matchDoubleStarPattern(pattern, path, isDir)
	}

	// Handle directory patterns - if pattern ends with /, it applies to directories and their contents
	isDirectoryPattern := strings.HasSuffix(pattern, "/")
	if isDirectoryPattern {
		pattern = strings.TrimSuffix(pattern, "/")
	}

	// Exact match
	if pattern == path || pattern == filepath.Base(path) {
		return true
	}

	// For directory patterns, check if the path is under that directory
	if isDirectoryPattern {
		// Pattern "test/" should match "test/file.py", "test/subdir/file.py", etc.
		if strings.HasPrefix(path, pattern+"/") {
			return true
		}
		// For the directory itself, check exact match
		if path == pattern && isDir {
			return true
		}
		// Also check if any directory in the path matches (for nested cases)
		pathParts := strings.Split(path, string(filepath.Separator))
		for _, part := range pathParts {
			if pattern == part {
				return true
			}
		}
	}

	// Handle glob patterns
	if strings.Contains(pattern, "*") {
		// Try matching against the full relative path
		matched, _ := filepath.Match(pattern, path)
		if matched {
			return true
		}

		// Try matching against the base name
		matched, _ = filepath.Match(pattern, filepath.Base(path))
		if matched {
			return true
		}

		// For non-directory patterns, also check each path component
		if !isDirectoryPattern {
			pathParts := strings.Split(path, string(filepath.Separator))
			for _, part := range pathParts {
				matched, _ := filepath.Match(pattern, part)
				if matched {
					return true
				}
			}
		}
	}

	// Check if the path starts with the pattern (for simple directory matching)
	if strings.HasPrefix(path, pattern+"/") {
		return true
	}

	return false
}

// matchDoubleStarPattern handles ** patterns specifically
func (p *IgnoreParser) matchDoubleStarPattern(pattern, path string, isDir bool) bool {
	// Split pattern by **
	parts := strings.Split(pattern, "**")
	if len(parts) != 2 {
		return false
	}

	prefix := strings.TrimSuffix(parts[0], "/")
	suffix := strings.TrimPrefix(parts[1], "/")

	// Check if path starts with prefix
	if prefix != "" {
		if !strings.HasPrefix(path, prefix) && !strings.HasPrefix(path, prefix+"/") {
			return false
		}
	}

	// If no suffix, match everything under prefix
	if suffix == "" {
		return true
	}

	// Extract the part after the prefix
	remainingPath := path
	if prefix != "" {
		if strings.HasPrefix(path, prefix+"/") {
			remainingPath = strings.TrimPrefix(path, prefix+"/")
		} else if path == prefix {
			remainingPath = ""
		} else {
			return false
		}
	}

	// Handle suffix matching
	if strings.Contains(suffix, "*") {
		// Check if remaining path matches the suffix pattern
		matched, _ := filepath.Match(suffix, remainingPath)
		if matched {
			return true
		}

		// Also check if any directory component matches
		pathParts := strings.Split(remainingPath, string(filepath.Separator))
		for i := range pathParts {
			subPath := strings.Join(pathParts[i:], string(filepath.Separator))
			matched, _ := filepath.Match(suffix, subPath)
			if matched {
				return true
			}
		}

		// Check if base name matches
		matched, _ = filepath.Match(suffix, filepath.Base(remainingPath))
		if matched {
			return true
		}
	} else {
		// Handle directory suffix
		if strings.HasSuffix(suffix, "/") {
			suffix = strings.TrimSuffix(suffix, "/")
			if !isDir {
				return false
			}
		}

		// Simple suffix match
		if remainingPath == suffix || strings.HasSuffix(remainingPath, "/"+suffix) {
			return true
		}

		// Check if any part of the remaining path matches
		pathParts := strings.Split(remainingPath, string(filepath.Separator))
		for _, part := range pathParts {
			if part == suffix {
				return true
			}
		}
	}

	return false
}

// CreateIgnoreParserForDir creates an ignore parser for a specific directory
func CreateIgnoreParserForDir(dir string) (*IgnoreParser, error) {
	parser := NewIgnoreParser()

	// Add default patterns first
	parser.AddDefaultPatterns()

	// Load .the0ignore file if it exists
	ignoreFilePath := filepath.Join(dir, ".the0ignore")
	if err := parser.LoadIgnoreFile(ignoreFilePath); err != nil {
		return nil, err
	}

	return parser, nil
}
