package imagebuilder

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

// RegistryClient provides access to container registry operations.
type RegistryClient interface {
	// ImageExists checks if an image with the given reference exists in the registry.
	ImageExists(ctx context.Context, imageRef string) (bool, error)
}

// HTTPRegistryClient implements RegistryClient using the Docker Registry HTTP API V2.
type HTTPRegistryClient struct {
	httpClient *http.Client
	// defaultRegistry is used when image ref doesn't include a registry
	defaultRegistry string
}

// RegistryClientConfig holds configuration for the registry client.
type RegistryClientConfig struct {
	// DefaultRegistry is the registry to use when image ref doesn't include one.
	// Example: "localhost:5000" or "registry.the0.svc.cluster.local:5000"
	DefaultRegistry string
	// Timeout is the HTTP request timeout.
	Timeout time.Duration
	// Insecure allows HTTP connections (instead of HTTPS) to registries.
	Insecure bool
}

// NewHTTPRegistryClient creates a new HTTPRegistryClient.
func NewHTTPRegistryClient(config RegistryClientConfig) *HTTPRegistryClient {
	timeout := config.Timeout
	if timeout == 0 {
		timeout = 10 * time.Second
	}

	return &HTTPRegistryClient{
		httpClient: &http.Client{
			Timeout: timeout,
		},
		defaultRegistry: config.DefaultRegistry,
	}
}

// ImageExists checks if an image exists in the registry by querying for its manifest.
// The imageRef should be in the format: [registry/]repository:tag
// Examples:
//   - "localhost:5000/the0/bots/price-alerts:1.0.0"
//   - "the0/bots/price-alerts:1.0.0" (uses default registry)
func (c *HTTPRegistryClient) ImageExists(ctx context.Context, imageRef string) (bool, error) {
	registry, repository, tag := parseImageRef(imageRef, c.defaultRegistry)
	if registry == "" {
		return false, fmt.Errorf("no registry specified and no default registry configured")
	}

	// Use Docker Registry HTTP API V2 to check for manifest
	// GET /v2/<name>/manifests/<reference>
	url := fmt.Sprintf("http://%s/v2/%s/manifests/%s", registry, repository, tag)

	req, err := http.NewRequestWithContext(ctx, http.MethodHead, url, nil)
	if err != nil {
		return false, fmt.Errorf("failed to create request: %w", err)
	}

	// Accept manifest types
	req.Header.Set("Accept", "application/vnd.docker.distribution.manifest.v2+json")

	resp, err := c.httpClient.Do(req)
	if err != nil {
		// Network error - could be registry unavailable
		return false, fmt.Errorf("failed to query registry: %w", err)
	}
	defer resp.Body.Close()

	switch resp.StatusCode {
	case http.StatusOK:
		// Image exists
		return true, nil
	case http.StatusNotFound:
		// Image does not exist
		return false, nil
	case http.StatusUnauthorized:
		// Try without auth - some registries allow anonymous read
		return false, fmt.Errorf("registry requires authentication")
	default:
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return false, fmt.Errorf("unexpected registry response: %d %s", resp.StatusCode, string(body))
	}
}

// ListTags lists all tags for a repository.
func (c *HTTPRegistryClient) ListTags(ctx context.Context, imageRef string) ([]string, error) {
	registry, repository, _ := parseImageRef(imageRef, c.defaultRegistry)
	if registry == "" {
		return nil, fmt.Errorf("no registry specified and no default registry configured")
	}

	url := fmt.Sprintf("http://%s/v2/%s/tags/list", registry, repository)

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to query registry: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected registry response: %d", resp.StatusCode)
	}

	var result struct {
		Name string   `json:"name"`
		Tags []string `json:"tags"`
	}

	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return nil, fmt.Errorf("failed to decode response: %w", err)
	}

	return result.Tags, nil
}

// parseImageRef parses an image reference into registry, repository, and tag.
// If no registry is specified, defaultRegistry is used.
// If no tag is specified, "latest" is used.
func parseImageRef(imageRef, defaultRegistry string) (registry, repository, tag string) {
	// Split off the tag
	parts := strings.Split(imageRef, ":")
	if len(parts) >= 2 {
		// Check if the last part looks like a tag (not a port)
		lastPart := parts[len(parts)-1]
		if !strings.Contains(lastPart, "/") {
			tag = lastPart
			imageRef = strings.Join(parts[:len(parts)-1], ":")
		}
	}
	if tag == "" {
		tag = "latest"
	}

	// Split registry from repository
	// If the first part contains a dot or colon, it's likely a registry
	slashParts := strings.SplitN(imageRef, "/", 2)
	if len(slashParts) == 2 && (strings.Contains(slashParts[0], ".") || strings.Contains(slashParts[0], ":")) {
		registry = slashParts[0]
		repository = slashParts[1]
	} else {
		// No registry specified, use default
		registry = defaultRegistry
		repository = imageRef
	}

	return registry, repository, tag
}

// GenerateImageRef generates a full image reference for a bot.
func GenerateImageRef(registry, customBotID, version string) string {
	// Sanitize customBotID for use in image ref
	// Docker image names can only contain lowercase letters, digits, periods, dashes, and underscores
	sanitized := sanitizeImageName(customBotID)
	sanitizedVersion := sanitizeImageTag(version)

	return fmt.Sprintf("%s/the0/bots/%s:%s", registry, sanitized, sanitizedVersion)
}

// sanitizeImageName sanitizes a string for use as a Docker image name component.
// Docker image names can only contain lowercase letters, digits, periods, dashes, and underscores.
func sanitizeImageName(s string) string {
	s = strings.ToLower(s)
	result := make([]byte, 0, len(s))
	for _, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '-' || c == '_' {
			result = append(result, byte(c))
		} else if c == ' ' {
			result = append(result, '-')
		}
		// Skip other invalid characters
	}
	if len(result) == 0 {
		return "bot"
	}
	return string(result)
}

// sanitizeImageTag sanitizes a string for use as a Docker image tag.
// Tags can contain lowercase/uppercase letters, digits, underscores, periods, and dashes.
func sanitizeImageTag(s string) string {
	result := make([]byte, 0, len(s))
	for _, c := range s {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
			c == '.' || c == '-' || c == '_' {
			result = append(result, byte(c))
		}
	}
	if len(result) == 0 {
		return "latest"
	}
	// Tags can be max 128 characters
	if len(result) > 128 {
		result = result[:128]
	}
	return string(result)
}

// MockRegistryClient is a mock implementation of RegistryClient for testing.
type MockRegistryClient struct {
	ExistingImages map[string]bool
	Error          error
	Calls          []string
}

// NewMockRegistryClient creates a new MockRegistryClient.
func NewMockRegistryClient() *MockRegistryClient {
	return &MockRegistryClient{
		ExistingImages: make(map[string]bool),
		Calls:          make([]string, 0),
	}
}

// ImageExists checks if an image exists in the mock registry.
func (m *MockRegistryClient) ImageExists(ctx context.Context, imageRef string) (bool, error) {
	m.Calls = append(m.Calls, imageRef)
	if m.Error != nil {
		return false, m.Error
	}
	return m.ExistingImages[imageRef], nil
}

// AddExistingImage marks an image as existing in the mock registry.
func (m *MockRegistryClient) AddExistingImage(imageRef string) {
	m.ExistingImages[imageRef] = true
}
