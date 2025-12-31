package imagebuilder

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestHTTPRegistryClient_ImageExists_Found(t *testing.T) {
	// Mock registry server that returns 200 OK
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, http.MethodHead, r.Method)
		assert.Contains(t, r.URL.Path, "/v2/the0/bots/price-alerts/manifests/1.0.0")
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	// Extract host from server URL (remove http://)
	registryHost := strings.TrimPrefix(server.URL, "http://")

	client := NewHTTPRegistryClient(RegistryClientConfig{
		DefaultRegistry: registryHost,
	})

	exists, err := client.ImageExists(context.Background(), "the0/bots/price-alerts:1.0.0")

	require.NoError(t, err)
	assert.True(t, exists)
}

func TestHTTPRegistryClient_ImageExists_NotFound(t *testing.T) {
	// Mock registry server that returns 404 Not Found
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotFound)
	}))
	defer server.Close()

	registryHost := strings.TrimPrefix(server.URL, "http://")

	client := NewHTTPRegistryClient(RegistryClientConfig{
		DefaultRegistry: registryHost,
	})

	exists, err := client.ImageExists(context.Background(), "the0/bots/missing:1.0.0")

	require.NoError(t, err)
	assert.False(t, exists)
}

func TestHTTPRegistryClient_ImageExists_Unauthorized(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
	}))
	defer server.Close()

	registryHost := strings.TrimPrefix(server.URL, "http://")

	client := NewHTTPRegistryClient(RegistryClientConfig{
		DefaultRegistry: registryHost,
	})

	_, err := client.ImageExists(context.Background(), "the0/bots/private:1.0.0")

	require.Error(t, err)
	assert.Contains(t, err.Error(), "authentication")
}

func TestHTTPRegistryClient_ImageExists_WithFullImageRef(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Contains(t, r.URL.Path, "/v2/the0/bots/test/manifests/2.0.0")
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	registryHost := strings.TrimPrefix(server.URL, "http://")

	client := NewHTTPRegistryClient(RegistryClientConfig{})

	// Use full image ref with registry
	imageRef := registryHost + "/the0/bots/test:2.0.0"
	exists, err := client.ImageExists(context.Background(), imageRef)

	require.NoError(t, err)
	assert.True(t, exists)
}

func TestHTTPRegistryClient_ImageExists_NoRegistry(t *testing.T) {
	client := NewHTTPRegistryClient(RegistryClientConfig{
		DefaultRegistry: "", // No default registry
	})

	_, err := client.ImageExists(context.Background(), "the0/bots/test:1.0.0")

	require.Error(t, err)
	assert.Contains(t, err.Error(), "no registry")
}

func TestHTTPRegistryClient_ListTags(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, http.MethodGet, r.Method)
		assert.Contains(t, r.URL.Path, "/v2/the0/bots/test/tags/list")
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"name":"the0/bots/test","tags":["1.0.0","1.1.0","2.0.0"]}`))
	}))
	defer server.Close()

	registryHost := strings.TrimPrefix(server.URL, "http://")

	client := NewHTTPRegistryClient(RegistryClientConfig{
		DefaultRegistry: registryHost,
	})

	tags, err := client.ListTags(context.Background(), "the0/bots/test")

	require.NoError(t, err)
	assert.Equal(t, []string{"1.0.0", "1.1.0", "2.0.0"}, tags)
}

func TestParseImageRef(t *testing.T) {
	tests := []struct {
		name            string
		imageRef        string
		defaultRegistry string
		wantRegistry    string
		wantRepository  string
		wantTag         string
	}{
		{
			name:            "full ref with registry and tag",
			imageRef:        "localhost:5000/the0/bots/test:1.0.0",
			defaultRegistry: "",
			wantRegistry:    "localhost:5000",
			wantRepository:  "the0/bots/test",
			wantTag:         "1.0.0",
		},
		{
			name:            "no registry uses default",
			imageRef:        "the0/bots/test:1.0.0",
			defaultRegistry: "registry.local:5000",
			wantRegistry:    "registry.local:5000",
			wantRepository:  "the0/bots/test",
			wantTag:         "1.0.0",
		},
		{
			name:            "no tag uses latest",
			imageRef:        "localhost:5000/the0/bots/test",
			defaultRegistry: "",
			wantRegistry:    "localhost:5000",
			wantRepository:  "the0/bots/test",
			wantTag:         "latest",
		},
		{
			name:            "gcr.io style registry",
			imageRef:        "gcr.io/my-project/test:v1",
			defaultRegistry: "",
			wantRegistry:    "gcr.io",
			wantRepository:  "my-project/test",
			wantTag:         "v1",
		},
		{
			name:            "docker hub style (no registry)",
			imageRef:        "library/nginx:latest",
			defaultRegistry: "docker.io",
			wantRegistry:    "docker.io",
			wantRepository:  "library/nginx",
			wantTag:         "latest",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			registry, repository, tag := parseImageRef(tt.imageRef, tt.defaultRegistry)
			assert.Equal(t, tt.wantRegistry, registry)
			assert.Equal(t, tt.wantRepository, repository)
			assert.Equal(t, tt.wantTag, tag)
		})
	}
}

func TestGenerateImageRef(t *testing.T) {
	tests := []struct {
		registry    string
		customBotID string
		version     string
		expected    string
	}{
		{
			registry:    "localhost:5000",
			customBotID: "price-alerts",
			version:     "1.0.0",
			expected:    "localhost:5000/the0/bots/price-alerts:1.0.0",
		},
		{
			registry:    "gcr.io/my-project",
			customBotID: "sma-crossover",
			version:     "2.1.0",
			expected:    "gcr.io/my-project/the0/bots/sma-crossover:2.1.0",
		},
		{
			registry:    "registry.local:5000",
			customBotID: "My Bot Name", // with spaces
			version:     "1.0.0",
			expected:    "registry.local:5000/the0/bots/my-bot-name:1.0.0",
		},
	}

	for _, tt := range tests {
		t.Run(tt.customBotID, func(t *testing.T) {
			result := GenerateImageRef(tt.registry, tt.customBotID, tt.version)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestMockRegistryClient_ImageExists(t *testing.T) {
	mock := NewMockRegistryClient()
	mock.AddExistingImage("registry/test:1.0.0")

	t.Run("existing image", func(t *testing.T) {
		exists, err := mock.ImageExists(context.Background(), "registry/test:1.0.0")
		require.NoError(t, err)
		assert.True(t, exists)
	})

	t.Run("missing image", func(t *testing.T) {
		exists, err := mock.ImageExists(context.Background(), "registry/missing:1.0.0")
		require.NoError(t, err)
		assert.False(t, exists)
	})

	t.Run("tracks calls", func(t *testing.T) {
		assert.Contains(t, mock.Calls, "registry/test:1.0.0")
		assert.Contains(t, mock.Calls, "registry/missing:1.0.0")
	})
}
