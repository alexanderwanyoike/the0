package internal

import (
	"fmt"
	"testing"
	"the0/internal"
	"time"
)

func TestSaveAndLoadAuth(t *testing.T) {
	// Create test auth
	testAuth := &internal.Auth{
		APIKey:    "test-api-key-12345",
		CreatedAt: time.Now(),
	}

	// Save auth
	err := internal.SaveAuth(testAuth)
	if err != nil {
		t.Fatalf("SaveAuth() error = %v", err)
	}

	// Load auth
	loadedAuth, err := internal.LoadAuth()
	if err != nil {
		t.Fatalf("LoadAuth() error = %v", err)
	}

	// Compare
	if loadedAuth.APIKey != testAuth.APIKey {
		t.Errorf("LoadAuth() APIKey = %v, want %v", loadedAuth.APIKey, testAuth.APIKey)
	}

	// Cleanup
	defer internal.RemoveAuth()
}

func TestIsAuthError(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		wantErr bool
	}{
		{
			name:    "401 error",
			err:     fmt.Errorf("HTTP 401 Unauthorized"),
			wantErr: true,
		},
		{
			name:    "403 error",
			err:     fmt.Errorf("HTTP 403 Forbidden"),
			wantErr: true,
		},
		{
			name:    "invalid key error",
			err:     fmt.Errorf("API key is invalid"),
			wantErr: true,
		},
		{
			name:    "revoked key error",
			err:     fmt.Errorf("API key has been revoked"),
			wantErr: true,
		},
		{
			name:    "network error",
			err:     fmt.Errorf("connection timeout"),
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := internal.IsAuthError(tt.err); got != tt.wantErr {
				t.Errorf("IsAuthError() = %v, want %v", got, tt.wantErr)
			}
		})
	}
}
