package internal

import (
	"errors"
	"fmt"
	"testing"
	"the0/internal"
	"time"
)

func TestSaveAndLoadAuth(t *testing.T) {
	internal.SetConfigDir(t.TempDir())
	t.Cleanup(func() { internal.SetConfigDir("") })

	testAuth := &internal.Auth{
		APIKey:    "test-api-key-12345",
		CreatedAt: time.Now(),
	}

	if err := internal.SaveAuth(testAuth); err != nil {
		t.Fatalf("SaveAuth() error = %v", err)
	}

	loadedAuth, err := internal.LoadAuth()
	if err != nil {
		t.Fatalf("LoadAuth() error = %v", err)
	}
	if loadedAuth.APIKey != testAuth.APIKey {
		t.Errorf("LoadAuth() APIKey = %v, want %v", loadedAuth.APIKey, testAuth.APIKey)
	}
}

func TestIsAuthError(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		wantErr bool
	}{
		{
			name:    "APIError with 401 status",
			err:     &internal.APIError{StatusCode: 401, Message: "Unauthorized"},
			wantErr: true,
		},
		{
			name:    "APIError with 403 status",
			err:     &internal.APIError{StatusCode: 403, Message: "Forbidden"},
			wantErr: true,
		},
		{
			name:    "APIError with 400 status",
			err:     &internal.APIError{StatusCode: 400, Message: "invalid version format"},
			wantErr: false,
		},
		{
			name:    "APIError with 500 status",
			err:     &internal.APIError{StatusCode: 500, Message: "Internal Server Error"},
			wantErr: false,
		},
		{
			name:    "plain error with invalid in message",
			err:     fmt.Errorf("invalid version format"),
			wantErr: false,
		},
		{
			name:    "plain error with 401 in message",
			err:     fmt.Errorf("HTTP 401 Unauthorized"),
			wantErr: false,
		},
		{
			name:    "plain error with revoked in message",
			err:     fmt.Errorf("API key has been revoked"),
			wantErr: false,
		},
		{
			name:    "network error",
			err:     fmt.Errorf("connection timeout"),
			wantErr: false,
		},
		{
			name:    "wrapped APIError with 401",
			err:     fmt.Errorf("request failed: %w", &internal.APIError{StatusCode: 401, Message: "Unauthorized"}),
			wantErr: true,
		},
		{
			name:    "wrapped APIError with 400",
			err:     fmt.Errorf("request failed: %w", &internal.APIError{StatusCode: 400, Message: "Bad Request"}),
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

func TestAPIError_Error(t *testing.T) {
	err := &internal.APIError{StatusCode: 401, Message: "Unauthorized"}
	expected := "API error 401: Unauthorized"
	if err.Error() != expected {
		t.Errorf("APIError.Error() = %q, want %q", err.Error(), expected)
	}
}

func TestAPIError_ErrorsAs(t *testing.T) {
	err := fmt.Errorf("wrapped: %w", &internal.APIError{StatusCode: 403, Message: "Forbidden"})

	var apiErr *internal.APIError
	if !errors.As(err, &apiErr) {
		t.Fatal("errors.As() should match APIError")
	}
	if apiErr.StatusCode != 403 {
		t.Errorf("APIError.StatusCode = %d, want 403", apiErr.StatusCode)
	}
}
