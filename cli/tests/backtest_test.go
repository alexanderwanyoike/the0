package internal

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"the0/internal"
	"time"
)

func TestAPIClient_CreateBacktest(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		request       *internal.BacktestDeployRequest
		expectedError bool
		errorContains string
		expectedID    string
		expectedName  string
	}{
		{
			name:       "successful backtest creation",
			statusCode: 201,
			responseBody: `{
				"id": "bt_123456789",
				"name": "test-backtest",
				"status": "pending",
				"config": {"type": "test"},
				"createdAt": "2024-01-01T00:00:00Z",
				"updatedAt": "2024-01-01T00:00:00Z"
			}`,
			request: &internal.BacktestDeployRequest{
				Name: "test-backtest",
				Config: map[string]interface{}{
					"name":    "test-backtest",
					"type":    "test",
					"version": "1.0.0",
				},
			},
			expectedError: false,
			expectedID:    "bt_123456789",
			expectedName:  "test-backtest",
		},
		{
			name:          "authentication failed",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			request:       &internal.BacktestDeployRequest{Name: "test-backtest", Config: map[string]interface{}{"name": "test-backtest"}},
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:         "invalid configuration",
			statusCode:   400,
			responseBody: `{"error": "Invalid configuration"}`,
			request: &internal.BacktestDeployRequest{
				Name: "invalid-backtest",
				Config: map[string]interface{}{
					"name": "invalid-backtest",
					"type": "invalid",
				},
			},
			expectedError: true,
			errorContains: "API error: 400",
		},
		{
			name:          "server error",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			request:       &internal.BacktestDeployRequest{Name: "test-backtest", Config: map[string]interface{}{"name": "test-backtest"}},
			expectedError: true,
			errorContains: "API error: 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/backtest" {
					t.Errorf("Expected path /backtest, got %s", r.URL.Path)
				}

				if r.Method != "POST" {
					t.Errorf("Expected POST method, got %s", r.Method)
				}

				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				contentType := r.Header.Get("Content-Type")
				if contentType != "application/json" {
					t.Errorf("Expected Content-Type application/json, got %s", contentType)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			client := internal.NewAPIClient(server.URL)
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			backtest, err := client.CreateBacktest(auth, tt.request)

			if tt.expectedError {
				if err == nil {
					t.Errorf("CreateBacktest() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("CreateBacktest() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("CreateBacktest() unexpected error = %v", err)
				}
				if backtest == nil {
					t.Errorf("CreateBacktest() returned nil backtest")
				} else {
					if tt.expectedID != "" && backtest.ID != tt.expectedID {
						t.Errorf("CreateBacktest() returned ID %s, expected %s", backtest.ID, tt.expectedID)
					}
					if tt.expectedName != "" && backtest.Name != tt.expectedName {
						t.Errorf("CreateBacktest() returned name %s, expected %s", backtest.Name, tt.expectedName)
					}
				}
			}
		})
	}
}

func TestAPIClient_ListBacktests(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		expectedCount int
		expectedError bool
		errorContains string
	}{
		{
			name:       "successful listing with multiple backtests",
			statusCode: 200,
			responseBody: `[
				{
					"id": "bt_123",
					"name": "backtest-1",
					"status": "completed",
					"config": {"type": "test"},
					"createdAt": "2024-01-01T00:00:00Z",
					"updatedAt": "2024-01-01T01:00:00Z"
				},
				{
					"id": "bt_456",
					"name": "backtest-2",
					"status": "running",
					"config": {"type": "test"},
					"createdAt": "2024-01-02T00:00:00Z",
					"updatedAt": "2024-01-02T00:30:00Z"
				}
			]`,
			expectedCount: 2,
			expectedError: false,
		},
		{
			name:          "empty list",
			statusCode:    200,
			responseBody:  `[]`,
			expectedCount: 0,
			expectedError: false,
		},
		{
			name:          "unauthorized",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "server error",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			expectedError: true,
			errorContains: "API error: 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/backtest" {
					t.Errorf("Expected path /backtest, got %s", r.URL.Path)
				}

				if r.Method != "GET" {
					t.Errorf("Expected GET method, got %s", r.Method)
				}

				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			client := internal.NewAPIClient(server.URL)
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			backtests, err := client.ListBacktests(auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("ListBacktests() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("ListBacktests() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("ListBacktests() unexpected error = %v", err)
				}
				if len(backtests) != tt.expectedCount {
					t.Errorf("ListBacktests() returned %d backtests, expected %d", len(backtests), tt.expectedCount)
				}
			}
		})
	}
}

func TestAPIClient_DeleteBacktest(t *testing.T) {
	tests := []struct {
		name          string
		backtestID    string
		statusCode    int
		responseBody  string
		expectedError bool
		errorContains string
	}{
		{
			name:          "successful deletion",
			backtestID:    "bt_123456789",
			statusCode:    200,
			responseBody:  `{"success": true, "message": "Backtest deleted"}`,
			expectedError: false,
		},
		{
			name:          "successful deletion with 204",
			backtestID:    "bt_123456789",
			statusCode:    204,
			responseBody:  ``,
			expectedError: false,
		},
		{
			name:          "backtest not found",
			backtestID:    "bt_nonexistent",
			statusCode:    404,
			responseBody:  `{"error": "Not found"}`,
			expectedError: true,
			errorContains: "backtest not found",
		},
		{
			name:          "unauthorized",
			backtestID:    "bt_123456789",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "server error",
			backtestID:    "bt_123456789",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			expectedError: true,
			errorContains: "API error: 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				expectedPath := "/backtest/" + tt.backtestID
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

				if r.Method != "DELETE" {
					t.Errorf("Expected DELETE method, got %s", r.Method)
				}

				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			client := internal.NewAPIClient(server.URL)
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			err := client.DeleteBacktest(auth, tt.backtestID)

			if tt.expectedError {
				if err == nil {
					t.Errorf("DeleteBacktest() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("DeleteBacktest() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("DeleteBacktest() unexpected error = %v", err)
				}
			}
		})
	}
}
