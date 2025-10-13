package internal

import (
	"net/http"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
	"the0/internal"
	"time"
)

func TestAPIClient_ListBots(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		expectedBots  int
		expectedError bool
		errorContains string
	}{
		{
			name:       "successful listing",
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": [
					{
						"id": "bot_123",
						"name": "trading-bot-1",
						"config": {
							"type": "trading",
							"version": "1.0.0",
							"schedule": "0 0 * * *"
						},
						"topic": "crypto",
						"createdAt": "2023-11-15T10:30:00.000Z",
						"updatedAt": "2023-11-15T10:30:00.000Z",
						"userId": "user_789",
						"customBotId": "custom_bot_123"
					},
					{
						"id": "bot_456",
						"name": "analysis-bot",
						"config": {
							"type": "analysis",
							"version": "2.1.0"
						},
						"topic": "stocks",
						"createdAt": "2023-11-16T14:20:00.000Z",
						"updatedAt": "2023-11-16T14:20:00.000Z",
						"userId": "user_789",
						"customBotId": "custom_bot_456"
					}
				],
				"message": "Success"
			}`,
			expectedBots:  2,
			expectedError: false,
		},
		{
			name:          "empty list",
			statusCode:    200,
			responseBody:  `{"success": true, "data": [], "message": "Success"}`,
			expectedBots:  0,
			expectedError: false,
		},
		{
			name:          "invalid auth",
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
		{
			name:          "malformed JSON",
			statusCode:    200,
			responseBody:  `{invalid json}`,
			expectedError: true,
			errorContains: "failed to parse API response",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				// Verify correct endpoint is called
				if r.URL.Path != "/bot" {
					t.Errorf("Expected path /bot, got %s", r.URL.Path)
				}

				// Verify authorization header
				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			// Create API client
			client := internal.NewAPIClient(server.URL)

			// Create test auth
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			// List bots
			bots, err := client.ListBots(auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("ListBots() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("ListBots() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("ListBots() unexpected error = %v", err)
				}
				if len(bots) != tt.expectedBots {
					t.Errorf("ListBots() returned %d bots, expected %d", len(bots), tt.expectedBots)
				}
			}
		})
	}
}

func TestAPIClient_DeployBotInstance(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		request       *internal.BotDeployRequest
		expectedError bool
		errorContains string
	}{
		{
			name:       "successful deployment",
			statusCode: 201,
			responseBody: `{
				"id": "bot_new123",
				"name": "new-trading-bot",
				"config": {
					"type": "trading",
					"version": "1.0.0"
				},
				"topic": "crypto",
				"createdAt": "2023-11-17T10:30:00.000Z",
				"updatedAt": "2023-11-17T10:30:00.000Z",
				"userId": "user_789",
				"customBotId": "custom_bot_new123"
			}`,
			request: &internal.BotDeployRequest{
				Name: "new-trading-bot",
				Config: map[string]interface{}{
					"name":    "new-trading-bot",
					"type":    "trading",
					"version": "1.0.0",
				},
			},
			expectedError: false,
		},
		{
			name:       "API success false",
			statusCode: 400,
			responseBody: `{
				"success": false,
				"message": "Invalid configuration"
			}`,
			request: &internal.BotDeployRequest{
				Name: "invalid-bot",
				Config: map[string]interface{}{
					"name": "invalid-bot",
					"type": "invalid",
				},
			},
			expectedError: true,
			errorContains: "deployment failed with status 400",
		},
		{
			name:          "invalid auth",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			request:       &internal.BotDeployRequest{Name: "test-bot", Config: map[string]interface{}{"name": "test-bot"}},
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "deployment failed",
			statusCode:    400,
			responseBody:  `{"error": "Bad Request"}`,
			request:       &internal.BotDeployRequest{Name: "test-bot", Config: map[string]interface{}{"name": "test-bot"}},
			expectedError: true,
			errorContains: "deployment failed with status 400",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				// Verify correct endpoint is called
				if r.URL.Path != "/bot" {
					t.Errorf("Expected path /bot, got %s", r.URL.Path)
				}

				// Verify method
				if r.Method != "POST" {
					t.Errorf("Expected POST method, got %s", r.Method)
				}

				// Verify authorization header
				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				// Verify content type
				contentType := r.Header.Get("Content-Type")
				if contentType != "application/json" {
					t.Errorf("Expected Content-Type application/json, got %s", contentType)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			// Create API client
			client := internal.NewAPIClient(server.URL)

			// Create test auth
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			// Deploy bot
			bot, err := client.DeployBotInstance(auth, tt.request)

			if tt.expectedError {
				if err == nil {
					t.Errorf("DeployBotInstance() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("DeployBotInstance() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("DeployBotInstance() unexpected error = %v", err)
				}
				if bot == nil {
					t.Errorf("DeployBotInstance() returned nil bot")
				} else if bot.Name != tt.request.Name {
					t.Errorf("DeployBotInstance() returned bot name %s, expected %s", bot.Name, tt.request.Name)
				}
			}
		})
	}
}

func TestAPIClient_UpdateBotInstance(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		botID         string
		request       *internal.BotUpdateRequest
		expectedError bool
		errorContains string
	}{
		{
			name:         "successful update",
			statusCode:   200,
			responseBody: `{"success": true}`,
			botID:        "bot_123",
			request: &internal.BotUpdateRequest{
				Name: "updated-bot",
				Config: map[string]interface{}{
					"name":    "updated-bot",
					"type":    "trading",
					"version": "1.1.0",
				},
			},
			expectedError: false,
		},
		{
			name:          "bot not found",
			statusCode:    404,
			responseBody:  `{"error": "Bot not found"}`,
			botID:         "bot_nonexistent",
			request:       &internal.BotUpdateRequest{Name: "test-bot", Config: map[string]interface{}{"name": "test-bot"}},
			expectedError: true,
			errorContains: "bot not found",
		},
		{
			name:          "invalid auth",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			botID:         "bot_123",
			request:       &internal.BotUpdateRequest{Name: "test-bot", Config: map[string]interface{}{"name": "test-bot"}},
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "update failed",
			statusCode:    400,
			responseBody:  `{"error": "Invalid configuration"}`,
			botID:         "bot_123",
			request:       &internal.BotUpdateRequest{Name: "test-bot", Config: map[string]interface{}{"name": "test-bot"}},
			expectedError: true,
			errorContains: "update failed with status 400",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				// Verify correct endpoint is called
				expectedPath := "/bot/" + tt.botID
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

				// Verify method
				if r.Method != "PUT" {
					t.Errorf("Expected PUT method, got %s", r.Method)
				}

				// Verify authorization header
				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			// Create API client
			client := internal.NewAPIClient(server.URL)

			// Create test auth
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			// Update bot
			err := client.UpdateBotInstance(auth, tt.botID, tt.request)

			if tt.expectedError {
				if err == nil {
					t.Errorf("UpdateBotInstance() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("UpdateBotInstance() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("UpdateBotInstance() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestAPIClient_DeleteBotInstance(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		botID         string
		expectedError bool
		errorContains string
	}{
		{
			name:          "successful deletion",
			statusCode:    200,
			responseBody:  `{"success": true}`,
			botID:         "bot_123",
			expectedError: false,
		},
		{
			name:          "successful deletion with 204",
			statusCode:    204,
			responseBody:  ``,
			botID:         "bot_123",
			expectedError: false,
		},
		{
			name:          "bot not found",
			statusCode:    404,
			responseBody:  `{"error": "Bot not found"}`,
			botID:         "bot_nonexistent",
			expectedError: true,
			errorContains: "bot not found",
		},
		{
			name:          "invalid auth",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			botID:         "bot_123",
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "deletion failed",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			botID:         "bot_123",
			expectedError: true,
			errorContains: "deletion failed with status 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				// Verify correct endpoint is called
				expectedPath := "/bot/" + tt.botID
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

				// Verify method
				if r.Method != "DELETE" {
					t.Errorf("Expected DELETE method, got %s", r.Method)
				}

				// Verify authorization header
				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				w.WriteHeader(tt.statusCode)
				w.Write([]byte(tt.responseBody))
			}))
			defer server.Close()

			// Create API client
			client := internal.NewAPIClient(server.URL)

			// Create test auth
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			// Delete bot
			err := client.DeleteBotInstance(auth, tt.botID)

			if tt.expectedError {
				if err == nil {
					t.Errorf("DeleteBotInstance() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("DeleteBotInstance() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("DeleteBotInstance() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestAPIClient_FindBotByNameOrID(t *testing.T) {
	responseBody := `{
		"success": true,
		"data": [
			{
				"id": "bot_123",
				"name": "trading-bot-1",
				"config": {
					"type": "trading",
					"version": "1.0.0"
				},
				"topic": "crypto",
				"createdAt": "2023-11-15T10:30:00.000Z",
				"updatedAt": "2023-11-15T10:30:00.000Z",
				"userId": "user_789",
				"customBotId": "custom_bot_123"
			},
			{
				"id": "bot_456",
				"name": "analysis-bot",
				"config": {
					"type": "analysis",
					"version": "2.1.0"
				},
				"topic": "stocks",
				"createdAt": "2023-11-16T14:20:00.000Z",
				"updatedAt": "2023-11-16T14:20:00.000Z",
				"userId": "user_789",
				"customBotId": "custom_bot_456"
			}
		],
		"message": "Success"
	}`

	tests := []struct {
		name         string
		identifier   string
		expectFound  bool
		expectedName string
		expectedID   string
	}{
		{
			name:         "find by ID",
			identifier:   "bot_123",
			expectFound:  true,
			expectedName: "trading-bot-1",
			expectedID:   "bot_123",
		},
		{
			name:         "find by name",
			identifier:   "analysis-bot",
			expectFound:  true,
			expectedName: "analysis-bot",
			expectedID:   "bot_456",
		},
		{
			name:        "not found",
			identifier:  "nonexistent-bot",
			expectFound: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(200)
				w.Write([]byte(responseBody))
			}))
			defer server.Close()

			// Create API client
			client := internal.NewAPIClient(server.URL)

			// Create test auth
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			// Find bot
			bot, err := client.FindBotByNameOrID(auth, tt.identifier)

			if tt.expectFound {
				if err != nil {
					t.Errorf("FindBotByNameOrID() unexpected error = %v", err)
				}
				if bot == nil {
					t.Errorf("FindBotByNameOrID() returned nil bot")
				} else {
					if bot.Name != tt.expectedName {
						t.Errorf("FindBotByNameOrID() returned name %s, expected %s", bot.Name, tt.expectedName)
					}
					if bot.ID != tt.expectedID {
						t.Errorf("FindBotByNameOrID() returned ID %s, expected %s", bot.ID, tt.expectedID)
					}
				}
			} else {
				if err == nil {
					t.Errorf("FindBotByNameOrID() expected error but got nil")
				}
				if !strings.Contains(err.Error(), "bot not found") {
					t.Errorf("FindBotByNameOrID() error = %v, expected to contain 'bot not found'", err)
				}
			}
		})
	}
}

func TestAPIClient_UploadFileDirect(t *testing.T) {
	tests := []struct {
		name          string
		botName       string
		version       string
		statusCode    int
		responseBody  string
		expectedError bool
		errorContains string
		expectedPath  string
	}{
		{
			name:       "successful direct upload",
			botName:    "test-bot",
			version:    "1.0.0",
			statusCode: 200,
			responseBody: `{
				"success": true,
				"filePath": "user123/test-bot/1.0.0",
				"message": "File uploaded successfully"
			}`,
			expectedError: false,
			expectedPath:  "user123/test-bot/1.0.0",
		},
		{
			name:          "invalid auth",
			botName:       "test-bot",
			version:       "1.0.0",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "upload failed with status 401",
		},
		{
			name:          "upload failed",
			botName:       "test-bot",
			version:       "1.0.0",
			statusCode:    400,
			responseBody:  `{"error": "Bad Request"}`,
			expectedError: true,
			errorContains: "upload failed with status 400",
		},
		{
			name:          "server error",
			botName:       "test-bot",
			version:       "1.0.0",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			expectedError: true,
			errorContains: "upload failed with status 500",
		},
		{
			name:          "API success false",
			botName:       "test-bot",
			version:       "1.0.0",
			statusCode:    200,
			responseBody: `{
				"success": false,
				"message": "Invalid file format"
			}`,
			expectedError: true,
			errorContains: "upload failed: Invalid file format",
		},
		{
			name:          "missing filePath in response",
			botName:       "test-bot",
			version:       "1.0.0",
			statusCode:    200,
			responseBody: `{
				"success": true,
				"message": "File uploaded successfully"
			}`,
			expectedError: true,
			errorContains: "file path not found in response",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a temporary file for testing
			tmpFile := "/tmp/test-bot.zip"
			testContent := []byte("test zip content")
			if err := os.WriteFile(tmpFile, testContent, 0644); err != nil {
				t.Fatalf("Failed to create test file: %v", err)
			}
			defer os.Remove(tmpFile)

			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				expectedPath := "/custom-bots/" + tt.botName + "/upload"
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

				if r.Method != "POST" {
					t.Errorf("Expected POST method, got %s", r.Method)
				}

				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				contentType := r.Header.Get("Content-Type")
				if !strings.Contains(contentType, "multipart/form-data") {
					t.Errorf("Expected Content-Type to contain multipart/form-data, got %s", contentType)
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

			filePath, err := client.UploadFileDirect(tt.botName, tt.version, tmpFile, auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("UploadFileDirect() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("UploadFileDirect() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("UploadFileDirect() unexpected error = %v", err)
				}
				if filePath != tt.expectedPath {
					t.Errorf("UploadFileDirect() returned path %s, expected %s", filePath, tt.expectedPath)
				}
			}
		})
	}
}
