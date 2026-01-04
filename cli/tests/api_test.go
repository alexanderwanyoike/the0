package internal

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"the0/internal"
	"time"
)

func TestAPIClient_TestAPIKey(t *testing.T) {
	tests := []struct {
		name          string
		statusCode    int
		responseBody  string
		expectedError bool
		errorContains string
	}{
		{
			name:          "valid API key",
			statusCode:    200,
			responseBody:  `{"success": true, "data": {"valid": true}}`,
			expectedError: false,
		},
		{
			name:          "invalid API key - 401",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "invalid or revoked",
		},
		{
			name:          "invalid API key - 403",
			statusCode:    403,
			responseBody:  `{"error": "Forbidden"}`,
			expectedError: true,
			errorContains: "invalid or revoked",
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
			// Create test server
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				// Verify correct endpoint is called
				if r.URL.Path != "/auth/validate-api-key" {
					t.Errorf("Expected path /auth/validate-api-key, got %s", r.URL.Path)
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

			// Test API key
			err := client.TestAPIKey(auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("TestAPIKey() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("TestAPIKey() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("TestAPIKey() unexpected error = %v", err)
				}
			}
		})
	}
}

func TestAPIClient_CheckBotExists(t *testing.T) {
	config := &internal.BotConfig{
		Name:    "test-bot",
		Version: "2.0.0",
	}
	_ = config // Mark as used to avoid compiler error
}

func TestAPIClient_ListUserBots(t *testing.T) {
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
						"id": "bot-123abc",
						"customBotName": "trade-bot",
						"customBot": {
							"id": "custom-bot-123",
							"name": "trade-bot",
							"userId": "user-abc123",
							"versions": [
								{
									"version": "1.0.0",
									"config": {
										"name": "trade-bot",
										"description": "A simple trading bot",
										"version": "1.0.0",
										"type": "realtime",
										"author": "trader123"
									}
								}
							],
							"latestVersion": "1.0.0"
						},
						"acquiredAt": "2023-11-15T10:30:00.000Z"
					},
					{
						"id": "bot-456def",
						"customBotName": "market-maker",
						"acquiredAt": "2023-11-16T14:20:00.000Z"
					}
				]
			}`,
			expectedBots: 2,
		},
		{
			name:         "empty response",
			statusCode:   200,
			responseBody: `{"success": true, "data": []}`,
			expectedBots: 0,
		},
		{
			name:          "unauthorized",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "api error",
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			expectedError: true,
			errorContains: "API error: 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/user-bots" {
					t.Errorf("Expected path /user-bots, got %s", r.URL.Path)
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

			userBots, err := client.ListUserBots(auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("ListUserBots() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("ListUserBots() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("ListUserBots() unexpected error = %v", err)
				}
				if len(userBots) != tt.expectedBots {
					t.Errorf("ListUserBots() got %d bots, expected %d", len(userBots), tt.expectedBots)
				}

				// Test specific structure of first bot if present
				if len(userBots) > 0 {
					firstBot := userBots[0]
					if firstBot.CustomBotName != "trade-bot" {
						t.Errorf("ListUserBots() first bot name = %s, expected trade-bot", firstBot.CustomBotName)
					}

					// Test customBot field if present
					if firstBot.CustomBot != nil {
						if firstBot.CustomBot.LatestVersion != "1.0.0" {
							t.Errorf("ListUserBots() first bot latest version = %s, expected 1.0.0", firstBot.CustomBot.LatestVersion)
						}
						if len(firstBot.CustomBot.Versions) > 0 {
							version := firstBot.CustomBot.Versions[0]
							if version.Config.Type != "realtime" {
								t.Errorf("ListUserBots() first bot config type = %s, expected realtime", version.Config.Type)
							}
						}
					}
				}
			}
		})
	}
}

func TestAPIClient_ListCustomBots(t *testing.T) {
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
						"id": "custom-bot-123",
						"name": "trade-bot",
						"userId": "user-abc123",
						"versions": [
							{
								"version": "1.0.0",
								"config": {
									"name": "trade-bot",
									"description": "A simple trading bot",
									"version": "1.0.0",
									"type": "realtime",
									"author": "trader123",
									"entrypoints": {
										"bot": "index.js",
										"backtest": "backtest.js"
									},
									"schema": {
										"backtest": {},
										"bot": {}
									},
									"readme": "# Trade Bot\nA simple trading bot"
								},
								"userId": "user-abc123",
								"id": "version-123",
								"FilePath": "custom-bots/trade-bot/1.0.0",
								"status": "published",
								"createdAt": "2023-11-15T10:30:00.000Z",
								"updatedAt": "2023-11-15T10:30:00.000Z"
							}
						],
						"latestVersion": "1.0.0",
						"createdAt": "2023-11-15T10:30:00.000Z",
						"updatedAt": "2023-11-15T10:30:00.000Z"
					}
				]
			}`,
			expectedBots: 1,
		},
		{
			name:         "empty response",
			statusCode:   200,
			responseBody: `{"success": true, "data": []}`,
			expectedBots: 0,
		},
		{
			name:          "unauthorized",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path != "/custom-bots" {
					t.Errorf("Expected path /custom-bots, got %s", r.URL.Path)
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

			customBots, err := client.ListCustomBots(auth)

			if tt.expectedError {
				if err == nil {
					t.Errorf("ListCustomBots() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("ListCustomBots() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("ListCustomBots() unexpected error = %v", err)
				}
				if len(customBots) != tt.expectedBots {
					t.Errorf("ListCustomBots() got %d bots, expected %d", len(customBots), tt.expectedBots)
				}
			}
		})
	}
}

func TestAPIClient_GetCustomBot(t *testing.T) {
	tests := []struct {
		name          string
		botName       string
		statusCode    int
		responseBody  string
		expectedError bool
		errorContains string
	}{
		{
			name:       "successful fetch",
			botName:    "trade-bot",
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": {
					"id": "custom-bot-123",
					"name": "trade-bot",
					"userId": "user-abc123",
					"versions": [
						{
							"version": "1.0.0",
							"config": {
								"name": "trade-bot",
								"description": "A simple trading bot",
								"version": "1.0.0",
								"type": "realtime",
								"author": "trader123",
								"entrypoints": {
									"bot": "index.js",
									"backtest": "backtest.js"
								},
								"schema": {
									"backtest": {"type": "object"},
									"bot": {"type": "object"}
								},
								"readme": "# Trade Bot\nA simple trading bot"
							}
						}
					],
					"latestVersion": "1.0.0",
					"createdAt": "2023-11-15T10:30:00.000Z",
					"updatedAt": "2023-11-15T10:30:00.000Z"
				}
			}`,
		},
		{
			name:          "bot not found",
			botName:       "nonexistent-bot",
			statusCode:    404,
			responseBody:  `{"error": "Bot not found"}`,
			expectedError: true,
			errorContains: "custom bot not found",
		},
		{
			name:          "unauthorized",
			botName:       "trade-bot",
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				expectedPath := "/custom-bots/" + tt.botName
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
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

			customBot, err := client.GetCustomBot(auth, tt.botName)

			if tt.expectedError {
				if err == nil {
					t.Errorf("GetCustomBot() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("GetCustomBot() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("GetCustomBot() unexpected error = %v", err)
				}
				if customBot == nil {
					t.Errorf("GetCustomBot() returned nil bot")
				} else if customBot.Name != tt.botName {
					t.Errorf("GetCustomBot() got bot name %s, expected %s", customBot.Name, tt.botName)
				}
			}
		})
	}
}

func TestAPIClient_GetBotLogs(t *testing.T) {
	tests := []struct {
		name          string
		botID         string
		params        *internal.LogsParams
		statusCode    int
		responseBody  string
		expectedLogs  int
		expectedError bool
		errorContains string
	}{
		{
			name:       "successful logs fetch",
			botID:      "bot-123",
			params:     &internal.LogsParams{Date: "20241201", Limit: 100},
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": [
					{
						"date": "2024-01-01T12:00:00Z",
						"content": "Bot started successfully\nInitialization complete"
					},
					{
						"date": "2024-01-01T12:01:00Z",
						"content": "API call failed\nRetrying connection"
					}
				]
			}`,
			expectedLogs: 2,
		},
		{
			name:       "empty logs response",
			botID:      "bot-456",
			params:     &internal.LogsParams{Date: "20241201"},
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": []
			}`,
			expectedLogs: 0,
		},
		{
			name:       "logs with date range",
			botID:      "bot-789",
			params:     &internal.LogsParams{DateRange: "20241201-20241202", Limit: 50},
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": [
					{
						"date": "2024-01-02T10:15:00Z",
						"content": "Debug log entry\nDebugging connection issues\nConnection restored"
					}
				]
			}`,
			expectedLogs: 1,
		},
		{
			name:          "bot not found",
			botID:         "nonexistent-bot",
			params:        &internal.LogsParams{Date: "20241201"},
			statusCode:    404,
			responseBody:  `{"error": "Bot not found"}`,
			expectedError: true,
			errorContains: "bot not found",
		},
		{
			name:          "unauthorized",
			botID:         "bot-123",
			params:        &internal.LogsParams{Date: "20241201"},
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:          "api error response",
			botID:         "bot-123",
			params:        &internal.LogsParams{Date: "20241201"},
			statusCode:    200,
			responseBody:  `{"success": false, "message": "Invalid date format"}`,
			expectedError: true,
			errorContains: "Invalid date format",
		},
		{
			name:          "server error",
			botID:         "bot-123",
			params:        &internal.LogsParams{Date: "20241201"},
			statusCode:    500,
			responseBody:  `{"error": "Internal Server Error"}`,
			expectedError: true,
			errorContains: "API error: 500",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				expectedPath := "/logs/" + tt.botID
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

				// Verify authorization header
				authHeader := r.Header.Get("Authorization")
				if !strings.HasPrefix(authHeader, "ApiKey ") {
					t.Errorf("Expected Authorization header with ApiKey prefix, got %s", authHeader)
				}

				// Verify query parameters if params provided
				if tt.params != nil {
					query := r.URL.Query()
					if tt.params.Date != "" && query.Get("date") != tt.params.Date {
						t.Errorf("Expected date query param %s, got %s", tt.params.Date, query.Get("date"))
					}
					if tt.params.DateRange != "" && query.Get("dateRange") != tt.params.DateRange {
						t.Errorf("Expected dateRange query param %s, got %s", tt.params.DateRange, query.Get("dateRange"))
					}
					if tt.params.Limit > 0 {
						expectedLimit := fmt.Sprintf("%d", tt.params.Limit)
						if query.Get("limit") != expectedLimit {
							t.Errorf("Expected limit query param %s, got %s", expectedLimit, query.Get("limit"))
						}
					}
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

			logs, err := client.GetBotLogs(auth, tt.botID, tt.params)

			if tt.expectedError {
				if err == nil {
					t.Errorf("GetBotLogs() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("GetBotLogs() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("GetBotLogs() unexpected error = %v", err)
				}
				if len(logs) != tt.expectedLogs {
					t.Errorf("GetBotLogs() got %d logs, expected %d", len(logs), tt.expectedLogs)
				}

				// Test structure of first log if present
				if len(logs) > 0 {
					firstLog := logs[0]
					if firstLog.Date == "" {
						t.Errorf("GetBotLogs() first log missing date")
					}
					if firstLog.Content == "" {
						t.Errorf("GetBotLogs() first log missing content")
					}
				}
			}
		})
	}
}

func TestAPIClient_ExecuteBotQuery(t *testing.T) {
	tests := []struct {
		name          string
		botID         string
		request       *internal.BotQueryRequest
		statusCode    int
		responseBody  string
		expectedError bool
		errorContains string
	}{
		{
			name:  "successful query",
			botID: "bot-123",
			request: &internal.BotQueryRequest{
				QueryPath:  "/portfolio",
				Params:     map[string]any{"symbol": "BTC"},
				TimeoutSec: 30,
			},
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": {"positions": [{"symbol": "BTC", "amount": 1.5}]}
			}`,
			expectedError: false,
		},
		{
			name:  "query with no params",
			botID: "bot-456",
			request: &internal.BotQueryRequest{
				QueryPath:  "/health",
				TimeoutSec: 10,
			},
			statusCode: 200,
			responseBody: `{
				"success": true,
				"data": {"status": "ok"}
			}`,
			expectedError: false,
		},
		{
			name:  "bot not found",
			botID: "nonexistent-bot",
			request: &internal.BotQueryRequest{
				QueryPath:  "/status",
				TimeoutSec: 30,
			},
			statusCode:    404,
			responseBody:  `{"success": false, "message": "Bot not found"}`,
			expectedError: true,
			errorContains: "bot not found",
		},
		{
			name:  "unauthorized",
			botID: "bot-789",
			request: &internal.BotQueryRequest{
				QueryPath:  "/portfolio",
				TimeoutSec: 30,
			},
			statusCode:    401,
			responseBody:  `{"error": "Unauthorized"}`,
			expectedError: true,
			errorContains: "authentication failed",
		},
		{
			name:  "query execution error",
			botID: "bot-error",
			request: &internal.BotQueryRequest{
				QueryPath:  "/failing-query",
				TimeoutSec: 30,
			},
			statusCode:    500,
			responseBody:  `{"success": false, "message": "Query handler threw an exception"}`,
			expectedError: true,
			errorContains: "query failed",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				expectedPath := "/bot/" + tt.botID + "/query"
				if r.URL.Path != expectedPath {
					t.Errorf("Expected path %s, got %s", expectedPath, r.URL.Path)
				}

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

			client := internal.NewAPIClient(server.URL)
			auth := &internal.Auth{
				APIKey:    "test-key",
				CreatedAt: time.Now(),
			}

			response, err := client.ExecuteBotQuery(auth, tt.botID, tt.request)

			if tt.expectedError {
				if err == nil {
					t.Errorf("ExecuteBotQuery() expected error but got nil")
				} else if tt.errorContains != "" && !strings.Contains(err.Error(), tt.errorContains) {
					t.Errorf("ExecuteBotQuery() error = %v, expected to contain %v", err, tt.errorContains)
				}
			} else {
				if err != nil {
					t.Errorf("ExecuteBotQuery() unexpected error = %v", err)
				}
				if response == nil {
					t.Errorf("ExecuteBotQuery() returned nil response")
				} else if !response.Success {
					t.Errorf("ExecuteBotQuery() expected success=true, got false")
				}
			}
		})
	}
}
