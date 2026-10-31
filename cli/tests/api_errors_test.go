package internal

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"the0/internal"
)

func TestAPIClient_ErrorsCarryTheServersMessage(t *testing.T) {
	zipPath := filepath.Join(t.TempDir(), "bot.zip")
	if err := os.WriteFile(zipPath, []byte("zip"), 0o600); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name    string
		call    func(c *internal.APIClient, auth *internal.Auth) error
		status  int
		body    string
		wantErr string
	}{
		{
			name: "a 404 keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				return c.UpdateBotInstance(auth, "bot-1", &internal.BotUpdateRequest{Name: "b"})
			},
			status:  404,
			body:    `{"statusCode":404,"message":"Bot not found or access denied","error":"Not Found"}`,
			wantErr: "API error 404: Bot not found or access denied",
		},
		{
			name: "a 404 from a query keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.ExecuteBotQuery(auth, "bot-1", &internal.BotQueryRequest{QueryPath: "/status", TimeoutSec: 1})
				return err
			},
			status:  404,
			body:    `{"statusCode":404,"message":"Bot not found or access denied","error":"Not Found"}`,
			wantErr: "API error 404: Bot not found or access denied",
		},
		{
			name: "a 404 from logs keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.GetBotLogs(auth, "bot-1", nil)
				return err
			},
			status:  404,
			body:    `{"statusCode":404,"message":"Bot bot-1 has no logs yet","error":"Not Found"}`,
			wantErr: "API error 404: Bot bot-1 has no logs yet",
		},
		{
			name: "a 404 from a state key keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.GetBotStateKey(auth, "bot-1", "k")
				return err
			},
			status:  404,
			body:    `{"statusCode":404,"message":"Bot or state key not found","error":"Not Found"}`,
			wantErr: "API error 404: Bot or state key not found",
		},
		{
			name: "a 400 shows the message, not the raw body",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				return c.UpdateBotInstance(auth, "bot-1", &internal.BotUpdateRequest{Name: "b"})
			},
			status:  400,
			body:    `{"statusCode":400,"message":"Bot not found","error":"Bad Request"}`,
			wantErr: "API error 400: Bot not found",
		},
		{
			name: "a list of validation messages is joined",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.DeployBotInstance(auth, &internal.BotDeployRequest{Name: "b"})
				return err
			},
			status:  400,
			body:    `{"statusCode":400,"message":["name should not be empty","config must be an object"],"error":"Bad Request"}`,
			wantErr: "API error 400: name should not be empty; config must be an object",
		},
		{
			name: "an upload rejection keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.UploadFileDirect("b", "1.0.0", zipPath, auth)
				return err
			},
			status:  400,
			body:    `{"statusCode":400,"message":"Only ZIP files are allowed","error":"Bad Request"}`,
			wantErr: "API error 400: Only ZIP files are allowed",
		},
		{
			name: "a 409 keeps the server's reason",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.DeleteBotStateKey(auth, "bot-1", "k")
				return err
			},
			status:  409,
			body:    `{"statusCode":409,"message":"State changed while deleting, try again","error":"Conflict"}`,
			wantErr: "API error 409: State changed while deleting, try again",
		},
		{
			name: "the error field is used when there is no message",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.ListBots(auth)
				return err
			},
			status:  400,
			body:    `{"error":"invalid version format"}`,
			wantErr: "API error 400: invalid version format",
		},
		{
			name: "a body that is not JSON is shown as sent",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				return c.DeleteCustomBot(auth, "b")
			},
			status:  502,
			body:    "Bad Gateway\n",
			wantErr: "API error 502: Bad Gateway",
		},
		{
			name: "an empty body falls back to the status text",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				_, err := c.DeleteBotInstance(auth, "bot-1")
				return err
			},
			status:  500,
			body:    "",
			wantErr: "API error 500: Internal Server Error",
		},
		{
			name: "a rejected key reads the same on every endpoint",
			call: func(c *internal.APIClient, auth *internal.Auth) error {
				return c.DeleteCustomBotVersion(auth, "b", "1.0.0")
			},
			status:  401,
			body:    `{"statusCode":401,"message":"Invalid API key","error":"Unauthorized"}`,
			wantErr: "API error 401: authentication failed: API key is invalid or revoked",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(tt.status)
				_, _ = w.Write([]byte(tt.body))
			}))
			defer server.Close()

			client := internal.NewAPIClient(server.URL)
			auth := &internal.Auth{APIKey: "test-key", CreatedAt: time.Now()}

			err := tt.call(client, auth)
			if err == nil {
				t.Fatalf("expected error %q, got nil", tt.wantErr)
			}
			if err.Error() != tt.wantErr {
				t.Errorf("error = %q, want %q", err.Error(), tt.wantErr)
			}
		})
	}
}
