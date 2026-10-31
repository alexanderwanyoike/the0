package internal

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"
)

// APIError represents an error response from the API with a status code
type APIError struct {
	StatusCode int
	Message    string
}

func (e *APIError) Error() string {
	return fmt.Sprintf("API error %d: %s", e.StatusCode, e.Message)
}

// IsNotFound reports whether err is an api 404.
func IsNotFound(err error) bool {
	var apiErr *APIError
	return errors.As(err, &apiErr) && apiErr.StatusCode == http.StatusNotFound
}

const authFailedMessage = "authentication failed: API key is invalid or revoked"

// apiErrorFromResponse turns a failed response into an *APIError.
// A rejected key reads the same everywhere because IsAuthError, not the
// text, drives the re-login; every other status keeps the server's reason.
func apiErrorFromResponse(status int, body []byte) *APIError {
	if status == http.StatusUnauthorized || status == http.StatusForbidden {
		return &APIError{StatusCode: status, Message: authFailedMessage}
	}
	return &APIError{StatusCode: status, Message: serverMessage(status, body)}
}

func serverMessage(status int, body []byte) string {
	var parsed struct {
		Message json.RawMessage `json:"message"`
		Error   json.RawMessage `json:"error"`
		Data    struct {
			Error string `json:"error"`
		} `json:"data"`
	}
	if err := json.Unmarshal(body, &parsed); err == nil {
		for _, raw := range []json.RawMessage{parsed.Message, parsed.Error} {
			if msg := messageText(raw); msg != "" {
				return msg
			}
		}
		if parsed.Data.Error != "" {
			return parsed.Data.Error
		}
	}
	if text := strings.TrimSpace(string(body)); text != "" {
		return text
	}
	return http.StatusText(status)
}

// messageText reads the shapes NestJS puts in "message" and "error": a
// string, a list of validation messages, or an object with its own message.
func messageText(raw json.RawMessage) string {
	if len(raw) == 0 {
		return ""
	}
	var text string
	if json.Unmarshal(raw, &text) == nil {
		return strings.TrimSpace(text)
	}
	var list []string
	if json.Unmarshal(raw, &list) == nil {
		return strings.Join(list, "; ")
	}
	var nested struct {
		Message string `json:"message"`
	}
	if json.Unmarshal(raw, &nested) == nil {
		return strings.TrimSpace(nested.Message)
	}
	return ""
}
