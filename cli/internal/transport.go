package internal

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"
)

// apiRequest is one call to the api, relative to APIClient.BaseURL.
type apiRequest struct {
	method      string
	path        string
	query       url.Values
	body        []byte
	contentType string
	// timeout replaces the client's default for calls the server may take
	// long to answer: uploads and bot queries.
	timeout time.Duration
}

// send makes r with auth's API key. It returns the body of a 2xx response;
// any other status becomes an *APIError.
func (c *APIClient) send(auth *Auth, r apiRequest) ([]byte, error) {
	target := c.BaseURL + r.path
	if len(r.query) > 0 {
		target += "?" + r.query.Encode()
	}

	var body io.Reader
	if r.body != nil {
		body = bytes.NewReader(r.body)
	}
	req, err := http.NewRequest(r.method, target, body)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %v", err)
	}
	req.Header.Set("Authorization", "ApiKey "+auth.APIKey)
	if r.contentType != "" {
		req.Header.Set("Content-Type", r.contentType)
	}

	client := c.HTTPClient
	if r.timeout > 0 {
		client = &http.Client{Timeout: r.timeout, Transport: c.HTTPClient.Transport}
	}

	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("network error: %v", err)
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %v", err)
	}

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return nil, apiErrorFromResponse(resp.StatusCode, respBody)
	}
	return respBody, nil
}

// sendJSON makes a call whose body is payload encoded as JSON.
func (c *APIClient) sendJSON(auth *Auth, method, path string, payload any) ([]byte, error) {
	encoded, err := json.Marshal(payload)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %v", err)
	}
	return c.send(auth, apiRequest{method: method, path: path, body: encoded, contentType: "application/json"})
}

// envelope is the {success, data, message} wrapper most api routes answer with.
type envelope[T any] struct {
	Success bool   `json:"success"`
	Data    T      `json:"data"`
	Message string `json:"message"`
}

// unwrap decodes an envelope and returns its data, or the api's message
// when it reports no success.
func unwrap[T any](body []byte) (T, error) {
	var env envelope[T]
	if err := json.Unmarshal(body, &env); err != nil {
		var zero T
		return zero, fmt.Errorf("failed to parse API response: %v", err)
	}
	if !env.Success {
		var zero T
		return zero, fmt.Errorf("API returned error: %s", env.Message)
	}
	return env.Data, nil
}

// decode reads a response that is not wrapped in an envelope.
func decode[T any](body []byte) (T, error) {
	var out T
	if err := json.Unmarshal(body, &out); err != nil {
		return out, fmt.Errorf("failed to parse API response: %v", err)
	}
	return out, nil
}
