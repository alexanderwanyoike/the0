package dockerrunner

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseContainerOutput_WithMarker(t *testing.T) {
	tests := []struct {
		name          string
		stdout        string
		stderr        string
		wantResult    string
		wantLogs      string
		wantHasMarker bool
	}{
		{
			name:          "simple marker on single line",
			stdout:        `THE0_RESULT:{"status":"success","message":"done"}`,
			stderr:        "",
			wantResult:    `{"status":"success","message":"done"}`,
			wantLogs:      "",
			wantHasMarker: true,
		},
		{
			name:          "marker with debug logs before",
			stdout:        "debug: starting bot\nprocessing data\nTHE0_RESULT:{\"status\":\"success\"}\n",
			stderr:        "",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "debug: starting bot\nprocessing data",
			wantHasMarker: true,
		},
		{
			name:          "marker with logs before and after",
			stdout:        "Starting...\nTHE0_RESULT:{\"status\":\"success\"}\nCleanup done",
			stderr:        "",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "Starting...\n\nCleanup done",
			wantHasMarker: true,
		},
		{
			name:          "multiple markers uses last",
			stdout:        "THE0_RESULT:{\"status\":\"pending\"}\nmore work\nTHE0_RESULT:{\"status\":\"success\"}",
			stderr:        "",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "THE0_RESULT:{\"status\":\"pending\"}\nmore work",
			wantHasMarker: true,
		},
		{
			name:          "marker with stderr",
			stdout:        "debug log\nTHE0_RESULT:{\"status\":\"success\"}\n",
			stderr:        "warning: something happened",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "debug log\n\nwarning: something happened",
			wantHasMarker: true,
		},
		{
			name:          "marker with whitespace",
			stdout:        "  THE0_RESULT:{\"status\":\"success\"}  \n",
			stderr:        "",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "",
			wantHasMarker: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ParseContainerOutput(tt.stdout, tt.stderr)
			assert.Equal(t, tt.wantResult, result.Result)
			assert.Equal(t, tt.wantHasMarker, result.HasMarker)
		})
	}
}

func TestParseContainerOutput_NoMarker(t *testing.T) {
	tests := []struct {
		name          string
		stdout        string
		stderr        string
		wantResult    string
		wantLogs      string
		wantHasMarker bool
	}{
		{
			name:          "plain json no marker - backwards compatible",
			stdout:        `{"status":"success"}`,
			stderr:        "",
			wantResult:    `{"status":"success"}`,
			wantLogs:      "",
			wantHasMarker: false,
		},
		{
			name:          "stdout and stderr combined - backwards compatible",
			stdout:        `{"status":"success"}`,
			stderr:        "some error output",
			wantResult:    `{"status":"success"}some error output`,
			wantLogs:      "",
			wantHasMarker: false,
		},
		{
			name:          "empty output",
			stdout:        "",
			stderr:        "",
			wantResult:    "",
			wantLogs:      "",
			wantHasMarker: false,
		},
		{
			name:          "text without marker",
			stdout:        "just some output\nno json here",
			stderr:        "",
			wantResult:    "just some output\nno json here",
			wantLogs:      "",
			wantHasMarker: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ParseContainerOutput(tt.stdout, tt.stderr)
			assert.Equal(t, tt.wantResult, result.Result)
			assert.Equal(t, tt.wantLogs, result.Logs)
			assert.Equal(t, tt.wantHasMarker, result.HasMarker)
		})
	}
}

func TestValidateResultJSON(t *testing.T) {
	tests := []struct {
		name    string
		result  string
		wantErr bool
	}{
		{
			name:    "valid json object",
			result:  `{"status":"success","message":"done"}`,
			wantErr: false,
		},
		{
			name:    "valid json array",
			result:  `[1, 2, 3]`,
			wantErr: false,
		},
		{
			name:    "valid json string",
			result:  `"hello"`,
			wantErr: false,
		},
		{
			name:    "valid json number",
			result:  `42`,
			wantErr: false,
		},
		{
			name:    "empty string is valid",
			result:  "",
			wantErr: false,
		},
		{
			name:    "invalid json",
			result:  `{invalid}`,
			wantErr: true,
		},
		{
			name:    "incomplete json",
			result:  `{"status": `,
			wantErr: true,
		},
		{
			name:    "plain text",
			result:  "not json at all",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateResultJSON(tt.result)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestResultMarkerConstant(t *testing.T) {
	// Ensure the marker is what we expect
	assert.Equal(t, "THE0_RESULT:", ResultMarker)
}
