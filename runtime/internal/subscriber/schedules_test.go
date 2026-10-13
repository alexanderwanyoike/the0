package subscriber

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"runtime/internal/util"
)

func TestParseScheduleMessage(t *testing.T) {
	subscriber := &Subscriber{logger: &util.DefaultLogger{}}

	tests := []struct {
		name        string
		data        []byte
		expectError bool
		expectedID  string
	}{
		{
			name: "Valid bot schedule event",
			data: []byte(`{
				"id": "550e8400-e29b-41d4-a716-446655440001",
				"config": {"test": "value"},
				"custom": {
					"config": {
						"name": "test-bot",
						"description": "Test bot",
						"runtime": "python3.11",
						"version": "1.0.0",
						"author": "test-author",
						"type": "scheduled",
						"entrypoints": {},
						"schema": {},
						"readme": "",
						"metadata": {}
					},
					"createdAt": "2023-01-01T00:00:00Z",
					"updatedAt": "2023-01-01T00:00:00Z",
					"filePath": "test.zip",
					"version": "1.0"
				}
			}`),
			expectError: false,
			expectedID:  "550e8400-e29b-41d4-a716-446655440001",
		},
		{
			name:        "Invalid JSON",
			data:        []byte(`{invalid json`),
			expectError: true,
		},
		{
			name: "Empty ID",
			data: []byte(`{
				"id": "",
				"config": {"test": "value"},
				"custom": {
					"config": {},
					"createdAt": "2023-01-01T00:00:00Z",
					"updatedAt": "2023-01-01T00:00:00Z",
					"filePath": "test.zip",
					"version": "1.0"
				}
			}`),
			expectError: false,
			expectedID:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			message, err := subscriber.parseScheduleMessage(tt.data)

			if tt.expectError {
				assert.Error(t, err)
				assert.Nil(t, message)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, message)
				assert.Equal(t, tt.expectedID, message.ID)
			}
		})
	}
}
