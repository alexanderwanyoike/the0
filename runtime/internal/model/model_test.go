package model

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// ===== Bot.DeepCopy Tests =====

func TestBot_DeepCopy_BasicFields(t *testing.T) {
	original := Bot{
		ID: "bot-123",
	}

	copy := original.DeepCopy()

	assert.Equal(t, original.ID, copy.ID)
}

func TestBot_DeepCopy_ConfigIndependence(t *testing.T) {
	original := Bot{
		ID: "bot-123",
		Config: map[string]interface{}{
			"key": "value",
			"nested": map[string]interface{}{
				"inner": "data",
			},
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Config["key"] = "modified"
	original.Config["nested"].(map[string]interface{})["inner"] = "changed"

	// Copy should be unaffected
	assert.Equal(t, "value", copy.Config["key"])
	assert.Equal(t, "data", copy.Config["nested"].(map[string]interface{})["inner"])
}

func TestBot_DeepCopy_NilConfig(t *testing.T) {
	original := Bot{
		ID:     "bot-123",
		Config: nil,
	}

	copy := original.DeepCopy()

	assert.Nil(t, copy.Config)
}

func TestBot_DeepCopy_ComplexNestedStructures(t *testing.T) {
	original := Bot{
		ID: "bot-123",
		Config: map[string]interface{}{
			"array": []interface{}{
				"string",
				123,
				map[string]interface{}{
					"nested": "value",
				},
			},
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Config["array"].([]interface{})[0] = "modified"
	original.Config["array"].([]interface{})[2].(map[string]interface{})["nested"] = "changed"

	// Copy should be unaffected
	assert.Equal(t, "string", copy.Config["array"].([]interface{})[0])
	assert.Equal(t, "value", copy.Config["array"].([]interface{})[2].(map[string]interface{})["nested"])
}

// ===== CustomBotVersion.DeepCopy Tests =====

func TestCustomBotVersion_DeepCopy_BasicFields(t *testing.T) {
	original := CustomBotVersion{
		Version:   "1.0.0",
		FilePath:  "/path/to/file",
		CreatedAt: "2024-01-01",
		UpdatedAt: "2024-01-02",
	}

	copy := original.DeepCopy()

	assert.Equal(t, original.Version, copy.Version)
	assert.Equal(t, original.FilePath, copy.FilePath)
	assert.Equal(t, original.CreatedAt, copy.CreatedAt)
	assert.Equal(t, original.UpdatedAt, copy.UpdatedAt)
}

func TestCustomBotVersion_DeepCopy_ConfigIndependence(t *testing.T) {
	original := CustomBotVersion{
		Version: "1.0.0",
		Config: APIBotConfig{
			Name:    "Test Bot",
			Runtime: "python3.11",
			Entrypoints: map[string]string{
				"bot": "main.py",
			},
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Config.Name = "Modified"
	original.Config.Entrypoints["bot"] = "changed.py"

	// Copy should be unaffected
	assert.Equal(t, "Test Bot", copy.Config.Name)
	assert.Equal(t, "main.py", copy.Config.Entrypoints["bot"])
}

// ===== APIBotConfig.DeepCopy Tests =====

func TestAPIBotConfig_DeepCopy_BasicFields(t *testing.T) {
	original := APIBotConfig{
		Name:        "Test Bot",
		Description: "A test bot",
		Runtime:     "python3.11",
		Version:     "1.0.0",
		Author:      "Test Author",
		Type:        "realtime",
		Readme:      "# README",
	}

	copy := original.DeepCopy()

	assert.Equal(t, original.Name, copy.Name)
	assert.Equal(t, original.Description, copy.Description)
	assert.Equal(t, original.Runtime, copy.Runtime)
	assert.Equal(t, original.Version, copy.Version)
	assert.Equal(t, original.Author, copy.Author)
	assert.Equal(t, original.Type, copy.Type)
	assert.Equal(t, original.Readme, copy.Readme)
}

func TestAPIBotConfig_DeepCopy_EntrypointsIndependence(t *testing.T) {
	original := APIBotConfig{
		Name: "Test Bot",
		Entrypoints: map[string]string{
			"bot":   "main.py",
			"query": "query.py",
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Entrypoints["bot"] = "modified.py"
	original.Entrypoints["new"] = "new.py"

	// Copy should be unaffected
	assert.Equal(t, "main.py", copy.Entrypoints["bot"])
	assert.NotContains(t, copy.Entrypoints, "new")
}

func TestAPIBotConfig_DeepCopy_SchemaIndependence(t *testing.T) {
	original := APIBotConfig{
		Name: "Test Bot",
		Schema: map[string]interface{}{
			"type": "object",
			"properties": map[string]interface{}{
				"symbol": "string",
			},
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Schema["type"] = "modified"
	original.Schema["properties"].(map[string]interface{})["symbol"] = "changed"

	// Copy should be unaffected
	assert.Equal(t, "object", copy.Schema["type"])
	assert.Equal(t, "string", copy.Schema["properties"].(map[string]interface{})["symbol"])
}

func TestAPIBotConfig_DeepCopy_MetadataIndependence(t *testing.T) {
	original := APIBotConfig{
		Name: "Test Bot",
		Metadata: map[string]interface{}{
			"key": "value",
		},
	}

	copy := original.DeepCopy()

	// Modify original
	original.Metadata["key"] = "modified"

	// Copy should be unaffected
	assert.Equal(t, "value", copy.Metadata["key"])
}

func TestAPIBotConfig_DeepCopy_NilMaps(t *testing.T) {
	original := APIBotConfig{
		Name:        "Test Bot",
		Entrypoints: nil,
		Schema:      nil,
		Metadata:    nil,
	}

	copy := original.DeepCopy()

	assert.Nil(t, copy.Entrypoints)
	assert.Nil(t, copy.Schema)
	assert.Nil(t, copy.Metadata)
}

// ===== Deep Copy Helper Function Tests =====

func TestDeepCopyValue_Primitives(t *testing.T) {
	tests := []struct {
		name  string
		value interface{}
	}{
		{"string", "test"},
		{"int", 42},
		{"float", 3.14},
		{"bool", true},
		{"nil", nil},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			copied := deepCopyValue(tt.value)
			assert.Equal(t, tt.value, copied)
		})
	}
}

func TestDeepCopyValue_NestedMaps(t *testing.T) {
	original := map[string]interface{}{
		"level1": map[string]interface{}{
			"level2": map[string]interface{}{
				"level3": "value",
			},
		},
	}

	copied := deepCopyValue(original).(map[string]interface{})

	// Modify original
	original["level1"].(map[string]interface{})["level2"].(map[string]interface{})["level3"] = "modified"

	// Copy should be unaffected
	assert.Equal(t, "value",
		copied["level1"].(map[string]interface{})["level2"].(map[string]interface{})["level3"])
}

func TestDeepCopyValue_NestedSlices(t *testing.T) {
	original := []interface{}{
		[]interface{}{
			[]interface{}{"value"},
		},
	}

	copied := deepCopyValue(original).([]interface{})

	// Modify original
	original[0].([]interface{})[0].([]interface{})[0] = "modified"

	// Copy should be unaffected
	assert.Equal(t, "value", copied[0].([]interface{})[0].([]interface{})[0])
}

func TestDeepCopyValue_MixedStructures(t *testing.T) {
	original := map[string]interface{}{
		"map": map[string]interface{}{
			"nested": "value",
		},
		"slice": []interface{}{
			"a", "b", "c",
		},
		"primitive": 42,
	}

	copied := deepCopyValue(original).(map[string]interface{})

	// Modify original
	original["map"].(map[string]interface{})["nested"] = "modified"
	original["slice"].([]interface{})[0] = "modified"
	original["primitive"] = 99

	// Copy should be unaffected
	assert.Equal(t, "value", copied["map"].(map[string]interface{})["nested"])
	assert.Equal(t, "a", copied["slice"].([]interface{})[0])
	assert.Equal(t, 42, copied["primitive"])
}

// Benchmark tests
func BenchmarkBot_DeepCopy(b *testing.B) {
	bot := Bot{
		ID: "bot-123",
		Config: map[string]interface{}{
			"key": "value",
			"nested": map[string]interface{}{
				"inner": "data",
			},
		},
		CustomBotVersion: CustomBotVersion{
			Version: "1.0.0",
			Config: APIBotConfig{
				Name:    "Test",
				Runtime: "python3.11",
			},
		},
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		bot.DeepCopy()
	}
}
