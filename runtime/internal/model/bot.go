package model

// Bot represents a bot instance for execution.
type Bot struct {
	ID               string                 `bson:"id" json:"id"`
	SegmentId        int32                  `bson:"segment_id"`
	Config           map[string]interface{} `bson:"config"`
	CustomBotVersion CustomBotVersion       `bson:"custom"`
	Enabled          *bool                  `bson:"enabled,omitempty"`
}

// CustomBotVersion contains version-specific bot configuration.
type CustomBotVersion struct {
	Version   string       `json:"version" bson:"version"`
	Config    APIBotConfig `json:"config" bson:"config"`
	FilePath  string       `json:"filePath" bson:"filePath"`
	CreatedAt string       `json:"createdAt" bson:"createdAt"`
	UpdatedAt string       `json:"updatedAt" bson:"updatedAt"`
}

// APIBotConfig defines the bot's runtime configuration.
type APIBotConfig struct {
	Name        string                 `json:"name" bson:"name"`
	Description string                 `json:"description" bson:"description"`
	Runtime     string                 `json:"runtime,omitempty" bson:"runtime,omitempty" default:"python3.11"`
	Version     string                 `json:"version" bson:"version"`
	Author      string                 `json:"author" bson:"author"`
	Type        string                 `json:"type" bson:"type"`
	Entrypoints map[string]string      `json:"entrypoints" bson:"entrypoints"`
	Schema      map[string]interface{} `json:"schema" bson:"schema"`
	Readme      string                 `json:"readme" bson:"readme"`
	Metadata    map[string]interface{} `json:"metadata" bson:"metadata"`
}

// IsEnabled returns true if the bot is enabled (default true for backward compatibility)
func (b *Bot) IsEnabled() bool {
	// Check struct field first (matches MongoDB query on top-level "enabled" field)
	if b.Enabled != nil {
		return *b.Enabled
	}

	// Fall back to Config["enabled"] for backward compatibility
	if b.Config == nil {
		return true // Default to enabled if no config
	}

	enabled, exists := b.Config["enabled"]
	if !exists {
		return true // Default to enabled if enabled field doesn't exist
	}

	// Handle different types that might be stored
	switch v := enabled.(type) {
	case bool:
		return v
	case string:
		return v != "false" && v != "0"
	case int:
		return v != 0
	case int32:
		return v != 0
	case int64:
		return v != 0
	case float32:
		return v != 0.0
	case float64:
		return v != 0.0
	default:
		return true // Default to enabled for unknown types
	}
}

// DeepCopy creates a complete copy of the Bot with no shared references.
func (b Bot) DeepCopy() Bot {
	cpy := Bot{
		ID:        b.ID,
		SegmentId: b.SegmentId,
	}

	// Deep copy Config map
	if b.Config != nil {
		cpy.Config = deepCopyMap(b.Config)
	}

	// Deep copy CustomBotVersion
	cpy.CustomBotVersion = b.CustomBotVersion.DeepCopy()

	// Copy Enabled pointer
	if b.Enabled != nil {
		enabledCopy := *b.Enabled
		cpy.Enabled = &enabledCopy
	}

	return cpy
}

// DeepCopy creates a complete copy of CustomBotVersion.
func (c CustomBotVersion) DeepCopy() CustomBotVersion {
	cpy := CustomBotVersion{
		Version:   c.Version,
		FilePath:  c.FilePath,
		CreatedAt: c.CreatedAt,
		UpdatedAt: c.UpdatedAt,
	}
	cpy.Config = c.Config.DeepCopy()
	return cpy
}

// DeepCopy creates a complete copy of APIBotConfig.
func (a APIBotConfig) DeepCopy() APIBotConfig {
	cpy := APIBotConfig{
		Name:        a.Name,
		Description: a.Description,
		Runtime:     a.Runtime,
		Version:     a.Version,
		Author:      a.Author,
		Type:        a.Type,
		Readme:      a.Readme,
	}

	// Deep copy Entrypoints
	if a.Entrypoints != nil {
		cpy.Entrypoints = make(map[string]string, len(a.Entrypoints))
		for k, v := range a.Entrypoints {
			cpy.Entrypoints[k] = v
		}
	}

	// Deep copy Schema
	if a.Schema != nil {
		cpy.Schema = deepCopyMap(a.Schema)
	}

	// Deep copy Metadata
	if a.Metadata != nil {
		cpy.Metadata = deepCopyMap(a.Metadata)
	}

	return cpy
}

// deepCopyMap creates a deep copy of a map[string]interface{}.
func deepCopyMap(m map[string]interface{}) map[string]interface{} {
	cpy := make(map[string]interface{}, len(m))
	for k, v := range m {
		cpy[k] = deepCopyValue(v)
	}
	return cpy
}

// deepCopyValue creates a deep copy of an interface{} value.
func deepCopyValue(v interface{}) interface{} {
	switch val := v.(type) {
	case map[string]interface{}:
		return deepCopyMap(val)
	case []interface{}:
		copySlice := make([]interface{}, len(val))
		for i, elem := range val {
			copySlice[i] = deepCopyValue(elem)
		}
		return copySlice
	default:
		// Primitive types (string, int, bool, float, nil) are safe to copy directly
		return v
	}
}
