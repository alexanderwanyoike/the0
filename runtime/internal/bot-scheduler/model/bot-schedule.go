package model

// Bot represents a scheduled bot for execution (compatible with bot-runner)
type Bot struct {
	ID               string                 `bson:"id"`
	SegmentId        int32                  `bson:"segment_id"`
	Config           map[string]interface{} `bson:"config"`
	CustomBotVersion CustomBotVersion       `bson:"custom"`
	Enabled          *bool                  `bson:"enabled,omitempty"`
}

// IsEnabled returns true if the bot is enabled (default true for backward compatibility)
func (b *Bot) IsEnabled() bool {
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

type BotSchedule struct {
	ID                string                 `bson:"id"`
	NextExecutionTime int64                  `bson:"next_execution_time"`
	SegmentId         int32                  `bson:"segment_id"`
	Config            map[string]interface{} `bson:"config"`
	CustomBotVersion  CustomBotVersion       `bson:"custom"`
	Enabled           *bool                  `bson:"enabled,omitempty"`
}

func (b *BotSchedule) IsEnabled() bool {
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

type CustomBotVersion struct {
	Version   string       `json:"version" bson:"version"`
	Config    APIBotConfig `json:"config" bson:"config"`
	FilePath  string       `json:"filePath" bson:"filePath"`
	CreatedAt string       `json:"createdAt" bson:"createdAt"`
	UpdatedAt string       `json:"updatedAt" bson:"updatedAt"`
}

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
