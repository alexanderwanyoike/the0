package model

// BotSchedule represents a scheduled bot for cron-based execution.
type BotSchedule struct {
	ID                string                 `bson:"id"`
	NextExecutionTime int64                  `bson:"next_execution_time"`
	SegmentId         int32                  `bson:"segment_id"`
	Config            map[string]interface{} `bson:"config"`
	CustomBotVersion  CustomBotVersion       `bson:"custom"`
	Enabled           *bool                  `bson:"enabled,omitempty"`
}

// IsEnabled returns true if the schedule is enabled (default true for backward compatibility)
func (b *BotSchedule) IsEnabled() bool {
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
