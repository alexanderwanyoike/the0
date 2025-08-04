package model

type Backtest struct {
	ID               string                 `bson:"id"`
	SegmentId        int32                  `bson:"segment_id"`
	Config           map[string]interface{} `bson:"config"`
	CustomBotVersion CustomBotVersion       `bson:"custom"`
}

type CustomBotVersion struct {
	Version   string       `json:"version"`
	Config    APIBotConfig `json:"config"`
	FilePath  string       `json:"filePath"`
	CreatedAt string       `json:"createdAt"`
	UpdatedAt string       `json:"updatedAt"`
}

type APIBotConfig struct {
	Name        string                 `json:"name"`
	Description string                 `json:"description"`
	Runtime     string                 `json:"runtime,omitempty" default:"python3.11"`
	Version     string                 `json:"version"`
	Author      string                 `json:"author"`
	Type        string                 `json:"type"`
	Entrypoints map[string]string      `json:"entrypoints"`
	Schema      map[string]interface{} `json:"schema"`
	Readme      string                 `json:"readme"`
	Metadata    map[string]interface{} `json:"metadata"`
}
