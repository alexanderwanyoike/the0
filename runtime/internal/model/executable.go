package model

type Executable struct {
	ID              string
	Runtime         string
	Entrypoint      string // Either `bot`, `backtest`, or `query`
	EntrypointFiles map[string]string
	Config          map[string]interface{}
	FilePath        string
	IsLongRunning   bool
	Segment         int32 // Worker segment for container labeling

	// Query execution fields (used when Entrypoint == "query")
	QueryPath      string                 // The query path to execute (e.g., "/portfolio")
	QueryParams    map[string]interface{} // Query parameters as key-value pairs
	ResultFilePath string                 // Custom result file path (default: /bot/result.json)
}
