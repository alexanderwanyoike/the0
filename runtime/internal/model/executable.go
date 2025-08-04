package model

type Executable struct {
	ID              string
	Runtime         string
	Entrypoint      string // Either `bot` or `backtest`
	EntrypointFiles map[string]string
	Config          map[string]interface{}
	FilePath        string
	IsLongRunning   bool
	PersistResults  bool
	Segment         int32 // Worker segment for container labeling
}
