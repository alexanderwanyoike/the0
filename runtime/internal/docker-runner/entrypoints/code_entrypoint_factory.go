package entrypoints

import (
	"fmt"
)

type codeEntrypointFactory struct {
	entryPointType string
	runtime        string
}

func NewCodeEntrypointFactory(
	entryPointType, runtime string,
) *codeEntrypointFactory {
	return &codeEntrypointFactory{
		entryPointType: entryPointType,
		runtime:        runtime,
	}
}

func (f *codeEntrypointFactory) GetCode() (string, error) {
	switch f.entryPointType {
	case "backtest":
		switch f.runtime {
		case "nodejs20":
			return NodeJsBacktestEntrypoint, nil
		case "python3", "python3.11":
			return PythonBacktestEntrypoint, nil
		}
	case "bot":
		switch f.runtime {
		case "nodejs20":
			return NodeJsBotEntrypoint, nil
		case "python3", "python3.11":
			return PythonBotEntrypoint, nil
		}
	}
	return "", fmt.Errorf("unsupported entry point type or runtime: %s, %s", f.entryPointType, f.runtime)
}
