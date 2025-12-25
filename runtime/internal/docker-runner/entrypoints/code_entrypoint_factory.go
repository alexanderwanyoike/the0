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
	case "bot":
		switch f.runtime {
		case "nodejs20":
			return NodeJsBotEntrypoint, nil
		case "python3", "python3.11":
			return PythonBotEntrypoint, nil
		case "rust-stable":
			return RustBotEntrypoint, nil
		case "dotnet8":
			return Dotnet8BotEntrypoint, nil
		case "gcc13":
			return Gcc13BotEntrypoint, nil
		case "scala3":
			return Scala3BotEntrypoint, nil
		}
	}
	return "", fmt.Errorf("unsupported entry point type or runtime: %s, %s", f.entryPointType, f.runtime)
}
