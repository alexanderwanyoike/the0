package entrypoints

import (
	"fmt"
	"strings"
	"text/template"
)

const nodeBashEntrypoint = `#!/bin/bash
#!/bin/sh
set -e

# Copy the Node.js entrypoint script to the container
cat > /tmp/nodejs_entrypoint.js << 'NODEJS_SCRIPT'
{{ .ScriptContent }}
NODEJS_SCRIPT

# Execute the Node.js entrypoint
{{if eq .EntryPointType "backtest"}}export BACKTEST_ID="{{ .BotId }}"
export BACKTEST_CONFIG='{{ .BotConfig }}'{{else}}export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'{{end}}
export SCRIPT_PATH="{{ .ScriptPath }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"
exec node /tmp/nodejs_entrypoint.js
`

const pythonBashEntrypoint = `#!/bin/bash
set -e

# Start from a known good directory to avoid getcwd issues
cd /tmp

# Validate that the entrypoint directory is accessible
if [ ! -d "/{{ .EntryPointType }}" ]; then
    echo "ERROR: /{{ .EntryPointType }} directory does not exist" >&2
    exit 1
fi

# Test directory access
if ! ls -la /{{ .EntryPointType }} >/dev/null 2>&1; then
    echo "ERROR: Cannot access /{{ .EntryPointType }} directory" >&2
    exit 1
fi

echo "PWD: $(pwd)" >&2
pwd >&2

# Copy the Python entrypoint script to the container
cat > /tmp/python_entrypoint.py << 'PYTHON_SCRIPT'
{{ .ScriptContent }}
PYTHON_SCRIPT

# Execute the Python entrypoint
{{if eq .EntryPointType "backtest"}}export BACKTEST_ID="{{ .BotId }}"
export BACKTEST_CONFIG='{{ .BotConfig }}'{{else}}export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'{{end}}
export SCRIPT_PATH="{{ .ScriptPath }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"
exec python3 /tmp/python_entrypoint.py
`

// rustBashEntrypoint executes pre-built Rust binary (built by CLI)
const rustBashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Find the pre-built binary in target/release/
BINARY=$(find target/release -maxdepth 1 -type f -executable ! -name "*.d" ! -name "*.so" 2>/dev/null | head -1)

if [ -z "$BINARY" ]; then
    echo '{"status":"error","message":"No pre-built binary found in target/release/"}'
    exit 1
fi

export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
exec "$BINARY"
`

type bashEntrypointFactory struct {
	EntryPointType string
	ScriptContent  string
	BotId          string
	BotConfig      string
	ScriptPath     string
}

func NewBashEntrypointFactory(
	entryPointType, scriptContent, botId, botConfig, scriptPath string,
) *bashEntrypointFactory {
	// Add logging for debugging
	fmt.Printf("ENTRYPOINT_FACTORY: Creating bash entrypoint factory\n")
	fmt.Printf("ENTRYPOINT_FACTORY: EntryPointType=%s\n", entryPointType)
	fmt.Printf("ENTRYPOINT_FACTORY: BotId=%s\n", botId)
	fmt.Printf("ENTRYPOINT_FACTORY: ScriptPath=%s\n", scriptPath)

	return &bashEntrypointFactory{
		EntryPointType: entryPointType,
		ScriptContent:  scriptContent,
		BotId:          botId,
		BotConfig:      botConfig,
		ScriptPath:     scriptPath,
	}
}

func (p *bashEntrypointFactory) BuildBashEntrypoint(
	runtime string,
) (string, error) {
	var selectedEntrypoint string
	switch {
	case runtime == "nodejs20":
		selectedEntrypoint = nodeBashEntrypoint
	case runtime == "python3.11":
		selectedEntrypoint = pythonBashEntrypoint
	case runtime == "rust-stable":
		selectedEntrypoint = rustBashEntrypoint
	// Add more cases for other runtimes as needed
	default:
		return "", fmt.Errorf("unsupported runtime: %s", runtime)
	}

	tmpl, err := template.New("bashEntrypoint").Parse(selectedEntrypoint)
	if err != nil {
		return "", fmt.Errorf("failed to parse bash entrypoint template: %w", err)
	}
	var result strings.Builder
	err = tmpl.Execute(&result, p)
	if err != nil {
		return "", fmt.Errorf("failed to execute bash entrypoint template: %w", err)
	}

	return result.String(), nil
}
