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
// Uses ScriptPath directly from bot-config.yaml entrypoint specification
const rustBashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Use the binary path specified in bot-config.yaml entrypoints.bot
BINARY="{{ .ScriptPath }}"

if [ ! -f "$BINARY" ]; then
    echo '{"status":"error","message":"Binary not found: '"$BINARY"'"}'
    exit 1
fi

# Ensure the binary is executable (ZIP extraction may not preserve permissions)
chmod +x "$BINARY"

export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
exec "$BINARY"
`

// dotnet8BashEntrypoint executes pre-built .NET DLL (built by CLI)
// Uses ScriptPath directly from bot-config.yaml entrypoint specification
const dotnet8BashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Use the DLL path specified in bot-config.yaml entrypoints.bot
DLL="{{ .ScriptPath }}"

if [ ! -f "$DLL" ]; then
    echo '{"status":"error","message":"DLL not found: '"$DLL"'"}'
    exit 1
fi

export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
exec dotnet "$DLL"
`

// gcc13BashEntrypoint executes pre-built C/C++ binary (built by CLI)
// Uses ScriptPath directly from bot-config.yaml entrypoint specification
const gcc13BashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Use the binary path specified in bot-config.yaml entrypoints.bot
BINARY="{{ .ScriptPath }}"

if [ ! -f "$BINARY" ]; then
    echo '{"status":"error","message":"Binary not found: '"$BINARY"'"}'
    exit 1
fi

# Ensure the binary is executable (ZIP extraction may not preserve permissions)
chmod +x "$BINARY"

export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
exec "$BINARY"
`

// scala3BashEntrypoint executes pre-built Scala assembly JAR (built by CLI)
// Uses ScriptPath directly from bot-config.yaml entrypoint specification
const scala3BashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Use the JAR path specified in bot-config.yaml entrypoints.bot
JAR="{{ .ScriptPath }}"

if [ ! -f "$JAR" ]; then
    echo '{"status":"error","message":"JAR not found: '"$JAR"'"}'
    exit 1
fi

export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
exec java -jar "$JAR"
`

// ghc96BashEntrypoint executes pre-built Haskell binary (built by CLI with Cabal)
// Uses ScriptPath directly from bot-config.yaml entrypoint specification
const ghc96BashEntrypoint = `#!/bin/bash
set -e

cd "/{{ .EntryPointType }}"

# Use the binary path specified in bot-config.yaml entrypoints.bot
BINARY="{{ .ScriptPath }}"

if [ ! -f "$BINARY" ]; then
    echo '{"status":"error","message":"Binary not found: '"$BINARY"'"}'
    exit 1
fi

# Ensure the binary is executable (ZIP extraction may not preserve permissions)
chmod +x "$BINARY"

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
	case runtime == "dotnet8":
		selectedEntrypoint = dotnet8BashEntrypoint
	case runtime == "gcc13":
		selectedEntrypoint = gcc13BashEntrypoint
	case runtime == "scala3":
		selectedEntrypoint = scala3BashEntrypoint
	case runtime == "ghc96":
		selectedEntrypoint = ghc96BashEntrypoint
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
