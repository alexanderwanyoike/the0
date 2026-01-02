package entrypoints

import (
	"fmt"
	"regexp"
	"strings"
	"text/template"
)

// validEntrypointPattern matches safe entrypoint paths.
// Allows alphanumeric characters, underscores, hyphens, dots, and forward slashes.
// No shell metacharacters, backticks, $, ;, &, |, >, <, etc.
var validEntrypointPattern = regexp.MustCompile(`^[a-zA-Z0-9_\-./]+$`)

// validateEntrypoint ensures the entrypoint path doesn't contain shell injection risks.
func validateEntrypoint(entrypoint string) error {
	if entrypoint == "" {
		return fmt.Errorf("entrypoint cannot be empty")
	}
	if !validEntrypointPattern.MatchString(entrypoint) {
		return fmt.Errorf("entrypoint contains invalid characters: %q", entrypoint)
	}
	// Prevent path traversal
	if strings.Contains(entrypoint, "..") {
		return fmt.Errorf("entrypoint cannot contain path traversal: %q", entrypoint)
	}
	return nil
}

// GeneratorOptions controls entrypoint script generation.
type GeneratorOptions struct {
	// EntryPointType is "bot" or "backtest"
	EntryPointType string
	// Entrypoint is the path to the main script (e.g., "main.py", "index.js")
	Entrypoint string
	// WorkDir is the working directory (default: "/bot")
	WorkDir string
}

// K8s-specific entrypoint templates that read config from environment variables
const k8sPythonEntrypoint = `#!/bin/bash
set -e

cd /bot

# Validate directory exists
if [ ! -d "/bot" ]; then
    echo "ERROR: /bot directory does not exist" >&2
    exit 1
fi

# Create Python entrypoint script
cat > /tmp/python_entrypoint.py << 'PYTHON_SCRIPT'
{{ .ScriptContent }}
PYTHON_SCRIPT

# Environment variables BOT_ID and BOT_CONFIG are already set by K8s pod spec
export SCRIPT_PATH="{{ .Entrypoint }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"
export CODE_MOUNT_DIR="bot"
exec python3 /tmp/python_entrypoint.py
`

const k8sNodeEntrypoint = `#!/bin/bash
set -e

cd /bot

# Create Node.js entrypoint script
cat > /tmp/nodejs_entrypoint.js << 'NODEJS_SCRIPT'
{{ .ScriptContent }}
NODEJS_SCRIPT

# Environment variables BOT_ID and BOT_CONFIG are already set by K8s pod spec
export SCRIPT_PATH="{{ .Entrypoint }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"
export CODE_MOUNT_DIR="bot"
exec node /tmp/nodejs_entrypoint.js
`

const k8sBinaryEntrypoint = `#!/bin/bash
set -e

cd /bot

BINARY="{{ .Entrypoint }}"

if [ ! -f "$BINARY" ]; then
    echo '{"status":"error","message":"Binary not found: '"$BINARY"'"}'
    exit 1
fi

chmod +x "$BINARY"

# Environment variables BOT_ID and BOT_CONFIG are already set by K8s pod spec
export CODE_MOUNT_DIR="bot"
exec "./$BINARY"
`

const k8sDotnetEntrypoint = `#!/bin/bash
set -e

cd /bot

DLL="{{ .Entrypoint }}"

if [ ! -f "$DLL" ]; then
    echo '{"status":"error","message":"DLL not found: '"$DLL"'"}'
    exit 1
fi

# Environment variables BOT_ID and BOT_CONFIG are already set by K8s pod spec
export CODE_MOUNT_DIR="bot"
exec dotnet "$DLL"
`

const k8sScalaEntrypoint = `#!/bin/bash
set -e

cd /bot

JAR="{{ .Entrypoint }}"

if [ ! -f "$JAR" ]; then
    echo '{"status":"error","message":"JAR not found: '"$JAR"'"}'
    exit 1
fi

# Environment variables BOT_ID and BOT_CONFIG are already set by K8s pod spec
export CODE_MOUNT_DIR="bot"
exec java -jar "$JAR"
`

// templateData holds the data for template execution
type templateData struct {
	EntryPointType string
	Entrypoint     string
	ScriptContent  string
}

// GenerateK8sEntrypoint generates an entrypoint script for K8s pods.
// Unlike Docker, K8s passes BOT_ID and BOT_CONFIG via pod environment variables,
// so we don't need to embed them in the script.
func GenerateK8sEntrypoint(runtime string, opts GeneratorOptions) (string, error) {
	// Validate entrypoint to prevent shell injection
	if err := validateEntrypoint(opts.Entrypoint); err != nil {
		return "", fmt.Errorf("invalid entrypoint: %w", err)
	}

	if opts.EntryPointType == "" {
		opts.EntryPointType = "bot"
	}
	if opts.WorkDir == "" {
		opts.WorkDir = "/bot"
	}

	data := templateData{
		EntryPointType: opts.EntryPointType,
		Entrypoint:     opts.Entrypoint,
	}

	var selectedTemplate string

	switch runtime {
	case "python3.11":
		data.ScriptContent = PythonBotEntrypoint
		selectedTemplate = k8sPythonEntrypoint

	case "nodejs20":
		data.ScriptContent = NodeJsBotEntrypoint
		selectedTemplate = k8sNodeEntrypoint

	case "rust-stable", "gcc13", "cpp-gcc13", "ghc96":
		selectedTemplate = k8sBinaryEntrypoint

	case "dotnet8":
		selectedTemplate = k8sDotnetEntrypoint

	case "scala3":
		selectedTemplate = k8sScalaEntrypoint

	default:
		return "", fmt.Errorf("unsupported runtime: %q (supported: python3.11, nodejs20, rust-stable, gcc13, cpp-gcc13, dotnet8, scala3, ghc96)", runtime)
	}

	tmpl, err := template.New("k8sEntrypoint").Parse(selectedTemplate)
	if err != nil {
		return "", fmt.Errorf("failed to parse entrypoint template: %w", err)
	}

	var result strings.Builder
	if err := tmpl.Execute(&result, data); err != nil {
		return "", fmt.Errorf("failed to execute entrypoint template: %w", err)
	}

	return result.String(), nil
}
