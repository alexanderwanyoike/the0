package entrypoints

// Dotnet8BotEntrypoint is the bash script that handles .NET 8 bot execution.
// It finds and runs the pre-built DLL from bin/Release/net8.0/publish/.
const Dotnet8BotEntrypoint = `#!/bin/bash
set -e

echo "STARTUP: .NET 8 bot wrapper starting" >&2
echo "STARTUP: Working directory: $(pwd)" >&2

# Handle signals for graceful shutdown
cleanup() {
    echo "SIGNAL: Received shutdown signal, cleaning up..." >&2
    if [ -n "$BOT_PID" ]; then
        kill -TERM "$BOT_PID" 2>/dev/null || true
        wait "$BOT_PID" 2>/dev/null || true
    fi
    exit 0
}
trap cleanup SIGTERM SIGINT

# Get entrypoint type from environment (defaults to 'bot')
ENTRYPOINT_TYPE="${ENTRYPOINT_TYPE:-bot}"
WORK_DIR="/$ENTRYPOINT_TYPE"

# Change to work directory
cd "$WORK_DIR"
echo "CHDIR_SUCCESS: Changed to $WORK_DIR" >&2

# Find the pre-built DLL in bin/Release/net8.0/publish/
# Skip .deps.json and .runtimeconfig.json files
DLL=$(find bin/Release/net8.0/publish -maxdepth 1 -name "*.dll" ! -name "*.deps.json" ! -name "*.runtimeconfig.json" 2>/dev/null | head -1)

if [ -z "$DLL" ]; then
    echo '{"status":"error","message":"No pre-built DLL found in bin/Release/net8.0/publish/"}'
    exit 1
fi

echo "DLL_FOUND: $DLL" >&2

# Set environment variables for the .NET application
export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
export SCRIPT_PATH="{{ .ScriptPath }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"

# Execute the .NET application
echo "EXECUTE: Running dotnet $DLL" >&2
exec dotnet "$DLL"
`
