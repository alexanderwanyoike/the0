package entrypoints

// RustHelperLibrary is the helper code that gets injected into the user's Rust project.
// It provides convenient input parsing and output functions.
const RustHelperLibrary = `//! the0 helper module for bot development
//! This module is automatically injected at build time.

pub mod input {
    use serde_json::Value;
    use std::env;
    use std::collections::HashMap;

    /// Parse bot configuration from environment variables.
    /// Returns a tuple of (bot_id, config).
    pub fn parse() -> (String, Value) {
        let id = env::var("BOT_ID").expect("BOT_ID environment variable not set");
        let config_str = env::var("BOT_CONFIG").expect("BOT_CONFIG environment variable not set");
        let config: Value = serde_json::from_str(&config_str).expect("Failed to parse BOT_CONFIG as JSON");
        (id, config)
    }

    /// Parse bot configuration with typed config.
    /// Returns a tuple of (bot_id, config) where config is a HashMap.
    pub fn parse_as_map() -> (String, HashMap<String, Value>) {
        let id = env::var("BOT_ID").expect("BOT_ID environment variable not set");
        let config_str = env::var("BOT_CONFIG").expect("BOT_CONFIG environment variable not set");
        let config: HashMap<String, Value> = serde_json::from_str(&config_str).expect("Failed to parse BOT_CONFIG as JSON");
        (id, config)
    }

    /// Output a success result and exit.
    pub fn success(message: &str) {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        println!(r#"{{"status":"success","message":"{}"}}"#, escaped);
    }

    /// Output an error result and exit with code 1.
    pub fn error(message: &str) {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        println!(r#"{{"status":"error","message":"{}"}}"#, escaped);
        std::process::exit(1);
    }

    /// Output a custom result as JSON.
    pub fn result(data: &Value) {
        println!("{}", serde_json::to_string(data).expect("Failed to serialize result"));
    }
}
`

// RustBacktestEntrypoint is the bash script that handles Rust backtest execution.
// It injects the helper library, builds if needed, and runs the binary.
// The wrapper captures output and formats it based on exit code, making the SDK optional.
const RustBacktestEntrypoint = `#!/bin/bash

echo "STARTUP: Rust backtest wrapper starting" >&2
echo "STARTUP: Working directory: $(pwd)" >&2

# Change to backtest directory
cd /backtest
echo "CHDIR_SUCCESS: Changed to /backtest" >&2

# Get project name from Cargo.toml
PROJECT_NAME=$(grep -m1 '^name' Cargo.toml | sed 's/.*"\([^"]*\)".*/\1/' | tr '-' '_')
if [ -z "$PROJECT_NAME" ]; then
    PROJECT_NAME="bot"
fi
echo "PROJECT_NAME: $PROJECT_NAME" >&2

# Create src directory if it doesn't exist
mkdir -p src

# Inject the helper library
echo "INJECT: Writing the0.rs helper library" >&2
cat > src/the0.rs << 'THE0_HELPER_EOF'
` + RustHelperLibrary + `THE0_HELPER_EOF

# Check if binary exists and is newer than source files
BINARY_PATH="target/release/$PROJECT_NAME"
NEEDS_BUILD=false

if [ ! -f "$BINARY_PATH" ]; then
    echo "BUILD_CHECK: Binary not found, building..." >&2
    NEEDS_BUILD=true
else
    # Check if any source files are newer than the binary
    if [ -n "$(find src -name '*.rs' -newer "$BINARY_PATH" 2>/dev/null)" ] || \
       [ -n "$(find . -maxdepth 1 -name 'Cargo.toml' -newer "$BINARY_PATH" 2>/dev/null)" ] || \
       [ -n "$(find . -maxdepth 1 -name 'Cargo.lock' -newer "$BINARY_PATH" 2>/dev/null)" ]; then
        echo "BUILD_CHECK: Source files changed, rebuilding..." >&2
        NEEDS_BUILD=true
    else
        echo "BUILD_CHECK: Binary is up to date" >&2
    fi
fi

# Build if needed
if [ "$NEEDS_BUILD" = true ]; then
    echo "BUILD: Running cargo build --release" >&2
    if ! cargo build --release 2>&1 | while read line; do echo "BUILD: $line" >&2; done; then
        echo '{"status":"error","message":"Cargo build failed"}'
        exit 1
    fi
    echo "BUILD: Build completed successfully" >&2
fi

# Set environment variables for the binary
export BACKTEST_ID="{{ .BotId }}"
export BACKTEST_CONFIG='{{ .BotConfig }}'
export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'

# Execute the binary and capture output
echo "EXECUTE: Running $BINARY_PATH" >&2
OUTPUT=$("./$BINARY_PATH")
EXIT_CODE=$?

# Check if output has JSON with status field - use it regardless of exit code
if echo "$OUTPUT" | grep -q '"status"'; then
    # Pass through as-is (developer provided custom status)
    echo "$OUTPUT"
elif [ $EXIT_CODE -eq 0 ]; then
    # No structured output, success exit
    echo '{"status":"success","message":"Bot executed successfully"}'
else
    # No structured output, error exit
    echo '{"status":"error","message":"Bot execution failed"}'
fi
`

// RustBotEntrypoint is the bash script that handles Rust bot execution.
// It injects the helper library, builds if needed, and runs the binary.
// The wrapper captures output and formats it based on exit code, making the SDK optional.
const RustBotEntrypoint = `#!/bin/bash

echo "STARTUP: Rust bot wrapper starting" >&2
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

# Get project name from Cargo.toml
PROJECT_NAME=$(grep -m1 '^name' Cargo.toml | sed 's/.*"\([^"]*\)".*/\1/' | tr '-' '_')
if [ -z "$PROJECT_NAME" ]; then
    PROJECT_NAME="bot"
fi
echo "PROJECT_NAME: $PROJECT_NAME" >&2

# Create src directory if it doesn't exist
mkdir -p src

# Inject the helper library
echo "INJECT: Writing the0.rs helper library" >&2
cat > src/the0.rs << 'THE0_HELPER_EOF'
` + RustHelperLibrary + `THE0_HELPER_EOF

# Check if binary exists and is newer than source files
BINARY_PATH="target/release/$PROJECT_NAME"
NEEDS_BUILD=false

if [ ! -f "$BINARY_PATH" ]; then
    echo "BUILD_CHECK: Binary not found, building..." >&2
    NEEDS_BUILD=true
else
    # Check if any source files are newer than the binary
    if [ -n "$(find src -name '*.rs' -newer "$BINARY_PATH" 2>/dev/null)" ] || \
       [ -n "$(find . -maxdepth 1 -name 'Cargo.toml' -newer "$BINARY_PATH" 2>/dev/null)" ] || \
       [ -n "$(find . -maxdepth 1 -name 'Cargo.lock' -newer "$BINARY_PATH" 2>/dev/null)" ]; then
        echo "BUILD_CHECK: Source files changed, rebuilding..." >&2
        NEEDS_BUILD=true
    else
        echo "BUILD_CHECK: Binary is up to date" >&2
    fi
fi

# Build if needed
if [ "$NEEDS_BUILD" = true ]; then
    echo "BUILD: Running cargo build --release" >&2
    if ! cargo build --release 2>&1 | while read line; do echo "BUILD: $line" >&2; done; then
        echo '{"status":"error","message":"Cargo build failed"}'
        exit 1
    fi
    echo "BUILD: Build completed successfully" >&2
fi

# Set environment variables for the binary
export BOT_ID="{{ .BotId }}"
export BOT_CONFIG='{{ .BotConfig }}'
export SCRIPT_PATH="{{ .ScriptPath }}"
export ENTRYPOINT_TYPE="{{ .EntryPointType }}"

# Execute the binary and capture output
echo "EXECUTE: Running $BINARY_PATH" >&2
OUTPUT=$("./$BINARY_PATH")
EXIT_CODE=$?

# Check if output has JSON with status field - use it regardless of exit code
if echo "$OUTPUT" | grep -q '"status"'; then
    # Pass through as-is (developer provided custom status)
    echo "$OUTPUT"
elif [ $EXIT_CODE -eq 0 ]; then
    # No structured output, success exit
    echo '{"status":"success","message":"Bot executed successfully"}'
else
    # No structured output, error exit
    echo '{"status":"error","message":"Bot execution failed"}'
fi
`
