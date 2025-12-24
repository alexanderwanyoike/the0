package entrypoints

// RustHelperLibrary is the helper code that gets injected into the user's Rust project.
// It provides convenient input parsing and output functions.
// NOTE: This may be used by the CLI at build time for injection.
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

// RustBotEntrypoint is an empty placeholder for Rust bots.
// Rust bots are pre-built by the CLI, so no code wrapper is needed.
// The bash entrypoint in bash_entrypoint_factory.go handles execution.
const RustBotEntrypoint = ""
