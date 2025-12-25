//! the0 SDK for Rust trading bots
//!
//! This crate provides utilities for building trading bots on the0 platform.
//!
//! # Example
//!
//! ```rust,no_run
//! use the0::input;
//!
//! fn main() {
//!     let (bot_id, config) = input::parse();
//!     eprintln!("Bot {} starting with config: {:?}", bot_id, config);
//!
//!     // Your trading logic here
//!     // Use eprintln! for debug output, println! works too but goes to logs
//!
//!     input::success("Bot executed successfully");
//! }
//! ```

use serde_json::Value;
use std::collections::HashMap;
use std::env;

/// Result marker for output protocol - runtime parses this to extract results.
/// All output functions use this prefix so users can freely use println! for logging.
const RESULT_MARKER: &str = "THE0_RESULT:";

/// Input parsing and output formatting utilities
pub mod input {
    use super::*;

    /// Parse bot configuration from environment variables.
    ///
    /// Returns a tuple of (bot_id, config) where config is a serde_json::Value.
    ///
    /// # Panics
    /// Panics if BOT_ID or BOT_CONFIG environment variables are not set,
    /// or if BOT_CONFIG is not valid JSON.
    pub fn parse() -> (String, Value) {
        let id = env::var("BOT_ID").expect("BOT_ID environment variable not set");
        let config_str = env::var("BOT_CONFIG").expect("BOT_CONFIG environment variable not set");
        let config: Value =
            serde_json::from_str(&config_str).expect("Failed to parse BOT_CONFIG as JSON");
        (id, config)
    }

    /// Parse bot configuration with typed config as HashMap.
    ///
    /// Returns a tuple of (bot_id, config) where config is a HashMap<String, Value>.
    ///
    /// # Panics
    /// Panics if BOT_ID or BOT_CONFIG environment variables are not set,
    /// or if BOT_CONFIG is not valid JSON.
    pub fn parse_as_map() -> (String, HashMap<String, Value>) {
        let id = env::var("BOT_ID").expect("BOT_ID environment variable not set");
        let config_str = env::var("BOT_CONFIG").expect("BOT_CONFIG environment variable not set");
        let config: HashMap<String, Value> =
            serde_json::from_str(&config_str).expect("Failed to parse BOT_CONFIG as JSON");
        (id, config)
    }

    /// Output a success result to stdout.
    ///
    /// Prints a JSON object with status "success" and the provided message.
    /// Uses the THE0_RESULT: marker so you can freely use println! for logging.
    pub fn success(message: &str) {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        println!(
            r#"{}{{"status":"success","message":"{}"}}"#,
            RESULT_MARKER, escaped
        );
    }

    /// Output an error result to stdout and exit with code 1.
    ///
    /// Prints a JSON object with status "error" and the provided message,
    /// then terminates the process with exit code 1.
    /// Uses the THE0_RESULT: marker so you can freely use println! for logging.
    pub fn error(message: &str) -> ! {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        println!(
            r#"{}{{"status":"error","message":"{}"}}"#,
            RESULT_MARKER, escaped
        );
        std::process::exit(1);
    }

    /// Output a custom JSON result to stdout.
    ///
    /// Serializes the provided Value as JSON and prints it.
    /// Uses the THE0_RESULT: marker so you can freely use println! for logging.
    pub fn result(data: &Value) {
        println!(
            "{}{}",
            RESULT_MARKER,
            serde_json::to_string(data).expect("Failed to serialize result")
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_success_output() {
        // This test just verifies the function doesn't panic
        // Actual output testing would require capturing stdout
        input::success("test message");
    }

    #[test]
    fn test_result_output() {
        let data = serde_json::json!({"key": "value"});
        input::result(&data);
    }
}
