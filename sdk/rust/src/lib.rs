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
//!     println!("Bot {} starting with config: {:?}", bot_id, config);
//!
//!     // Your trading logic here
//!
//!     input::success("Bot executed successfully");
//! }
//! ```

use serde_json::Value;
use std::collections::HashMap;
use std::env;
use std::fs;

/// Input parsing and output formatting utilities
pub mod input {
    use super::*;

    /// Get the path to the result file
    fn result_file_path() -> String {
        let mount_dir = env::var("CODE_MOUNT_DIR").unwrap_or_else(|_| "bot".to_string());
        format!("/{}/result.json", mount_dir)
    }

    /// Write result to the result file
    fn write_result(content: &str) {
        let path = result_file_path();
        if let Err(e) = fs::write(&path, content) {
            eprintln!("RESULT_ERROR: Failed to write result file: {}", e);
        }
    }

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

    /// Output a success result to the result file.
    ///
    /// Writes a JSON object with status "success" and the provided message.
    pub fn success(message: &str) {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        write_result(&format!(r#"{{"status":"success","message":"{}"}}"#, escaped));
    }

    /// Output an error result to the result file and exit with code 1.
    ///
    /// Writes a JSON object with status "error" and the provided message,
    /// then terminates the process with exit code 1.
    pub fn error(message: &str) -> ! {
        let escaped = message.replace('\\', "\\\\").replace('"', "\\\"");
        write_result(&format!(r#"{{"status":"error","message":"{}"}}"#, escaped));
        std::process::exit(1);
    }

    /// Output a custom JSON result to the result file.
    ///
    /// Serializes the provided Value as JSON and writes it.
    pub fn result(data: &Value) {
        write_result(&serde_json::to_string(data).expect("Failed to serialize result"));
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
