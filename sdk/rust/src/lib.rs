//! the0 SDK for Rust trading bots
//!
//! This crate provides utilities for building trading bots on the0 platform.
//!
//! # Example
//!
//! ```rust,no_run
//! use the0_sdk::input;
//!
//! fn main() {
//!     let (bot_id, config) = input::parse().expect("Failed to parse bot config");
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
use std::fmt;

/// Errors that can occur when parsing bot configuration
#[derive(Debug)]
pub enum ParseError {
    /// BOT_ID environment variable is not set
    MissingBotId,
    /// BOT_CONFIG environment variable is not set
    MissingBotConfig,
    /// BOT_CONFIG contains invalid JSON
    InvalidJson(serde_json::Error),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::MissingBotId => write!(f, "BOT_ID environment variable not set"),
            ParseError::MissingBotConfig => write!(f, "BOT_CONFIG environment variable not set"),
            ParseError::InvalidJson(e) => write!(f, "Failed to parse BOT_CONFIG as JSON: {}", e),
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::InvalidJson(e) => Some(e),
            _ => None,
        }
    }
}

/// Input parsing and output formatting utilities
pub mod input {
    use super::*;

    // Re-export ParseError for convenience
    pub use crate::ParseError;

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
    /// # Errors
    /// Returns an error if BOT_ID or BOT_CONFIG environment variables are not set,
    /// or if BOT_CONFIG is not valid JSON.
    pub fn parse() -> Result<(String, Value), ParseError> {
        let id = env::var("BOT_ID").map_err(|_| ParseError::MissingBotId)?;
        let config_str = env::var("BOT_CONFIG").map_err(|_| ParseError::MissingBotConfig)?;
        let config: Value = serde_json::from_str(&config_str).map_err(ParseError::InvalidJson)?;
        Ok((id, config))
    }

    /// Parse bot configuration with typed config as HashMap.
    ///
    /// Returns a tuple of (bot_id, config) where config is a HashMap<String, Value>.
    ///
    /// # Errors
    /// Returns an error if BOT_ID or BOT_CONFIG environment variables are not set,
    /// or if BOT_CONFIG is not valid JSON.
    pub fn parse_as_map() -> Result<(String, HashMap<String, Value>), ParseError> {
        let id = env::var("BOT_ID").map_err(|_| ParseError::MissingBotId)?;
        let config_str = env::var("BOT_CONFIG").map_err(|_| ParseError::MissingBotConfig)?;
        let config: HashMap<String, Value> =
            serde_json::from_str(&config_str).map_err(ParseError::InvalidJson)?;
        Ok((id, config))
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

    /// Emit a metric to stdout.
    ///
    /// Outputs a JSON object with `_metric` field and timestamp.
    pub fn metric(metric_type: &str, data: &Value) {
        let timestamp = current_timestamp();
        let mut output = data.as_object().cloned().unwrap_or_default();
        output.insert("_metric".to_string(), Value::String(metric_type.to_string()));
        output.insert("timestamp".to_string(), Value::Number(serde_json::Number::from(timestamp)));
        println!("{}", serde_json::to_string(&output).expect("Failed to serialize metric"));
    }

    /// Log levels supported by the platform
    #[derive(Debug, Clone, Copy)]
    pub enum LogLevel {
        Info,
        Warn,
        Error,
    }

    impl LogLevel {
        fn as_str(&self) -> &'static str {
            match self {
                LogLevel::Info => "info",
                LogLevel::Warn => "warn",
                LogLevel::Error => "error",
            }
        }
    }

    /// Log a structured message to stderr.
    ///
    /// Outputs a JSON object with `level`, `message`, `timestamp`, and any additional fields.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use the0_sdk::input::{log, LogLevel};
    /// use serde_json::json;
    ///
    /// // Simple log (defaults to info level)
    /// log("Starting trade execution", None, None);
    ///
    /// // Log with level
    /// log("Connection lost", None, Some(LogLevel::Warn));
    ///
    /// // Log with structured data
    /// log("Order placed", Some(&json!({"order_id": "12345", "symbol": "BTC/USD"})), None);
    ///
    /// // Log with data and level
    /// log("Order failed", Some(&json!({"order_id": "12345"})), Some(LogLevel::Error));
    /// ```
    pub fn log(message: &str, data: Option<&Value>, level: Option<LogLevel>) {
        let log_level = level.unwrap_or(LogLevel::Info);
        let timestamp = current_timestamp();

        let mut output = match data {
            Some(v) => v.as_object().cloned().unwrap_or_default(),
            None => serde_json::Map::new(),
        };

        output.insert("level".to_string(), Value::String(log_level.as_str().to_string()));
        output.insert("message".to_string(), Value::String(message.to_string()));
        output.insert("timestamp".to_string(), Value::Number(serde_json::Number::from(timestamp)));

        eprintln!("{}", serde_json::to_string(&output).expect("Failed to serialize log"));
    }

    /// Convenience function: log an info message
    pub fn log_info(message: &str, data: Option<&Value>) {
        log(message, data, Some(LogLevel::Info));
    }

    /// Convenience function: log a warning message
    pub fn log_warn(message: &str, data: Option<&Value>) {
        log(message, data, Some(LogLevel::Warn));
    }

    /// Convenience function: log an error message
    pub fn log_error(message: &str, data: Option<&Value>) {
        log(message, data, Some(LogLevel::Error));
    }

    /// Get current timestamp as milliseconds since Unix epoch.
    fn current_timestamp() -> u64 {
        use std::time::{SystemTime, UNIX_EPOCH};
        let duration = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap();
        duration.as_secs() * 1000 + duration.subsec_millis() as u64
    }
}
