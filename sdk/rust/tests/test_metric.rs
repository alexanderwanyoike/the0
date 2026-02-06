//! Tests for the0_sdk::input::metric() and log() functions
//!
//! These functions write to stdout. Since capturing stdout in Rust tests
//! is complex, we primarily test that they don't panic and produce valid output.
//! Full integration testing is done via the example bots.

mod common;

use common::EnvGuard;
use serde_json::json;
use the0_sdk::input;

#[test]
fn metric_does_not_panic_with_valid_data() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "metric-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::metric("price", &json!({"symbol": "BTC/USD", "value": 45000.0}));
}

#[test]
fn metric_does_not_panic_with_empty_data() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "metric-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::metric("heartbeat", &json!({}));
}

#[test]
fn metric_does_not_panic_with_nested_data() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "metric-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::metric("trade", &json!({
        "order": {
            "id": "12345",
            "side": "buy",
            "quantity": 1.5
        }
    }));
}

#[test]
fn metric_does_not_panic_with_array_data() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "metric-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::metric("prices", &json!({"values": [100.0, 101.5, 99.8]}));
}

#[test]
fn log_does_not_panic_with_message() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic - simple log with defaults
    input::log("Starting bot execution", None, None);
}

#[test]
fn log_does_not_panic_with_level() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic - log with level
    input::log("Warning message", None, Some(input::LogLevel::Warn));
    input::log("Error message", None, Some(input::LogLevel::Error));
}

#[test]
fn log_does_not_panic_with_data() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic - log with structured data
    input::log("Order placed", Some(&json!({"order_id": "123", "symbol": "BTC"})), None);
}

#[test]
fn log_does_not_panic_with_data_and_level() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic - log with data and level
    input::log("Order failed", Some(&json!({"order_id": "123"})), Some(input::LogLevel::Error));
}

#[test]
fn log_does_not_panic_with_empty_message() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::log("", None, None);
}

#[test]
fn log_does_not_panic_with_special_characters() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic - quotes and backslashes get escaped
    input::log(r#"Error: "file not found" at C:\path"#, None, None);
}

#[test]
fn log_does_not_panic_with_newlines() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::log("Line 1\nLine 2\nLine 3", None, None);
}

#[test]
fn log_does_not_panic_with_long_message() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    let long_message = "x".repeat(10000);
    // Should not panic
    input::log(&long_message, None, None);
}

#[test]
fn log_convenience_functions_do_not_panic() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "log-test");
    env.set("BOT_CONFIG", "{}");

    // Test convenience functions
    input::log_info("Info message", None);
    input::log_warn("Warning message", None);
    input::log_error("Error message", None);
    input::log_info("With data", Some(&json!({"key": "value"})));
}

#[test]
fn metric_handles_various_numeric_types() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "numeric-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic with various numeric types
    input::metric("numbers", &json!({
        "integer": 42,
        "negative": -100,
        "float": 3.14159,
        "large": 9999999999_i64,
        "small": 0.0000001
    }));
}

#[test]
fn metric_handles_boolean_and_null() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "types-test");
    env.set("BOT_CONFIG", "{}");

    // Should not panic
    input::metric("types", &json!({
        "active": true,
        "disabled": false,
        "value": null
    }));
}
