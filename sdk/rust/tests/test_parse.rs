//! Tests for the0_sdk::input::parse() and parse_as_map() functions

mod common;

use common::EnvGuard;
use serde_json::json;
use the0_sdk::input;

#[test]
fn parse_returns_bot_id_and_config() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "test-bot-123");
    env.set("BOT_CONFIG", r#"{"symbol":"BTC/USDT","amount":100}"#);

    let (bot_id, config) = input::parse();

    assert_eq!(bot_id, "test-bot-123");
    assert_eq!(config["symbol"], "BTC/USDT");
    assert_eq!(config["amount"], 100);
}

#[test]
fn parse_handles_nested_config() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "nested-bot");
    env.set(
        "BOT_CONFIG",
        r#"{"exchange":{"name":"binance","testnet":true},"symbols":["BTC","ETH"]}"#,
    );

    let (bot_id, config) = input::parse();

    assert_eq!(bot_id, "nested-bot");
    assert_eq!(config["exchange"]["name"], "binance");
    assert_eq!(config["exchange"]["testnet"], true);
    assert!(config["symbols"].is_array());
    assert_eq!(config["symbols"][0], "BTC");
    assert_eq!(config["symbols"][1], "ETH");
}

#[test]
fn parse_handles_all_json_types() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "types-bot");
    env.set(
        "BOT_CONFIG",
        r#"{
            "string": "hello",
            "integer": 42,
            "float": 3.14159,
            "boolean_true": true,
            "boolean_false": false,
            "null_value": null,
            "array": [1, 2, 3],
            "object": {"nested": "value"}
        }"#,
    );

    let (_, config) = input::parse();

    assert_eq!(config["string"], "hello");
    assert_eq!(config["integer"], 42);
    assert!((config["float"].as_f64().unwrap() - 3.14159).abs() < 0.0001);
    assert_eq!(config["boolean_true"], true);
    assert_eq!(config["boolean_false"], false);
    assert!(config["null_value"].is_null());
    assert!(config["array"].is_array());
    assert_eq!(config["object"]["nested"], "value");
}

#[test]
fn parse_handles_empty_object_config() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "empty-config-bot");
    env.set("BOT_CONFIG", "{}");

    let (bot_id, config) = input::parse();

    assert_eq!(bot_id, "empty-config-bot");
    assert!(config.is_object());
    assert!(config.as_object().unwrap().is_empty());
}

#[test]
fn parse_handles_unicode_in_config() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "unicode-bot");
    env.set("BOT_CONFIG", r#"{"message":"Hello World! 🚀"}"#);

    let (_, config) = input::parse();

    assert_eq!(config["message"], "Hello World! 🚀");
}

#[test]
#[should_panic(expected = "BOT_ID environment variable not set")]
fn parse_panics_when_bot_id_not_set() {
    // Use unlocked guard to avoid poisoning mutex on panic
    let mut env = EnvGuard::new_unlocked();
    env.remove("BOT_ID");
    env.set("BOT_CONFIG", "{}");

    let _ = input::parse();
}

#[test]
#[should_panic(expected = "BOT_CONFIG environment variable not set")]
fn parse_panics_when_bot_config_not_set() {
    // Use unlocked guard to avoid poisoning mutex on panic
    let mut env = EnvGuard::new_unlocked();
    env.set("BOT_ID", "test-bot");
    env.remove("BOT_CONFIG");

    let _ = input::parse();
}

#[test]
#[should_panic(expected = "Failed to parse BOT_CONFIG as JSON")]
fn parse_panics_on_invalid_json() {
    // Use unlocked guard to avoid poisoning mutex on panic
    let mut env = EnvGuard::new_unlocked();
    env.set("BOT_ID", "test-bot");
    env.set("BOT_CONFIG", "not valid json {{{");

    let _ = input::parse();
}

// Tests for parse_as_map

#[test]
fn parse_as_map_returns_hashmap() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "map-bot");
    env.set("BOT_CONFIG", r#"{"key1":"value1","key2":42}"#);

    let (bot_id, config) = input::parse_as_map();

    assert_eq!(bot_id, "map-bot");
    assert_eq!(config.get("key1").unwrap(), &json!("value1"));
    assert_eq!(config.get("key2").unwrap(), &json!(42));
}

#[test]
fn parse_as_map_returns_empty_hashmap_for_empty_object() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "empty-map-bot");
    env.set("BOT_CONFIG", "{}");

    let (_, config) = input::parse_as_map();

    assert!(config.is_empty());
}

#[test]
fn parse_as_map_handles_complex_values() {
    let mut env = EnvGuard::new();
    env.set("BOT_ID", "complex-bot");
    env.set(
        "BOT_CONFIG",
        r#"{"settings":{"enabled":true},"items":[1,2,3]}"#,
    );

    let (_, config) = input::parse_as_map();

    let settings = config.get("settings").unwrap();
    assert_eq!(settings["enabled"], true);

    let items = config.get("items").unwrap();
    assert!(items.is_array());
    assert_eq!(items.as_array().unwrap().len(), 3);
}
