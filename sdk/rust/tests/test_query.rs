//! Tests for the0_sdk::query module

mod common;

use common::EnvGuard;
use the0_sdk::query;
use the0_sdk::state;

#[test]
fn is_query_mode_returns_false_when_not_set() {
    let mut env = EnvGuard::new();
    env.remove("QUERY_PATH");

    assert!(!query::is_query_mode());
}

#[test]
fn is_query_mode_returns_true_when_set() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");

    assert!(query::is_query_mode());
}

#[test]
fn get_params_returns_empty_by_default() {
    let _env = EnvGuard::new();
    let params = query::get_params();
    assert!(params.is_empty());
}

#[test]
fn get_config_returns_empty_object_by_default() {
    let _env = EnvGuard::new();
    let config = query::get_config();
    assert!(config.is_object());
}

#[test]
fn handler_registers_without_calling() {
    let _env = EnvGuard::new();

    // Just verify that handler registration works without panicking
    // We can't easily test the actual execution without starting the server
    query::handler("/test", |_req| {
        serde_json::json!({"test": true})
    });
}

#[test]
fn query_request_get_returns_value() {
    let mut params = std::collections::HashMap::new();
    params.insert("symbol".to_string(), "BTC/USD".to_string());

    let req = query::QueryRequest::new("/test", params);

    assert_eq!(req.get("symbol"), Some("BTC/USD".to_string()));
    assert_eq!(req.get("missing"), None);
}

#[test]
fn query_request_get_or_returns_default() {
    let mut params = std::collections::HashMap::new();
    params.insert("symbol".to_string(), "BTC/USD".to_string());

    let req = query::QueryRequest::new("/test", params);

    assert_eq!(req.get_or("symbol", "default"), "BTC/USD");
    assert_eq!(req.get_or("missing", "default"), "default");
}

#[test]
fn state_set_throws_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state");

    let result = state::set("test_key", &serde_json::json!({"test": true}));

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err, state::StateError::ReadOnly(_)));
}

#[test]
fn state_delete_returns_false_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state");

    // delete returns false in query mode (can't modify state)
    let result = state::delete("test_key");
    assert!(!result);
}

#[test]
fn state_clear_throws_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state");

    let result = state::clear();

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err, state::StateError::ReadOnly(_)));
}

#[test]
fn state_get_allowed_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state-nonexistent");

    // get should work even in query mode (read-only operation)
    // Returns Err for non-existent file, but NOT a ReadOnly error
    let result: Result<String, _> = state::get("nonexistent");
    assert!(result.is_err());
    // Verify it's an IO error (file not found), not a ReadOnly error
    let err = result.unwrap_err();
    assert!(!matches!(err, state::StateError::ReadOnly(_)));
}

#[test]
fn state_get_or_allowed_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state-nonexistent");

    // get_or should work even in query mode (read-only operation)
    let result: String = state::get_or("nonexistent", "default".to_string());
    assert_eq!(result, "default");
}

#[test]
fn state_list_allowed_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state-nonexistent");

    // list should work even in query mode (read-only operation)
    let result = state::list();
    assert!(result.is_empty());
}

#[test]
fn state_exists_allowed_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state-nonexistent");

    // exists should work even in query mode (read-only operation)
    let result = state::exists("nonexistent");
    assert!(!result);
}
