//! Tests for the0_sdk::query module

mod common;

use common::EnvGuard;
use serde_json::{json, Value};
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
    query::handler("/test", |_req| serde_json::json!({"test": true}));
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

fn typed_request() -> query::QueryRequest {
    let values = [
        ("symbol", json!("BTC/USD")),
        ("limit", json!(5)),
        ("live", json!(true)),
        ("symbols", json!(["AAPL", "MSFT"])),
        ("filter", json!({"side": "buy"})),
        ("cursor", Value::Null),
    ]
    .into_iter()
    .map(|(key, value)| (key.to_string(), value))
    .collect();
    query::QueryRequest::from_values("/test", values)
}

#[test]
fn query_request_get_returns_a_non_string_param_as_its_json_text() {
    let req = typed_request();

    assert_eq!(req.get("symbol"), Some("BTC/USD".to_string()));
    assert_eq!(req.get("limit"), Some("5".to_string()));
    assert_eq!(req.get("live"), Some("true".to_string()));
    assert_eq!(req.get("symbols"), Some(r#"["AAPL","MSFT"]"#.to_string()));
    assert_eq!(req.get("filter"), Some(r#"{"side":"buy"}"#.to_string()));
    assert_eq!(req.get_or("limit", "10"), "5");
}

#[test]
fn query_request_get_treats_a_null_param_as_absent() {
    let req = typed_request();

    assert_eq!(req.get("cursor"), None);
    assert_eq!(req.get_or("cursor", "start"), "start");
    assert!(!req.params.contains_key("cursor"));
}

#[test]
fn query_request_values_keep_their_json_types() {
    let req = typed_request();

    assert_eq!(req.values()["limit"], json!(5));
    assert_eq!(req.values()["filter"], json!({"side": "buy"}));
    assert_eq!(req.values()["cursor"], Value::Null);
}

#[test]
fn query_request_get_as_deserializes_a_param() {
    let req = typed_request();

    assert_eq!(req.get_as::<u32>("limit"), Some(5));
    assert_eq!(req.get_as::<bool>("live"), Some(true));
    assert_eq!(
        req.get_as::<Vec<String>>("symbols"),
        Some(vec!["AAPL".to_string(), "MSFT".to_string()])
    );
    assert_eq!(req.get_as::<Option<String>>("cursor"), Some(None));
}

#[test]
fn query_request_get_as_string_gives_a_non_string_param_as_its_json_text() {
    let req = typed_request();

    assert_eq!(req.get_as::<String>("limit"), Some("5".to_string()));
    assert_eq!(req.get_as::<String>("live"), Some("true".to_string()));
    assert_eq!(
        req.get_as::<String>("filter"),
        Some(r#"{"side":"buy"}"#.to_string())
    );
    assert_eq!(req.get_as::<String>("cursor"), None);
}

#[test]
fn query_request_get_as_is_none_for_a_missing_or_mistyped_param() {
    let req = typed_request();

    assert_eq!(req.get_as::<u32>("offset"), None);
    assert_eq!(req.get_as::<u32>("symbol"), None);
}

#[test]
fn query_request_new_gives_string_values() {
    let mut params = std::collections::HashMap::new();
    params.insert("limit".to_string(), "5".to_string());

    let req = query::QueryRequest::new("/test", params);

    assert_eq!(req.values()["limit"], json!("5"));
}

#[test]
fn query_request_get_as_parses_a_string_param() {
    let params = [
        ("limit", "5"),
        ("live", "true"),
        ("symbols", r#"["AAPL","MSFT"]"#),
        ("symbol", "BTC/USD"),
    ]
    .into_iter()
    .map(|(key, value)| (key.to_string(), value.to_string()))
    .collect();
    let req = query::QueryRequest::new("/test", params);

    assert_eq!(req.get_as::<usize>("limit"), Some(5));
    assert_eq!(req.get_as::<bool>("live"), Some(true));
    assert_eq!(
        req.get_as::<Vec<String>>("symbols"),
        Some(vec!["AAPL".to_string(), "MSFT".to_string()])
    );
    assert_eq!(req.get_as::<String>("limit"), Some("5".to_string()));
    assert_eq!(req.get_as::<u32>("symbol"), None);
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
fn state_write_fails_with_the_query_modules_read_only_error() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state");

    let err: Box<dyn std::error::Error> = Box::new(state::set("test_key", &1).unwrap_err());

    let read_only = std::iter::successors(Some(&*err), |&e| e.source())
        .find_map(|e| e.downcast_ref::<query::ReadOnlyStateError>());
    assert!(
        read_only.is_some(),
        "{err:?} carries no query::ReadOnlyStateError"
    );
}

#[test]
fn state_delete_fails_in_query_mode() {
    let mut env = EnvGuard::new();
    env.set("QUERY_PATH", "/portfolio");
    env.set("STATE_DIR", "/tmp/the0-test-state");

    let result = state::delete("test_key");

    assert!(
        matches!(result, Err(state::StateError::ReadOnly(_))),
        "{result:?}"
    );
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
