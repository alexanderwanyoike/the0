//! Tests for the0_sdk::input::success() and result() functions
//!
//! These functions write to a result file, not stdout.

mod common;

use common::ResultContext;
use serde_json::json;
use the0_sdk::input;

#[test]
fn success_writes_json_to_result_file() {
    let ctx = ResultContext::new();
    input::success("Test message");

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["status"], "success");
    assert_eq!(parsed["message"], "Test message");
}

#[test]
fn success_escapes_quotes() {
    let ctx = ResultContext::new();
    input::success(r#"Message with "quotes""#);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["message"], r#"Message with "quotes""#);
}

#[test]
fn success_escapes_backslashes() {
    let ctx = ResultContext::new();
    input::success(r#"Path: C:\Users\test"#);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["message"], r#"Path: C:\Users\test"#);
}

#[test]
fn success_handles_empty_message() {
    let ctx = ResultContext::new();
    input::success("");

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["status"], "success");
    assert_eq!(parsed["message"], "");
}

#[test]
fn success_handles_long_message() {
    let ctx = ResultContext::new();
    let long_message = "x".repeat(10000);
    input::success(&long_message);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["message"].as_str().unwrap().len(), 10000);
}

#[test]
fn result_writes_custom_json() {
    let ctx = ResultContext::new();
    let data = json!({
        "status": "success",
        "trade_id": "abc123",
        "amount": 100.5
    });
    input::result(&data);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["status"], "success");
    assert_eq!(parsed["trade_id"], "abc123");
    assert_eq!(parsed["amount"], 100.5);
}

#[test]
fn result_writes_nested_objects() {
    let ctx = ResultContext::new();
    let data = json!({
        "status": "success",
        "data": {
            "price": 45000.0,
            "volume": 1.5
        }
    });
    input::result(&data);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["data"]["price"], 45000.0);
    assert_eq!(parsed["data"]["volume"], 1.5);
}

#[test]
fn result_writes_arrays() {
    let ctx = ResultContext::new();
    let data = json!({
        "trades": ["trade1", "trade2", "trade3"]
    });
    input::result(&data);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert!(parsed["trades"].is_array());
    assert_eq!(parsed["trades"].as_array().unwrap().len(), 3);
}

#[test]
fn result_writes_empty_object() {
    let ctx = ResultContext::new();
    input::result(&json!({}));

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert!(parsed.is_object());
    assert!(parsed.as_object().unwrap().is_empty());
}

#[test]
fn result_overwrites_previous_result() {
    let ctx = ResultContext::new();

    input::success("First message");
    let content1 = ctx.read_result().expect("Result file should exist");
    let parsed1: serde_json::Value = serde_json::from_str(&content1).unwrap();
    assert_eq!(parsed1["message"], "First message");

    input::success("Second message");
    let content2 = ctx.read_result().expect("Result file should exist");
    let parsed2: serde_json::Value = serde_json::from_str(&content2).unwrap();
    assert_eq!(parsed2["message"], "Second message");
}

#[test]
fn success_creates_valid_json() {
    let ctx = ResultContext::new();
    input::success("Test");

    let content = ctx.read_result().expect("Result file should exist");

    // Should not panic
    let _: serde_json::Value = serde_json::from_str(&content)
        .expect("Result should be valid JSON");
}

#[test]
fn result_handles_special_characters() {
    let ctx = ResultContext::new();
    let data = json!({
        "message": "Line1\nLine2\tTabbed",
        "path": "C:\\Users\\test"
    });
    input::result(&data);

    let content = ctx.read_result().expect("Result file should exist");
    let parsed: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert_eq!(parsed["message"], "Line1\nLine2\tTabbed");
    assert_eq!(parsed["path"], "C:\\Users\\test");
}
