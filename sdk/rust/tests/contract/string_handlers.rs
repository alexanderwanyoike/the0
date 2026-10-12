//! A bot written the way Rust bots were before params had JSON types: its
//! handler is registered with `query::handler()` and reads params with
//! `req.get()`. It runs through `query::run()` in both query modes, to show
//! such bots keep working.

use crate::harness::{self, Sandbox};
use libtest_mimic::{Failed, Trial};
use serde_json::{json, Value};
use std::ffi::OsString;
use the0_sdk::query;

pub const STRING_BOT_ARG: &str = "string-bot";

pub fn main(query_result_path: &str) {
    query::set_result_path(query_result_path);
    query::handler("/echo", |req| {
        json!({
            "symbol": req.get("symbol"),
            "limit": req.get_or("limit", "10"),
            "filter": req.get("filter"),
            "cursor": req.get("cursor"),
            "offset": req.get_or("offset", "0"),
        })
    });
    query::run();
}

pub fn trials() -> Vec<Trial> {
    vec![
        Trial::test("handler_reads_text_params_in_an_ephemeral_query", || {
            check(ephemeral_case())
        }),
        Trial::test("handler_reads_text_params_in_the_query_server", || {
            check(server_case())
        }),
    ]
}

fn params() -> Value {
    json!({"symbol": "BTC/USD", "limit": 5, "filter": {"side": "buy"}, "cursor": null})
}

/// What the handler answers for `params()`: every value as text, a null as
/// absent.
fn text_params() -> Value {
    json!({
        "symbol": "BTC/USD",
        "limit": "5",
        "filter": "{\"side\":\"buy\"}",
        "cursor": null,
        "offset": "0"
    })
}

fn env() -> Value {
    json!({
        "BOT_ID": "string-bot",
        "BOT_CONFIG": "{}",
        "CODE_MOUNT_DIR": "{{code_mount_dir}}",
        "STATE_DIR": "{{state_dir}}"
    })
}

fn ephemeral_case() -> Value {
    let mut env = env();
    env["QUERY_PATH"] = json!("/echo");
    env["QUERY_PARAMS"] = json!(params().to_string());
    json!({
        "env": env,
        "expect": {
            "exit_code": 0,
            "query_result": {"status": "ok", "data": text_params()}
        }
    })
}

fn server_case() -> Value {
    let mut env = env();
    env["BOT_TYPE"] = json!("realtime");
    env["THE0_QUERY_PORT"] = json!("{{port}}");
    json!({
        "env": env,
        "requests": [
            {
                // A runtime query: the params as the body, and copied into
                // the query string as text, with the null left out.
                "method": "POST",
                "target": "/echo?filter=%7B%22side%22%3A%22buy%22%7D&limit=5&symbol=BTC%2FUSD",
                "body": params(),
                "expect": {"status": 200, "body": {"status": "ok", "data": text_params()}}
            },
            {
                "method": "GET",
                "target": "/echo?symbol=BTC%2FUSD&limit=5",
                "expect": {
                    "status": 200,
                    "body": {
                        "status": "ok",
                        "data": {
                            "symbol": "BTC/USD",
                            "limit": "5",
                            "filter": null,
                            "cursor": null,
                            "offset": "0"
                        }
                    }
                }
            }
        ]
    })
}

fn check(case: Value) -> Result<(), Failed> {
    let (problems, output) = harness::run_case(&case, bot_args);
    if problems.is_empty() {
        Ok(())
    } else {
        Err(format!("{}\n{}", problems.join("\n"), output).into())
    }
}

fn bot_args(sandbox: &Sandbox) -> Vec<OsString> {
    vec![STRING_BOT_ARG.into(), sandbox.query_result().into()]
}
