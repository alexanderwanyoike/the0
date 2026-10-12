//! The contract bot: plays one sdk/contract case through the SDK's public API,
//! the way a bot would. Each op makes the SDK call sdk/contract/README.md maps
//! it to. The harness in main.rs starts it with the case's environment.

use serde_json::{json, Map, Value};
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use the0_sdk::input::{self, LogLevel};
use the0_sdk::query::{self, QueryRequest};
use the0_sdk::state;

type OpError = Box<dyn Error + Send + Sync>;
type Bound = HashMap<String, Value>;

pub fn main(case_path: &str, query_result_path: &str) {
    let text = fs::read_to_string(case_path).expect("contract_bot: cannot read the case");
    let case: Value = serde_json::from_str(&text).expect("contract_bot: the case is not JSON");

    query::set_result_path(query_result_path);

    if case["entrypoint"] == "bot" {
        let script = case["script"].as_array().cloned().unwrap_or_default();
        if let Err(e) = run_steps(&script, None) {
            // What a bot's `fn main() -> Result<_, _>` does with an Err.
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        }
        return;
    }

    let handlers = case["handlers"]
        .as_object()
        .expect("contract_bot: a query case needs handlers");
    for (path, steps) in handlers {
        let steps = steps.as_array().cloned().unwrap_or_default();
        query::handler(path, move |req| match run_steps(&steps, Some(req)) {
            Ok(value) => value,
            // A Rust handler has no other way to fail.
            Err(e) => panic!("{e}"),
        });
    }
    query::run();
}

fn resolve(value: &Value, bound: &Bound) -> Value {
    match value {
        Value::Object(map) if map.len() == 1 && map.contains_key("$ref") => {
            let name = map["$ref"]
                .as_str()
                .expect("contract_bot: $ref needs a name");
            bound
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("contract_bot: nothing is bound to {name:?}"))
        }
        Value::Object(map) => Value::Object(
            map.iter()
                .map(|(key, item)| (key.clone(), resolve(item, bound)))
                .collect(),
        ),
        Value::Array(items) => {
            Value::Array(items.iter().map(|item| resolve(item, bound)).collect())
        }
        other => other.clone(),
    }
}

fn run_steps(steps: &[Value], request: Option<&QueryRequest>) -> Result<Value, OpError> {
    let mut bound = Bound::new();
    for step in steps {
        if step["op"] == "return" {
            return Ok(resolve(&step["value"], &bound));
        }
        let value = match perform(step, request, &bound) {
            Ok(value) => value,
            Err(e) => match step.get("catch").and_then(Value::as_str) {
                Some(kind) if is_kind(kind, &*e) => json!({ "caught": kind }),
                _ => return Err(e),
            },
        };
        if let Some(name) = step.get("as").and_then(Value::as_str) {
            bound.insert(name.to_string(), value);
        }
    }
    Ok(Value::Null)
}

fn is_kind(kind: &str, error: &(dyn Error + 'static)) -> bool {
    match kind {
        "any" => true,
        // The query module's type, which must be the one state fails with.
        "read_only_state" => std::iter::successors(Some(error), |&e| e.source())
            .any(|e| e.is::<query::ReadOnlyStateError>()),
        _ => panic!("contract_bot: unknown catch kind {kind:?}"),
    }
}

fn perform(step: &Value, request: Option<&QueryRequest>, bound: &Bound) -> Result<Value, OpError> {
    let op = step["op"]
        .as_str()
        .expect("contract_bot: a step needs an op");
    let args: Map<String, Value> = step
        .as_object()
        .expect("contract_bot: a step is an object")
        .iter()
        .filter(|(key, _)| !matches!(key.as_str(), "op" | "as" | "catch"))
        .map(|(key, value)| (key.clone(), resolve(value, bound)))
        .collect();
    let text = |name: &str| -> String {
        args.get(name)
            .and_then(Value::as_str)
            .unwrap_or_else(|| panic!("contract_bot: op {op:?} needs a string {name:?}"))
            .to_string()
    };

    let value = match op {
        "parse" => {
            let (bot_id, config) = input::parse()?;
            json!({ "bot_id": bot_id, "config": config })
        }
        "success" => {
            if args.contains_key("data") {
                return Err("this SDK's success() takes no data".into());
            }
            input::success(&text("message"));
            Value::Null
        }
        "error" => {
            if args.contains_key("data") {
                return Err("this SDK's error() takes no data".into());
            }
            input::error(&text("message"))
        }
        "result" => {
            input::result(&args["value"]);
            Value::Null
        }
        "metric" => {
            input::metric(&text("type"), &args["data"]);
            Value::Null
        }
        "log" => {
            let level = args.get("level").and_then(Value::as_str).map(log_level);
            input::log(&text("message"), args.get("data"), level);
            Value::Null
        }
        "raise" => return Err(text("message").into()),

        "state.get" => match args.get("default") {
            Some(default) => state::get_or(&text("key"), default.clone()),
            None => state::get::<Value>(&text("key"))?,
        },
        "state.set" => {
            state::set(&text("key"), &args["value"])?;
            Value::Null
        }
        "state.delete" => Value::Bool(state::delete(&text("key"))),
        "state.list" => json!(state::list()),
        "state.exists" => Value::Bool(state::exists(&text("key"))),
        "state.clear" => {
            state::clear()?;
            Value::Null
        }

        "query.params" => json!(handler_request(request, op).params),
        "query.param" => handler_request(request, op)
            .get(&text("key"))
            .map(Value::String)
            .or_else(|| args.get("default").cloned())
            .unwrap_or(Value::Null),
        "query.config" => query::get_config(),

        _ => panic!("contract_bot: unknown op {op:?}"),
    };
    Ok(value)
}

fn handler_request<'a>(request: Option<&'a QueryRequest>, op: &str) -> &'a QueryRequest {
    request
        .unwrap_or_else(|| panic!("contract_bot: op {op:?} is only valid inside a query handler"))
}

fn log_level(level: &str) -> LogLevel {
    match level {
        "info" => LogLevel::Info,
        "warn" => LogLevel::Warn,
        "error" => LogLevel::Error,
        _ => panic!("contract_bot: unknown log level {level:?}"),
    }
}
