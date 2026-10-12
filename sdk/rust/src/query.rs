//! the0 Query Module - Express-like handler interface for bot queries.
//!
//! This module provides query handling capabilities for bots, allowing users to define
//! custom read-only query handlers that can be executed on demand.
//!
//! Separate namespace from state and main SDK. Users explicitly import:
//! ```rust,no_run
//! use the0_sdk::query;
//! use the0_sdk::state; // If needed in handlers
//! ```
//!
//! # Example
//!
//! ```rust,no_run
//! use the0_sdk::query::{self, QueryRequest};
//! use the0_sdk::state;
//! use serde_json::json;
//!
//! fn main() {
//!     query::handler("/portfolio", |req| {
//!         let positions: Vec<String> = state::get_or("positions", vec![]);
//!         json!({ "positions": positions, "count": positions.len() })
//!     });
//!
//!     query::handler("/status", |req| {
//!         let symbol = req.get("symbol").unwrap_or("BTC/USD".to_string());
//!         json!({ "symbol": symbol, "active": true })
//!     });
//!
//!     query::run();
//! }
//! ```

use serde_json::{json, Value};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// Global handler registry
static HANDLERS: Mutex<Option<HashMap<String, Box<dyn Fn(&QueryRequest) -> Value + Send + Sync>>>> =
    Mutex::new(None);

/// Global current params
static CURRENT_PARAMS: Mutex<Option<HashMap<String, String>>> = Mutex::new(None);

/// Global config
static CONFIG: Mutex<Option<Value>> = Mutex::new(None);

const QUERY_RESULT_PATH: &str = "/query/result.json";

static QUERY_RESULT_PATH_OVERRIDE: Mutex<Option<PathBuf>> = Mutex::new(None);

/// Test-only: point ephemeral query results at `path` instead of
/// `/query/result.json`, which tests cannot write. The runtime always reads
/// `/query/result.json`, so bots must not call this.
#[doc(hidden)]
pub fn set_result_path(path: impl Into<PathBuf>) {
    *QUERY_RESULT_PATH_OVERRIDE.lock().unwrap() = Some(path.into());
}

/// The error state writes fail with during a query: `state::set`,
/// `state::delete` and `state::clear` return it as
/// `StateError::ReadOnly(ReadOnlyStateError)`.
pub use crate::state::ReadOnlyStateError;

/// Request object passed to handlers (Express-like).
#[derive(Debug, Clone)]
pub struct QueryRequest {
    /// The query path being requested (e.g., "/portfolio")
    pub path: String,
    /// Dictionary of query parameters
    pub params: HashMap<String, String>,
}

impl QueryRequest {
    /// Create a new QueryRequest
    pub fn new(path: &str, params: HashMap<String, String>) -> Self {
        Self {
            path: path.to_string(),
            params,
        }
    }

    /// Get a parameter value with optional default.
    pub fn get(&self, key: &str) -> Option<String> {
        self.params.get(key).cloned()
    }

    /// Get a parameter value with a default.
    pub fn get_or(&self, key: &str, default: &str) -> String {
        self.params
            .get(key)
            .cloned()
            .unwrap_or_else(|| default.to_string())
    }
}

/// Register a query handler (Express-like).
///
/// # Arguments
///
/// * `path` - The query path to handle (e.g., "/portfolio", "/signals")
/// * `handler_fn` - Handler function that receives request and returns JSON response
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::query;
/// use serde_json::json;
///
/// query::handler("/portfolio", |req| {
///     let symbol = req.get_or("symbol", "BTC/USD");
///     json!({ "symbol": symbol, "positions": [] })
/// });
/// ```
pub fn handler<F>(path: &str, handler_fn: F)
where
    F: Fn(&QueryRequest) -> Value + Send + Sync + 'static,
{
    let mut handlers = HANDLERS.lock().unwrap();
    if handlers.is_none() {
        *handlers = Some(HashMap::new());
    }
    if let Some(ref mut h) = *handlers {
        h.insert(path.to_string(), Box::new(handler_fn));
    }
}

/// Get current query parameters (alternative to request object).
///
/// Returns a copy of the current query parameters.
pub fn get_params() -> HashMap<String, String> {
    let params = CURRENT_PARAMS.lock().unwrap();
    params.clone().unwrap_or_default()
}

/// Get the bot configuration.
///
/// Returns a copy of the bot configuration.
pub fn get_config() -> Value {
    let config = CONFIG.lock().unwrap();
    config.clone().unwrap_or(json!({}))
}

/// Check if currently running in query mode (read-only).
///
/// Used by state module to enforce read-only behavior.
pub fn is_query_mode() -> bool {
    env::var("QUERY_PATH").is_ok()
}

/// Run the query system with automatic mode detection.
///
/// Modes:
/// - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
/// - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
/// - Neither: Info mode (print available handlers)
pub fn run() {
    // Load bot config from environment
    {
        let config_str = env::var("BOT_CONFIG").unwrap_or_else(|_| "{}".to_string());
        let mut config = CONFIG.lock().unwrap();
        *config = Some(serde_json::from_str(&config_str).unwrap_or(json!({})));
    }

    // Register built-in handlers
    {
        let mut handlers = HANDLERS.lock().unwrap();
        if handlers.is_none() {
            *handlers = Some(HashMap::new());
        }
        if let Some(ref mut h) = *handlers {
            h.entry("/health".to_string())
                .or_insert_with(|| Box::new(|_| json!({"status": "ok"})));

            if !h.contains_key("/info") {
                let mut paths: Vec<String> = h.keys().cloned().collect();
                paths.push("/info".to_string());
                h.insert(
                    "/info".to_string(),
                    Box::new(move |_| json!({"available_queries": paths})),
                );
            }
        }
    }

    let query_path = env::var("QUERY_PATH").ok();
    let bot_type = env::var("BOT_TYPE").ok();

    if let Some(path) = query_path {
        run_ephemeral(&path);
    } else if bot_type.as_deref() == Some("realtime") {
        run_server();
    } else {
        run_ephemeral("/info");
    }
}

/// Write query result to /query/result.json file (matches Python SDK behavior).
/// This avoids stdout pollution from runtime logs mixing with query results.
fn write_result(result: Value) {
    let result_path = QUERY_RESULT_PATH_OVERRIDE
        .lock()
        .unwrap()
        .clone()
        .unwrap_or_else(|| PathBuf::from(QUERY_RESULT_PATH));
    let result_dir = result_path.parent().unwrap_or(Path::new("/"));
    if let Err(e) = fs::create_dir_all(result_dir) {
        eprintln!(
            "RESULT_ERROR: Failed to create {} directory: {}",
            result_dir.display(),
            e
        );
        return;
    }
    if let Err(e) = fs::write(&result_path, result.to_string()) {
        eprintln!("RESULT_ERROR: Failed to write result file: {}", e);
    }
}

/// What a query comes to in either mode: the status the query server answers
/// with, and the body both modes send. An ephemeral query exits 0 only on 200.
struct Answer {
    status: u16,
    body: Value,
}

/// Run the handler registered for `path`, or report that there is none.
fn answer(path: &str, params: HashMap<String, String>) -> Answer {
    *CURRENT_PARAMS.lock().unwrap() = Some(params.clone());

    let handlers = HANDLERS.lock().unwrap();
    let h = handlers.as_ref().expect("run() registers the built-ins");
    match h.get(path) {
        Some(handler_fn) => Answer {
            status: 200,
            body: json!({"status": "ok", "data": handler_fn(&QueryRequest::new(path, params))}),
        },
        None => Answer {
            status: 404,
            body: json!({
                "status": "error",
                "error": format!("No handler for path: {}", path),
                "available": h.keys().cloned().collect::<Vec<String>>()
            }),
        },
    }
}

/// Execute a single query and write result to /query/result.json.
fn run_ephemeral(query_path: &str) {
    let params_str = env::var("QUERY_PARAMS").unwrap_or_else(|_| "{}".to_string());
    let params: HashMap<String, String> = serde_json::from_str(&params_str).unwrap_or_default();

    let answer = answer(query_path, params);
    write_result(answer.body);
    if answer.status != 200 {
        std::process::exit(1);
    }
}

/// Start HTTP server on port 9476 for realtime bots.
fn run_server() {
    let port: u16 = env::var("THE0_QUERY_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(9476);

    let listener = TcpListener::bind(format!("0.0.0.0:{}", port)).expect("Failed to bind to port");

    crate::input::log(
        &format!("Query server started on port {}", port),
        None,
        None,
    );

    for stream in listener.incoming() {
        if let Ok(stream) = stream {
            handle_connection(stream);
        }
    }
}

/// Handle an HTTP connection.
fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&stream);
    let request_line = buf_reader.lines().next();

    let answer = match request_line {
        Some(Ok(line)) => match line.split_whitespace().nth(1) {
            Some(target) => match target.split_once('?') {
                Some((path, query_string)) => answer(path, query_string_params(query_string)),
                None => answer(target, HashMap::new()),
            },
            None => bad_request("Invalid request"),
        },
        _ => bad_request("Failed to read request"),
    };

    let body = answer.body.to_string();
    let response = format!(
        "HTTP/1.1 {} {}\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}",
        answer.status,
        reason_phrase(answer.status),
        body.len(),
        body
    );

    stream.write_all(response.as_bytes()).ok();
}

fn query_string_params(query_string: &str) -> HashMap<String, String> {
    query_string
        .split('&')
        .filter_map(|pair| {
            let mut parts = pair.splitn(2, '=');
            Some((
                parts.next()?.to_string(),
                parts.next().unwrap_or("").to_string(),
            ))
        })
        .collect()
}

fn bad_request(error: &str) -> Answer {
    Answer {
        status: 400,
        body: json!({"status": "error", "error": error}),
    }
}

fn reason_phrase(status: u16) -> &'static str {
    match status {
        200 => "OK",
        400 => "Bad Request",
        404 => "Not Found",
        _ => "Internal Server Error",
    }
}
