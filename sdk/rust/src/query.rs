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
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::Mutex;

/// Global handler registry
static HANDLERS: Mutex<Option<HashMap<String, Box<dyn Fn(&QueryRequest) -> Value + Send + Sync>>>> =
    Mutex::new(None);

/// Global current params
static CURRENT_PARAMS: Mutex<Option<HashMap<String, String>>> = Mutex::new(None);

/// Global config
static CONFIG: Mutex<Option<Value>> = Mutex::new(None);

/// Error thrown when attempting to modify state during query execution.
#[derive(Debug)]
pub struct ReadOnlyStateError {
    pub message: String,
}

impl std::fmt::Display for ReadOnlyStateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ReadOnlyStateError {}

impl ReadOnlyStateError {
    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

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
        self.params.get(key).cloned().unwrap_or_else(|| default.to_string())
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

            // Store paths for /info handler
            let paths: Vec<String> = h.keys().cloned().collect();
            h.entry("/info".to_string())
                .or_insert_with(move || {
                    let paths = paths.clone();
                    Box::new(move |_| json!({"available_queries": paths}))
                });
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

/// Execute a single query and output JSON to stdout.
fn run_ephemeral(query_path: &str) {
    // Parse parameters from environment
    let params_str = env::var("QUERY_PARAMS").unwrap_or_else(|_| "{}".to_string());
    let params: HashMap<String, String> =
        serde_json::from_str(&params_str).unwrap_or_default();

    {
        let mut current = CURRENT_PARAMS.lock().unwrap();
        *current = Some(params.clone());
    }

    // Find handler
    let result = {
        let handlers = HANDLERS.lock().unwrap();
        if let Some(ref h) = *handlers {
            if let Some(handler_fn) = h.get(query_path) {
                let req = QueryRequest::new(query_path, params);
                Some(handler_fn(&req))
            } else {
                None
            }
        } else {
            None
        }
    };

    match result {
        Some(data) => {
            println!("{}", json!({"status": "ok", "data": data}));
        }
        None => {
            let handlers = HANDLERS.lock().unwrap();
            let available: Vec<String> = handlers
                .as_ref()
                .map(|h| h.keys().cloned().collect())
                .unwrap_or_default();
            println!(
                "{}",
                json!({
                    "status": "error",
                    "error": format!("No handler for path: {}", query_path),
                    "available": available
                })
            );
            std::process::exit(1);
        }
    }
}

/// Start HTTP server on port 9476 for realtime bots.
fn run_server() {
    let port: u16 = env::var("THE0_QUERY_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(9476);

    let listener = TcpListener::bind(format!("0.0.0.0:{}", port))
        .expect("Failed to bind to port");

    eprintln!(
        "{}",
        json!({"_log": "info", "message": format!("Query server started on port {}", port)})
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

    let (status, response_body) = match request_line {
        Some(Ok(line)) => {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                let path_with_query = parts[1];
                let (path, query_string) = if let Some(idx) = path_with_query.find('?') {
                    (&path_with_query[..idx], Some(&path_with_query[idx + 1..]))
                } else {
                    (path_with_query, None)
                };

                // Parse query parameters
                let params: HashMap<String, String> = query_string
                    .map(|qs| {
                        qs.split('&')
                            .filter_map(|pair| {
                                let mut parts = pair.splitn(2, '=');
                                Some((
                                    parts.next()?.to_string(),
                                    parts.next().unwrap_or("").to_string(),
                                ))
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                {
                    let mut current = CURRENT_PARAMS.lock().unwrap();
                    *current = Some(params.clone());
                }

                // Find and execute handler
                let result = {
                    let handlers = HANDLERS.lock().unwrap();
                    if let Some(ref h) = *handlers {
                        if let Some(handler_fn) = h.get(path) {
                            let req = QueryRequest::new(path, params);
                            Some(Ok(handler_fn(&req)))
                        } else {
                            Some(Err(format!("No handler for path: {}", path)))
                        }
                    } else {
                        Some(Err("No handlers registered".to_string()))
                    }
                };

                match result {
                    Some(Ok(data)) => {
                        ("200 OK", json!({"status": "ok", "data": data}).to_string())
                    }
                    Some(Err(err)) => {
                        ("404 Not Found", json!({"status": "error", "error": err}).to_string())
                    }
                    None => (
                        "500 Internal Server Error",
                        json!({"status": "error", "error": "Internal error"}).to_string(),
                    ),
                }
            } else {
                (
                    "400 Bad Request",
                    json!({"status": "error", "error": "Invalid request"}).to_string(),
                )
            }
        }
        _ => (
            "400 Bad Request",
            json!({"status": "error", "error": "Failed to read request"}).to_string(),
        ),
    };

    let response = format!(
        "HTTP/1.1 {}\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}",
        status,
        response_body.len(),
        response_body
    );

    stream.write_all(response.as_bytes()).ok();
}
