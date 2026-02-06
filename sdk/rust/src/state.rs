//! the0 State Module
//!
//! Provides persistent state management for bots across executions.
//! State is automatically synced to MinIO storage between bot runs.
//!
//! # Example
//!
//! ```rust,no_run
//! use the0_sdk::state;
//! use serde::{Deserialize, Serialize};
//!
//! #[derive(Serialize, Deserialize)]
//! struct Portfolio {
//!     holdings: std::collections::HashMap<String, u64>,
//! }
//!
//! fn main() {
//!     // Store state
//!     state::set("portfolio", &Portfolio {
//!         holdings: [("AAPL".to_string(), 100)].into()
//!     }).expect("Failed to save state");
//!
//!     // Retrieve state
//!     let portfolio: Option<Portfolio> = state::get("portfolio").ok();
//!
//!     // List all keys
//!     let keys = state::list();
//!
//!     // Delete a key
//!     state::delete("portfolio");
//!
//!     // Clear all state
//!     state::clear().ok();
//! }
//! ```

use serde::{de::DeserializeOwned, Serialize};
use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

/// Errors that can occur during state operations
#[derive(Debug)]
pub enum StateError {
    /// Invalid key (empty or contains path separators)
    InvalidKey(String),
    /// IO error during file operations
    Io(io::Error),
    /// JSON serialization/deserialization error
    Json(serde_json::Error),
    /// Attempted to modify state in query mode
    ReadOnly(String),
}

impl std::fmt::Display for StateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StateError::InvalidKey(msg) => write!(f, "Invalid state key: {}", msg),
            StateError::Io(e) => write!(f, "IO error: {}", e),
            StateError::Json(e) => write!(f, "JSON error: {}", e),
            StateError::ReadOnly(msg) => write!(f, "Read-only state: {}", msg),
        }
    }
}

impl std::error::Error for StateError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            StateError::Io(e) => Some(e),
            StateError::Json(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for StateError {
    fn from(err: io::Error) -> Self {
        StateError::Io(err)
    }
}

impl From<serde_json::Error> for StateError {
    fn from(err: serde_json::Error) -> Self {
        StateError::Json(err)
    }
}

/// Get the path to the state directory.
fn get_state_dir() -> PathBuf {
    env::var("STATE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/state/.the0-state"))
}

/// Get the file path for a state key.
fn get_key_path(key: &str) -> PathBuf {
    get_state_dir().join(format!("{}.json", key))
}

/// Validate that a key is safe to use as a filename.
fn validate_key(key: &str) -> Result<(), StateError> {
    if key.is_empty() {
        return Err(StateError::InvalidKey("State key cannot be empty".to_string()));
    }
    if key.contains('/') || key.contains('\\') || key.contains("..") {
        return Err(StateError::InvalidKey(
            "State key cannot contain path separators or '..'".to_string(),
        ));
    }
    Ok(())
}

/// Check if currently running in query mode (read-only).
fn is_query_mode() -> bool {
    env::var("QUERY_PATH").is_ok()
}

/// Check if write operations are allowed.
/// Returns an error if in query mode.
fn check_write_allowed() -> Result<(), StateError> {
    if is_query_mode() {
        return Err(StateError::ReadOnly(
            "State modifications are not allowed during query execution. \
             Queries are read-only. Use state::get() to read state values."
                .to_string(),
        ));
    }
    Ok(())
}

/// Get a value from persistent state.
///
/// Returns `None` if the key doesn't exist or deserialization fails.
///
/// # Arguments
///
/// * `key` - The state key (alphanumeric, hyphens, underscores)
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Portfolio { holdings: std::collections::HashMap<String, u64> }
///
/// let portfolio: Option<Portfolio> = state::get("portfolio").ok();
/// ```
pub fn get<T: DeserializeOwned>(key: &str) -> Result<T, StateError> {
    validate_key(key)?;
    let filepath = get_key_path(key);
    let content = fs::read_to_string(&filepath)?;
    let value = serde_json::from_str(&content)?;
    Ok(value)
}

/// Get a value from persistent state with a default.
///
/// Returns the default value if the key doesn't exist or deserialization fails.
///
/// # Arguments
///
/// * `key` - The state key
/// * `default` - Default value if key doesn't exist
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// let count: i32 = state::get_or("trade_count", 0);
/// ```
pub fn get_or<T: DeserializeOwned>(key: &str, default: T) -> T {
    get(key).unwrap_or(default)
}

/// Set a value in persistent state.
///
/// The value must be serializable to JSON.
/// Note: This function will return an error if called during query execution.
///
/// # Arguments
///
/// * `key` - The state key (alphanumeric, hyphens, underscores)
/// * `value` - The value to store (must be Serialize)
///
/// # Errors
///
/// Returns `StateError::ReadOnly` if called during query execution (queries are read-only).
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// state::set("trade_count", &42).expect("Failed to save state");
/// state::set("last_prices", &vec![45000.5, 45100.0]).expect("Failed to save state");
/// ```
pub fn set<T: Serialize>(key: &str, value: &T) -> Result<(), StateError> {
    check_write_allowed()?;
    validate_key(key)?;
    let state_dir = get_state_dir();
    fs::create_dir_all(&state_dir)?;
    let filepath = get_key_path(key);
    let content = serde_json::to_string(value)?;
    fs::write(&filepath, content)?;
    Ok(())
}

/// Delete a key from persistent state.
///
/// Note: This function will return false if called during query execution.
///
/// # Arguments
///
/// * `key` - The state key to delete
///
/// # Returns
///
/// `true` if the key existed and was deleted, `false` otherwise.
/// Also returns `false` if in query mode (queries are read-only).
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// if state::delete("old_data") {
///     println!("Cleaned up old data");
/// }
/// ```
pub fn delete(key: &str) -> bool {
    if check_write_allowed().is_err() {
        return false;
    }
    if validate_key(key).is_err() {
        return false;
    }
    let filepath = get_key_path(key);
    fs::remove_file(&filepath).is_ok()
}

/// List all keys in persistent state.
///
/// # Returns
///
/// A vector of state keys.
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// let keys = state::list();
/// println!("State contains {} keys: {:?}", keys.len(), keys);
/// ```
pub fn list() -> Vec<String> {
    let state_dir = get_state_dir();
    let entries = match fs::read_dir(&state_dir) {
        Ok(entries) => entries,
        Err(_) => return Vec::new(),
    };

    entries
        .filter_map(|entry| entry.ok())
        .filter_map(|entry| {
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "json") {
                path.file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
            } else {
                None
            }
        })
        .collect()
}

/// Clear all state.
///
/// Removes all stored state keys.
/// Note: This function will return an error if called during query execution.
///
/// # Errors
///
/// Returns `StateError::ReadOnly` if called during query execution (queries are read-only).
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// state::clear().ok();
/// println!("All state cleared");
/// ```
pub fn clear() -> Result<(), StateError> {
    check_write_allowed()?;
    let state_dir = get_state_dir();
    let entries = match fs::read_dir(&state_dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(()),
        Err(e) => return Err(e.into()),
    };

    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().map_or(false, |ext| ext == "json") {
            fs::remove_file(&path)?;
        }
    }
    Ok(())
}

/// Check if a key exists in state.
///
/// # Arguments
///
/// * `key` - The state key to check
///
/// # Returns
///
/// `true` if the key exists, `false` otherwise.
///
/// # Example
///
/// ```rust,no_run
/// use the0_sdk::state;
///
/// if state::exists("portfolio") {
///     // Load the portfolio
/// }
/// ```
pub fn exists(key: &str) -> bool {
    if validate_key(key).is_err() {
        return false;
    }
    let filepath = get_key_path(key);
    filepath.exists()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};
    use serial_test::serial;
    use std::collections::HashMap;
    use tempfile::TempDir;

    /// Helper to set up a temp state directory
    fn setup_test_env() -> TempDir {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        env::set_var("STATE_DIR", temp_dir.path());
        temp_dir
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Portfolio {
        holdings: HashMap<String, u64>,
    }

    #[test]
    #[serial]
    fn test_set_and_get_dict() {
        let _temp = setup_test_env();
        let mut holdings = HashMap::new();
        holdings.insert("AAPL".to_string(), 100);
        holdings.insert("GOOGL".to_string(), 50);
        let portfolio = Portfolio { holdings };

        set("portfolio", &portfolio).unwrap();
        let retrieved: Portfolio = get("portfolio").unwrap();
        assert_eq!(retrieved, portfolio);
    }

    #[test]
    #[serial]
    fn test_set_and_get_vec() {
        let _temp = setup_test_env();
        let prices = vec![45000.5, 45100.0, 45050.25];
        set("prices", &prices).unwrap();
        let retrieved: Vec<f64> = get("prices").unwrap();
        assert_eq!(retrieved, prices);
    }

    #[test]
    #[serial]
    fn test_set_and_get_number() {
        let _temp = setup_test_env();
        set("count", &42i32).unwrap();
        let retrieved: i32 = get("count").unwrap();
        assert_eq!(retrieved, 42);
    }

    #[test]
    #[serial]
    fn test_set_and_get_string() {
        let _temp = setup_test_env();
        set("symbol", &"BTC/USD").unwrap();
        let retrieved: String = get("symbol").unwrap();
        assert_eq!(retrieved, "BTC/USD");
    }

    #[test]
    #[serial]
    fn test_get_nonexistent_returns_error() {
        let _temp = setup_test_env();
        let result: Result<i32, _> = get("nonexistent");
        assert!(result.is_err());
    }

    #[test]
    #[serial]
    fn test_get_or_returns_default() {
        let _temp = setup_test_env();
        let result: i32 = get_or("nonexistent", 42);
        assert_eq!(result, 42);
    }

    #[test]
    #[serial]
    fn test_get_or_returns_stored_value() {
        let _temp = setup_test_env();
        set("key", &100i32).unwrap();
        let result: i32 = get_or("key", 42);
        assert_eq!(result, 100);
    }

    #[test]
    #[serial]
    fn test_exists_true() {
        let _temp = setup_test_env();
        set("exists_test", &"value").unwrap();
        assert!(exists("exists_test"));
    }

    #[test]
    #[serial]
    fn test_exists_false() {
        let _temp = setup_test_env();
        assert!(!exists("nonexistent"));
    }

    #[test]
    #[serial]
    fn test_delete_existing_key() {
        let _temp = setup_test_env();
        set("to_delete", &"value").unwrap();
        assert!(exists("to_delete"));
        let result = delete("to_delete");
        assert!(result);
        assert!(!exists("to_delete"));
    }

    #[test]
    #[serial]
    fn test_delete_nonexistent_key() {
        let _temp = setup_test_env();
        let result = delete("nonexistent");
        assert!(!result);
    }

    #[test]
    #[serial]
    fn test_list_keys() {
        let _temp = setup_test_env();
        set("key1", &"value1").unwrap();
        set("key2", &"value2").unwrap();
        set("key3", &"value3").unwrap();
        let mut keys = list();
        keys.sort();
        assert_eq!(keys, vec!["key1", "key2", "key3"]);
    }

    #[test]
    #[serial]
    fn test_list_empty() {
        let _temp = setup_test_env();
        let keys = list();
        assert!(keys.is_empty());
    }

    #[test]
    #[serial]
    fn test_clear() {
        let _temp = setup_test_env();
        set("key1", &"value1").unwrap();
        set("key2", &"value2").unwrap();
        assert_eq!(list().len(), 2);
        clear().unwrap();
        assert_eq!(list().len(), 0);
    }

    #[test]
    #[serial]
    fn test_clear_empty_state() {
        let _temp = setup_test_env();
        // Should not error when already empty
        assert!(clear().is_ok());
        assert_eq!(list().len(), 0);
    }

    #[test]
    #[serial]
    fn test_invalid_key_empty() {
        let _temp = setup_test_env();
        let result = set("", &"value");
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::InvalidKey(msg) => assert!(msg.to_lowercase().contains("empty")),
            _ => panic!("Expected InvalidKey error"),
        }
    }

    #[test]
    #[serial]
    fn test_invalid_key_path_separator() {
        let _temp = setup_test_env();
        let result = set("../escape", &"evil");
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::InvalidKey(msg) => assert!(msg.to_lowercase().contains("path")),
            _ => panic!("Expected InvalidKey error"),
        }
    }

    #[test]
    #[serial]
    fn test_invalid_key_backslash() {
        let _temp = setup_test_env();
        let result = set("..\\escape", &"evil");
        assert!(result.is_err());
        match result.unwrap_err() {
            StateError::InvalidKey(msg) => assert!(msg.to_lowercase().contains("path")),
            _ => panic!("Expected InvalidKey error"),
        }
    }

    #[test]
    #[serial]
    fn test_complex_nested_data() {
        let _temp = setup_test_env();

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Holding {
            symbol: String,
            quantity: u64,
            price: f64,
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct Signal {
            signal_type: String,
            symbol: String,
            confidence: f64,
        }

        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct ComplexData {
            holdings: Vec<Holding>,
            signals: Vec<Signal>,
            total_value: f64,
        }

        let complex_data = ComplexData {
            holdings: vec![
                Holding {
                    symbol: "AAPL".to_string(),
                    quantity: 100,
                    price: 150.25,
                },
                Holding {
                    symbol: "GOOGL".to_string(),
                    quantity: 50,
                    price: 2800.00,
                },
            ],
            signals: vec![
                Signal {
                    signal_type: "BUY".to_string(),
                    symbol: "AAPL".to_string(),
                    confidence: 0.85,
                },
                Signal {
                    signal_type: "SELL".to_string(),
                    symbol: "TSLA".to_string(),
                    confidence: 0.72,
                },
            ],
            total_value: 155025.0,
        };

        set("complex", &complex_data).unwrap();
        let retrieved: ComplexData = get("complex").unwrap();
        assert_eq!(retrieved, complex_data);
    }

    #[test]
    #[serial]
    fn test_overwrite_existing_key() {
        let _temp = setup_test_env();
        set("key", &"original").unwrap();
        assert_eq!(get::<String>("key").unwrap(), "original");
        set("key", &"updated").unwrap();
        assert_eq!(get::<String>("key").unwrap(), "updated");
    }
}
