//! Common test utilities for the0 SDK tests

use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard};
use tempfile::TempDir;

/// Global mutex to prevent env var races between tests
/// Rust tests run in parallel by default, which can cause issues with env vars
static ENV_MUTEX: Mutex<()> = Mutex::new(());

/// RAII guard that sets environment variables and restores on drop
pub struct EnvGuard {
    vars: Vec<(String, Option<String>)>,
    #[allow(dead_code)]
    lock: Option<MutexGuard<'static, ()>>,
}

impl EnvGuard {
    /// Create a new EnvGuard, acquiring the env mutex
    /// Handles poisoned mutex from previous panics (expected in should_panic tests)
    pub fn new() -> Self {
        // Acquire lock, recovering from poison if needed
        let lock = match ENV_MUTEX.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        };
        Self {
            vars: Vec::new(),
            lock: Some(lock),
        }
    }

    /// Create a new EnvGuard without holding the mutex lock
    /// Use this for should_panic tests to avoid poisoning the mutex
    pub fn new_unlocked() -> Self {
        Self {
            vars: Vec::new(),
            lock: None,
        }
    }

    /// Set an environment variable, saving the old value for restoration
    pub fn set(&mut self, key: &str, value: &str) -> &mut Self {
        let old = env::var(key).ok();
        self.vars.push((key.to_string(), old));
        env::set_var(key, value);
        self
    }

    /// Remove an environment variable, saving the old value for restoration
    pub fn remove(&mut self, key: &str) -> &mut Self {
        let old = env::var(key).ok();
        self.vars.push((key.to_string(), old));
        env::remove_var(key);
        self
    }
}

impl Drop for EnvGuard {
    fn drop(&mut self) {
        // Restore original values in reverse order
        for (key, old_value) in self.vars.iter().rev() {
            match old_value {
                Some(val) => env::set_var(key, val),
                None => env::remove_var(key),
            }
        }
    }
}

/// Context for testing result file functions
/// Sets up a temp directory and CODE_MOUNT_DIR
pub struct ResultContext {
    pub temp_dir: TempDir,
    pub env_guard: EnvGuard,
}

impl ResultContext {
    /// Create a new result context with a temp directory
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp directory");
        let mut env_guard = EnvGuard::new();

        // Set CODE_MOUNT_DIR to the temp directory path (without leading /)
        // The SDK prepends "/" to CODE_MOUNT_DIR
        let path = temp_dir.path().to_str().unwrap();
        // Remove leading slash since SDK adds it
        let path = path.strip_prefix('/').unwrap_or(path);
        env_guard.set("CODE_MOUNT_DIR", path);

        Self { temp_dir, env_guard }
    }

    /// Get the path to the result file
    pub fn result_file_path(&self) -> PathBuf {
        self.temp_dir.path().join("result.json")
    }

    /// Read the contents of the result file
    pub fn read_result(&self) -> Option<String> {
        let path = self.result_file_path();
        fs::read_to_string(path).ok()
    }

    /// Check if result file exists
    pub fn result_exists(&self) -> bool {
        self.result_file_path().exists()
    }
}
