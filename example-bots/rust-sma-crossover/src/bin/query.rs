//! SMA Crossover Query Handlers (Rust)
//! ====================================
//! Query endpoints for the SMA crossover bot.
//!
//! These handlers provide read-only access to bot state and computed data.
//! They can be executed via:
//!   - CLI: the0 bot query <bot_id> /status
//!   - API: POST /bot/:id/query
//!
//! Available queries:
//!   /status     - Get current SMA values and configuration
//!   /signals    - Get signal count with optional limit param
//!   /sma-values - Get current short/long SMA values

use the0_sdk::{query, state};
use serde::{Deserialize, Serialize};
use serde_json::json;

/// Persistent state structure (matches main.rs)
#[derive(Deserialize, Serialize, Default)]
struct PersistedState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
    signal_count: u64,
}

fn main() {
    // /status - Get bot status and current SMA values
    query::handler("/status", |_req| {
        let persisted: PersistedState = state::get("bot_state").unwrap_or_default();
        let config = query::get_config();

        json!({
            "prev_short_sma": persisted.prev_short_sma,
            "prev_long_sma": persisted.prev_long_sma,
            "signal_count": persisted.signal_count,
            "config": {
                "symbol": config["symbol"].as_str().unwrap_or("AAPL"),
                "short_period": config["short_period"].as_i64().unwrap_or(5),
                "long_period": config["long_period"].as_i64().unwrap_or(20)
            }
        })
    });

    // /signals - Get signal statistics
    query::handler("/signals", |req| {
        let limit: i32 = req.get("limit")
            .and_then(|s| s.parse().ok())
            .unwrap_or(10);

        let persisted: PersistedState = state::get("bot_state").unwrap_or_default();

        json!({
            "signal_count": persisted.signal_count,
            "limit_applied": limit
            // Note: Individual signals are not persisted, only the count
        })
    });

    // /sma-values - Get current SMA values
    query::handler("/sma-values", |_req| {
        let persisted: PersistedState = state::get("bot_state").unwrap_or_default();

        json!({
            "short_sma": persisted.prev_short_sma,
            "long_sma": persisted.prev_long_sma
        })
    });

    // Run query system
    query::run();
}
