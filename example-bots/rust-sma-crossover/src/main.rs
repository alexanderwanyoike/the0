/**
 * SMA Crossover Bot (Rust)
 * ========================
 * A realtime bot that implements Simple Moving Average crossover strategy
 * using live data from Yahoo Finance.
 *
 * This example demonstrates:
 * - Fetching real market data from Yahoo Finance REST API
 * - Calculating Simple Moving Averages (SMA)
 * - Detecting SMA crossovers for trading signals
 * - Structured metric emission for dashboard visualization
 * - Persistent state for SMA values across restarts
 *
 * Metrics emitted:
 * - price: Current stock price with change percentage
 * - sma: Short and long SMA values
 * - signal: BUY/SELL signals when crossover detected
 *
 * State Usage:
 * - Persists previous SMA values for crossover detection across restarts
 * - Tracks total signal count for monitoring
 */

use the0_sdk::{input, state};
use serde::Deserialize;
use serde_json::json;
use std::thread;
use std::time::Duration;
use tracing::{info, error, Level};
use tracing_subscriber;

// Yahoo Finance API response structures
#[derive(Deserialize)]
struct YahooResponse {
    chart: ChartResponse,
}

#[derive(Deserialize)]
struct ChartResponse {
    result: Option<Vec<ChartResult>>,
}

#[derive(Deserialize)]
struct ChartResult {
    indicators: Indicators,
}

#[derive(Deserialize)]
struct Indicators {
    quote: Vec<Quote>,
}

#[derive(Deserialize)]
struct Quote {
    close: Vec<Option<f64>>,
}

// Bot state (in-memory)
struct BotState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
    signal_count: u64,
}

// Persistent state (saved to disk)
#[derive(Deserialize, serde::Serialize, Default)]
struct PersistedState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
    signal_count: u64,
}

fn main() {
    // Initialize tracing for structured logging
    tracing_subscriber::fmt()
        .json()
        .with_max_level(Level::INFO)
        .init();

    // Get configuration using the0 SDK
    let (bot_id, config) = input::parse().expect("Failed to parse bot configuration");

    // Extract configuration with defaults
    let symbol = config["symbol"].as_str().unwrap_or("AAPL");
    let short_period = config["short_period"].as_i64().unwrap_or(5) as usize;
    let long_period = config["long_period"].as_i64().unwrap_or(20) as usize;
    let update_interval_ms = config["update_interval_ms"].as_i64().unwrap_or(60000) as u64;

    // Load persistent state from previous runs
    let persisted: PersistedState = state::get("bot_state").unwrap_or_default();

    info!(
        bot_id = %bot_id,
        symbol = %symbol,
        short_period,
        long_period,
        loaded_signals = persisted.signal_count,
        "Bot started"
    );

    let client = reqwest::blocking::Client::builder()
        .user_agent("the0-sma-bot/1.0")
        .build()
        .expect("Failed to create HTTP client");

    let mut bot_state = BotState {
        prev_short_sma: persisted.prev_short_sma,
        prev_long_sma: persisted.prev_long_sma,
        signal_count: persisted.signal_count,
    };

    // Main loop
    let mut iteration = 0u64;
    loop {
        match fetch_and_process(&client, symbol, short_period, long_period, &mut bot_state) {
            Ok(_) => {}
            Err(e) => error!(error = %e, "Processing error"),
        }

        // Persist state every 10 iterations
        iteration += 1;
        if iteration % 10 == 0 {
            let _ = state::set("bot_state", &PersistedState {
                prev_short_sma: bot_state.prev_short_sma,
                prev_long_sma: bot_state.prev_long_sma,
                signal_count: bot_state.signal_count,
            });
        }

        thread::sleep(Duration::from_millis(update_interval_ms));
    }
}

fn fetch_and_process(
    client: &reqwest::blocking::Client,
    symbol: &str,
    short_period: usize,
    long_period: usize,
    state: &mut BotState,
) -> Result<(), Box<dyn std::error::Error>> {
    // Fetch historical data from Yahoo Finance
    let prices = fetch_yahoo_finance(client, symbol)?;

    if prices.len() < long_period {
        info!(symbol = %symbol, required = long_period, have = prices.len(), "Insufficient data");
        return Ok(());
    }

    // Get current price
    let current_price = prices[prices.len() - 1];
    let previous_price = if prices.len() > 1 {
        prices[prices.len() - 2]
    } else {
        current_price
    };
    let change_pct = if previous_price != 0.0 {
        ((current_price - previous_price) / previous_price) * 100.0
    } else {
        0.0
    };

    // Emit price metric
    input::metric("price", &json!({
        "symbol": symbol,
        "value": round(current_price, 2),
        "change_pct": round(change_pct, 3)
    }));

    // Calculate SMAs
    let short_sma = calculate_sma(&prices, short_period);
    let long_sma = calculate_sma(&prices, long_period);

    // Emit SMA metric
    input::metric("sma", &json!({
        "symbol": symbol,
        "short_sma": round(short_sma, 2),
        "long_sma": round(long_sma, 2),
        "short_period": short_period,
        "long_period": long_period
    }));

    // Check for crossover signal
    if let (Some(prev_short), Some(prev_long)) = (state.prev_short_sma, state.prev_long_sma) {
        if let Some(signal) = check_crossover(prev_short, prev_long, short_sma, long_sma) {
            state.signal_count += 1;
            let confidence = (short_sma - long_sma).abs() / long_sma * 100.0;
            let confidence = confidence.min(0.95);

            input::metric("signal", &json!({
                "type": signal,
                "symbol": symbol,
                "price": round(current_price, 2),
                "confidence": round(confidence, 2),
                "total_signals": state.signal_count,
                "reason": format!("SMA{} crossed {} SMA{}",
                    short_period,
                    if signal == "BUY" { "above" } else { "below" },
                    long_period)
            }));
        }
    }

    // Update previous SMA values
    state.prev_short_sma = Some(short_sma);
    state.prev_long_sma = Some(long_sma);

    Ok(())
}

fn fetch_yahoo_finance(
    client: &reqwest::blocking::Client,
    symbol: &str,
) -> Result<Vec<f64>, Box<dyn std::error::Error>> {
    let url = format!(
        "https://query1.finance.yahoo.com/v8/finance/chart/{}?interval=1d&range=1mo",
        symbol
    );

    let response: YahooResponse = client.get(&url).send()?.json()?;

    let prices: Vec<f64> = response
        .chart
        .result
        .and_then(|results| results.into_iter().next())
        .map(|result| {
            result
                .indicators
                .quote
                .into_iter()
                .next()
                .map(|quote| quote.close.into_iter().filter_map(|p| p).collect())
                .unwrap_or_default()
        })
        .unwrap_or_default();

    Ok(prices)
}

fn calculate_sma(prices: &[f64], period: usize) -> f64 {
    if prices.len() < period {
        return 0.0;
    }
    let start = prices.len() - period;
    let sum: f64 = prices[start..].iter().sum();
    sum / period as f64
}

fn check_crossover(
    prev_short: f64,
    prev_long: f64,
    curr_short: f64,
    curr_long: f64,
) -> Option<&'static str> {
    // Golden cross: short SMA crosses above long SMA
    if prev_short <= prev_long && curr_short > curr_long {
        return Some("BUY");
    }
    // Death cross: short SMA crosses below long SMA
    if prev_short >= prev_long && curr_short < curr_long {
        return Some("SELL");
    }
    None
}

fn round(value: f64, decimals: i32) -> f64 {
    let multiplier = 10f64.powi(decimals);
    (value * multiplier).round() / multiplier
}
