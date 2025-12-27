use serde_json::Value;
use std::env;

fn main() {
    // Get BOT_ID and BOT_CONFIG from environment
    let bot_id = env::var("BOT_ID").unwrap_or_else(|_| "unknown".to_string());
    let config_str = env::var("BOT_CONFIG").unwrap_or_else(|_| "{}".to_string());

    // Parse config
    let config: Value = serde_json::from_str(&config_str).unwrap_or(Value::Object(Default::default()));

    // Print inputs to stderr for test verification
    eprintln!("BOT_ID: {}", bot_id);
    eprintln!("BOT_CONFIG: {}", config);

    // Output success JSON to stdout
    println!(r#"{{"status":"success","message":"Rust bot executed","bot_id":"{}"}}"#, bot_id);
}
