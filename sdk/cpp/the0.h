/**
 * the0 SDK for C/C++ trading bots
 *
 * This is a header-only library for building trading bots on the0 platform.
 * Requires nlohmann/json: https://github.com/nlohmann/json
 *
 * Example:
 *   #include "the0.h"
 *
 *   int main() {
 *       auto [bot_id, config] = the0::parse();
 *       std::cerr << "Bot " << bot_id << " starting" << std::endl;
 *
 *       // Access config as JSON
 *       std::string symbol = config.value("symbol", "BTC/USDT");
 *
 *       // Your trading logic here
 *
 *       the0::success("Bot executed successfully");
 *       return 0;
 *   }
 */

#ifndef THE0_H
#define THE0_H

#include <nlohmann/json.hpp>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <utility>

namespace the0 {

using json = nlohmann::json;

/**
 * Get the path to the result file.
 */
inline std::string result_file_path() {
    const char* mount = std::getenv("CODE_MOUNT_DIR");
    return std::string("/") + (mount ? mount : "bot") + "/result.json";
}

/**
 * Write result to the result file.
 */
inline void write_result(const std::string& content) {
    std::ofstream f(result_file_path());
    if (f.is_open()) {
        f << content;
        f.close();
    } else {
        std::cerr << "RESULT_ERROR: Failed to write result file" << std::endl;
    }
}

/**
 * Parse bot configuration from environment variables.
 *
 * Returns a pair of (bot_id, config) where config is a parsed JSON object.
 *
 * If BOT_ID is not set, returns empty string.
 * If BOT_CONFIG is not set or invalid, returns empty JSON object.
 */
inline std::pair<std::string, json> parse() {
    const char* id = std::getenv("BOT_ID");
    const char* config_str = std::getenv("BOT_CONFIG");

    json config = json::object();
    if (config_str) {
        try {
            config = json::parse(config_str);
        } catch (const json::parse_error&) {
            // Invalid JSON, use empty object
        }
    }

    return {
        id ? std::string(id) : std::string(),
        config
    };
}

/**
 * Parse bot configuration, returning raw config string.
 *
 * Use this if you want to parse the JSON yourself or use a different library.
 */
inline std::pair<std::string, std::string> parse_raw() {
    const char* id = std::getenv("BOT_ID");
    const char* config = std::getenv("BOT_CONFIG");
    return {
        id ? std::string(id) : std::string(),
        config ? std::string(config) : std::string("{}")
    };
}

/**
 * Output a success result to the result file.
 *
 * Writes a JSON object with status "success" and the provided message.
 * The message is properly escaped for JSON.
 *
 * @param message The success message to include in the output
 */
inline void success(const std::string& message) {
    json r = {
        {"status", "success"},
        {"message", message}
    };
    write_result(r.dump());
}

/**
 * Output an error result to the result file and exit with code 1.
 *
 * Writes a JSON object with status "error" and the provided message,
 * then terminates the process with exit code 1.
 * The message is properly escaped for JSON.
 *
 * @param message The error message to include in the output
 */
[[noreturn]] inline void error(const std::string& message) {
    json r = {
        {"status", "error"},
        {"message", message}
    };
    write_result(r.dump());
    std::exit(1);
}

/**
 * Output a custom JSON result to the result file.
 *
 * @param data The JSON object to output
 */
inline void result(const json& data) {
    write_result(data.dump());
}

/**
 * Output a custom result with additional data fields.
 *
 * Creates a JSON object with status, message, and any additional fields.
 *
 * @param status The status string (e.g., "success", "error")
 * @param message The message string
 * @param data Additional data to merge into the result
 */
inline void result(const std::string& status, const std::string& message, const json& data = json::object()) {
    json output = {
        {"status", status},
        {"message", message}
    };
    // Merge additional data
    for (auto& [key, value] : data.items()) {
        output[key] = value;
    }
    write_result(output.dump());
}

/**
 * Get current timestamp as milliseconds since epoch.
 */
inline std::string current_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()).count();
    return std::to_string(ms) + "Z";
}

/**
 * Emit a metric to stdout.
 *
 * Outputs a JSON object with the _metric field and timestamp.
 *
 * @param metric_type The type of metric (e.g., "price", "signal")
 * @param data The metric data
 */
inline void metric(const std::string& metric_type, json data) {
    data["_metric"] = metric_type;
    data["timestamp"] = current_timestamp();
    std::cout << data.dump() << std::endl;
}

/**
 * Log levels supported by the platform.
 */
enum class LogLevel {
    Info,
    Warn,
    Error
};

/**
 * Convert log level to string.
 */
inline std::string log_level_str(LogLevel level) {
    switch (level) {
        case LogLevel::Warn: return "warn";
        case LogLevel::Error: return "error";
        default: return "info";
    }
}

/**
 * Log a structured message to stderr.
 *
 * Outputs a JSON object with level, message, timestamp, and any additional fields.
 *
 * Examples:
 *   // Simple log (defaults to info level)
 *   the0::log("Starting trade execution");
 *
 *   // Log with level
 *   the0::log("Connection lost", {}, LogLevel::Warn);
 *
 *   // Log with structured data
 *   the0::log("Order placed", {{"order_id", "12345"}, {"symbol", "BTC/USD"}});
 *
 *   // Log with data and level
 *   the0::log("Order failed", {{"order_id", "12345"}}, LogLevel::Error);
 *
 * @param message The message to log
 * @param data Optional JSON object with additional fields
 * @param level Optional log level (defaults to Info)
 */
inline void log(const std::string& message, const json& data = json::object(), LogLevel level = LogLevel::Info) {
    json output = data;
    output["level"] = log_level_str(level);
    output["message"] = message;
    output["timestamp"] = current_timestamp();
    std::cerr << output.dump() << std::endl;
}

} // namespace the0

#endif // THE0_H
