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
#include <cstdlib>
#include <iostream>
#include <string>
#include <utility>

namespace the0 {

using json = nlohmann::json;

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
 * Output a success result to stdout.
 *
 * Prints a JSON object with status "success" and the provided message.
 * The message is properly escaped for JSON.
 *
 * @param message The success message to include in the output
 */
inline void success(const std::string& message) {
    json result = {
        {"status", "success"},
        {"message", message}
    };
    std::cout << result.dump() << std::endl;
}

/**
 * Output an error result to stdout and exit with code 1.
 *
 * Prints a JSON object with status "error" and the provided message,
 * then terminates the process with exit code 1.
 * The message is properly escaped for JSON.
 *
 * @param message The error message to include in the output
 */
[[noreturn]] inline void error(const std::string& message) {
    json result = {
        {"status", "error"},
        {"message", message}
    };
    std::cout << result.dump() << std::endl;
    std::exit(1);
}

/**
 * Output a custom JSON result to stdout.
 *
 * @param data The JSON object to output
 */
inline void result(const json& data) {
    std::cout << data.dump() << std::endl;
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
    std::cout << output.dump() << std::endl;
}

} // namespace the0

#endif // THE0_H
