/**
 * the0 SDK for C/C++ trading bots
 *
 * This is a header-only library for building trading bots on the0 platform.
 * Simply include this header in your project to get started.
 *
 * Example:
 *   #include "the0.h"
 *
 *   int main() {
 *       auto [bot_id, config] = the0::parse();
 *       std::cerr << "Bot " << bot_id << " starting" << std::endl;
 *
 *       // Your trading logic here
 *
 *       the0::success("Bot executed successfully");
 *       return 0;
 *   }
 */

#ifndef THE0_H
#define THE0_H

#include <cstdlib>
#include <iostream>
#include <string>
#include <utility>

namespace the0 {

namespace detail {

/**
 * Escape a string for JSON output.
 * Handles backslashes and double quotes.
 */
inline std::string escape_json(const std::string& s) {
    std::string result;
    result.reserve(s.size());
    for (char c : s) {
        switch (c) {
            case '\\': result += "\\\\"; break;
            case '"':  result += "\\\""; break;
            case '\n': result += "\\n"; break;
            case '\r': result += "\\r"; break;
            case '\t': result += "\\t"; break;
            default:   result += c; break;
        }
    }
    return result;
}

} // namespace detail

/**
 * Parse bot configuration from environment variables.
 *
 * Returns a pair of (bot_id, config) where config is the raw JSON string
 * from BOT_CONFIG environment variable.
 *
 * If BOT_ID is not set, returns empty string.
 * If BOT_CONFIG is not set, returns "{}".
 */
inline std::pair<std::string, std::string> parse() {
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
 * The message is automatically escaped for JSON.
 *
 * @param message The success message to include in the output
 */
inline void success(const std::string& message) {
    std::cout << "{\"status\":\"success\",\"message\":\""
              << detail::escape_json(message)
              << "\"}" << std::endl;
}

/**
 * Output an error result to stdout and exit with code 1.
 *
 * Prints a JSON object with status "error" and the provided message,
 * then terminates the process with exit code 1.
 * The message is automatically escaped for JSON.
 *
 * @param message The error message to include in the output
 */
[[noreturn]] inline void error(const std::string& message) {
    std::cout << "{\"status\":\"error\",\"message\":\""
              << detail::escape_json(message)
              << "\"}" << std::endl;
    std::exit(1);
}

/**
 * Output a custom JSON result to stdout.
 *
 * Prints the provided JSON string directly. The caller is responsible
 * for ensuring the string is valid JSON.
 *
 * @param json_str The JSON string to output
 */
inline void result(const std::string& json_str) {
    std::cout << json_str << std::endl;
}

} // namespace the0

#endif // THE0_H
