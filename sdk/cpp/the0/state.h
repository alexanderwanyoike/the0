/**
 * the0 State Module for C/C++
 *
 * Provides persistent state management for bots across executions.
 * State is automatically synced to MinIO storage between bot runs.
 *
 * Requires nlohmann/json: https://github.com/nlohmann/json
 *
 * Example:
 *   #include <the0/state.h>
 *
 *   int main() {
 *       // Store state
 *       the0::state::set("portfolio", {{"AAPL", 100}, {"GOOGL", 50}});
 *
 *       // Retrieve state
 *       auto portfolio = the0::state::get("portfolio");
 *
 *       // List all keys
 *       auto keys = the0::state::list();
 *
 *       // Delete a key
 *       the0::state::remove("portfolio");
 *
 *       // Clear all state
 *       the0::state::clear();
 *
 *       return 0;
 *   }
 */

#ifndef THE0_STATE_H
#define THE0_STATE_H

#include <nlohmann/json.hpp>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace the0 {
namespace state {

using json = nlohmann::json;
namespace fs = std::filesystem;

/**
 * Exception thrown for invalid state keys.
 */
class InvalidKeyError : public std::runtime_error {
public:
    explicit InvalidKeyError(const std::string& msg)
        : std::runtime_error(msg) {}
};

/**
 * Exception thrown when attempting to modify state during query execution.
 */
class ReadOnlyStateError : public std::runtime_error {
public:
    explicit ReadOnlyStateError(const std::string& msg)
        : std::runtime_error(msg) {}
};

/**
 * Check if currently running in query mode (read-only).
 */
inline bool is_query_mode() {
    const char* path = std::getenv("QUERY_PATH");
    return path != nullptr && path[0] != '\0';
}

/**
 * Check if write operations are allowed.
 * Throws ReadOnlyStateError if in query mode.
 */
inline void check_write_allowed() {
    if (is_query_mode()) {
        throw ReadOnlyStateError(
            "State modifications are not allowed during query execution. "
            "Queries are read-only. Use state::get() to read state values.");
    }
}

/**
 * Get the path to the state directory.
 */
inline std::string get_state_dir() {
    const char* dir = std::getenv("STATE_DIR");
    return dir ? std::string(dir) : "/state/.the0-state";
}

/**
 * Get the file path for a state key.
 */
inline std::string get_key_path(const std::string& key) {
    return get_state_dir() + "/" + key + ".json";
}

/**
 * Validate that a key is safe to use as a filename.
 */
inline void validate_key(const std::string& key) {
    if (key.empty()) {
        throw InvalidKeyError("State key cannot be empty");
    }
    if (key.find('/') != std::string::npos ||
        key.find('\\') != std::string::npos ||
        key.find("..") != std::string::npos) {
        throw InvalidKeyError("State key cannot contain path separators or '..'");
    }
}

/**
 * Get a value from persistent state.
 *
 * @param key The state key (alphanumeric, hyphens, underscores)
 * @return The stored JSON value, or std::nullopt if not found
 *
 * @example
 *   auto portfolio = the0::state::get("portfolio");
 *   if (portfolio) {
 *       int aapl = portfolio->value("AAPL", 0);
 *   }
 */
inline std::optional<json> get(const std::string& key) {
    validate_key(key);
    std::string filepath = get_key_path(key);
    std::ifstream f(filepath);
    if (!f.is_open()) {
        return std::nullopt;
    }
    try {
        json value;
        f >> value;
        return value;
    } catch (const json::parse_error&) {
        return std::nullopt;
    }
}

/**
 * Get a value from persistent state with a default.
 *
 * @param key The state key
 * @param default_value Default value if key doesn't exist
 * @return The stored value or default
 */
inline json get_or(const std::string& key, const json& default_value) {
    auto result = get(key);
    return result.value_or(default_value);
}

/**
 * Set a value in persistent state.
 *
 * Note: This function will throw ReadOnlyStateError if called during query execution.
 *
 * @param key The state key (alphanumeric, hyphens, underscores)
 * @param value The JSON value to store
 * @throws ReadOnlyStateError if called during query execution (queries are read-only)
 *
 * @example
 *   the0::state::set("portfolio", {{"AAPL", 100}, {"GOOGL", 50}});
 *   the0::state::set("trade_count", 42);
 */
inline void set(const std::string& key, const json& value) {
    check_write_allowed();
    validate_key(key);
    std::string state_dir = get_state_dir();
    fs::create_directories(state_dir);
    std::string filepath = get_key_path(key);
    std::ofstream f(filepath);
    if (!f.is_open()) {
        throw std::runtime_error("Failed to open state file for writing: " + filepath);
    }
    f << value.dump();
}

/**
 * Delete a key from persistent state.
 * Note: Named 'remove' since 'delete' is a C++ reserved word.
 *
 * Note: This function will throw ReadOnlyStateError if called during query execution.
 *
 * @param key The state key to delete
 * @return True if the key existed and was deleted, false otherwise
 * @throws ReadOnlyStateError if called during query execution (queries are read-only)
 *
 * @example
 *   if (the0::state::remove("old_data")) {
 *       std::cerr << "Cleaned up old data" << std::endl;
 *   }
 */
inline bool remove(const std::string& key) {
    check_write_allowed();
    validate_key(key);
    std::string filepath = get_key_path(key);
    std::error_code ec;
    return fs::remove(filepath, ec);
}

/**
 * List all keys in persistent state.
 *
 * @return Vector of state keys
 *
 * @example
 *   auto keys = the0::state::list();
 *   std::cerr << "State contains " << keys.size() << " keys" << std::endl;
 */
inline std::vector<std::string> list() {
    std::vector<std::string> keys;
    std::string state_dir = get_state_dir();

    std::error_code ec;
    if (!fs::exists(state_dir, ec)) {
        return keys;
    }

    for (const auto& entry : fs::directory_iterator(state_dir, ec)) {
        if (entry.path().extension() == ".json") {
            keys.push_back(entry.path().stem().string());
        }
    }
    return keys;
}

/**
 * Clear all state.
 * Removes all stored state keys.
 *
 * Note: This function will throw ReadOnlyStateError if called during query execution.
 *
 * @throws ReadOnlyStateError if called during query execution (queries are read-only)
 *
 * @example
 *   the0::state::clear();
 *   std::cerr << "All state cleared" << std::endl;
 */
inline void clear() {
    check_write_allowed();
    std::string state_dir = get_state_dir();

    std::error_code ec;
    if (!fs::exists(state_dir, ec)) {
        return;
    }

    for (const auto& entry : fs::directory_iterator(state_dir, ec)) {
        if (entry.path().extension() == ".json") {
            fs::remove(entry.path(), ec);
        }
    }
}

/**
 * Check if a key exists in state.
 *
 * @param key The state key to check
 * @return True if the key exists, false otherwise
 *
 * @example
 *   if (the0::state::exists("portfolio")) {
 *       auto portfolio = the0::state::get("portfolio");
 *   }
 */
inline bool exists(const std::string& key) {
    validate_key(key);
    std::string filepath = get_key_path(key);
    std::error_code ec;
    return fs::exists(filepath, ec);
}

} // namespace state
} // namespace the0

#endif // THE0_STATE_H
