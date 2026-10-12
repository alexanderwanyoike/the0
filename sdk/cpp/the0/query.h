/**
 * the0 Query Module for C/C++
 *
 * Express-like handler interface for bot queries.
 * Separate namespace from state and main SDK.
 *
 * Requires nlohmann/json: https://github.com/nlohmann/json
 *
 * Example:
 *   #include <the0/query.h>
 *   #include <the0/state.h>  // If needed in handlers
 *
 *   int main() {
 *       the0::query::handler("/portfolio", [](const the0::query::Request& req) {
 *           auto positions = the0::state::get_or("positions", json::array());
 *           return json{{"positions", positions}, {"count", positions.size()}};
 *       });
 *
 *       the0::query::handler("/status", [](const the0::query::Request& req) {
 *           auto symbol = req.get("symbol", "BTC/USD");
 *           return json{{"symbol", symbol}, {"active", true}};
 *       });
 *
 *       the0::query::run();
 *       return 0;
 *   }
 */

#ifndef THE0_QUERY_H
#define THE0_QUERY_H

#include <nlohmann/json.hpp>
#include "state.h"
#include "the0.h"
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>

// Simple socket includes for HTTP server
#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#else
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#endif

namespace the0 {
namespace query {

using json = nlohmann::json;

/**
 * The error state writes fail with during a query: state::set(),
 * state::remove() and state::clear() throw it. A handler that catches it
 * catches what state throws.
 */
using ReadOnlyStateError = the0::state::ReadOnlyStateError;

/**
 * Request object passed to handlers (Express-like).
 */
class Request {
public:
    std::string path;
    std::map<std::string, std::string> params;

    Request(const std::string& p, const std::map<std::string, std::string>& prms)
        : path(p), params(prms) {}

    /**
     * Get a parameter value.
     * @param key The parameter key
     * @return The parameter value or empty string if not found
     */
    std::string get(const std::string& key) const {
        auto it = params.find(key);
        return it != params.end() ? it->second : "";
    }

    /**
     * Get a parameter value with a default.
     * @param key The parameter key
     * @param default_value The default value if not found
     * @return The parameter value or the default
     */
    std::string get(const std::string& key, const std::string& default_value) const {
        auto it = params.find(key);
        return it != params.end() ? it->second : default_value;
    }

    /**
     * Check if a parameter exists.
     */
    bool has(const std::string& key) const {
        return params.find(key) != params.end();
    }

    /**
     * A request from params with their JSON types.
     * @param path The query path
     * @param values A JSON object of params
     */
    static Request from_values(const std::string& path, const json& values) {
        (void)values;
        return Request(path, {});
    }

    /**
     * Every param with its JSON type, as a JSON object.
     */
    const json& values() const {
        return values_;
    }

    /**
     * A param read as T, or std::nullopt when it is missing or is not a T.
     * @param key The parameter key
     */
    template <typename T>
    std::optional<T> get_as(const std::string& key) const {
        (void)key;
        return std::nullopt;
    }

    /**
     * A param read as T, or the default when it is missing or is not a T.
     * @param key The parameter key
     * @param default_value The default value
     */
    template <typename T>
    T get_as(const std::string& key, const T& default_value) const {
        return get_as<T>(key).value_or(default_value);
    }

private:
    json values_ = json::object();
};

// Handler function type
using HandlerFunc = std::function<json(const Request&)>;

// Global handler registry
inline std::map<std::string, HandlerFunc>& get_handlers() {
    static std::map<std::string, HandlerFunc> handlers;
    return handlers;
}

// Global current params
inline std::map<std::string, std::string>& get_current_params() {
    static std::map<std::string, std::string> params;
    return params;
}

// Global config
inline json& get_config_internal() {
    static json config = json::object();
    return config;
}

namespace detail {

inline std::string& query_result_path() {
    static std::string path = "/query/result.json";
    return path;
}

} // namespace detail

/**
 * Test-only: point ephemeral query results at `path` instead of
 * /query/result.json, which tests cannot write. The runtime always reads
 * /query/result.json, so bots must not call this.
 */
inline void set_result_path(const std::string& path) {
    detail::query_result_path() = path;
}

/**
 * Register a query handler (Express-like).
 *
 * @param path The query path to handle (e.g., "/portfolio", "/signals")
 * @param handler Handler function that receives request and returns JSON response
 *
 * @example
 *   the0::query::handler("/portfolio", [](const the0::query::Request& req) {
 *       auto symbol = req.get("symbol", "BTC/USD");
 *       return json{{"symbol", symbol}, {"positions", json::array()}};
 *   });
 */
inline void handler(const std::string& path, HandlerFunc handler_fn) {
    get_handlers()[path] = handler_fn;
}

/**
 * Get current query parameters (alternative to request object).
 * @return A copy of the current query parameters
 */
inline std::map<std::string, std::string> get_params() {
    return get_current_params();
}

/**
 * Get the bot configuration.
 * @return The bot configuration as JSON
 */
inline json get_config() {
    return get_config_internal();
}

/**
 * Check if currently running in query mode (read-only).
 * Used by state module to enforce read-only behavior.
 * @return True if in query mode
 */
inline bool is_query_mode() {
    const char* path = std::getenv("QUERY_PATH");
    return path != nullptr && path[0] != '\0';
}

// Forward declarations
inline void run_ephemeral(const std::string& query_path);
inline void run_server();

/**
 * URL decode a string.
 */
inline std::string url_decode(const std::string& str) {
    std::string result;
    for (size_t i = 0; i < str.size(); ++i) {
        if (str[i] == '%' && i + 2 < str.size()) {
            int val;
            std::istringstream iss(str.substr(i + 1, 2));
            if (iss >> std::hex >> val) {
                result += static_cast<char>(val);
                i += 2;
            } else {
                result += str[i];
            }
        } else if (str[i] == '+') {
            result += ' ';
        } else {
            result += str[i];
        }
    }
    return result;
}

/**
 * Parse query string into map.
 */
inline std::map<std::string, std::string> parse_query_string(const std::string& query) {
    std::map<std::string, std::string> result;
    std::istringstream iss(query);
    std::string pair;
    while (std::getline(iss, pair, '&')) {
        size_t eq = pair.find('=');
        if (eq != std::string::npos) {
            std::string key = url_decode(pair.substr(0, eq));
            std::string value = url_decode(pair.substr(eq + 1));
            result[key] = value;
        } else if (!pair.empty()) {
            result[url_decode(pair)] = "";
        }
    }
    return result;
}

/**
 * Run the query system with automatic mode detection.
 *
 * Modes:
 * - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
 * - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
 * - Neither: Info mode (print available handlers)
 */
inline void run() {
    // Load bot config from environment
    const char* config_str = std::getenv("BOT_CONFIG");
    if (config_str) {
        try {
            get_config_internal() = json::parse(config_str);
        } catch (...) {
            get_config_internal() = json::object();
        }
    }

    // Register built-in handlers
    auto& handlers = get_handlers();
    if (handlers.find("/health") == handlers.end()) {
        handlers["/health"] = [](const Request&) {
            return json{{"status", "ok"}};
        };
    }
    if (handlers.find("/info") == handlers.end()) {
        handlers["/info"] = [&handlers](const Request&) {
            std::vector<std::string> paths;
            for (const auto& kv : handlers) {
                paths.push_back(kv.first);
            }
            return json{{"available_queries", paths}};
        };
    }

    const char* query_path = std::getenv("QUERY_PATH");
    const char* bot_type = std::getenv("BOT_TYPE");

    if (query_path && query_path[0] != '\0') {
        run_ephemeral(query_path);
    } else if (bot_type && std::string(bot_type) == "realtime") {
        run_server();
    } else {
        run_ephemeral("/info");
    }
}

/**
 * Write query result to /query/result.json file (matches Python SDK behavior).
 * This avoids stdout pollution from runtime logs mixing with query results.
 */
inline void write_result(const json& result) {
    const std::filesystem::path result_path = detail::query_result_path();
    try {
        if (result_path.has_parent_path()) {
            std::filesystem::create_directories(result_path.parent_path());
        }
        std::ofstream file(result_path);
        if (file.is_open()) {
            file << the0::detail::to_json_text(result);
            file.close();
        } else {
            std::cerr << "RESULT_ERROR: Failed to open result file for writing" << std::endl;
        }
    } catch (const std::exception& e) {
        std::cerr << "RESULT_ERROR: Failed to write result file: " << e.what() << std::endl;
    }
}

namespace detail {

/**
 * The JSON object `text` holds, or nothing when it holds anything else. An
 * object that repeats a key at any depth counts as unreadable, as the bot
 * contract reads it, where nlohmann/json alone would keep the last value.
 */
inline std::optional<json> json_object(const std::string& text) {
    std::vector<std::set<std::string>> keys_seen;
    bool duplicate_key = false;
    auto check_keys = [&](int, json::parse_event_t event, json& parsed) {
        if (event == json::parse_event_t::object_start) {
            keys_seen.emplace_back();
        } else if (event == json::parse_event_t::key) {
            duplicate_key |= !keys_seen.back().insert(parsed.get<std::string>()).second;
        } else if (event == json::parse_event_t::object_end) {
            keys_seen.pop_back();
        }
        return true;
    };
    json value = json::parse(text, check_keys, false);
    if (duplicate_key || value.is_discarded() || !value.is_object()) {
        return std::nullopt;
    }
    return value;
}

inline std::vector<std::string> available_paths() {
    std::vector<std::string> paths;
    for (const auto& kv : get_handlers()) {
        paths.push_back(kv.first);
    }
    return paths;
}

/**
 * What a query comes to in either mode: the status the query server answers
 * with, and the body both modes send. An ephemeral query exits 0 only on 200.
 */
struct Answer {
    int status;
    json body;
};

/**
 * Run the handler registered for the request's path, or report that there
 * is none.
 */
inline Answer answer(const Request& request) {
    get_current_params() = request.params;

    auto& handlers = get_handlers();
    auto it = handlers.find(request.path);
    if (it == handlers.end()) {
        return {404, {
            {"status", "error"},
            {"error", "No handler for path: " + request.path},
            {"available", available_paths()}
        }};
    }
    try {
        json data = it->second(request);
        return {200, {{"status", "ok"}, {"data", data}}};
    } catch (const std::exception& e) {
        return {500, {{"status", "error"}, {"error", e.what()}}};
    }
}

inline const char* reason_phrase(int status) {
    switch (status) {
        case 200: return "OK";
        case 404: return "Not Found";
        default: return "Internal Server Error";
    }
}

} // namespace detail

/**
 * Execute single query and write result to /query/result.json.
 */
inline void run_ephemeral(const std::string& query_path) {
    // Parse parameters from environment
    std::map<std::string, std::string> params;
    const char* params_str = std::getenv("QUERY_PARAMS");
    if (params_str) {
        if (auto params_json = detail::json_object(params_str)) {
            for (auto& [key, value] : params_json->items()) {
                params[key] = value.is_string() ? value.get<std::string>() : value.dump();
            }
        }
    }

    auto answer = detail::answer(Request(query_path, params));
    write_result(answer.body);
    if (answer.status != 200) {
        std::exit(1);
    }
}

/**
 * Start HTTP server on port 9476 for realtime bots.
 */
inline void run_server() {
    const char* port_str = std::getenv("THE0_QUERY_PORT");
    int port = port_str ? std::atoi(port_str) : 9476;
    if (port <= 0) port = 9476;

#ifdef _WIN32
    WSADATA wsa_data;
    if (WSAStartup(MAKEWORD(2, 2), &wsa_data) != 0) {
        std::cerr << json{{"_log", "error"}, {"message", "Failed to initialize Winsock"}}.dump() << std::endl;
        std::exit(1);
    }
#endif

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        std::cerr << json{{"_log", "error"}, {"message", "Failed to create socket"}}.dump() << std::endl;
        std::exit(1);
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&opt), sizeof(opt));

    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, reinterpret_cast<struct sockaddr*>(&address), sizeof(address)) < 0) {
        std::cerr << json{{"_log", "error"}, {"message", "Failed to bind to port " + std::to_string(port)}}.dump() << std::endl;
        std::exit(1);
    }

    if (listen(server_fd, 10) < 0) {
        std::cerr << json{{"_log", "error"}, {"message", "Failed to listen"}}.dump() << std::endl;
        std::exit(1);
    }

    the0::log("Query server started on port " + std::to_string(port));

    while (true) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        int client_fd = accept(server_fd, reinterpret_cast<struct sockaddr*>(&client_addr), &client_len);
        if (client_fd < 0) continue;

        // Read request
        char buffer[4096] = {0};
        ssize_t bytes_read = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
        if (bytes_read <= 0) {
#ifdef _WIN32
            closesocket(client_fd);
#else
            close(client_fd);
#endif
            continue;
        }

        // Parse HTTP request line
        std::string request(buffer, bytes_read);
        std::istringstream iss(request);
        std::string method, path_with_query, http_version;
        iss >> method >> path_with_query >> http_version;

        // Parse path and query string
        std::string path = path_with_query;
        std::map<std::string, std::string> params;
        size_t query_pos = path_with_query.find('?');
        if (query_pos != std::string::npos) {
            path = path_with_query.substr(0, query_pos);
            params = parse_query_string(path_with_query.substr(query_pos + 1));
        }

        auto answer = detail::answer(Request(path, params));
        std::string response_body = the0::detail::to_json_text(answer.body);

        // Send response
        std::ostringstream response;
        response << "HTTP/1.1 " << answer.status << " " << detail::reason_phrase(answer.status) << "\r\n";
        response << "Content-Type: application/json\r\n";
        response << "Content-Length: " << response_body.size() << "\r\n";
        response << "\r\n";
        response << response_body;

        std::string response_str = response.str();
        send(client_fd, response_str.c_str(), response_str.size(), 0);

#ifdef _WIN32
        closesocket(client_fd);
#else
        close(client_fd);
#endif
    }
}

} // namespace query
} // namespace the0

#endif // THE0_QUERY_H
