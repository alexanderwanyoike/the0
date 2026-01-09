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
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
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
 * Error thrown when attempting to modify state during query execution.
 */
class ReadOnlyStateError : public std::runtime_error {
public:
    explicit ReadOnlyStateError(const std::string& msg)
        : std::runtime_error(msg) {}
};

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
    const std::string result_path = "/query/result.json";
    try {
        std::filesystem::create_directories("/query");
        std::ofstream file(result_path);
        if (file.is_open()) {
            file << result.dump();
            file.close();
        } else {
            std::cerr << "RESULT_ERROR: Failed to open result file for writing" << std::endl;
        }
    } catch (const std::exception& e) {
        std::cerr << "RESULT_ERROR: Failed to write result file: " << e.what() << std::endl;
    }
}

/**
 * Execute single query and write result to /query/result.json.
 */
inline void run_ephemeral(const std::string& query_path) {
    // Parse parameters from environment
    const char* params_str = std::getenv("QUERY_PARAMS");
    if (params_str) {
        try {
            auto params_json = json::parse(params_str);
            for (auto& [key, value] : params_json.items()) {
                get_current_params()[key] = value.is_string() ? value.get<std::string>() : value.dump();
            }
        } catch (...) {
            // Invalid JSON, ignore
        }
    }

    // Find and execute handler
    auto& handlers = get_handlers();
    auto it = handlers.find(query_path);
    if (it == handlers.end()) {
        std::vector<std::string> available;
        for (const auto& kv : handlers) {
            available.push_back(kv.first);
        }
        json result = {
            {"status", "error"},
            {"error", "No handler for path: " + query_path},
            {"available", available}
        };
        write_result(result);
        std::exit(1);
    }

    try {
        Request req(query_path, get_current_params());
        json data = it->second(req);
        json result = {{"status", "ok"}, {"data", data}};
        write_result(result);
    } catch (const std::exception& e) {
        json result = {{"status", "error"}, {"error", e.what()}};
        write_result(result);
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

    std::cerr << json{{"_log", "info"}, {"message", "Query server started on port " + std::to_string(port)}}.dump() << std::endl;

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

        get_current_params() = params;

        // Find and execute handler
        std::string response_body;
        std::string status;
        auto& handlers = get_handlers();
        auto it = handlers.find(path);
        if (it == handlers.end()) {
            status = "404 Not Found";
            response_body = json{{"status", "error"}, {"error", "No handler for path: " + path}}.dump();
        } else {
            try {
                Request req(path, params);
                json data = it->second(req);
                status = "200 OK";
                response_body = json{{"status", "ok"}, {"data", data}}.dump();
            } catch (const std::exception& e) {
                status = "500 Internal Server Error";
                response_body = json{{"status", "error"}, {"error", e.what()}}.dump();
            }
        }

        // Send response
        std::ostringstream response;
        response << "HTTP/1.1 " << status << "\r\n";
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
