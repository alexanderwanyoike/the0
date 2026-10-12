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
#include <cctype>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

// Simple socket includes for HTTP server
#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#else
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
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

namespace detail {

/**
 * `value` as a T, or nothing when it is not one. nlohmann/json alone would
 * turn 0.25 into the int 0 and true into 1, so numbers and booleans must
 * already be of the kind asked for, and integers must fit.
 */
template <typename T>
std::optional<T> json_as(const json& value) {
    if constexpr (std::is_same_v<T, bool>) {
        if (!value.is_boolean()) return std::nullopt;
    } else if constexpr (std::is_integral_v<T>) {
        if (!value.is_number_integer()) return std::nullopt;
        if (value.is_number_unsigned()) {
            if (value.get<std::uint64_t>() > static_cast<std::uint64_t>(std::numeric_limits<T>::max())) {
                return std::nullopt;
            }
        } else {
            const auto number = value.get<std::int64_t>();
            if constexpr (std::is_signed_v<T>) {
                if (number < std::numeric_limits<T>::min() || number > std::numeric_limits<T>::max()) {
                    return std::nullopt;
                }
            } else if (number < 0 || static_cast<std::uint64_t>(number) > std::numeric_limits<T>::max()) {
                return std::nullopt;
            }
        }
    } else if constexpr (std::is_floating_point_v<T>) {
        if (!value.is_number()) return std::nullopt;
    }
    try {
        return value.get<T>();
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

} // namespace detail

/**
 * Request object passed to handlers (Express-like).
 */
class Request {
public:
    std::string path;
    std::map<std::string, std::string> params;

    Request(const std::string& p, const std::map<std::string, std::string>& prms)
        : path(p), params(prms), values_(prms) {}

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
        Request request(path, {});
        if (!values.is_object()) {
            return request;
        }
        for (const auto& [key, value] : values.items()) {
            // The runtime leaves a null param out of the query string, so
            // string handlers have always seen it as absent.
            if (!value.is_null()) {
                request.params[key] =
                    value.is_string() ? value.get<std::string>() : the0::detail::to_json_text(value);
            }
        }
        request.values_ = values;
        return request;
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
        auto it = values_.find(key);
        if (it == values_.end()) {
            return std::nullopt;
        }
        if constexpr (std::is_same_v<T, std::string>) {
            if (it->is_null()) {
                return std::nullopt;
            }
            return it->is_string() ? it->get<std::string>() : the0::detail::to_json_text(*it);
        } else {
            if (auto typed = detail::json_as<T>(*it)) {
                return typed;
            }
            // Query string params are all strings, so ?limit=5 must still
            // read as a number.
            if (it->is_string()) {
                json parsed = json::parse(it->get<std::string>(), nullptr, false);
                if (!parsed.is_discarded()) {
                    return detail::json_as<T>(parsed);
                }
            }
            return std::nullopt;
        }
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
    auto hex_digit = [&](size_t at) {
        return at < str.size() && std::isxdigit(static_cast<unsigned char>(str[at]));
    };
    std::string result;
    for (size_t i = 0; i < str.size(); ++i) {
        if (str[i] == '%' && hex_digit(i + 1) && hex_digit(i + 2)) {
            result += static_cast<char>(std::stoi(str.substr(i + 1, 2), nullptr, 16));
            i += 2;
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
    // C++ lets a handler throw anything; whatever it throws must fail the
    // query rather than reach std::terminate and end the process.
    try {
        json data = it->second(request);
        return {200, {{"status", "ok"}, {"data", data}}};
    } catch (const std::exception& e) {
        return {500, {{"status", "error"}, {"error", e.what()}}};
    } catch (const char* text) {
        return {500, {{"status", "error"}, {"error", text ? text : ""}}};
    } catch (const std::string& text) {
        return {500, {{"status", "error"}, {"error", text}}};
    } catch (...) {
        return {500, {{"status", "error"}, {"error", "The handler threw something that is not a std::exception"}}};
    }
}

inline const char* reason_phrase(int status) {
    switch (status) {
        case 200: return "OK";
        case 400: return "Bad Request";
        case 404: return "Not Found";
        default: return "Internal Server Error";
    }
}

} // namespace detail

/**
 * Execute single query and write result to /query/result.json.
 */
inline void run_ephemeral(const std::string& query_path) {
    json params = json::object();
    const char* params_str = std::getenv("QUERY_PARAMS");
    if (params_str) {
        params = detail::json_object(params_str).value_or(json::object());
    }

    auto answer = detail::answer(Request::from_values(query_path, params));
    write_result(answer.body);
    if (answer.status != 200) {
        std::exit(1);
    }
}

namespace detail {

/**
 * The parts of one HTTP request the query server uses. A request the server
 * cannot read carries the reason in `error`.
 */
struct HttpRequest {
    std::string target;
    std::string body;
    std::string error;
};

// Params are small; these bound what one client can make the server hold.
constexpr std::size_t MAX_REQUEST_HEAD_BYTES = 64 * 1024;
constexpr std::size_t MAX_REQUEST_BODY_BYTES = 1024 * 1024;
// The server reads one connection at a time, so a client that stops sending
// would otherwise hold up every query after it.
constexpr int REQUEST_TIMEOUT_SECONDS = 5;

inline void limit_wait_for_request(int client_fd) {
#ifdef _WIN32
    DWORD timeout = REQUEST_TIMEOUT_SECONDS * 1000;
    setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, reinterpret_cast<const char*>(&timeout), sizeof(timeout));
#else
    timeval timeout{};
    timeout.tv_sec = REQUEST_TIMEOUT_SECONDS;
    setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
#endif
}

/** Why a recv() that returned `received` ended the request early. */
inline std::string unfinished_request(long received, const char* part) {
    if (received < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        return "The client sent no complete request within " + std::to_string(REQUEST_TIMEOUT_SECONDS) +
               " seconds";
    }
    return std::string("The request ended before its ") + part + " did";
}

inline std::string trim(const std::string& text) {
    const auto first = text.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) {
        return "";
    }
    return text.substr(first, text.find_last_not_of(" \t\r\n") - first + 1);
}

inline bool same_header_name(const std::string& name, const char* wanted) {
    return std::equal(name.begin(), name.end(), wanted, wanted + std::strlen(wanted),
                      [](char a, char b) { return std::tolower(static_cast<unsigned char>(a)) == b; });
}

/**
 * Reads one HTTP request: its head, then a body of Content-Length bytes.
 * Gives nothing when the client closes without sending anything, and a
 * request with an error when the server cannot read what it sent.
 */
inline std::optional<HttpRequest> read_request(int client_fd) {
    HttpRequest request;
    std::string received;
    char buffer[4096];
    std::size_t head_end;
    while ((head_end = received.find("\r\n\r\n")) == std::string::npos) {
        if (received.size() > MAX_REQUEST_HEAD_BYTES) {
            request.error = "The request head is too large";
            return request;
        }
        auto n = recv(client_fd, buffer, sizeof(buffer), 0);
        if (n <= 0) {
            if (received.empty()) {
                return std::nullopt;
            }
            request.error = unfinished_request(static_cast<long>(n), "head");
            return request;
        }
        received.append(buffer, static_cast<std::size_t>(n));
    }

    std::istringstream head(received.substr(0, head_end));
    std::string request_line;
    std::getline(head, request_line);
    std::istringstream request_words(request_line);
    std::string method;
    request_words >> method >> request.target;

    std::size_t content_length = 0;
    std::string header;
    while (std::getline(head, header)) {
        const auto colon = header.find(':');
        if (colon == std::string::npos) {
            continue;
        }
        const std::string name = trim(header.substr(0, colon));
        const std::string value = trim(header.substr(colon + 1));
        if (same_header_name(name, "transfer-encoding")) {
            request.error = "Transfer-Encoding is not supported; send the body with a Content-Length";
            return request;
        }
        if (same_header_name(name, "content-length")) {
            const bool digits = !value.empty() && value.size() <= 19 &&
                std::all_of(value.begin(), value.end(), [](unsigned char c) { return std::isdigit(c); });
            if (!digits) {
                request.error = "Content-Length is not a number of bytes: " + value;
                return request;
            }
            content_length = static_cast<std::size_t>(std::stoull(value));
            if (content_length > MAX_REQUEST_BODY_BYTES) {
                request.error = "The request body is too large";
                return request;
            }
        }
    }

    request.body = received.substr(head_end + 4);
    while (request.body.size() < content_length) {
        auto n = recv(client_fd, buffer, sizeof(buffer), 0);
        if (n <= 0) {
            request.error = unfinished_request(static_cast<long>(n), "body");
            return request;
        }
        request.body.append(buffer, static_cast<std::size_t>(n));
    }
    request.body.resize(content_length);
    return request;
}

/**
 * A JSON object body supplies the params with their JSON types; otherwise
 * they come from the query string.
 */
inline Request query_request(const HttpRequest& http_request) {
    const auto query_start = http_request.target.find('?');
    const std::string path = http_request.target.substr(0, query_start);
    if (auto values = json_object(http_request.body)) {
        return Request::from_values(path, *values);
    }
    std::map<std::string, std::string> params;
    if (query_start != std::string::npos) {
        params = parse_query_string(http_request.target.substr(query_start + 1));
    }
    return Request(path, params);
}

} // namespace detail

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
        the0::log("Failed to initialize Winsock", json::object(), the0::LogLevel::Error);
        std::exit(1);
    }
#endif

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        the0::log("Failed to create socket", json::object(), the0::LogLevel::Error);
        std::exit(1);
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&opt), sizeof(opt));

    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, reinterpret_cast<struct sockaddr*>(&address), sizeof(address)) < 0) {
        the0::log("Failed to bind to port " + std::to_string(port), json::object(), the0::LogLevel::Error);
        std::exit(1);
    }

    if (listen(server_fd, 10) < 0) {
        the0::log("Failed to listen", json::object(), the0::LogLevel::Error);
        std::exit(1);
    }

    the0::log("Query server started on port " + std::to_string(port));

    while (true) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        int client_fd = accept(server_fd, reinterpret_cast<struct sockaddr*>(&client_addr), &client_len);
        if (client_fd < 0) continue;

        detail::limit_wait_for_request(client_fd);
        auto http_request = detail::read_request(client_fd);
        if (!http_request) {
#ifdef _WIN32
            closesocket(client_fd);
#else
            close(client_fd);
#endif
            continue;
        }

        detail::Answer answer;
        if (http_request->error.empty()) {
            answer = detail::answer(detail::query_request(*http_request));
        } else {
            the0::log("Query server could not read a request: " + http_request->error,
                      json::object(), the0::LogLevel::Error);
            answer = {400, {{"status", "error"}, {"error", http_request->error}}};
        }
        std::string response_body = the0::detail::to_json_text(answer.body);

        // Send response
        std::ostringstream response;
        response << "HTTP/1.1 " << answer.status << " " << detail::reason_phrase(answer.status) << "\r\n";
        response << "Content-Type: application/json\r\n";
        response << "Content-Length: " << response_body.size() << "\r\n";
        response << "\r\n";
        response << response_body;

        // A client can reset the connection before its answer, and a plain
        // send() to it raises SIGPIPE, which would end the server.
#if defined(MSG_NOSIGNAL)
        const int send_flags = MSG_NOSIGNAL;
#else
        const int send_flags = 0;
#if defined(SO_NOSIGPIPE)
        int no_sigpipe = 1;
        setsockopt(client_fd, SOL_SOCKET, SO_NOSIGPIPE, &no_sigpipe, sizeof(no_sigpipe));
#endif
#endif
        std::string response_str = response.str();
        send(client_fd, response_str.c_str(), response_str.size(), send_flags);

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
