/**
 * Runs the bot contract cases in sdk/contract/cases against this SDK.
 *
 * The case format and the matching rules are in sdk/contract/README.md. Each
 * case runs the contract bot (contract_bot.cpp) in a sandbox of its own.
 *
 *   the0_contract_test                         every case
 *   the0_contract_test <case file>...          those cases
 *   the0_contract_test --check-known-failures  the guards on KNOWN_FAILURES
 *
 * ctest registers one test per case file (tests/CMakeLists.txt).
 *
 * The cases describe a Linux container (absolute paths, SIGTERM, signal exit
 * statuses), so the harness is POSIX only.
 */

#include <nlohmann/json.hpp>

#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <spawn.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <unistd.h>

#include <algorithm>
#include <cerrno>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace fs = std::filesystem;
using json = nlohmann::json;
using Clock = std::chrono::steady_clock;

namespace {

// Cases this SDK does not pass yet, each with the reason; sdk/contract/README.md
// says how the list is kept.
const std::map<std::string, std::string> KNOWN_FAILURES = {
    {"query-ephemeral-params", "every param is turned into a string"},
    {"query-ephemeral-params-not-object", "a JSON array gives params named after its indexes"},
    {"query-server-params-json-body", "the server reads params from the query string only"},
    {"query-server-unknown-path", "the 404 does not list the available paths"},
};

constexpr auto RUN_TIMEOUT = std::chrono::seconds(60);
constexpr auto SERVER_START_TIMEOUT = std::chrono::seconds(30);
constexpr auto STOP_TIMEOUT = std::chrono::seconds(10);
// sdk/contract/README.md, step 5, says why a server case may need a fresh port.
constexpr int PORT_ATTEMPTS = 3;

#ifdef MSG_NOSIGNAL
constexpr int SEND_FLAGS = MSG_NOSIGNAL;
#else
constexpr int SEND_FLAGS = 0;
#endif

const fs::path CASES_DIR = THE0_CONTRACT_CASES_DIR;
const fs::path CPP_CASES_DIR = THE0_CPP_CASES_DIR;
const char* const CONTRACT_BOT = THE0_CONTRACT_BOT;

std::vector<fs::path> json_files(const fs::path& dir) {
    std::vector<fs::path> paths;
    std::error_code ec;
    for (const auto& entry : fs::directory_iterator(dir, ec)) {
        if (entry.path().extension() == ".json") {
            paths.push_back(entry.path());
        }
    }
    std::sort(paths.begin(), paths.end());
    return paths;
}

json read_json(const fs::path& path) {
    std::ifstream file(path);
    if (!file) {
        throw std::runtime_error("cannot read " + path.string());
    }
    return json::parse(file);
}

std::string replace_all(std::string text, const std::string& from, const std::string& to) {
    for (size_t at = text.find(from); at != std::string::npos; at = text.find(from, at + to.size())) {
        text.replace(at, from.size(), to);
    }
    return text;
}

// --- Sandbox -----------------------------------------------------------------

/** The directories the runtime gives a bot, removed on destruction. */
class Sandbox {
public:
    Sandbox(const json& test_case, std::optional<int> port) : port_(port) {
        std::string root = (fs::temp_directory_path() / "the0-contract-XXXXXX").string();
        if (mkdtemp(root.data()) == nullptr) {
            throw std::runtime_error("cannot make a sandbox");
        }
        root_ = root;
        fs::create_directories(code_dir());
        fs::create_directories(query_result().parent_path());
        if (test_case.contains("state_before")) {
            fs::create_directories(state_dir());
            for (const auto& [key, value] : test_case["state_before"].items()) {
                std::ofstream(state_dir() / (key + ".json")) << value.dump();
            }
        }
    }

    ~Sandbox() {
        std::error_code ec;
        fs::remove_all(root_, ec);
    }

    Sandbox(const Sandbox&) = delete;
    Sandbox& operator=(const Sandbox&) = delete;

    fs::path code_dir() const { return root_ / "bot"; }
    fs::path state_dir() const { return root_ / "state" / ".the0-state"; }
    fs::path query_result() const { return root_ / "query" / "result.json"; }
    std::optional<int> port() const { return port_; }

    std::vector<std::string> env(const json& test_case) const {
        std::string code_mount_dir = code_dir().string();
        code_mount_dir.erase(0, code_mount_dir.find_first_not_of('/'));
        const std::vector<std::pair<std::string, std::string>> placeholders = {
            {"{{code_mount_dir}}", code_mount_dir},
            {"{{state_dir}}", state_dir().string()},
            {"{{port}}", port_ ? std::to_string(*port_) : ""},
        };
        std::vector<std::string> env;
        for (const auto& [name, value] : test_case.at("env").items()) {
            std::string text = value.get<std::string>();
            for (const auto& [placeholder, actual] : placeholders) {
                text = replace_all(text, placeholder, actual);
            }
            env.push_back(name + "=" + text);
        }
        return env;
    }

private:
    fs::path root_;
    std::optional<int> port_;
};

// --- The bot process ---------------------------------------------------------

json exit_code_of(int status) {
    if (WIFEXITED(status)) {
        return WEXITSTATUS(status);
    }
    // A process ended by a signal gets the negated signal number, as Python
    // reports it, so `nonzero` still matches.
    if (WIFSIGNALED(status)) {
        return -WTERMSIG(status);
    }
    return nullptr;
}

class Child {
public:
    Child(const std::vector<std::string>& args, const std::vector<std::string>& env) {
        int out[2];
        int err[2];
        if (pipe(out) != 0 || pipe(err) != 0) {
            throw std::runtime_error("cannot make pipes");
        }
        for (int fd : {out[0], out[1], err[0], err[1]}) {
            fcntl(fd, F_SETFD, FD_CLOEXEC);
        }

        posix_spawn_file_actions_t actions;
        posix_spawn_file_actions_init(&actions);
        posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0);
        posix_spawn_file_actions_adddup2(&actions, out[1], 1);
        posix_spawn_file_actions_adddup2(&actions, err[1], 2);

        // The bot starts with every signal at its default action, as in a
        // container, whatever the test runner ignores or blocks.
        posix_spawnattr_t attributes;
        posix_spawnattr_init(&attributes);
        sigset_t all;
        sigset_t none;
        sigfillset(&all);
        sigemptyset(&none);
        posix_spawnattr_setsigdefault(&attributes, &all);
        posix_spawnattr_setsigmask(&attributes, &none);
        posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK);

        std::vector<char*> argv;
        for (const auto& arg : args) argv.push_back(const_cast<char*>(arg.c_str()));
        argv.push_back(nullptr);
        std::vector<char*> envp;
        for (const auto& entry : env) envp.push_back(const_cast<char*>(entry.c_str()));
        envp.push_back(nullptr);

        int spawned = posix_spawn(&pid_, argv[0], &actions, &attributes, argv.data(), envp.data());
        posix_spawn_file_actions_destroy(&actions);
        posix_spawnattr_destroy(&attributes);
        close(out[1]);
        close(err[1]);
        if (spawned != 0) {
            close(out[0]);
            close(err[0]);
            throw std::runtime_error(std::string("cannot start the bot: ") + std::strerror(spawned));
        }
        stdout_reader_ = std::thread(capture, out[0], &stdout_);
        stderr_reader_ = std::thread(capture, err[0], &stderr_);
    }

    ~Child() {
        kill();
        join_output();
    }

    Child(const Child&) = delete;
    Child& operator=(const Child&) = delete;

    bool try_wait() {
        if (status_) return true;
        int status = 0;
        if (waitpid(pid_, &status, WNOHANG) == pid_) {
            status_ = status;
            return true;
        }
        return false;
    }

    bool wait_until(Clock::time_point deadline) {
        while (!try_wait()) {
            if (Clock::now() >= deadline) return false;
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        return true;
    }

    void kill() {
        if (try_wait()) return;
        ::kill(pid_, SIGKILL);
        int status = 0;
        waitpid(pid_, &status, 0);
        status_ = status;
    }

    /** Stops a query server the way the runtime does, with SIGTERM. */
    void stop() {
        if (try_wait()) return;
        ::kill(pid_, SIGTERM);
        if (!wait_until(Clock::now() + STOP_TIMEOUT)) kill();
    }

    json exit_code() const { return status_ ? exit_code_of(*status_) : json(nullptr); }

    /** Waits for both streams to close; call once the process has ended. */
    void join_output() {
        if (stdout_reader_.joinable()) stdout_reader_.join();
        if (stderr_reader_.joinable()) stderr_reader_.join();
    }

    const std::string& out() const { return stdout_; }
    const std::string& err() const { return stderr_; }

private:
    static void capture(int fd, std::string* into) {
        char buffer[4096];
        while (true) {
            ssize_t n = read(fd, buffer, sizeof(buffer));
            if (n > 0) {
                into->append(buffer, static_cast<size_t>(n));
            } else if (n == 0 || errno != EINTR) {
                break;
            }
        }
        close(fd);
    }

    pid_t pid_ = -1;
    std::optional<int> status_;
    std::string stdout_;
    std::string stderr_;
    std::thread stdout_reader_;
    std::thread stderr_reader_;
};

// --- HTTP --------------------------------------------------------------------

int free_port() {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    sockaddr_in address{};
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    address.sin_port = 0;
    socklen_t length = sizeof(address);
    if (fd < 0 || bind(fd, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0 ||
        getsockname(fd, reinterpret_cast<sockaddr*>(&address), &length) != 0) {
        throw std::runtime_error("cannot find a free port");
    }
    close(fd);
    return ntohs(address.sin_port);
}

/** A connected socket, or -1. */
int connect_to(int port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    fcntl(fd, F_SETFD, FD_CLOEXEC);
    sockaddr_in address{};
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    address.sin_port = htons(static_cast<uint16_t>(port));
    if (connect(fd, reinterpret_cast<sockaddr*>(&address), sizeof(address)) != 0) {
        close(fd);
        return -1;
    }
    return fd;
}

std::optional<std::string> wait_for_port(int port, Child& child) {
    const auto deadline = Clock::now() + SERVER_START_TIMEOUT;
    while (Clock::now() < deadline) {
        if (child.try_wait()) {
            return "query server exited with " + child.exit_code().dump() +
                   " before accepting connections";
        }
        int fd = connect_to(port);
        if (fd >= 0) {
            close(fd);
            return std::nullopt;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    return "query server did not accept connections within " +
           std::to_string(SERVER_START_TIMEOUT.count()) + "s";
}

std::string lowercase(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return text;
}

std::string trim(const std::string& text) {
    const auto first = text.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) return "";
    const auto last = text.find_last_not_of(" \t\r\n");
    return text.substr(first, last - first + 1);
}

struct Response {
    int status = 0;
    std::map<std::string, std::string> headers;
    std::string body;
};

std::optional<Response> parse_response(const std::string& raw, std::string& error) {
    const auto head_end = raw.find("\r\n\r\n");
    if (head_end == std::string::npos) {
        error = "not an HTTP response: " + raw;
        return std::nullopt;
    }
    std::istringstream head(raw.substr(0, head_end));
    std::string line;
    std::getline(head, line);
    Response response;
    std::istringstream status_line(line);
    std::string version;
    if (!(status_line >> version >> response.status)) {
        error = "no status line: " + line;
        return std::nullopt;
    }
    while (std::getline(head, line)) {
        const auto colon = line.find(':');
        if (colon != std::string::npos) {
            response.headers[lowercase(trim(line.substr(0, colon)))] = trim(line.substr(colon + 1));
        }
    }
    response.body = raw.substr(head_end + 4);
    auto length = response.headers.find("content-length");
    if (length != response.headers.end()) {
        try {
            size_t n = std::stoul(length->second);
            if (n <= response.body.size()) response.body.resize(n);
        } catch (const std::exception&) {
        }
    }
    return response;
}

/** One request on a new connection, then the whole response. */
std::optional<Response> send_request(int port, const json& request, std::string& error) {
    const std::string method = request.at("method").get<std::string>();
    const std::string target = request.at("target").get<std::string>();
    std::string body;
    std::optional<std::string> content_type;
    if (request.contains("body")) {
        body = request["body"].dump();
        content_type = "application/json";
    } else if (request.contains("raw_body")) {
        body = request["raw_body"].get<std::string>();
        content_type = request.value("content_type", "text/plain");
    }

    std::string message = method + " " + target + " HTTP/1.1\r\nHost: 127.0.0.1:" +
                          std::to_string(port) + "\r\nConnection: close\r\n";
    if (content_type) message += "Content-Type: " + *content_type + "\r\n";
    if (!body.empty() || method == "POST") {
        message += "Content-Length: " + std::to_string(body.size()) + "\r\n";
    }
    message += "\r\n" + body;

    int fd = connect_to(port);
    if (fd < 0) {
        error = std::string("cannot connect: ") + std::strerror(errno);
        return std::nullopt;
    }
    timeval timeout{};
    timeout.tv_sec = std::chrono::duration_cast<std::chrono::seconds>(RUN_TIMEOUT).count();
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));

    size_t sent = 0;
    while (sent < message.size()) {
        ssize_t n = send(fd, message.data() + sent, message.size() - sent, SEND_FLAGS);
        if (n <= 0) {
            if (n < 0 && errno == EINTR) continue;
            // The server may answer before reading a body it does not want;
            // its response is still worth reading.
            break;
        }
        sent += static_cast<size_t>(n);
    }

    std::string raw;
    char buffer[4096];
    while (true) {
        ssize_t n = recv(fd, buffer, sizeof(buffer), 0);
        if (n > 0) {
            raw.append(buffer, static_cast<size_t>(n));
        } else if (n < 0 && errno == EINTR) {
            continue;
        } else {
            // A server that closes without reading the whole request may reset
            // the connection after it has sent its response.
            if (n < 0 && raw.empty()) {
                error = std::string("no response: ") + std::strerror(errno);
                close(fd);
                return std::nullopt;
            }
            break;
        }
    }
    close(fd);
    return parse_response(raw, error);
}

// --- Matching ----------------------------------------------------------------

std::vector<std::string> mismatches(const json& expected, const json& actual, const std::string& at);

/** `^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$` */
bool is_timestamp(const json& value) {
    static const std::regex pattern(R"(^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$)");
    return value.is_string() && std::regex_match(value.get<std::string>(), pattern);
}

bool unordered_matches(const json& items, const json& actual) {
    if (!actual.is_array() || actual.size() != items.size()) return false;
    std::vector<json> remaining(actual.begin(), actual.end());
    for (const auto& item : items) {
        auto found = std::find_if(remaining.begin(), remaining.end(), [&](const json& got) {
            return mismatches(item, got, "").empty();
        });
        if (found == remaining.end()) return false;
        remaining.erase(found);
    }
    return true;
}

/** Whether `actual` matches a `{"$match": ...}` matcher; throws for an unknown one. */
bool matches(const json& matcher, const json& actual) {
    const std::string kind = matcher.at("$match").get<std::string>();
    if (kind == "timestamp") return is_timestamp(actual);
    if (kind == "string") return actual.is_string();
    if (kind == "contains") {
        return actual.is_string() &&
               actual.get<std::string>().find(matcher.at("value").get<std::string>()) != std::string::npos;
    }
    if (kind == "nonzero") return actual.is_number() && actual.get<double>() != 0.0;
    if (kind == "unordered") return unordered_matches(matcher.at("items"), actual);
    throw std::invalid_argument("unknown matcher " + kind);
}

/** Every way `actual` fails to match `expected`, as readable lines. */
std::vector<std::string> mismatches(const json& expected, const json& actual, const std::string& at) {
    auto differ = [&] {
        return std::vector<std::string>{at + ": expected " + expected.dump() + ", got " + actual.dump()};
    };
    if (expected.is_object() && expected.contains("$match")) {
        try {
            return matches(expected, actual) ? std::vector<std::string>{} : differ();
        } catch (const std::invalid_argument& unknown) {
            return {at + ": " + unknown.what()};
        }
    }
    if (expected.is_object()) {
        if (!actual.is_object()) return {at + ": expected an object, got " + actual.dump()};
        std::vector<std::string> problems;
        for (const auto& [key, want] : expected.items()) {
            if (!actual.contains(key)) {
                problems.push_back(at + "." + key + ": missing");
            } else {
                auto more = mismatches(want, actual[key], at + "." + key);
                problems.insert(problems.end(), more.begin(), more.end());
            }
        }
        for (const auto& [key, got] : actual.items()) {
            if (!expected.contains(key)) {
                problems.push_back(at + "." + key + ": unexpected, got " + got.dump());
            }
        }
        return problems;
    }
    if (expected.is_array()) {
        if (!actual.is_array() || actual.size() != expected.size()) return differ();
        std::vector<std::string> problems;
        for (size_t i = 0; i < expected.size(); ++i) {
            auto more = mismatches(expected[i], actual[i], at + "[" + std::to_string(i) + "]");
            problems.insert(problems.end(), more.begin(), more.end());
        }
        return problems;
    }
    if (expected.is_number()) {
        if (actual.is_number() && actual.get<double>() == expected.get<double>()) return {};
        return differ();
    }
    if (expected.type() == actual.type() && expected == actual) return {};
    return differ();
}

std::vector<std::string> line_mismatches(const json& expected, const std::string& text,
                                         const std::string& stream) {
    std::vector<std::string> lines;
    std::istringstream input(text);
    std::string line;
    while (std::getline(input, line)) {
        if (!trim(line).empty()) lines.push_back(line);
    }
    if (lines.size() != expected.size()) {
        return {stream + ": expected " + std::to_string(expected.size()) + " lines, got " +
                std::to_string(lines.size())};
    }
    std::vector<std::string> problems;
    for (size_t i = 0; i < lines.size(); ++i) {
        const std::string at = stream + "[" + std::to_string(i) + "]";
        json got = json::parse(lines[i], nullptr, false);
        if (got.is_discarded()) {
            problems.push_back(at + ": not JSON: " + lines[i]);
            continue;
        }
        auto more = mismatches(expected[i], got, at);
        problems.insert(problems.end(), more.begin(), more.end());
    }
    return problems;
}

std::vector<std::string> file_mismatches(const json& expected, const fs::path& path,
                                         const std::string& name) {
    if (expected.is_null()) {
        if (fs::exists(path)) return {name + ": expected no file, found one"};
        return {};
    }
    std::ifstream file(path);
    if (!file) return {name + ": no file written"};
    std::stringstream text;
    text << file.rdbuf();
    json actual = json::parse(text.str(), nullptr, false);
    if (actual.is_discarded()) return {name + ": not JSON: " + text.str()};
    return mismatches(expected, actual, name);
}

json read_state(const fs::path& state_dir) {
    json state = json::object();
    for (const auto& path : json_files(state_dir)) {
        std::ifstream file(path);
        std::stringstream text;
        text << file.rdbuf();
        json value = json::parse(text.str(), nullptr, false);
        state[path.stem().string()] = value.is_discarded() ? json(text.str()) : value;
    }
    return state;
}

// --- Running a case ----------------------------------------------------------

struct Run {
    json exit_code;
    std::string out;
    std::string err;
    // Mismatches found while the process ran, such as HTTP responses.
    std::vector<std::string> problems;
    bool exited_before_serving = false;

    std::string output() const {
        return "exit code: " + exit_code.dump() + "\n--- stdout\n" + out + "--- stderr\n" + err;
    }
};

std::vector<std::string> request_mismatches(int port, size_t index, const json& request) {
    const std::string at = "requests[" + std::to_string(index) + "] " +
                           request.at("method").get<std::string>() + " " +
                           request.at("target").get<std::string>();
    std::string error;
    auto response = send_request(port, request, error);
    if (!response) return {at + ": " + error};

    const json& expect = request.at("expect");
    auto problems = mismatches(expect.at("status"), response->status, at + " status");
    if (expect.contains("headers")) {
        for (const auto& [name, want] : expect["headers"].items()) {
            auto got = response->headers.find(lowercase(name));
            if (got == response->headers.end()) {
                problems.push_back(at + " header " + name + ": missing");
            } else {
                auto more = mismatches(want, got->second, at + " header " + name);
                problems.insert(problems.end(), more.begin(), more.end());
            }
        }
    }
    json body = json::parse(response->body, nullptr, false);
    if (body.is_discarded()) {
        problems.push_back(at + " body: not JSON: " + response->body);
    } else {
        auto more = mismatches(expect.at("body"), body, at + " body");
        problems.insert(problems.end(), more.begin(), more.end());
    }
    return problems;
}

Run run_bot(const json& test_case, const fs::path& case_path, const Sandbox& sandbox) {
    Run run;
    Child child({CONTRACT_BOT, case_path.string(), sandbox.query_result().string()},
               sandbox.env(test_case));

    if (auto port = sandbox.port()) {
        if (auto not_ready = wait_for_port(*port, child)) {
            run.exited_before_serving = child.try_wait();
            run.problems.push_back(*not_ready);
        } else {
            const json& requests = test_case.at("requests");
            for (size_t i = 0; i < requests.size(); ++i) {
                auto more = request_mismatches(*port, i, requests[i]);
                run.problems.insert(run.problems.end(), more.begin(), more.end());
            }
        }
        child.stop();
    } else if (!child.wait_until(Clock::now() + RUN_TIMEOUT)) {
        run.problems.push_back("did not exit within " + std::to_string(RUN_TIMEOUT.count()) + "s");
        child.kill();
    }

    child.join_output();
    run.exit_code = child.exit_code();
    run.out = child.out();
    run.err = child.err();
    return run;
}

std::vector<std::string> compare(const json& test_case, const Sandbox& sandbox, const Run& run) {
    std::vector<std::string> problems = run.problems;
    if (!test_case.contains("expect")) return problems;
    const json& expect = test_case["expect"];
    auto add = [&](std::vector<std::string> more) {
        problems.insert(problems.end(), more.begin(), more.end());
    };
    if (expect.contains("exit_code")) add(mismatches(expect["exit_code"], run.exit_code, "exit_code"));
    if (expect.contains("stdout")) add(line_mismatches(expect["stdout"], run.out, "stdout"));
    if (expect.contains("stderr")) add(line_mismatches(expect["stderr"], run.err, "stderr"));
    if (expect.contains("result")) {
        add(file_mismatches(expect["result"], sandbox.code_dir() / "result.json", "result"));
    }
    if (expect.contains("query_result")) {
        add(file_mismatches(expect["query_result"], sandbox.query_result(), "query_result"));
    }
    if (expect.contains("state_after")) {
        add(mismatches(expect["state_after"], read_state(sandbox.state_dir()), "state_after"));
    }
    return problems;
}

/** The mismatches, and the bot's output, of the case's last attempt. */
std::pair<std::vector<std::string>, std::string> run_case(const fs::path& case_path) {
    const json test_case = read_json(case_path);
    const bool serves_http = test_case.contains("requests");
    for (int attempt = 1;; ++attempt) {
        Sandbox sandbox(test_case, serves_http ? std::optional<int>(free_port()) : std::nullopt);
        Run run = run_bot(test_case, case_path, sandbox);
        if (run.exited_before_serving && attempt < PORT_ATTEMPTS) continue;
        return {compare(test_case, sandbox, run), run.output()};
    }
}

/** Whether the case came out as expected, with a report when it did not. */
bool check_case(const fs::path& case_path, std::string& report) {
    const std::string name = case_path.stem().string();
    std::optional<std::string> known_failure;
    std::error_code ec;
    if (fs::equivalent(case_path.parent_path(), CASES_DIR, ec)) {
        auto listed = KNOWN_FAILURES.find(name);
        if (listed != KNOWN_FAILURES.end()) known_failure = listed->second;
    }

    auto [problems, output] = run_case(case_path);
    if (known_failure) {
        if (problems.empty()) {
            report = "listed in KNOWN_FAILURES (" + *known_failure + ") but passes: take it off the list";
            return false;
        }
        return true;
    }
    if (problems.empty()) return true;
    for (const auto& problem : problems) report += problem + "\n";
    report += output;
    return false;
}

int check_known_failures() {
    const auto cases = json_files(CASES_DIR);
    if (cases.empty()) {
        std::cerr << "no contract cases in " << CASES_DIR << std::endl;
        return 1;
    }
    int unknown = 0;
    for (const auto& [name, reason] : KNOWN_FAILURES) {
        bool real = std::any_of(cases.begin(), cases.end(),
                                [&](const fs::path& path) { return path.stem() == name; });
        if (!real) {
            std::cerr << "KNOWN_FAILURES names no such case: " << name << std::endl;
            ++unknown;
        }
    }
    return unknown == 0 ? 0 : 1;
}

} // namespace

int main(int argc, char** argv) {
    if (argc == 2 && std::string(argv[1]) == "--check-known-failures") {
        return check_known_failures();
    }

    std::vector<fs::path> paths;
    if (argc > 1) {
        paths.assign(argv + 1, argv + argc);
    } else {
        paths = json_files(CASES_DIR);
        auto cpp_cases = json_files(CPP_CASES_DIR);
        paths.insert(paths.end(), cpp_cases.begin(), cpp_cases.end());
        if (check_known_failures() != 0) return 1;
    }

    int failed = 0;
    for (const auto& path : paths) {
        std::string report;
        bool ok = false;
        try {
            ok = check_case(path, report);
        } catch (const std::exception& e) {
            report = std::string("harness error: ") + e.what();
        }
        std::cout << (ok ? "ok      " : "FAILED  ") << path.stem().string() << std::endl;
        if (!ok) {
            std::cout << report << std::endl;
            ++failed;
        }
    }
    std::cout << paths.size() - static_cast<size_t>(failed) << " passed, " << failed << " failed"
              << std::endl;
    return failed == 0 ? 0 : 1;
}
