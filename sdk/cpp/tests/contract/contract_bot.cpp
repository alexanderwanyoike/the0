/**
 * The contract bot: plays one sdk/contract case through the SDK's public API,
 * the way a bot would. Each op makes the SDK call sdk/contract/README.md maps
 * it to. The harness (harness.cpp) starts it with the case's environment:
 *
 *   the0_contract_bot <case file> <query result path>
 */

#include <the0/the0.h>
#include <the0/query.h>
#include <the0/state.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>
#include <string>

using json = nlohmann::json;

namespace {

using Bound = std::map<std::string, json>;
using the0::query::Request;

// A mistake in the case or in this bot, never an outcome of the case.
// std::_Exit skips the core dump an abort would leave.
[[noreturn]] void panic(const std::string& message) {
    std::cerr << "contract_bot: " << message << std::endl;
    std::_Exit(70);
}

json resolve(const json& value, const Bound& bound) {
    if (value.is_object() && value.size() == 1 && value.contains("$ref")) {
        const std::string name = value["$ref"].get<std::string>();
        auto it = bound.find(name);
        if (it == bound.end()) {
            panic("nothing is bound to " + name);
        }
        return it->second;
    }
    if (value.is_object()) {
        json resolved = json::object();
        for (const auto& [key, item] : value.items()) {
            resolved[key] = resolve(item, bound);
        }
        return resolved;
    }
    if (value.is_array()) {
        json resolved = json::array();
        for (const auto& item : value) {
            resolved.push_back(resolve(item, bound));
        }
        return resolved;
    }
    return value;
}

the0::LogLevel log_level(const std::string& level) {
    if (level == "info") return the0::LogLevel::Info;
    if (level == "warn") return the0::LogLevel::Warn;
    if (level == "error") return the0::LogLevel::Error;
    panic("unknown log level " + level);
}

const Request& handler_request(const Request* request, const std::string& op) {
    if (request == nullptr) {
        panic("op " + op + " is only valid inside a query handler");
    }
    return *request;
}

json perform(const json& step, const Request* request, const Bound& bound) {
    if (!step.contains("op") || !step["op"].is_string()) {
        panic("a step needs an op");
    }
    const std::string op = step["op"].get<std::string>();
    json args = json::object();
    for (const auto& [key, value] : step.items()) {
        if (key != "op" && key != "as" && key != "catch") {
            args[key] = resolve(value, bound);
        }
    }
    auto text = [&](const std::string& name) -> std::string {
        if (!args.contains(name) || !args[name].is_string()) {
            panic("op " + op + " needs a string " + name);
        }
        return args[name].get<std::string>();
    };

    if (op == "parse") {
        auto [bot_id, config] = the0::parse();
        return {{"bot_id", bot_id}, {"config", config}};
    }
    if (op == "success") {
        if (args.contains("data")) {
            throw std::runtime_error("this SDK's success() takes no data");
        }
        the0::success(text("message"));
        return nullptr;
    }
    if (op == "error") {
        if (args.contains("data")) {
            throw std::runtime_error("this SDK's error() takes no data");
        }
        the0::error(text("message"));
    }
    if (op == "result") {
        the0::result(args["value"]);
        return nullptr;
    }
    if (op == "metric") {
        the0::metric(text("type"), args["data"]);
        return nullptr;
    }
    if (op == "log") {
        json data = args.contains("data") ? args["data"] : json::object();
        auto level = args.contains("level") ? log_level(text("level")) : the0::LogLevel::Info;
        the0::log(text("message"), data, level);
        return nullptr;
    }
    if (op == "raise") {
        throw std::runtime_error(text("message"));
    }

    if (op == "state.get") {
        if (args.contains("default")) {
            return the0::state::get_or(text("key"), args["default"]);
        }
        return the0::state::get(text("key")).value_or(json(nullptr));
    }
    if (op == "state.set") {
        the0::state::set(text("key"), args["value"]);
        return nullptr;
    }
    if (op == "state.delete") {
        return the0::state::remove(text("key"));
    }
    if (op == "state.list") {
        return the0::state::list();
    }
    if (op == "state.exists") {
        return the0::state::exists(text("key"));
    }
    if (op == "state.clear") {
        the0::state::clear();
        return nullptr;
    }

    if (op == "query.params") {
        return handler_request(request, op).params;
    }
    if (op == "query.param") {
        const Request& req = handler_request(request, op);
        const std::string key = text("key");
        if (req.has(key)) {
            return req.get(key);
        }
        return args.contains("default") ? args["default"] : json(nullptr);
    }
    if (op == "query.config") {
        return the0::query::get_config();
    }

    panic("unknown op " + op);
}

json run_steps(const json& steps, const Request* request) {
    Bound bound;
    for (const auto& step : steps) {
        if (step.value("op", "") == "return") {
            return resolve(step["value"], bound);
        }
        json value;
        if (step.contains("catch")) {
            const std::string kind = step["catch"].get<std::string>();
            if (kind != "any" && kind != "read_only_state") {
                panic("unknown catch kind " + kind);
            }
            try {
                value = perform(step, request, bound);
            } catch (const the0::query::ReadOnlyStateError&) {
                // The query module's type, which must be the one state fails with.
                value = {{"caught", kind}};
            } catch (...) {
                if (kind != "any") {
                    throw;
                }
                value = {{"caught", kind}};
            }
        } else {
            value = perform(step, request, bound);
        }
        if (step.contains("as")) {
            bound[step["as"].get<std::string>()] = value;
        }
    }
    return nullptr;
}

} // namespace

int main(int argc, char** argv) {
    if (argc != 3) {
        panic("usage: the0_contract_bot <case file> <query result path>");
    }
    std::ifstream case_file(argv[1]);
    if (!case_file) {
        panic(std::string("cannot read the case ") + argv[1]);
    }
    const json test_case = json::parse(case_file);

    the0::query::set_result_path(argv[2]);

    if (test_case.value("entrypoint", "") == "bot") {
        try {
            run_steps(test_case.value("script", json::array()), nullptr);
        } catch (const std::exception& e) {
            // An exception that leaves a bot's main ends it in std::terminate,
            // which exits non-zero; this exits 1 without leaving a core dump.
            std::cerr << "uncaught exception: " << e.what() << std::endl;
            return 1;
        }
        return 0;
    }

    if (!test_case.contains("handlers") || !test_case["handlers"].is_object()) {
        panic("a query case needs handlers");
    }
    for (const auto& [path, steps] : test_case["handlers"].items()) {
        json handler_steps = steps;
        the0::query::handler(path, [handler_steps](const Request& req) {
            return run_steps(handler_steps, &req);
        });
    }
    the0::query::run();
    return 0;
}
