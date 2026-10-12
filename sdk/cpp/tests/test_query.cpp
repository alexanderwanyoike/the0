/**
 * Tests for the0::query module
 */

#include <doctest/doctest.h>
#include "helpers/test_helpers.hpp"
#include "../the0/query.h"
#include "../the0/state.h"

using namespace the0;
using namespace the0_test;
using json = nlohmann::json;

TEST_CASE("query::is_query_mode") {
    SUBCASE("returns false when QUERY_PATH not set") {
        unsetenv("QUERY_PATH");

        CHECK_FALSE(query::is_query_mode());
    }

    SUBCASE("returns true when QUERY_PATH is set") {
        SetEnv env("QUERY_PATH", "/portfolio");

        CHECK(query::is_query_mode());
    }

    SUBCASE("returns false when QUERY_PATH is empty") {
        SetEnv env("QUERY_PATH", "");

        CHECK_FALSE(query::is_query_mode());
    }
}

TEST_CASE("query::get_params") {
    SUBCASE("returns empty map by default") {
        auto params = query::get_params();

        CHECK(params.empty());
    }
}

TEST_CASE("query::get_config") {
    SUBCASE("returns empty object by default") {
        auto config = query::get_config();

        CHECK(config.is_object());
    }
}

TEST_CASE("query::handler registration") {
    SUBCASE("registers handler without throwing") {
        CHECK_NOTHROW(
            query::handler("/test", [](const query::Request& req) {
                return json{{"test", true}};
            })
        );
    }

    SUBCASE("can register multiple handlers") {
        CHECK_NOTHROW([&]() {
            query::handler("/one", [](const query::Request& req) {
                return json{{"id", 1}};
            });
            query::handler("/two", [](const query::Request& req) {
                return json{{"id", 2}};
            });
        }());
    }
}

TEST_CASE("query::Request") {
    SUBCASE("get returns value for existing key") {
        std::map<std::string, std::string> params = {{"symbol", "BTC/USD"}};
        query::Request req("/test", params);

        CHECK(req.get("symbol") == "BTC/USD");
    }

    SUBCASE("get returns empty for missing key") {
        std::map<std::string, std::string> params;
        query::Request req("/test", params);

        CHECK(req.get("missing") == "");
    }

    SUBCASE("get with default returns value for existing key") {
        std::map<std::string, std::string> params = {{"symbol", "BTC/USD"}};
        query::Request req("/test", params);

        CHECK(req.get("symbol", "default") == "BTC/USD");
    }

    SUBCASE("get with default returns default for missing key") {
        std::map<std::string, std::string> params;
        query::Request req("/test", params);

        CHECK(req.get("missing", "default") == "default");
    }

    SUBCASE("has returns true for existing key") {
        std::map<std::string, std::string> params = {{"symbol", "BTC/USD"}};
        query::Request req("/test", params);

        CHECK(req.has("symbol"));
    }

    SUBCASE("has returns false for missing key") {
        std::map<std::string, std::string> params;
        query::Request req("/test", params);

        CHECK_FALSE(req.has("missing"));
    }

    SUBCASE("path is accessible") {
        std::map<std::string, std::string> params;
        query::Request req("/portfolio", params);

        CHECK(req.path == "/portfolio");
    }
}

TEST_CASE("query::url_decode") {
    SUBCASE("turns a plus into a space and percent escapes into bytes") {
        CHECK(query::url_decode("two+words") == "two words");
        CHECK(query::url_decode("BTC%2FUSD") == "BTC/USD");
        CHECK(query::url_decode("caf%C3%A9") == "caf\xC3\xA9");
    }

    SUBCASE("keeps a percent sign that starts no escape") {
        CHECK(query::url_decode("100%") == "100%");
        CHECK(query::url_decode("%2") == "%2");
        CHECK(query::url_decode("%zz") == "%zz");
        CHECK(query::url_decode("%1z") == "%1z");
        CHECK(query::url_decode("%+1") == "% 1");
        CHECK(query::url_decode("% 1") == "% 1");
    }
}

TEST_CASE("query::Request from JSON values") {
    const json values = {
        {"symbol", "BTC/USD"},
        {"limit", 5},
        {"ratio", 0.25},
        {"live", true},
        {"symbols", {"AAPL", "MSFT"}},
        {"filter", {{"side", "buy"}}},
    };
    auto req = query::Request::from_values("/echo", values);

    SUBCASE("keeps the path") {
        CHECK(req.path == "/echo");
    }

    SUBCASE("string accessors give a string param as it is and any other as its JSON text") {
        const std::map<std::string, std::string> text = {
            {"symbol", "BTC/USD"},
            {"limit", "5"},
            {"ratio", "0.25"},
            {"live", "true"},
            {"symbols", R"(["AAPL","MSFT"])"},
            {"filter", R"({"side":"buy"})"},
        };
        CHECK(req.params == text);
        CHECK(req.get("limit") == "5");
        CHECK(req.get("filter", "{}") == R"({"side":"buy"})");
        CHECK(req.has("live"));
    }

    SUBCASE("string accessors treat a null param as absent") {
        auto with_null = query::Request::from_values("/echo", {{"cursor", nullptr}});

        CHECK(with_null.params.empty());
        CHECK(with_null.get("cursor") == "");
        CHECK(with_null.get("cursor", "start") == "start");
        CHECK_FALSE(with_null.has("cursor"));
    }

    SUBCASE("values() keeps every param's JSON type, null included") {
        CHECK(req.values() == values);

        auto with_null = query::Request::from_values("/echo", {{"cursor", nullptr}});
        CHECK(with_null.values() == json{{"cursor", nullptr}});
    }

    SUBCASE("anything but a JSON object gives no params") {
        auto from_array = query::Request::from_values("/echo", json::array({"limit", 5}));

        CHECK(from_array.values() == json::object());
        CHECK(from_array.params.empty());
    }
}

TEST_CASE("query::Request::get_as") {
    auto req = query::Request::from_values("/echo", {
        {"symbol", "BTC/USD"},
        {"limit", 5},
        {"ratio", 0.25},
        {"live", true},
        {"symbols", {"AAPL", "MSFT"}},
        {"filter", {{"side", "buy"}}},
        {"cursor", nullptr},
        {"big", 300},
        {"negative", -1},
    });

    SUBCASE("reads a param as T") {
        CHECK(req.get_as<int>("limit") == 5);
        CHECK(req.get_as<double>("ratio") == 0.25);
        CHECK(req.get_as<double>("limit") == 5.0);
        CHECK(req.get_as<bool>("live") == true);
        CHECK(req.get_as<std::vector<std::string>>("symbols") == std::vector<std::string>{"AAPL", "MSFT"});
        CHECK(req.get_as<json>("filter") == json{{"side", "buy"}});
        CHECK(req.get_as<std::string>("symbol") == "BTC/USD");
    }

    SUBCASE("reads a null param as JSON null") {
        CHECK(req.get_as<json>("cursor") == json(nullptr));
    }

    SUBCASE("gives nothing for a missing param") {
        CHECK_FALSE(req.get_as<int>("offset").has_value());
    }

    SUBCASE("gives nothing for a param that is not a T") {
        CHECK_FALSE(req.get_as<int>("ratio").has_value());
        CHECK_FALSE(req.get_as<int>("live").has_value());
        CHECK_FALSE(req.get_as<bool>("limit").has_value());
        CHECK_FALSE(req.get_as<std::int8_t>("big").has_value());
        CHECK_FALSE(req.get_as<unsigned>("negative").has_value());
        CHECK_FALSE(req.get_as<int>("filter").has_value());
        CHECK_FALSE(req.get_as<int>("cursor").has_value());
    }

    SUBCASE("falls back to the default") {
        CHECK(req.get_as("offset", 0) == 0);
        CHECK(req.get_as("limit", 10) == 5);
        CHECK(req.get_as<int>("ratio", 10) == 10);
    }

    SUBCASE("asked for a string, gives any other param as its JSON text and a null as absent") {
        CHECK(req.get_as<std::string>("limit") == "5");
        CHECK(req.get_as<std::string>("live") == "true");
        CHECK(req.get_as<std::string>("filter") == R"({"side":"buy"})");
        CHECK_FALSE(req.get_as<std::string>("cursor").has_value());
        CHECK(req.get_as<std::string>("cursor", "start") == "start");
    }
}

TEST_CASE("query::Request from text params") {
    query::Request req("/echo", {{"symbol", "BTC/USD"}, {"limit", "5"}});

    SUBCASE("values() holds every param as a JSON string") {
        CHECK(req.values() == json{{"symbol", "BTC/USD"}, {"limit", "5"}});
    }

    SUBCASE("get_as<T>() parses a string param as T") {
        query::Request text("/echo", {
            {"limit", "5"},
            {"live", "true"},
            {"ratio", "0.25"},
            {"symbols", R"(["AAPL","MSFT"])"},
            {"symbol", "BTC/USD"},
        });

        CHECK(text.get_as<int>("limit") == 5);
        CHECK(text.get_as<bool>("live") == true);
        CHECK(text.get_as<double>("ratio") == 0.25);
        CHECK(text.get_as<std::vector<std::string>>("symbols") == std::vector<std::string>{"AAPL", "MSFT"});
        CHECK(text.get_as<std::string>("limit") == "5");
        CHECK_FALSE(text.get_as<int>("symbol").has_value());
        CHECK_FALSE(text.get_as<int>("ratio").has_value());
    }
}

TEST_CASE("state read-only enforcement in query mode") {
    // Set up temp directory for state
    auto temp_dir = std::filesystem::temp_directory_path() / ("the0_query_test_" + std::to_string(rand()));
    std::filesystem::create_directories(temp_dir);
    setenv("STATE_DIR", temp_dir.c_str(), 1);

    SUBCASE("state::set throws in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        CHECK_THROWS_AS(
            state::set("key", json{{"test", true}}),
            state::ReadOnlyStateError
        );
    }

    SUBCASE("state::remove throws in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        CHECK_THROWS_AS(
            state::remove("key"),
            state::ReadOnlyStateError
        );
    }

    SUBCASE("state::clear throws in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        CHECK_THROWS_AS(
            state::clear(),
            state::ReadOnlyStateError
        );
    }

    SUBCASE("query::ReadOnlyStateError catches what state throws") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        CHECK_THROWS_AS(
            state::set("key", json{{"test", true}}),
            query::ReadOnlyStateError
        );
    }

    SUBCASE("state::get allowed in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        // Should not throw
        auto result = state::get("nonexistent");
        CHECK_FALSE(result.has_value());
    }

    SUBCASE("state::get_or allowed in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        // Should not throw
        auto result = state::get_or("nonexistent", json("default"));
        CHECK(result == json("default"));
    }

    SUBCASE("state::list allowed in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        // Should not throw
        auto result = state::list();
        CHECK(result.empty());
    }

    SUBCASE("state::exists allowed in query mode") {
        SetEnv queryEnv("QUERY_PATH", "/test");

        // Should not throw
        bool result = state::exists("nonexistent");
        CHECK_FALSE(result);
    }

    // Cleanup
    unsetenv("STATE_DIR");
    std::filesystem::remove_all(temp_dir);
}
