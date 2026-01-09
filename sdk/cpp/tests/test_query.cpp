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
