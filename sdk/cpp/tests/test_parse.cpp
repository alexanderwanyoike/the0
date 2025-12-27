/**
 * Tests for the0::parse() and the0::parse_raw() functions
 *
 * These functions parse BOT_ID and BOT_CONFIG from environment variables.
 */

#include <doctest/doctest.h>
#include "../the0.h"
#include "helpers/test_helpers.hpp"

using namespace the0_test;

TEST_SUITE("the0::parse") {
    TEST_CASE("returns empty string and empty object when env vars not set") {
        unsetenv("BOT_ID");
        unsetenv("BOT_CONFIG");

        auto [bot_id, config] = the0::parse();

        CHECK(bot_id.empty());
        CHECK(config.is_object());
        CHECK(config.empty());
    }

    TEST_CASE("parses BOT_ID correctly") {
        SetEnv id("BOT_ID", "test-bot-123");
        unsetenv("BOT_CONFIG");

        auto [bot_id, config] = the0::parse();

        CHECK(bot_id == "test-bot-123");
    }

    TEST_CASE("parses simple BOT_CONFIG correctly") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", R"({"symbol":"BTC/USDT","amount":100})");

        auto [bot_id, config] = the0::parse();

        CHECK(bot_id == "test-bot");
        CHECK(config["symbol"] == "BTC/USDT");
        CHECK(config["amount"] == 100);
    }

    TEST_CASE("handles nested JSON config") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", R"({"exchange":{"name":"binance","testnet":true},"symbols":["BTC","ETH"]})");

        auto [bot_id, config] = the0::parse();

        CHECK(config["exchange"]["name"] == "binance");
        CHECK(config["exchange"]["testnet"] == true);
        CHECK(config["symbols"].is_array());
        CHECK(config["symbols"].size() == 2);
        CHECK(config["symbols"][0] == "BTC");
        CHECK(config["symbols"][1] == "ETH");
    }

    TEST_CASE("handles invalid JSON gracefully") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", "not valid json {{{");

        auto [bot_id, config] = the0::parse();

        CHECK(bot_id == "test-bot");
        CHECK(config.is_object());
        CHECK(config.empty());
    }

    TEST_CASE("config.value() returns default for missing keys") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", R"({"existing":"value"})");

        auto [bot_id, config] = the0::parse();

        CHECK(config.value("existing", "default") == "value");
        CHECK(config.value("missing", "default") == "default");
        CHECK(config.value("missing_int", 42) == 42);
        CHECK(config.value("missing_double", 3.14) == 3.14);
        CHECK(config.value("missing_bool", true) == true);
    }

    TEST_CASE("handles config with all JSON types") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", R"({
            "string": "hello",
            "integer": 42,
            "float": 3.14159,
            "boolean_true": true,
            "boolean_false": false,
            "null_value": null,
            "array": [1, 2, 3],
            "object": {"nested": "value"}
        })");

        auto [bot_id, config] = the0::parse();

        CHECK(config["string"] == "hello");
        CHECK(config["integer"] == 42);
        CHECK(std::abs(config["float"].get<double>() - 3.14159) < 0.0001);
        CHECK(config["boolean_true"] == true);
        CHECK(config["boolean_false"] == false);
        CHECK(config["null_value"].is_null());
        CHECK(config["array"].is_array());
        CHECK(config["object"]["nested"] == "value");
    }
}

TEST_SUITE("the0::parse_raw") {
    TEST_CASE("returns raw config string") {
        SetEnv id("BOT_ID", "test-bot");
        SetEnv cfg("BOT_CONFIG", R"({"key":"value"})");

        auto [bot_id, config_str] = the0::parse_raw();

        CHECK(bot_id == "test-bot");
        CHECK(config_str == R"({"key":"value"})");
    }

    TEST_CASE("returns empty braces when BOT_CONFIG not set") {
        SetEnv id("BOT_ID", "test-bot");
        unsetenv("BOT_CONFIG");

        auto [bot_id, config_str] = the0::parse_raw();

        CHECK(config_str == "{}");
    }

    TEST_CASE("returns empty string for BOT_ID when not set") {
        unsetenv("BOT_ID");
        SetEnv cfg("BOT_CONFIG", R"({})");

        auto [bot_id, config_str] = the0::parse_raw();

        CHECK(bot_id.empty());
    }
}
