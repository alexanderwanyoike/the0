#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "../the0.h"
#include <cstdlib>
#include <sstream>
#include <iostream>

// Helper to capture stdout
class CaptureStdout {
public:
    CaptureStdout() : old_buf(std::cout.rdbuf(buffer.rdbuf())) {}
    ~CaptureStdout() { std::cout.rdbuf(old_buf); }
    std::string get() const { return buffer.str(); }
private:
    std::stringstream buffer;
    std::streambuf* old_buf;
};

// Helper to set environment variable
class SetEnv {
public:
    SetEnv(const char* name, const char* value) : name_(name) {
        setenv(name, value, 1);
    }
    ~SetEnv() {
        unsetenv(name_);
    }
private:
    const char* name_;
};

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
}

TEST_SUITE("the0::success") {
    TEST_CASE("outputs valid JSON with status success") {
        CaptureStdout capture;
        the0::success("Test message");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "success");
        CHECK(json["message"] == "Test message");
    }

    TEST_CASE("properly escapes special characters") {
        CaptureStdout capture;
        the0::success("Message with \"quotes\" and \\backslash");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["message"] == "Message with \"quotes\" and \\backslash");
    }

    TEST_CASE("handles newlines and tabs") {
        CaptureStdout capture;
        the0::success("Line1\nLine2\tTabbed");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["message"] == "Line1\nLine2\tTabbed");
    }

    TEST_CASE("handles unicode characters") {
        CaptureStdout capture;
        the0::success("Hello 世界 🚀");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["message"] == "Hello 世界 🚀");
    }

    TEST_CASE("handles empty message") {
        CaptureStdout capture;
        the0::success("");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "success");
        CHECK(json["message"] == "");
    }
}

TEST_SUITE("the0::result with json") {
    TEST_CASE("outputs custom JSON object") {
        CaptureStdout capture;
        the0::result({
            {"status", "success"},
            {"trade_id", "abc123"},
            {"amount", 100.5}
        });
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "success");
        CHECK(json["trade_id"] == "abc123");
        CHECK(json["amount"] == 100.5);
    }

    TEST_CASE("handles nested objects") {
        CaptureStdout capture;
        the0::result({
            {"status", "success"},
            {"data", {
                {"price", 45000.0},
                {"volume", 1.5}
            }}
        });
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["data"]["price"] == 45000.0);
        CHECK(json["data"]["volume"] == 1.5);
    }

    TEST_CASE("handles arrays") {
        CaptureStdout capture;
        the0::result({
            {"trades", {"trade1", "trade2", "trade3"}}
        });
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["trades"].is_array());
        CHECK(json["trades"].size() == 3);
    }
}

TEST_SUITE("the0::result with status/message/data") {
    TEST_CASE("creates result with status and message") {
        CaptureStdout capture;
        the0::result("success", "Trade completed");
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "success");
        CHECK(json["message"] == "Trade completed");
    }

    TEST_CASE("merges additional data fields") {
        CaptureStdout capture;
        the0::result("success", "Trade executed", {
            {"trade_id", "12345"},
            {"price", 45000.50},
            {"quantity", 0.1}
        });
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "success");
        CHECK(json["message"] == "Trade executed");
        CHECK(json["trade_id"] == "12345");
        CHECK(json["price"] == 45000.50);
        CHECK(json["quantity"] == 0.1);
    }

    TEST_CASE("works with empty data") {
        CaptureStdout capture;
        the0::result("error", "Something failed", {});
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["status"] == "error");
        CHECK(json["message"] == "Something failed");
        CHECK(json.size() == 2); // Only status and message
    }
}

TEST_SUITE("edge cases") {
    TEST_CASE("handles very long messages") {
        std::string long_message(10000, 'x');
        CaptureStdout capture;
        the0::success(long_message);
        std::string output = capture.get();

        auto json = the0::json::parse(output);
        CHECK(json["message"].get<std::string>().size() == 10000);
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
