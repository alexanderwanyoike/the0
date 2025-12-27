/**
 * Tests for the0::metric() and the0::log() functions
 *
 * These functions write to stdout (not to result file).
 */

#include <doctest/doctest.h>
#include "../the0.h"
#include "helpers/test_helpers.hpp"

using namespace the0_test;

TEST_SUITE("the0::metric") {
    TEST_CASE("outputs JSON with _metric field to stdout") {
        CaptureStdout capture;
        the0::metric("price", {{"symbol", "BTC/USD"}, {"value", 45000.0}});

        std::string output = capture.get();
        REQUIRE(!output.empty());

        auto json = the0::json::parse(output);
        CHECK(json["_metric"] == "price");
        CHECK(json["symbol"] == "BTC/USD");
        CHECK(json["value"] == 45000.0);
    }

    TEST_CASE("includes timestamp field") {
        CaptureStdout capture;
        the0::metric("signal", {{"type", "BUY"}});

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json.contains("timestamp"));
        CHECK(json["timestamp"].is_string());
        // Timestamp should end with 'Z'
        std::string ts = json["timestamp"].get<std::string>();
        CHECK(ts.back() == 'Z');
    }

    TEST_CASE("handles empty data") {
        CaptureStdout capture;
        the0::metric("heartbeat", {});

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["_metric"] == "heartbeat");
        CHECK(json.contains("timestamp"));
    }

    TEST_CASE("handles nested data") {
        CaptureStdout capture;
        the0::metric("trade", {
            {"order", {
                {"id", "12345"},
                {"side", "buy"},
                {"quantity", 1.5}
            }}
        });

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["_metric"] == "trade");
        CHECK(json["order"]["id"] == "12345");
        CHECK(json["order"]["side"] == "buy");
        CHECK(json["order"]["quantity"] == 1.5);
    }

    TEST_CASE("handles array data") {
        CaptureStdout capture;
        the0::metric("prices", {{"values", {100.0, 101.5, 99.8}}});

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["values"].is_array());
        CHECK(json["values"].size() == 3);
        CHECK(json["values"][0] == 100.0);
    }

    TEST_CASE("outputs newline at end") {
        CaptureStdout capture;
        the0::metric("test", {});

        std::string output = capture.get();
        CHECK(output.back() == '\n');
    }
}

TEST_SUITE("the0::log") {
    TEST_CASE("outputs JSON with message field to stderr") {
        CaptureStderr capture;
        the0::log("Starting bot execution");

        std::string output = capture.get();
        REQUIRE(!output.empty());

        auto json = the0::json::parse(output);
        CHECK(json["message"] == "Starting bot execution");
    }

    TEST_CASE("defaults to info level") {
        CaptureStderr capture;
        the0::log("Test message");

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["level"] == "info");
    }

    TEST_CASE("includes timestamp") {
        CaptureStderr capture;
        the0::log("Test message");

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json.contains("timestamp"));
        std::string ts = json["timestamp"].get<std::string>();
        CHECK(ts.back() == 'Z');
    }

    TEST_CASE("supports warn level") {
        CaptureStderr capture;
        the0::log("Warning message", {}, the0::LogLevel::Warn);

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["level"] == "warn");
        CHECK(json["message"] == "Warning message");
    }

    TEST_CASE("supports error level") {
        CaptureStderr capture;
        the0::log("Error message", {}, the0::LogLevel::Error);

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["level"] == "error");
        CHECK(json["message"] == "Error message");
    }

    TEST_CASE("merges data fields") {
        CaptureStderr capture;
        the0::log("Order placed", {{"order_id", "12345"}, {"symbol", "BTC"}});

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["message"] == "Order placed");
        CHECK(json["order_id"] == "12345");
        CHECK(json["symbol"] == "BTC");
    }

    TEST_CASE("supports data and level together") {
        CaptureStderr capture;
        the0::log("Order failed", {{"order_id", "12345"}}, the0::LogLevel::Error);

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["level"] == "error");
        CHECK(json["message"] == "Order failed");
        CHECK(json["order_id"] == "12345");
    }

    TEST_CASE("properly escapes special characters in message") {
        CaptureStderr capture;
        the0::log("Error: \"file not found\"");

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["message"] == "Error: \"file not found\"");
    }

    TEST_CASE("handles empty message") {
        CaptureStderr capture;
        the0::log("");

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["message"] == "");
    }

    TEST_CASE("handles newlines in message") {
        CaptureStderr capture;
        the0::log("Line 1\nLine 2");

        std::string output = capture.get();
        auto json = the0::json::parse(output);

        CHECK(json["message"] == "Line 1\nLine 2");
    }

    TEST_CASE("outputs newline at end") {
        CaptureStderr capture;
        the0::log("test");

        std::string output = capture.get();
        CHECK(output.back() == '\n');
    }
}
