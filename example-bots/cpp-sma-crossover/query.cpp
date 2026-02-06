/**
 * SMA Crossover Query Handlers (C++)
 * ===================================
 * Query endpoints for the SMA crossover bot.
 *
 * These handlers provide read-only access to bot state and computed data.
 * They can be executed via:
 *   - CLI: the0 bot query <bot_id> /status
 *   - API: POST /bot/:id/query
 *
 * Available queries:
 *   /status     - Get current SMA values and configuration
 *   /signals    - Get signal count with optional limit param
 *   /sma-values - Get current short/long SMA values
 */

#include <the0/query.h>
#include <the0/state.h>

using json = nlohmann::json;

int main() {
    /**
     * Get bot status and current SMA values.
     *
     * Returns:
     *   prev_short_sma: Previous short SMA value
     *   prev_long_sma: Previous long SMA value
     *   signal_count: Total signals generated
     */
    the0::query::handler("/status", [](const the0::query::Request& req) {
        auto state = the0::state::get("bot_state");
        auto config = the0::query::get_config();

        json result;
        if (state.has_value()) {
            result["prev_short_sma"] = state.value().value("prev_short_sma", nullptr);
            result["prev_long_sma"] = state.value().value("prev_long_sma", nullptr);
            result["signal_count"] = state.value().value("signal_count", 0);
        } else {
            result["prev_short_sma"] = nullptr;
            result["prev_long_sma"] = nullptr;
            result["signal_count"] = 0;
        }

        result["config"] = {
            {"symbol", config.value("symbol", "AAPL")},
            {"short_period", config.value("short_period", 5)},
            {"long_period", config.value("long_period", 20)}
        };

        return result;
    });

    /**
     * Get signal statistics.
     *
     * Query params:
     *   limit: Not used (for future signal history)
     *
     * Returns:
     *   signal_count: Total signals generated
     *   limit_applied: The limit parameter (for future use)
     */
    the0::query::handler("/signals", [](const the0::query::Request& req) {
        int limit = 10;
        try {
            limit = std::stoi(req.get("limit", "10"));
        } catch (...) {
            limit = 10;
        }

        auto state = the0::state::get("bot_state");
        long signal_count = state.has_value() ? state.value().value("signal_count", 0) : 0;

        return json{
            {"signal_count", signal_count},
            {"limit_applied", limit}
            // Note: Individual signals are not persisted, only the count
        };
    });

    /**
     * Get current SMA values.
     *
     * Returns:
     *   short_sma: Current short SMA value
     *   long_sma: Current long SMA value
     */
    the0::query::handler("/sma-values", [](const the0::query::Request& req) {
        auto state = the0::state::get("bot_state");

        if (state.has_value()) {
            return json{
                {"short_sma", state.value().value("prev_short_sma", nullptr)},
                {"long_sma", state.value().value("prev_long_sma", nullptr)}
            };
        }

        return json{
            {"short_sma", nullptr},
            {"long_sma", nullptr}
        };
    });

    // Run query system
    the0::query::run();
    return 0;
}
