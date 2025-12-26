/**
 * SMA Crossover Bot (C++)
 * =======================
 * A realtime bot that implements Simple Moving Average crossover strategy
 * using live data from Yahoo Finance.
 *
 * This example demonstrates:
 * - Fetching real market data from Yahoo Finance REST API using libcurl
 * - Calculating Simple Moving Averages (SMA)
 * - Detecting SMA crossovers for trading signals
 * - Structured metric emission for dashboard visualization
 *
 * Metrics emitted:
 * - price: Current stock price with change percentage
 * - sma: Short and long SMA values
 * - signal: BUY/SELL signals when crossover detected
 */

#include <the0.h>
#include <iostream>
#include <string>
#include <vector>
#include <chrono>
#include <thread>
#include <optional>
#include <cmath>
#include <curl/curl.h>

using json = the0::json;

// Bot state
struct BotState {
    std::optional<double> prevShortSma;
    std::optional<double> prevLongSma;
};

// Callback for CURL to write response
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// Round to specified decimal places
double roundTo(double value, int decimals) {
    double multiplier = std::pow(10.0, decimals);
    return std::round(value * multiplier) / multiplier;
}

// Fetch data from Yahoo Finance
std::vector<double> fetchYahooFinance(CURL* curl, const std::string& symbol) {
    std::vector<double> prices;
    std::string url = "https://query1.finance.yahoo.com/v8/finance/chart/" + symbol + "?interval=1d&range=1mo";
    std::string response;

    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "the0-sma-bot/1.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
        throw std::runtime_error(std::string("CURL error: ") + curl_easy_strerror(res));
    }

    // Parse JSON response
    json data = json::parse(response);

    if (data.contains("chart") &&
        data["chart"].contains("result") &&
        !data["chart"]["result"].empty()) {

        auto& result = data["chart"]["result"][0];
        if (result.contains("indicators") &&
            result["indicators"].contains("quote") &&
            !result["indicators"]["quote"].empty()) {

            auto& closeData = result["indicators"]["quote"][0]["close"];
            for (const auto& price : closeData) {
                if (!price.is_null()) {
                    prices.push_back(price.get<double>());
                }
            }
        }
    }

    return prices;
}

// Calculate Simple Moving Average
double calculateSMA(const std::vector<double>& prices, size_t period) {
    if (prices.size() < period) return 0.0;

    double sum = 0.0;
    for (size_t i = prices.size() - period; i < prices.size(); ++i) {
        sum += prices[i];
    }
    return sum / static_cast<double>(period);
}

// Check for crossover
std::optional<std::string> checkCrossover(double prevShort, double prevLong,
                                           double currShort, double currLong) {
    // Golden cross: short SMA crosses above long SMA
    if (prevShort <= prevLong && currShort > currLong) {
        return "BUY";
    }
    // Death cross: short SMA crosses below long SMA
    if (prevShort >= prevLong && currShort < currLong) {
        return "SELL";
    }
    return std::nullopt;
}

int main() {
    // Get configuration using the0 SDK
    auto [botId, config] = the0::parse();

    // Extract configuration with defaults
    std::string symbol = config.value("symbol", "AAPL");
    int shortPeriod = config.value("short_period", 5);
    int longPeriod = config.value("long_period", 20);
    int updateIntervalMs = config.value("update_interval_ms", 60000);

    the0::log("Bot " + botId + " started - " + symbol + " SMA(" + std::to_string(shortPeriod) + "/" + std::to_string(longPeriod) + ")");

    // Initialize CURL
    curl_global_init(CURL_GLOBAL_DEFAULT);
    CURL* curl = curl_easy_init();
    if (!curl) {
        the0::error("Failed to initialize CURL");
    }

    BotState state;

    // Main loop
    while (true) {
        try {
            // Fetch historical data
            std::vector<double> prices = fetchYahooFinance(curl, symbol);

            if (prices.size() < static_cast<size_t>(longPeriod)) {
                the0::log("Insufficient data for " + symbol + ": need " + std::to_string(longPeriod) + " prices, have " + std::to_string(prices.size()));
                std::this_thread::sleep_for(std::chrono::milliseconds(updateIntervalMs));
                continue;
            }

            // Get current price
            double currentPrice = prices.back();
            double previousPrice = prices.size() > 1 ? prices[prices.size() - 2] : currentPrice;
            double changePct = previousPrice != 0.0 ?
                ((currentPrice - previousPrice) / previousPrice) * 100.0 : 0.0;

            // Emit price metric
            the0::metric("price", {
                {"symbol", symbol},
                {"value", roundTo(currentPrice, 2)},
                {"change_pct", roundTo(changePct, 3)}
            });

            // Calculate SMAs
            double shortSma = calculateSMA(prices, shortPeriod);
            double longSma = calculateSMA(prices, longPeriod);

            // Emit SMA metric
            the0::metric("sma", {
                {"symbol", symbol},
                {"short_sma", roundTo(shortSma, 2)},
                {"long_sma", roundTo(longSma, 2)},
                {"short_period", shortPeriod},
                {"long_period", longPeriod}
            });

            // Check for crossover signal
            if (state.prevShortSma.has_value() && state.prevLongSma.has_value()) {
                auto signal = checkCrossover(
                    state.prevShortSma.value(),
                    state.prevLongSma.value(),
                    shortSma,
                    longSma
                );

                if (signal.has_value()) {
                    double confidence = std::min(std::abs(shortSma - longSma) / longSma * 100.0, 0.95);
                    std::string direction = signal.value() == "BUY" ? "above" : "below";

                    the0::metric("signal", {
                        {"type", signal.value()},
                        {"symbol", symbol},
                        {"price", roundTo(currentPrice, 2)},
                        {"confidence", roundTo(confidence, 2)},
                        {"reason", "SMA" + std::to_string(shortPeriod) + " crossed " + direction + " SMA" + std::to_string(longPeriod)}
                    });
                }
            }

            // Update previous SMA values
            state.prevShortSma = shortSma;
            state.prevLongSma = longSma;

        } catch (const std::exception& e) {
            the0::log(std::string("Error: ") + e.what());
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(updateIntervalMs));
    }

    curl_easy_cleanup(curl);
    curl_global_cleanup();
    return 0;
}
