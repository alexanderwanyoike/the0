/**
 * SMA Crossover Bot (C# .NET 8)
 * =============================
 * A realtime bot that implements Simple Moving Average crossover strategy
 * using live data from Yahoo Finance.
 *
 * This example demonstrates:
 * - Fetching real market data from Yahoo Finance REST API
 * - Calculating Simple Moving Averages (SMA)
 * - Detecting SMA crossovers for trading signals
 * - Structured metric emission for dashboard visualization
 * - Persistent state for SMA values across restarts
 *
 * Metrics emitted:
 * - price: Current stock price with change percentage
 * - sma: Short and long SMA values
 * - signal: BUY/SELL signals when crossover detected
 *
 * State Usage:
 * - Persists previous SMA values for crossover detection across restarts
 * - Tracks total signal count for monitoring
 */

using System.Text.Json.Nodes;
using The0;

// Persistent state structure
record PersistedState(double? PrevShortSma, double? PrevLongSma, long SignalCount);

class SmaBot
{
    // Previous SMA values for crossover detection
    private static double? _prevShortSma = null;
    private static double? _prevLongSma = null;
    private static long _signalCount = 0;

    static async Task Main(string[] args)
    {
        // Get configuration using the0 SDK
        var (botId, config) = Input.Parse();

        // Extract configuration with defaults
        var symbol = config?["symbol"]?.GetValue<string>() ?? "AAPL";
        var shortPeriod = config?["short_period"]?.GetValue<int>() ?? 5;
        var longPeriod = config?["long_period"]?.GetValue<int>() ?? 20;
        var updateIntervalMs = config?["update_interval_ms"]?.GetValue<int>() ?? 60000;

        // Load persistent state from previous runs
        var persisted = State.Get<PersistedState>("bot_state");
        if (persisted != null)
        {
            _prevShortSma = persisted.PrevShortSma;
            _prevLongSma = persisted.PrevLongSma;
            _signalCount = persisted.SignalCount;
        }

        Input.Log($"Bot {botId} started - {symbol} SMA({shortPeriod}/{longPeriod}) - loaded {_signalCount} signals");

        using var httpClient = new HttpClient();
        httpClient.DefaultRequestHeaders.Add("User-Agent", "the0-sma-bot/1.0");

        // Main loop - runs until process is terminated
        while (true)
        {
            try
            {
                // Fetch historical data from Yahoo Finance
                var prices = await FetchYahooFinanceData(httpClient, symbol);

                if (prices.Count < longPeriod)
                {
                    Input.Log($"Insufficient data: {prices.Count}/{longPeriod} required for {symbol}");
                    await Task.Delay(updateIntervalMs);
                    continue;
                }

                // Get current price
                var currentPrice = prices[^1];
                var previousPrice = prices.Count > 1 ? prices[^2] : currentPrice;
                var changePct = previousPrice != 0 ? ((currentPrice - previousPrice) / previousPrice) * 100 : 0;

                // Emit price metric using SDK
                Input.Metric("price", new
                {
                    symbol,
                    value = Math.Round(currentPrice, 2),
                    change_pct = Math.Round(changePct, 3)
                });

                // Calculate SMAs
                var shortSma = CalculateSMA(prices, shortPeriod);
                var longSma = CalculateSMA(prices, longPeriod);

                // Emit SMA metric using SDK
                Input.Metric("sma", new
                {
                    symbol,
                    short_sma = Math.Round(shortSma, 2),
                    long_sma = Math.Round(longSma, 2),
                    short_period = shortPeriod,
                    long_period = longPeriod
                });

                // Check for crossover signal
                if (_prevShortSma.HasValue && _prevLongSma.HasValue)
                {
                    var signal = CheckCrossover(_prevShortSma.Value, _prevLongSma.Value, shortSma, longSma);
                    if (signal != null)
                    {
                        _signalCount++;
                        var confidence = Math.Min(Math.Abs(shortSma - longSma) / longSma * 100, 0.95);
                        Input.Metric("signal", new
                        {
                            type = signal,
                            symbol,
                            price = Math.Round(currentPrice, 2),
                            confidence = Math.Round(confidence, 2),
                            total_signals = _signalCount,
                            reason = $"SMA{shortPeriod} crossed {(signal == "BUY" ? "above" : "below")} SMA{longPeriod}"
                        });
                    }
                }

                // Update previous SMA values
                _prevShortSma = shortSma;
                _prevLongSma = longSma;

                // Persist state every 10 iterations
                iteration++;
                if (iteration % 10 == 0)
                {
                    State.Set("bot_state", new PersistedState(_prevShortSma, _prevLongSma, _signalCount));
                }
            }
            catch (Exception ex)
            {
                Input.Log($"Error: {ex.Message}");
            }

            await Task.Delay(updateIntervalMs);
        }
    }

    private static int iteration = 0;

    static async Task<List<double>> FetchYahooFinanceData(HttpClient client, string symbol)
    {
        var url = $"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=1mo";
        var response = await client.GetStringAsync(url);
        var json = JsonNode.Parse(response);

        var prices = new List<double>();
        var quotes = json?["chart"]?["result"]?[0]?["indicators"]?["quote"]?[0]?["close"]?.AsArray();

        if (quotes != null)
        {
            foreach (var quote in quotes)
            {
                if (quote != null)
                {
                    prices.Add(quote.GetValue<double>());
                }
            }
        }

        return prices;
    }

    static double CalculateSMA(List<double> prices, int period)
    {
        if (prices.Count < period) return 0;
        var slice = prices.Skip(prices.Count - period).Take(period);
        return slice.Average();
    }

    static string? CheckCrossover(double prevShort, double prevLong, double currShort, double currLong)
    {
        // Golden cross: short SMA crosses above long SMA
        if (prevShort <= prevLong && currShort > currLong)
        {
            return "BUY";
        }
        // Death cross: short SMA crosses below long SMA
        if (prevShort >= prevLong && currShort < currLong)
        {
            return "SELL";
        }
        return null;
    }
}
