/**
 * SMA Crossover Bot - Query Handlers
 * ===================================
 * Query endpoints for the SMA crossover bot.
 *
 * These handlers provide read-only access to bot state and computed data.
 * They can be executed via:
 *   - CLI: the0 bot query <bot_id> /status
 *   - API: POST /query/:botId
 *
 * Available queries:
 *   /status  - Get bot configuration and current state
 *   /signals - Get signal statistics
 *   /sma     - Get current SMA values
 */

using The0;

class QueryProgram
{
    static void Main(string[] args)
    {
        // Register query handlers
        Query.Handler("/status", req =>
        {
            var config = Query.GetConfig();
            var botState = State.Get<BotState>("bot_state");

            return new
            {
                config = new
                {
                    symbol = config?["symbol"]?.GetValue<string>() ?? "AAPL",
                    short_period = config?["short_period"]?.GetValue<int>() ?? 5,
                    long_period = config?["long_period"]?.GetValue<int>() ?? 20,
                    update_interval_ms = config?["update_interval_ms"]?.GetValue<int>() ?? 60000
                },
                state = new
                {
                    prev_short_sma = botState?.PrevShortSma,
                    prev_long_sma = botState?.PrevLongSma,
                    signal_count = botState?.SignalCount ?? 0,
                    has_data = botState != null
                }
            };
        });

        Query.Handler("/signals", req =>
        {
            var botState = State.Get<BotState>("bot_state");

            return new
            {
                total_signals = botState?.SignalCount ?? 0,
                has_data = botState != null
            };
        });

        Query.Handler("/sma", req =>
        {
            var config = Query.GetConfig();
            var botState = State.Get<BotState>("bot_state");

            return new
            {
                symbol = config?["symbol"]?.GetValue<string>() ?? "AAPL",
                short_sma = botState?.PrevShortSma,
                long_sma = botState?.PrevLongSma,
                short_period = config?["short_period"]?.GetValue<int>() ?? 5,
                long_period = config?["long_period"]?.GetValue<int>() ?? 20
            };
        });

        // Run the query system (auto-detects mode)
        Query.Run();
    }
}

// State structure (must match the one in Program.cs)
record BotState(double? PrevShortSma, double? PrevLongSma, long SignalCount);
