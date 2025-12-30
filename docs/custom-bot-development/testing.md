---
title: "Testing"
description: "Testing strategies for custom bots"
tags: ["custom-bots", "testing", "validation"]
order: 5
---

# Testing Custom Bots

Testing a trading bot requires verifying that your strategy logic works correctly, that your configuration is valid, and that error conditions are handled gracefully. Since the0 is framework-agnostic, you can use whatever testing tools you prefer—pytest for Python, Jest for TypeScript, or the standard testing libraries in other languages. The platform doesn't impose a particular testing framework, but it does define how your bot receives configuration and reports results, which shapes how you structure your tests.

## Local Execution

The most direct way to test your bot is running it locally with the same environment variables the platform sets. When the platform starts your bot, it provides `BOT_ID` to identify the bot instance and `BOT_CONFIG` containing the JSON configuration. You can simulate this by exporting these variables before running your code:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20}'
export CODE_MOUNT_DIR="/tmp"
python main.py
```

For TypeScript:

```bash
BOT_ID="test-bot" BOT_CONFIG='{"symbol":"BTC/USD","alert_threshold":5}' npx ts-node main.ts
```

This validates the complete execution path—configuration parsing, strategy logic, metric emission, and result reporting. Watch for structured log output and check that metrics emit as expected. For realtime bots, you'll see repeated metric emissions until you terminate the process.

Local execution catches integration issues but doesn't isolate individual functions. For that, you need unit tests.

## Unit Testing

Unit tests verify individual functions in isolation. When testing trading logic, you typically mock external dependencies like price APIs and test that your calculations produce correct results for known inputs.

### Python Unit Tests

Python's unittest module combined with pytest provides a solid foundation. Here's how to test an SMA calculation and crossover detection:

```python
# test_strategy.py
import unittest
from unittest.mock import patch
import os

class TestSMAStrategy(unittest.TestCase):

    def test_sma_calculation(self):
        """Verify SMA calculation produces correct results."""
        from strategy import calculate_sma

        prices = [100, 102, 104, 103, 105]
        sma = calculate_sma(prices, period=3)

        # SMA of last 3 values: (104 + 103 + 105) / 3 = 104
        self.assertAlmostEqual(sma, 104.0)

    def test_crossover_detection(self):
        """Verify crossover detection identifies golden cross."""
        from strategy import detect_crossover

        # Previous: short below long, Current: short above long
        prev_short, prev_long = 98, 100
        curr_short, curr_long = 102, 100

        signal = detect_crossover(prev_short, prev_long, curr_short, curr_long)
        self.assertEqual(signal, "BUY")

    def test_no_crossover_when_parallel(self):
        """No signal when SMAs move in parallel."""
        from strategy import detect_crossover

        signal = detect_crossover(100, 100, 101, 101)
        self.assertIsNone(signal)

    @patch('main.fetch_historical_prices')
    def test_bot_execution_with_mocked_data(self, mock_fetch):
        """Test complete bot execution with mocked price data."""
        mock_fetch.return_value = [100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
                                    110, 111, 112, 113, 114, 115, 116, 117, 118, 119]

        os.environ['BOT_ID'] = 'test-bot'
        os.environ['BOT_CONFIG'] = '{"symbol":"AAPL","short_period":5,"long_period":20}'
        os.environ['CODE_MOUNT_DIR'] = '/tmp'

        from main import main
        main()

        mock_fetch.assert_called_once()

if __name__ == '__main__':
    unittest.main()
```

Run tests with coverage reporting:

```bash
pip install pytest pytest-cov
pytest --cov=. --cov-report=term-missing
```

The coverage report shows which lines your tests exercise, helping identify untested code paths.

### TypeScript Unit Tests

Jest works well for TypeScript bots. Structure your tests to verify calculations and mock external calls:

```typescript
// strategy.test.ts
import { calculateSMA, detectCrossover } from './strategy';

describe('SMA Strategy', () => {
    describe('calculateSMA', () => {
        it('calculates correctly for simple data', () => {
            const prices = [100, 102, 104, 103, 105];
            const sma = calculateSMA(prices, 3);

            expect(sma).toBeCloseTo(104.0);
        });

        it('returns 0 for insufficient data', () => {
            const prices = [100, 102];
            const sma = calculateSMA(prices, 5);

            expect(sma).toBe(0);
        });
    });

    describe('detectCrossover', () => {
        it('detects golden cross', () => {
            const signal = detectCrossover(98, 100, 102, 100);
            expect(signal).toBe('BUY');
        });

        it('detects death cross', () => {
            const signal = detectCrossover(102, 100, 98, 100);
            expect(signal).toBe('SELL');
        });

        it('returns null when no crossover', () => {
            const signal = detectCrossover(100, 100, 101, 101);
            expect(signal).toBeNull();
        });
    });
});
```

Configure Jest for TypeScript in your package.json:

```json
{
  "scripts": {
    "test": "jest",
    "test:coverage": "jest --coverage"
  },
  "devDependencies": {
    "jest": "^29.0.0",
    "@types/jest": "^29.0.0",
    "ts-jest": "^29.0.0"
  },
  "jest": {
    "preset": "ts-jest",
    "testEnvironment": "node"
  }
}
```

## Integration Testing

Integration tests verify that components work together. For a trading bot, this means testing the complete flow from configuration parsing through metric emission, with external services mocked at the boundary.

```python
# test_integration.py
import unittest
from unittest.mock import patch, MagicMock
import os
import json

class TestBotIntegration(unittest.TestCase):

    def setUp(self):
        os.environ['BOT_ID'] = 'integration-test'
        os.environ['BOT_CONFIG'] = json.dumps({
            'symbol': 'AAPL',
            'short_period': 5,
            'long_period': 20,
            'update_interval_ms': 1000
        })
        os.environ['CODE_MOUNT_DIR'] = '/tmp'

    @patch('main.fetch_yahoo_finance')
    def test_full_execution_cycle(self, mock_yahoo):
        """Test that a complete execution cycle produces expected metrics."""
        # Provide enough data for SMA calculation
        mock_yahoo.return_value = [
            150.0, 151.0, 152.0, 153.0, 154.0,
            155.0, 156.0, 157.0, 158.0, 159.0,
            160.0, 161.0, 162.0, 163.0, 164.0,
            165.0, 166.0, 167.0, 168.0, 169.0,
        ]

        from main import main
        # For realtime bots, you'd need to interrupt after one iteration
        main()

        mock_yahoo.assert_called_with('AAPL')

    @patch('main.fetch_yahoo_finance')
    def test_handles_api_failure(self, mock_yahoo):
        """Test graceful handling of API errors."""
        mock_yahoo.side_effect = Exception('Network timeout')

        from main import main
        # Bot should catch exception and call error()
        main()
```

The key difference from unit tests is that integration tests exercise the real code paths rather than isolated functions. They catch issues like incorrect import paths, misconfigured logging, and subtle bugs in how components interact.

## Schema Validation Testing

Your bot-schema.json defines what configuration is valid. Test that the schema correctly accepts good configurations and rejects bad ones:

```python
# test_schema.py
import unittest
import json
import jsonschema

class TestConfigSchema(unittest.TestCase):

    def setUp(self):
        with open('bot-schema.json') as f:
            self.schema = json.load(f)

    def test_accepts_valid_config(self):
        """Valid configuration passes validation."""
        config = {
            'symbol': 'AAPL',
            'short_period': 5,
            'long_period': 20,
            'update_interval_ms': 60000
        }
        # Should not raise
        jsonschema.validate(config, self.schema)

    def test_accepts_minimal_config(self):
        """Config with only required fields passes."""
        config = {'symbol': 'BTC'}
        jsonschema.validate(config, self.schema)

    def test_rejects_missing_required(self):
        """Missing required field fails validation."""
        config = {'short_period': 5}

        with self.assertRaises(jsonschema.ValidationError) as context:
            jsonschema.validate(config, self.schema)

        self.assertIn('symbol', str(context.exception))

    def test_rejects_invalid_type(self):
        """Wrong type fails validation."""
        config = {
            'symbol': 'AAPL',
            'short_period': 'five'  # should be integer
        }

        with self.assertRaises(jsonschema.ValidationError):
            jsonschema.validate(config, self.schema)

    def test_rejects_out_of_range(self):
        """Value outside min/max fails validation."""
        config = {
            'symbol': 'AAPL',
            'short_period': 1  # minimum is 2
        }

        with self.assertRaises(jsonschema.ValidationError):
            jsonschema.validate(config, self.schema)
```

These tests ensure that when users configure bot instances, the schema catches errors before deployment rather than at runtime.

## Testing Best Practices

**Test error paths, not just success paths.** Your bot will encounter API timeouts, invalid data, and unexpected responses. Write tests that simulate these conditions and verify your error handling works correctly. A bot that crashes on bad data is worse than one that logs the error and continues.

**Mock at the boundary.** Mock external services like exchange APIs, but don't mock your own code. If you're mocking internal functions extensively, that's often a sign your code needs restructuring for better testability.

**Use realistic test data.** Price data has characteristics—it trends, it's noisy, it has gaps. Using synthetic data like `[100, 101, 102, 103]` can miss edge cases that real market data would expose. Consider capturing real historical data for test fixtures.

**Test configuration validation early.** A user entering invalid configuration should get a clear error message, not a cryptic runtime failure. Schema validation tests ensure your bot-schema.json is correctly specified.

## Related

- [Development Overview](./overview) - Bot structure and workflow
- [Configuration](./configuration) - Schema definitions
- [Deployment](./deployment) - Deploy tested bots
