---
title: 'Testing'
description: 'Comprehensive testing strategies for custom bots'
tags: ['custom-bots', 'testing', 'debugging', 'validation']
order: 5
---

# Testing Custom Bots

Comprehensive testing is crucial for reliable trading bots. This guide covers testing strategies, tools, and best practices for custom bot development. Since the0 is framework-agnostic, you can use any testing framework or library you prefer, such as `unittest` for Python or `Jest` for JavaScript. The examples below demonstrate how to implement tests in both languages.

---

## Testing Strategy Overview

### Testing Pyramid

```bash
    ┌─────────────────┐
    │   End-to-End    │ ← Integration with live systems (usually in CI/CD)
    │     Tests       │
    ├─────────────────┤
    │  Integration    │ ← Component interactions
    │     Tests       │
    ├─────────────────┤
    │   Unit Tests    │ ← Individual functions
    └─────────────────┘
```

### Test Types

1. **Unit Tests**: Test individual functions and components
2. **Integration Tests**: Test component interactions
3. **Schema Tests**: Validate configuration schemas
4. **Backtest Tests**: Validate backtesting logic
5. **End-to-End Tests**: Test complete bot workflows
6. **Performance Tests**: Test under various loads
7. **Security Tests**: Validate security measures

## Unit Testing

### Python Unit Tests

#### Test Structure

```python
# test_trading_logic.py
import unittest
from unittest.mock import Mock, patch

class TestTradingBot(unittest.TestCase):
    """Test the TradingBot class methods."""

    def setUp(self):
        self.config = {
            'symbol': 'BTCUSDT',
            'position_size': 100,
            'api_key': 'test-key',
            'api_secret': 'test-secret',
            'dry_run': True
        }

    @patch('main.ccxt.binance')
    def test_rsi_calculation(self, mock_exchange):
        """Test RSI calculation with sample data."""
        prices = [100, 102, 99, 105, 103, 108, 106, 110]

        # Calculate RSI
        from main import calculate_rsi
        rsi = calculate_rsi(prices, period=5)

        # RSI should be between 0 and 100
        self.assertGreaterEqual(rsi, 0)
        self.assertLessEqual(rsi, 100)

    @patch('main.ccxt.binance')
    def test_buy_signal(self, mock_exchange_class):
        """Test buy signal generation."""
        mock_exchange = Mock()
        mock_exchange_class.return_value = mock_exchange

        # Mock oversold condition (RSI < 30)
        mock_exchange.fetch_ohlcv.return_value = [
            [1640995200000, 50000, 50100, 49900, 49950, 1000],
            [1641001200000, 49950, 50000, 49800, 49850, 1100]
        ]

        from main import main
        result = main('test-bot', self.config)

        self.assertEqual(result['status'], 'success')
        mock_exchange.fetch_ohlcv.assert_called_once()
```

#### Running Python Tests

```bash
# Install testing dependencies
pip install pytest pytest-cov pytest-mock

# Run tests with coverage
pytest --cov=main --cov-report=html

# Run specific test file
pytest test_trading_logic.py

# Run with verbose output
pytest -v

# Run tests matching a pattern
pytest -k "test_rsi"
```

### JavaScript Unit Tests

#### Test Structure

````javascript
// test/trading-logic.test.js
```javascript
// test/trading-logic.test.js
const { main, TradingBot } = require('../main');

// Mock ccxt
jest.mock('ccxt', () => ({
    binance: jest.fn().mockImplementation(() => ({
        fetchTicker: jest.fn(),
        fetchOHLCV: jest.fn(),
        createMarketBuyOrder: jest.fn(),
        createMarketSellOrder: jest.fn(),
    })),
}));

describe('Trading Logic', () => {
    const mockConfig = {
        symbol: 'BTCUSDT',
        position_size: 100,
        api_key: 'test-key',
        api_secret: 'test-secret',
        dry_run: true,
    };

    beforeEach(() => {
        jest.clearAllMocks();
    });

    test('should calculate RSI correctly', () => {
        const prices = [100, 102, 101, 103, 105, 104, 106, 108];
        const rsi = TradingBot.calculateRSI(prices, 5);

        expect(rsi).toBeGreaterThanOrEqual(0);
        expect(rsi).toBeLessThanOrEqual(100);
    });

    test('should generate buy signal when oversold', async () => {
        const ccxt = require('ccxt');
        const mockExchange = ccxt.binance();

        // Mock declining prices (oversold condition)
        const ohlcvData = Array(20).fill(null).map((_, i) => [
            Date.now() - i * 3600000,
            50000 * Math.pow(0.99, i), // declining prices
            50000 * Math.pow(0.99, i) * 1.01,
            50000 * Math.pow(0.99, i) * 0.99,
            50000 * Math.pow(0.99, i),
            1000
        ]);

        mockExchange.fetchOHLCV.mockResolvedValue(ohlcvData);
        mockExchange.fetchTicker.mockResolvedValue({ last: 45000 });

        const result = await main('test-bot', mockConfig);

        expect(result.status).toBe('success');
        expect(mockExchange.fetchOHLCV).toHaveBeenCalled();
    });

    test('should handle exchange errors', async () => {
        const ccxt = require('ccxt');
        const mockExchange = ccxt.binance();

        mockExchange.fetchOHLCV.mockRejectedValue(new Error('API Error'));

        const result = await main('test-bot', mockConfig);

        expect(result.status).toBe('error');
        expect(result.message).toContain('API Error');
    });
});
````

#### Running JavaScript Tests

```bash
# Install testing dependencies
npm install --save-dev jest

# Run tests
npm test

# Run tests with coverage
npx jest --coverage

# Run specific test file
npx jest test/trading-logic.test.js

# Run tests in watch mode
npx jest --watch

# Run tests matching a pattern
npx jest --testNamePattern="rsi"
```

## Integration Testing

Integration tests validate how different components of your bot interact with each other and external systems like exchanges. These tests ensure that your bot can fetch data, execute trades, and handle errors correctly.
Your effectively testing the bot as a whole, including its interaction with the exchange API, data fetching, and trading logic.

### Exchange Integration Tests

```python
# test_exchange_integration.py
import unittest
import ccxt
from unittest.mock import Mock

class TestExchangeIntegration(unittest.TestCase):
    """Test bot integration with exchange APIs."""

    def setUp(self):
        """Set up mock exchange for testing."""
        self.mock_exchange = Mock(spec=ccxt.binance)
        self.test_config = {
            'symbol': 'BTCUSDT',
            'position_size': 100,
            'dry_run': True
        }

    def test_data_fetching(self):
        """Test fetching market data."""
        # Mock responses
        self.mock_exchange.fetch_ticker.return_value = {
            'symbol': 'BTC/USDT',
            'last': 50000,
            'bid': 49950,
            'ask': 50050
        }

        self.mock_exchange.fetch_ohlcv.return_value = [
            [1640995200000, 50000, 50100, 49900, 49950, 1000],
            [1641001200000, 49950, 50000, 49800, 49850, 1100]
        ]

        # Test data structure
        ticker = self.mock_exchange.fetch_ticker('BTC/USDT')
        self.assertIn('last', ticker)
        self.assertGreater(ticker['last'], 0)

        ohlcv = self.mock_exchange.fetch_ohlcv('BTC/USDT', '1h')
        self.assertEqual(len(ohlcv[0]), 6)  # timestamp, o, h, l, c, v

    def test_bot_workflow(self):
        """Test complete bot execution."""
        from main import main

        # Mock exchange responses
        self.mock_exchange.fetch_ohlcv.return_value = self._generate_ohlcv_data()

        with patch('main.ccxt.binance', return_value=self.mock_exchange):
            result = main('test-bot', self.test_config)

        self.assertEqual(result['status'], 'success')
        self.mock_exchange.fetch_ohlcv.assert_called_once()

    def _generate_ohlcv_data(self):
        """Generate sample OHLCV data."""
        return [[1640995200000, 50000, 50100, 49900, 49950, 1000] for _ in range(10)]
```

## Schema Testing

### JSON Schema Validation Tests

```python
# test_schemas.py
import unittest
import json
import jsonschema

class TestBotSchemas(unittest.TestCase):
    """Test JSON schema validation for bot configurations."""

    def setUp(self):
        """Load schemas once per test."""
        with open('bot-schema.json') as f:
            self.bot_schema = json.load(f)

    def test_valid_bot_config(self):
        """Test that valid configuration passes validation."""
        config = {
            'symbol': 'BTCUSDT',
            'position_size': 100,
            'api_key': 'test-key',
            'api_secret': 'test-secret'
        }

        # Should not raise an exception
        jsonschema.validate(config, self.bot_schema)

    def test_invalid_config_fails(self):
        """Test that invalid configurations are rejected."""
        invalid_configs = [
            {'symbol': 'btcusdt'},  # lowercase symbol
            {'position_size': -100},  # negative size
            {},  # missing required fields
        ]

        for config in invalid_configs:
            with self.assertRaises(jsonschema.ValidationError):
                jsonschema.validate(config, self.bot_schema)
```

### Other Aspect to consider when testing

- **Testing Backtesting Logic**: Ensure your backtesting logic correctly simulates trades and ensure it return the expected structure see [Backtesting](./backtesting) for more details.
- **Test the unhappy path**: Always test for error conditions, such as API failures, invalid configurations, and edge cases.
- **Profile Performance**: Use profiling tools to identify bottlenecks in your code. The [Overview](./overview) section outlines limitations that you will need to consider when testing your bot.

---

## Related Documentation

- [Quick Start Guide](/docs/custom-bot-development/quick-start-guide) - Build your first bot
- [Backtesting Guide](/docs/custom-bot-development/backtesting) - Implementing backtesting
- [Overview](/docs/custom-bot-development/overview) - Understanding custom bot development
