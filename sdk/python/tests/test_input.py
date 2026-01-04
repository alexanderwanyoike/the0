"""
Tests for the0 Python SDK input module (parse, success, error, result, metric, log, sleep)
"""

import json
import os
import sys
import tempfile
import unittest
from unittest.mock import patch
from io import StringIO

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from the0 import parse, success, error, result, metric, log, sleep


class TestParse(unittest.TestCase):
    """Tests for the parse() function"""

    def test_parse_valid_config(self):
        """Test parsing valid configuration"""
        with patch.dict(os.environ, {
            'BOT_ID': 'test-bot-123',
            'BOT_CONFIG': '{"symbol": "BTC/USDT", "amount": 100.5}'
        }):
            bot_id, config = parse()
            self.assertEqual(bot_id, 'test-bot-123')
            self.assertEqual(config['symbol'], 'BTC/USDT')
            self.assertEqual(config['amount'], 100.5)

    def test_parse_missing_bot_id(self):
        """Test error when BOT_ID is missing"""
        with patch.dict(os.environ, {'BOT_CONFIG': '{}'}, clear=True):
            # Remove BOT_ID if it exists
            os.environ.pop('BOT_ID', None)
            with self.assertRaises(ValueError) as context:
                parse()
            self.assertIn('BOT_ID', str(context.exception))

    def test_parse_missing_bot_config(self):
        """Test error when BOT_CONFIG is missing"""
        with patch.dict(os.environ, {'BOT_ID': 'test'}, clear=True):
            os.environ.pop('BOT_CONFIG', None)
            with self.assertRaises(ValueError) as context:
                parse()
            self.assertIn('BOT_CONFIG', str(context.exception))

    def test_parse_invalid_json(self):
        """Test error when BOT_CONFIG is invalid JSON"""
        with patch.dict(os.environ, {
            'BOT_ID': 'test',
            'BOT_CONFIG': 'not valid json'
        }):
            with self.assertRaises(ValueError) as context:
                parse()
            self.assertIn('JSON', str(context.exception))

    def test_parse_nested_config(self):
        """Test parsing nested configuration"""
        config_data = {
            'symbol': 'ETH/USD',
            'credentials': {
                'api_key': 'key123',
                'secret': 'secret456'
            },
            'settings': {
                'paper': True,
                'amount': 50.0
            }
        }
        with patch.dict(os.environ, {
            'BOT_ID': 'nested-bot',
            'BOT_CONFIG': json.dumps(config_data)
        }):
            bot_id, config = parse()
            self.assertEqual(config['credentials']['api_key'], 'key123')
            self.assertEqual(config['settings']['paper'], True)


class TestSuccess(unittest.TestCase):
    """Tests for the success() function"""

    def test_success_simple(self):
        """Test simple success message"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                success("Trade completed")

                result_path = os.path.join('/', tmpdir, 'result.json')
                with open(result_path) as f:
                    data = json.load(f)

                self.assertEqual(data['status'], 'success')
                self.assertEqual(data['message'], 'Trade completed')

    def test_success_with_data(self):
        """Test success with additional data"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                success("Order filled", {"order_id": "123", "filled": 0.5})

                result_path = os.path.join('/', tmpdir, 'result.json')
                with open(result_path) as f:
                    data = json.load(f)

                self.assertEqual(data['status'], 'success')
                self.assertEqual(data['data']['order_id'], '123')
                self.assertEqual(data['data']['filled'], 0.5)


class TestError(unittest.TestCase):
    """Tests for the error() function"""

    def test_error_exits_with_code_1(self):
        """Test that error() exits with code 1"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                with self.assertRaises(SystemExit) as context:
                    error("Something went wrong")

                self.assertEqual(context.exception.code, 1)

    def test_error_writes_result(self):
        """Test that error() writes error result before exiting"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                try:
                    error("Connection failed")
                except SystemExit:
                    pass

                result_path = os.path.join('/', tmpdir, 'result.json')
                with open(result_path) as f:
                    data = json.load(f)

                self.assertEqual(data['status'], 'error')
                self.assertEqual(data['message'], 'Connection failed')

    def test_error_with_data(self):
        """Test error with additional context"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                try:
                    error("Insufficient funds", {"available": 100, "required": 150})
                except SystemExit:
                    pass

                result_path = os.path.join('/', tmpdir, 'result.json')
                with open(result_path) as f:
                    data = json.load(f)

                self.assertEqual(data['data']['available'], 100)
                self.assertEqual(data['data']['required'], 150)


class TestResult(unittest.TestCase):
    """Tests for the result() function"""

    def test_result_custom_data(self):
        """Test writing custom result data"""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'CODE_MOUNT_DIR': tmpdir}):
                result({
                    "status": "success",
                    "trade_id": "abc123",
                    "signals": [{"symbol": "BTC", "direction": "long"}]
                })

                result_path = os.path.join('/', tmpdir, 'result.json')
                with open(result_path) as f:
                    data = json.load(f)

                self.assertEqual(data['trade_id'], 'abc123')
                self.assertEqual(len(data['signals']), 1)
                self.assertEqual(data['signals'][0]['direction'], 'long')


class TestMetric(unittest.TestCase):
    """Tests for the metric() function"""

    def test_metric_output(self):
        """Test metric outputs JSON to stdout"""
        captured_output = StringIO()
        with patch('sys.stdout', captured_output):
            metric("price", {"symbol": "BTC/USD", "value": 45000})

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        self.assertEqual(data['_metric'], 'price')
        self.assertEqual(data['symbol'], 'BTC/USD')
        self.assertEqual(data['value'], 45000)
        self.assertIn('timestamp', data)

    def test_metric_timestamp_format(self):
        """Test metric includes ISO timestamp"""
        captured_output = StringIO()
        with patch('sys.stdout', captured_output):
            metric("signal", {"direction": "long"})

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        # Timestamp should end with Z (UTC)
        self.assertTrue(data['timestamp'].endswith('Z'))


class TestLog(unittest.TestCase):
    """Tests for the log() function"""

    def test_log_simple_message(self):
        """Test simple log message defaults to info level"""
        captured_output = StringIO()
        with patch('sys.stderr', captured_output):
            log("Starting trade execution")

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        self.assertEqual(data['level'], 'info')
        self.assertEqual(data['message'], 'Starting trade execution')
        self.assertIn('timestamp', data)

    def test_log_with_level(self):
        """Test log with explicit level"""
        captured_output = StringIO()
        with patch('sys.stderr', captured_output):
            log("Connection lost", "warn")

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        self.assertEqual(data['level'], 'warn')
        self.assertEqual(data['message'], 'Connection lost')

    def test_log_with_data(self):
        """Test log with structured data"""
        captured_output = StringIO()
        with patch('sys.stderr', captured_output):
            log("Order placed", {"order_id": "123", "symbol": "BTC"})

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        self.assertEqual(data['level'], 'info')
        self.assertEqual(data['message'], 'Order placed')
        self.assertEqual(data['order_id'], '123')
        self.assertEqual(data['symbol'], 'BTC')

    def test_log_with_data_and_level(self):
        """Test log with data and explicit level"""
        captured_output = StringIO()
        with patch('sys.stderr', captured_output):
            log("Order failed", {"order_id": "123", "reason": "insufficient funds"}, "error")

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        self.assertEqual(data['level'], 'error')
        self.assertEqual(data['message'], 'Order failed')
        self.assertEqual(data['order_id'], '123')
        self.assertEqual(data['reason'], 'insufficient funds')

    def test_log_timestamp_format(self):
        """Test log includes ISO timestamp"""
        captured_output = StringIO()
        with patch('sys.stderr', captured_output):
            log("Test message")

        output = captured_output.getvalue().strip()
        data = json.loads(output)

        # Timestamp should end with Z (UTC)
        self.assertTrue(data['timestamp'].endswith('Z'))


class TestSleep(unittest.TestCase):
    """Tests for the sleep() function"""

    def test_sleep_duration(self):
        """Test sleep pauses for correct duration"""
        import time

        start = time.time()
        sleep(0.1)  # 100ms
        elapsed = time.time() - start

        # Should be at least 100ms (0.1s)
        self.assertGreaterEqual(elapsed, 0.09)
        # But not too much longer
        self.assertLess(elapsed, 0.2)


if __name__ == '__main__':
    unittest.main()
