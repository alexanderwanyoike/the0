"""
Tests for the0 Python SDK state module
"""

import json
import os
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from the0 import state


class TestState(unittest.TestCase):
    """Tests for the state module"""

    def setUp(self):
        """Create a temporary directory for state tests"""
        self.state_dir = tempfile.mkdtemp()
        self.env_patcher = patch.dict(os.environ, {'STATE_DIR': self.state_dir})
        self.env_patcher.start()

    def tearDown(self):
        """Clean up temporary directory"""
        self.env_patcher.stop()
        shutil.rmtree(self.state_dir, ignore_errors=True)

    def test_set_and_get_dict(self):
        """Test storing and retrieving a dictionary"""
        portfolio = {"AAPL": 100, "GOOGL": 50}
        state.set("portfolio", portfolio)
        retrieved = state.get("portfolio")
        self.assertEqual(retrieved, portfolio)

    def test_set_and_get_list(self):
        """Test storing and retrieving a list"""
        prices = [45000.5, 45100.0, 45050.25]
        state.set("prices", prices)
        retrieved = state.get("prices")
        self.assertEqual(retrieved, prices)

    def test_set_and_get_number(self):
        """Test storing and retrieving a number"""
        state.set("count", 42)
        self.assertEqual(state.get("count"), 42)

    def test_set_and_get_string(self):
        """Test storing and retrieving a string"""
        state.set("symbol", "BTC/USD")
        self.assertEqual(state.get("symbol"), "BTC/USD")

    def test_get_nonexistent_returns_default(self):
        """Test that getting a non-existent key returns default"""
        result = state.get("nonexistent", {"default": True})
        self.assertEqual(result, {"default": True})

    def test_get_nonexistent_returns_none(self):
        """Test that getting a non-existent key returns None by default"""
        result = state.get("nonexistent")
        self.assertIsNone(result)

    def test_exists_true(self):
        """Test exists returns True for existing key"""
        state.set("exists_test", {"value": 1})
        self.assertTrue(state.exists("exists_test"))

    def test_exists_false(self):
        """Test exists returns False for non-existent key"""
        self.assertFalse(state.exists("nonexistent"))

    def test_delete_existing_key(self):
        """Test deleting an existing key"""
        state.set("to_delete", "value")
        self.assertTrue(state.exists("to_delete"))
        result = state.delete("to_delete")
        self.assertTrue(result)
        self.assertFalse(state.exists("to_delete"))

    def test_delete_nonexistent_key(self):
        """Test deleting a non-existent key returns False"""
        result = state.delete("nonexistent")
        self.assertFalse(result)

    def test_list_keys(self):
        """Test listing all keys"""
        state.set("key1", "value1")
        state.set("key2", "value2")
        state.set("key3", "value3")
        keys = state.list()
        self.assertEqual(sorted(keys), ["key1", "key2", "key3"])

    def test_list_empty(self):
        """Test listing keys when state is empty"""
        keys = state.list()
        self.assertEqual(keys, [])

    def test_clear(self):
        """Test clearing all state"""
        state.set("key1", "value1")
        state.set("key2", "value2")
        self.assertEqual(len(state.list()), 2)
        state.clear()
        self.assertEqual(len(state.list()), 0)

    def test_clear_empty_state(self):
        """Test clearing when state is already empty"""
        state.clear()  # Should not raise
        self.assertEqual(len(state.list()), 0)

    def test_invalid_key_empty(self):
        """Test that empty key raises ValueError"""
        with self.assertRaises(ValueError) as context:
            state.get("")
        self.assertIn("empty", str(context.exception).lower())

    def test_invalid_key_path_separator(self):
        """Test that keys with path separators are rejected"""
        with self.assertRaises(ValueError) as context:
            state.set("../escape", "evil")
        self.assertIn("path", str(context.exception).lower())

    def test_invalid_key_backslash(self):
        """Test that keys with backslash are rejected"""
        with self.assertRaises(ValueError) as context:
            state.set("..\\escape", "evil")
        self.assertIn("path", str(context.exception).lower())

    def test_complex_nested_data(self):
        """Test storing complex nested data structures"""
        complex_data = {
            "portfolio": {
                "holdings": [
                    {"symbol": "AAPL", "quantity": 100, "price": 150.25},
                    {"symbol": "GOOGL", "quantity": 50, "price": 2800.00}
                ],
                "total_value": 155025.0
            },
            "signals": [
                {"type": "BUY", "symbol": "AAPL", "confidence": 0.85},
                {"type": "SELL", "symbol": "TSLA", "confidence": 0.72}
            ],
            "metadata": {
                "last_update": "2024-01-15T10:30:00Z",
                "version": 2
            }
        }
        state.set("complex", complex_data)
        retrieved = state.get("complex")
        self.assertEqual(retrieved, complex_data)

    def test_overwrite_existing_key(self):
        """Test that setting a key overwrites existing value"""
        state.set("key", "original")
        self.assertEqual(state.get("key"), "original")
        state.set("key", "updated")
        self.assertEqual(state.get("key"), "updated")


if __name__ == '__main__':
    unittest.main()
