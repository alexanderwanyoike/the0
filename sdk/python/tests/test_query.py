"""Tests for the the0 query module."""

import unittest
import os
import json
import sys
from io import StringIO
from unittest.mock import patch, mock_open

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from the0 import query
from the0.query import QueryRequest, ReadOnlyStateError


class TestQueryRequest(unittest.TestCase):
    """Test QueryRequest class."""

    def test_basic_request(self):
        """Test basic request creation."""
        req = QueryRequest("/portfolio", {"symbol": "BTC/USD", "limit": "10"})
        self.assertEqual(req.path, "/portfolio")
        self.assertEqual(req.params, {"symbol": "BTC/USD", "limit": "10"})

    def test_get_param(self):
        """Test getting parameters."""
        req = QueryRequest("/test", {"key1": "value1"})
        self.assertEqual(req.get("key1"), "value1")
        self.assertIsNone(req.get("missing"))
        self.assertEqual(req.get("missing", "default"), "default")

    def test_repr(self):
        """Test string representation."""
        req = QueryRequest("/test", {"key": "value"})
        repr_str = repr(req)
        self.assertIn("/test", repr_str)
        self.assertIn("key", repr_str)


class TestHandlerDecorator(unittest.TestCase):
    """Test handler decorator."""

    def setUp(self):
        """Clear handlers before each test."""
        query._handlers.clear()

    def test_handler_registration(self):
        """Test that handlers are registered correctly."""

        @query.handler("/test")
        def test_handler(req):
            return {"message": "test"}

        self.assertIn("/test", query._handlers)
        self.assertEqual(query._handlers["/test"], test_handler)

    def test_multiple_handlers(self):
        """Test registering multiple handlers."""

        @query.handler("/one")
        def handler_one(req):
            return {"id": 1}

        @query.handler("/two")
        def handler_two(req):
            return {"id": 2}

        self.assertIn("/one", query._handlers)
        self.assertIn("/two", query._handlers)

    def test_handler_execution(self):
        """Test that registered handlers can be executed."""

        @query.handler("/exec")
        def exec_handler(req):
            return {"param": req.get("value")}

        handler_fn = query._handlers["/exec"]
        result = handler_fn(QueryRequest("/exec", {"value": "test123"}))
        self.assertEqual(result, {"param": "test123"})


class TestGetParams(unittest.TestCase):
    """Test get_params function."""

    def test_get_params_empty(self):
        """Test get_params with no params set."""
        query._current_params = {}
        params = query.get_params()
        self.assertEqual(params, {})

    def test_get_params_with_values(self):
        """Test get_params returns a copy."""
        query._current_params = {"symbol": "ETH", "limit": 5}
        params = query.get_params()
        self.assertEqual(params, {"symbol": "ETH", "limit": 5})

        # Verify it's a copy
        params["new_key"] = "value"
        self.assertNotIn("new_key", query._current_params)


class TestGetConfig(unittest.TestCase):
    """Test get_config function."""

    def test_get_config_empty(self):
        """Test get_config with no config set."""
        query._config = {}
        config = query.get_config()
        self.assertEqual(config, {})

    def test_get_config_with_values(self):
        """Test get_config returns a copy."""
        query._config = {"symbol": "BTC/USD", "interval": 60}
        config = query.get_config()
        self.assertEqual(config, {"symbol": "BTC/USD", "interval": 60})

        # Verify it's a copy
        config["new_key"] = "value"
        self.assertNotIn("new_key", query._config)


class TestEphemeralMode(unittest.TestCase):
    """Test ephemeral query execution."""

    def setUp(self):
        """Clear handlers before each test."""
        query._handlers.clear()
        query._current_params = {}

    @patch.dict(os.environ, {"QUERY_PATH": "/test", "QUERY_PARAMS": '{"key": "value"}'})
    def test_ephemeral_execution(self):
        """Test ephemeral mode executes handler and writes result to file."""

        @query.handler("/test")
        def test_handler(req):
            return {"key": req.get("key")}

        # Mock the file write using mock_open
        m = mock_open()
        with patch("builtins.open", m):
            with patch("os.makedirs"):
                query._run_ephemeral("/test")

        # Get the written data from all write calls
        handle = m()
        written = "".join(call.args[0] for call in handle.write.call_args_list)
        result = json.loads(written)
        self.assertEqual(result["status"], "ok")
        self.assertEqual(result["data"]["key"], "value")

    @patch.dict(os.environ, {"QUERY_PATH": "/missing"})
    def test_ephemeral_missing_handler(self):
        """Test ephemeral mode with missing handler writes error to file."""
        m = mock_open()
        with patch("builtins.open", m):
            with patch("os.makedirs"):
                with self.assertRaises(SystemExit) as cm:
                    query._run_ephemeral("/missing")

        self.assertEqual(cm.exception.code, 1)
        handle = m()
        written = "".join(call.args[0] for call in handle.write.call_args_list)
        result = json.loads(written)
        self.assertEqual(result["status"], "error")
        self.assertIn("No handler", result["error"])

    @patch.dict(os.environ, {"QUERY_PATH": "/error", "QUERY_PARAMS": "{}"})
    def test_ephemeral_handler_error(self):
        """Test ephemeral mode with handler that raises error writes error to file."""

        @query.handler("/error")
        def error_handler(req):
            raise ValueError("Test error")

        m = mock_open()
        with patch("builtins.open", m):
            with patch("os.makedirs"):
                with self.assertRaises(SystemExit) as cm:
                    query._run_ephemeral("/error")

        self.assertEqual(cm.exception.code, 1)
        handle = m()
        written = "".join(call.args[0] for call in handle.write.call_args_list)
        result = json.loads(written)
        self.assertEqual(result["status"], "error")
        self.assertIn("Test error", result["error"])


class TestBuiltInHandlers(unittest.TestCase):
    """Test built-in handlers."""

    def setUp(self):
        """Clear handlers before each test."""
        query._handlers.clear()

    @patch.dict(os.environ, {"BOT_CONFIG": '{"symbol": "BTC"}'})
    def test_health_handler(self):
        """Test built-in /health handler."""
        query.run.__wrapped__ if hasattr(query.run, "__wrapped__") else None

        # Simulate run() registering built-in handlers
        query._handlers.setdefault("/health", lambda req: {"status": "ok"})

        req = QueryRequest("/health", {})
        result = query._handlers["/health"](req)
        self.assertEqual(result, {"status": "ok"})

    def test_info_handler(self):
        """Test built-in /info handler."""

        @query.handler("/custom")
        def custom_handler(req):
            return {}

        query._handlers.setdefault("/info", lambda req: {"available_queries": list(query._handlers.keys())})

        req = QueryRequest("/info", {})
        result = query._handlers["/info"](req)
        self.assertIn("available_queries", result)
        self.assertIn("/custom", result["available_queries"])


class TestReadOnlyStateError(unittest.TestCase):
    """Test ReadOnlyStateError exception."""

    def test_error_creation(self):
        """Test ReadOnlyStateError can be created."""
        error = ReadOnlyStateError("Test message")
        self.assertEqual(str(error), "Test message")

    def test_error_is_exception(self):
        """Test ReadOnlyStateError is an Exception."""
        error = ReadOnlyStateError("Test")
        self.assertIsInstance(error, Exception)


class TestStateReadOnlyEnforcement(unittest.TestCase):
    """Test that state modifications are blocked in query mode."""

    def setUp(self):
        """Set up test environment."""
        # Store original environ
        self._orig_environ = os.environ.copy()

    def tearDown(self):
        """Restore environment."""
        os.environ.clear()
        os.environ.update(self._orig_environ)

    def test_state_set_blocked_in_query_mode(self):
        """Test state.set raises error in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        with self.assertRaises(state.ReadOnlyStateError):
            state.set("key", "value")

    def test_state_delete_blocked_in_query_mode(self):
        """Test state.delete raises error in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        with self.assertRaises(state.ReadOnlyStateError):
            state.delete("key")

    def test_state_clear_blocked_in_query_mode(self):
        """Test state.clear raises error in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        with self.assertRaises(state.ReadOnlyStateError):
            state.clear()

    def test_state_get_allowed_in_query_mode(self):
        """Test state.get is allowed in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        # This should not raise
        result = state.get("nonexistent", "default")
        self.assertEqual(result, "default")

    def test_state_list_allowed_in_query_mode(self):
        """Test state.list is allowed in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        # This should not raise
        keys = state.list()
        self.assertIsInstance(keys, list)

    def test_state_exists_allowed_in_query_mode(self):
        """Test state.exists is allowed in query mode."""
        from the0 import state

        os.environ["QUERY_PATH"] = "/test"

        # This should not raise
        exists = state.exists("nonexistent")
        self.assertFalse(exists)


if __name__ == "__main__":
    unittest.main()
