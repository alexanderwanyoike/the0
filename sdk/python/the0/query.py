"""
the0 Query Module - Express-like handler interface for bot queries.

This module provides query handling capabilities for bots, allowing users to define
custom read-only query handlers that can be executed on demand.

Separate namespace from state and main SDK. Users explicitly import:
    from the0 import query
    from the0 import state  # If needed in handlers

Example:
    from the0 import query, state

    @query.handler("/portfolio")
    def get_portfolio(req):
        positions = state.get("positions", [])
        return {"positions": positions, "count": len(positions)}

    @query.handler("/status")
    def get_status(req):
        symbol = req.get("symbol", "BTC/USD")
        return {"symbol": symbol, "active": True}

    query.run()
"""

import os
import sys
import json
from typing import Any, Callable, Dict, Optional, List
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

__version__ = "1.0.0"
__all__ = ["handler", "run", "get_params", "get_config", "QueryRequest", "ReadOnlyStateError"]

# Handler registry - maps paths to handler functions
_handlers: Dict[str, Callable] = {}

# Query parameters for current request (set before handler called)
_current_params: Dict[str, Any] = {}

# Bot config loaded from environment
_config: Dict[str, Any] = {}


class ReadOnlyStateError(Exception):
    """Raised when attempting to modify state during query execution."""

    pass


class QueryRequest:
    """
    Request object passed to handlers (Express-like).

    Attributes:
        path: The query path being requested (e.g., "/portfolio")
        params: Dictionary of query parameters

    Example:
        @query.handler("/signals")
        def get_signals(req):
            symbol = req.get("symbol")  # Get param with None default
            limit = req.get("limit", 10)  # Get param with custom default
            return {"symbol": symbol, "limit": limit}
    """

    def __init__(self, path: str, params: Dict[str, Any]):
        self.path = path
        self.params = params

    def get(self, key: str, default: Any = None) -> Any:
        """Get a parameter value with optional default."""
        return self.params.get(key, default)

    def __repr__(self) -> str:
        return f"QueryRequest(path={self.path!r}, params={self.params!r})"


def handler(path: str):
    """
    Decorator to register a query handler (Express-like).

    Args:
        path: The query path to handle (e.g., "/portfolio", "/signals")

    Returns:
        Decorator function that registers the handler

    Example:
        @query.handler("/portfolio")
        def get_portfolio(req: QueryRequest):
            symbol = req.get("symbol")
            positions = state.get("positions", [])
            return {"positions": positions, "symbol": symbol}

        @query.handler("/health")
        def health_check(req):
            return {"status": "ok", "uptime": 12345}
    """

    def decorator(func: Callable):
        _handlers[path] = func
        return func

    return decorator


def get_params() -> Dict[str, Any]:
    """
    Get current query parameters (alternative to request object).

    Returns:
        Copy of the current query parameters dictionary

    Example:
        @query.handler("/example")
        def example_handler(req):
            # Both approaches work:
            params = query.get_params()
            symbol = params.get("symbol")
            # Or use request object:
            symbol = req.get("symbol")
    """
    return _current_params.copy()


def get_config() -> Dict[str, Any]:
    """
    Get the bot configuration.

    Returns:
        Copy of the bot configuration dictionary

    Example:
        @query.handler("/status")
        def get_status(req):
            config = query.get_config()
            return {"symbol": config.get("symbol"), "interval": config.get("interval")}
    """
    return _config.copy()


def run():
    """
    Run the query system with automatic mode detection.

    Modes:
    - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
    - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
    - Neither: Info mode (print available handlers)

    Environment Variables:
        QUERY_PATH: Path of query to execute (ephemeral mode)
        QUERY_PARAMS: JSON string of query parameters
        BOT_TYPE: Set to "realtime" to run as HTTP server
        THE0_QUERY_PORT: HTTP server port (default: 9476)
        BOT_CONFIG: JSON string of bot configuration

    Example:
        # In query.py
        from the0 import query, state

        @query.handler("/portfolio")
        def get_portfolio(req):
            return {"positions": state.get("positions", [])}

        query.run()  # Automatically detects mode
    """
    global _config

    # Load bot config from environment
    config_str = os.environ.get("BOT_CONFIG", "{}")
    try:
        _config = json.loads(config_str)
    except json.JSONDecodeError:
        _config = {}

    # Register built-in handlers
    _handlers.setdefault("/health", lambda req: {"status": "ok"})
    _handlers.setdefault("/info", lambda req: {"available_queries": list(_handlers.keys())})

    query_path = os.environ.get("QUERY_PATH")
    bot_type = os.environ.get("BOT_TYPE")

    if query_path:
        _run_ephemeral(query_path)
    elif bot_type == "realtime":
        _run_server()
    else:
        _run_ephemeral("/info")


def _write_result(result: dict):
    """Write query result to /query/result.json file."""
    result_path = "/query/result.json"
    try:
        os.makedirs("/query", exist_ok=True)
        with open(result_path, "w") as f:
            json.dump(result, f)
    except Exception as e:
        print(f"RESULT_ERROR: Failed to write result file: {e}", file=sys.stderr)


def _run_ephemeral(query_path: str):
    """Execute single query and write result to /bot/result.json."""
    global _current_params

    # Parse parameters from environment
    params_str = os.environ.get("QUERY_PARAMS", "{}")
    try:
        _current_params = json.loads(params_str)
    except json.JSONDecodeError:
        _current_params = {}

    # Find and execute handler
    handler_fn = _handlers.get(query_path)
    if not handler_fn:
        result = {
            "status": "error",
            "error": f"No handler for path: {query_path}",
            "available": list(_handlers.keys()),
        }
        _write_result(result)
        sys.exit(1)

    try:
        request = QueryRequest(query_path, _current_params)
        data = handler_fn(request)
        _write_result({"status": "ok", "data": data})
    except Exception as e:
        _write_result({"status": "error", "error": str(e)})
        sys.exit(1)


def _run_server():
    """Start HTTP server on port 9476 for realtime bots."""
    port = int(os.environ.get("THE0_QUERY_PORT", "9476"))

    class QueryHTTPHandler(BaseHTTPRequestHandler):
        def do_GET(self):
            self._handle_request()

        def do_POST(self):
            self._handle_request()

        def _handle_request(self):
            global _current_params

            parsed = urlparse(self.path)
            path = parsed.path
            _current_params = {k: v[0] for k, v in parse_qs(parsed.query).items()}

            handler_fn = _handlers.get(path)
            if not handler_fn:
                self._send_json(404, {"status": "error", "error": f"No handler for path: {path}"})
                return

            try:
                request = QueryRequest(path, _current_params)
                result = handler_fn(request)
                self._send_json(200, {"status": "ok", "data": result})
            except Exception as e:
                self._send_json(500, {"status": "error", "error": str(e)})

        def _send_json(self, status: int, data: dict):
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(data).encode())

        def log_message(self, format, *args):
            pass  # Suppress default logging

    server = HTTPServer(("0.0.0.0", port), QueryHTTPHandler)
    print(
        json.dumps({"_log": "info", "message": f"Query server started on port {port}"}),
        file=sys.stderr,
    )
    server.serve_forever()


def is_query_mode() -> bool:
    """
    Check if currently running in query mode.

    Returns:
        True if QUERY_PATH environment variable is set

    This is used by the state module to enforce read-only behavior.
    """
    return os.environ.get("QUERY_PATH") is not None
