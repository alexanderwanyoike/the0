"""
the0 State Module
=================

Provides persistent state management for bots across executions.
State is automatically synced to MinIO storage between bot runs.

Example:
    from the0 import state

    # Store state
    state.set("portfolio", {"AAPL": 100, "GOOGL": 50})

    # Retrieve state
    portfolio = state.get("portfolio", {})

    # List all keys
    keys = state.list()

    # Delete a key
    state.delete("portfolio")

    # Clear all state
    state.clear()
"""

import os
import json
from typing import Any, List, Optional


class ReadOnlyStateError(Exception):
    """Raised when attempting to modify state during query execution."""

    pass


def _is_query_mode() -> bool:
    """Check if currently running in query mode (read-only)."""
    return os.environ.get("QUERY_PATH") is not None


def _check_write_allowed() -> None:
    """Raise error if state modification is not allowed (query mode)."""
    if _is_query_mode():
        raise ReadOnlyStateError(
            "State modifications are not allowed during query execution. "
            "Queries are read-only. Use state.get() to read state values."
        )


def _get_state_dir() -> str:
    """Get the path to the state directory."""
    state_dir = os.environ.get("STATE_DIR")
    if state_dir:
        return state_dir
    # Fallback to default location
    return "/state/.the0-state"


def _get_key_path(key: str) -> str:
    """Get the file path for a state key."""
    state_dir = _get_state_dir()
    return os.path.join(state_dir, f"{key}.json")


def _validate_key(key: str) -> None:
    """Validate that a key is safe to use as a filename."""
    if not key:
        raise ValueError("State key cannot be empty")
    # Prevent directory traversal
    if "/" in key or "\\" in key or ".." in key:
        raise ValueError("State key cannot contain path separators or '..'")


def get(key: str, default: Any = None) -> Any:
    """
    Get a value from persistent state.

    Args:
        key: The state key (alphanumeric, hyphens, underscores)
        default: Default value if key doesn't exist

    Returns:
        The stored value, or default if not found

    Example:
        portfolio = state.get("portfolio", {})
        trade_count = state.get("trade_count", 0)
    """
    _validate_key(key)
    filepath = _get_key_path(key)
    try:
        with open(filepath, "r") as f:
            return json.load(f)
    except FileNotFoundError:
        return default
    except json.JSONDecodeError:
        return default


def set(key: str, value: Any) -> None:
    """
    Set a value in persistent state.

    The value must be JSON serializable.
    Note: This function will raise ReadOnlyStateError if called during query execution.

    Args:
        key: The state key (alphanumeric, hyphens, underscores)
        value: The value to store (must be JSON serializable)

    Raises:
        ReadOnlyStateError: If called during query execution (queries are read-only)

    Example:
        state.set("portfolio", {"AAPL": 100, "GOOGL": 50})
        state.set("trade_count", 42)
        state.set("last_prices", [45000.5, 45100.0, 45050.25])
    """
    _check_write_allowed()
    _validate_key(key)
    state_dir = _get_state_dir()
    os.makedirs(state_dir, exist_ok=True)
    filepath = _get_key_path(key)
    with open(filepath, "w") as f:
        json.dump(value, f)


def delete(key: str) -> bool:
    """
    Delete a key from persistent state.

    Note: This function will raise ReadOnlyStateError if called during query execution.

    Args:
        key: The state key to delete

    Returns:
        True if the key existed and was deleted, False otherwise

    Raises:
        ReadOnlyStateError: If called during query execution (queries are read-only)

    Example:
        if state.delete("old_data"):
            print("Cleaned up old data")
    """
    _check_write_allowed()
    _validate_key(key)
    filepath = _get_key_path(key)
    try:
        os.remove(filepath)
        return True
    except FileNotFoundError:
        return False


def list() -> List[str]:
    """
    List all keys in persistent state.

    Returns:
        List of state keys

    Example:
        keys = state.list()
        print(f"State contains {len(keys)} keys: {keys}")
    """
    state_dir = _get_state_dir()
    try:
        files = os.listdir(state_dir)
        return [f[:-5] for f in files if f.endswith(".json")]
    except FileNotFoundError:
        return []


def clear() -> None:
    """
    Clear all state.

    Removes all stored state keys.
    Note: This function will raise ReadOnlyStateError if called during query execution.

    Raises:
        ReadOnlyStateError: If called during query execution (queries are read-only)

    Example:
        state.clear()
        print("All state cleared")
    """
    _check_write_allowed()
    state_dir = _get_state_dir()
    try:
        for filename in os.listdir(state_dir):
            if filename.endswith(".json"):
                os.remove(os.path.join(state_dir, filename))
    except FileNotFoundError:
        pass


def exists(key: str) -> bool:
    """
    Check if a key exists in state.

    Args:
        key: The state key to check

    Returns:
        True if the key exists, False otherwise

    Example:
        if state.exists("portfolio"):
            portfolio = state.get("portfolio")
    """
    _validate_key(key)
    filepath = _get_key_path(key)
    return os.path.exists(filepath)
