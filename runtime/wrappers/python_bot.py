#!/usr/bin/env python3
"""
Python Bot Entrypoint Script
Handles bot execution with signal management and graceful shutdown.
"""

import json
import sys
import os
import signal
import traceback
import threading
import logging
from datetime import datetime, timezone

DEFAULT_SUCCESS = {"status": "success", "message": "Bot executed successfully"}

# Signal handling for graceful shutdown
main_thread = None
shutdown_timer = None
shutdown_event = threading.Event()

def signal_handler(signum, frame):
    signal_name = signal.Signals(signum).name
    print(f"SIGNAL: Received {signal_name} ({signum}), setting shutdown event.", file=sys.stderr)
    # --- Set the event to signal all threads to stop ---
    shutdown_event.set()

def get_result_file_path():
    """Get the path to the result file"""
    code_mount_dir = os.environ.get('CODE_MOUNT_DIR', 'bot')
    return f"/{code_mount_dir}/result.json"

def write_result(data):
    """Write result to the result file"""
    result_path = get_result_file_path()
    try:
        with open(result_path, 'w') as f:
            json.dump(data, f)
    except Exception as e:
        print(f"RESULT_ERROR: Failed to write result file: {e}", file=sys.stderr)

def clear_stale_result():
    try:
        os.remove(get_result_file_path())
    except FileNotFoundError:
        pass
    except OSError as e:
        print(f"RESULT_ERROR: Failed to remove stale result file: {e}", file=sys.stderr)

def read_result():
    try:
        with open(get_result_file_path()) as f:
            return json.load(f)
    except (OSError, ValueError):
        return None

def settle_result(fallback):
    """Keep the result the bot wrote this run, or record the fallback; return the run's result."""
    if os.path.exists(get_result_file_path()):
        return read_result()
    write_result(fallback)
    return fallback

def is_error_result(result):
    return isinstance(result, dict) and result.get("status") == "error"

def report_error(result):
    # The SDK's error() only writes result.json, which nothing reads for bot
    # runs, so without this line a failed run leaves no trace in the logs.
    if not is_error_result(result):
        return
    entry = {
        "level": "error",
        "message": result.get("message", "Bot reported an error"),
        "timestamp": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    }
    print(json.dumps(entry), file=sys.stderr)

def exit_code_of(exc):
    # Same mapping the interpreter applies when SystemExit reaches the top level.
    if exc.code is None:
        return 0
    if isinstance(exc.code, int):
        return exc.code
    print(exc.code, file=sys.stderr)
    return 1

def force_exit():
    print("SIGNAL: Timeout reached, forcing exit", file=sys.stderr)
    sys.exit(0)

def graceful_exit(status, message):
    global shutdown_timer
    if shutdown_timer:
        shutdown_timer.cancel()
    write_result({"status": status, "message": message})
    sys.exit(0)

def run_main_with_callback(main_func, bot_id, config):
    """Run the bot and return its exit code and failure message, or None on shutdown."""
    outcome = {"exit_code": 0, "message": None}

    def run_bot():
        # A thread swallows SystemExit and uncaught exceptions, so the SDK's
        # error() (sys.exit(1)) and a crash would otherwise look like success.
        try:
            main_func(bot_id, config)
        except SystemExit as e:
            outcome["exit_code"] = exit_code_of(e)
        except BaseException as e:
            print(f"EXECUTE_ERROR: Bot execution failed: {e}", file=sys.stderr)
            print(f"EXECUTE_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
            outcome["exit_code"] = 1
            outcome["message"] = str(e)

    # The bot's main function is NOT changed. It doesn't know about the event.
    main_thread = threading.Thread(target=run_bot)
    main_thread.daemon = True # Allows main thread to exit even if this one is running
    main_thread.start()

    # The main thread waits here, but checks the shutdown event periodically
    while main_thread.is_alive():
        if shutdown_event.is_set():
            print("SIGNAL: Shutdown detected, exiting main process.", file=sys.stderr)
            # We don't wait for the thread, we just exit the script.
            return None

        # Wait for 1 second at a time
        main_thread.join(timeout=1.0)

    return outcome

def setup_environment():
    """Setup Python environment and working directory"""
    # Register signal handlers
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)

    # Get code mount directory from environment (where the code is actually mounted)
    code_mount_dir = os.environ.get('CODE_MOUNT_DIR', 'bot')
    work_dir = f"/{code_mount_dir}"

    # Ensure we can access the working directory
    try:
        os.chdir(work_dir)
    except Exception as e:
        print(f"CHDIR_ERROR: Could not change to {work_dir} directory: {e}", file=sys.stderr)
        sys.exit(1)

    # Setup Python path
    sys.path.insert(0, work_dir)

    # Add vendor directory to path if it exists
    vendor_dir = f"{work_dir}/vendor"
    if os.path.exists(vendor_dir):
        sys.path.insert(0, vendor_dir)

    # Configure logging to show info messages
    logging.basicConfig(level=logging.INFO)

def import_main_module(script_path):
    """Import the main module using explicit module loading"""
    try:
        import importlib.util

        # Get code mount directory from environment (where the code is actually mounted)
        code_mount_dir = os.environ.get('CODE_MOUNT_DIR', 'bot')
        work_dir = f"/{code_mount_dir}"

        spec = importlib.util.spec_from_file_location("main", f"{work_dir}/{script_path}")

        if spec and spec.loader:
            main_module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(main_module)
            main = getattr(main_module, 'main')
            return main
        else:
            raise ImportError(f"Could not load spec for {work_dir}/{script_path}")

    except ImportError as e:
        print(f"IMPORT_ERROR: Import failed: {e}", file=sys.stderr)
        print(f"IMPORT_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"IMPORT_ERROR: Unexpected error during import: {e}", file=sys.stderr)
        print(f"IMPORT_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)

def parse_config():
    """Parse bot configuration from environment variables"""
    try:
        config = json.loads(os.environ.get('BOT_CONFIG', '{}'))
        bot_id = os.environ.get('BOT_ID', '')
        script_path = os.environ.get('SCRIPT_PATH', 'main.py')
        return bot_id, config, script_path
    except Exception as e:
        print(f"CONFIG_ERROR: Failed to parse configuration: {e}", file=sys.stderr)
        sys.exit(1)

def main():
    """Main entrypoint function"""
    # Setup environment
    setup_environment()

    # Parse configuration
    bot_id, config, script_path = parse_config()

    # A result.json from an earlier run in the same code directory must not
    # pass for one this run's bot wrote.
    clear_stale_result()

    # Import main module
    main_func = import_main_module(script_path)

    # Execute the main function
    outcome = run_main_with_callback(main_func, bot_id, config)
    if shutdown_event.is_set():
        settle_result({"status": "terminated", "message": "Bot execution terminated by signal"})
        sys.exit(0) # Exit gracefully if shutdown event is set

    exit_code = outcome["exit_code"]
    if exit_code == 0:
        fallback = DEFAULT_SUCCESS
    else:
        fallback = {"status": "error", "message": outcome["message"] or f"Bot exited with code {exit_code}"}
    report_error(settle_result(fallback))
    sys.exit(exit_code)

if __name__ == "__main__":
    main()
