package entrypoints

const PythonBotEntrypoint = `
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
    # The bot's main function is NOT changed. It doesn't know about the event.
    main_thread = threading.Thread(target=main_func, args=(bot_id, config))
    main_thread.daemon = True # Allows main thread to exit even if this one is running
    main_thread.start()
    
    # The main thread waits here, but checks the shutdown event periodically
    while main_thread.is_alive():
        if shutdown_event.is_set():
            print("SIGNAL: Shutdown detected, exiting main process.", file=sys.stderr)
            # We don't wait for the thread, we just exit the script.
            return # Return control to the main function's final block
        
        # Wait for 1 second at a time
        main_thread.join(timeout=1.0)

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

    # Import main module
    main_func = import_main_module(script_path)

    # Execute the main function
    try:
        result = run_main_with_callback(main_func, bot_id, config)
        if shutdown_event.is_set():
            write_result({"status": "terminated", "message": "Bot execution terminated by signal"})
            sys.exit(0) # Exit gracefully if shutdown event is set
        else:
            write_result({"status": "success", "message": "Bot executed successfully"})
    except Exception as e:
        print(f"EXECUTE_ERROR: Bot execution failed: {e}", file=sys.stderr)
        print(f"EXECUTE_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        write_result({"status": "error", "message": str(e)})
        sys.exit(1)

if __name__ == "__main__":
    main()
`
