package entrypoints

const PythonBacktestEntrypoint = `#!/usr/bin/env python3
"""
Python Backtest Entrypoint Script
Handles backtest execution with signal management and graceful shutdown.
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

def force_exit():
    print("SIGNAL: Timeout reached, forcing exit", file=sys.stderr)
    sys.exit(0)

def graceful_exit(status, message):
    global shutdown_timer
    if shutdown_timer:
        shutdown_timer.cancel()
    print(json.dumps({"status": status, "message": message}))
    sys.exit(0)

def run_main_with_callback(main_func, backtest_id, config):
    # Store the result from the user's main function
    result_container = [None]
    exception_container = [None]
    
    def target_wrapper():
        try:
            result_container[0] = main_func(backtest_id, config)
        except Exception as e:
            exception_container[0] = e
    
    # The backtest's main function is NOT changed. It doesn't know about the event.
    main_thread = threading.Thread(target=target_wrapper)
    main_thread.daemon = True # Allows main thread to exit even if this one is running
    main_thread.start()
    
    # The main thread waits here, but checks the shutdown event periodically
    while main_thread.is_alive():
        if shutdown_event.is_set():
            print("SIGNAL: Shutdown detected, exiting main process.", file=sys.stderr)
            # We don't wait for the thread, we just exit the script.
            return None # Return control to the main function's final block
        
        # Wait for 1 second at a time
        main_thread.join(timeout=1.0)
    
    # Check if there was an exception
    if exception_container[0]:
        raise exception_container[0]
    
    # Return the result from the user's main function
    return result_container[0]

def setup_environment():
    """Setup Python environment and working directory"""
    # Register signal handlers
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)

    print("STARTUP: Python backtest wrapper starting", file=sys.stderr)
    print(f"STARTUP: Python version {sys.version}", file=sys.stderr)
    print(f"STARTUP: Current working directory: {os.getcwd()}", file=sys.stderr)
    print("STARTUP: Signal handlers registered for SIGTERM and SIGINT", file=sys.stderr)

    # Ensure we can access the working directory
    try:
        print("CHDIR_ATTEMPT: Changing to /backtest directory", file=sys.stderr)
        os.chdir('/backtest')
        current_dir = os.getcwd()
        print(f"CHDIR_SUCCESS: Changed to working directory: {current_dir}", file=sys.stderr)
    except Exception as e:
        print(f"CHDIR_ERROR: Could not change to /backtest directory: {e}", file=sys.stderr)
        print(f"CHDIR_ERROR: Current directory: {os.getcwd()}", file=sys.stderr)
        sys.exit(1)

    # Setup Python path
    print("PATH_SETUP: Adding /backtest to Python path", file=sys.stderr)
    sys.path.insert(0, '/backtest')

    # Check for vendor directory exists and add to path
    if os.path.exists('/backtest/vendor'):
        print("VENDOR_CHECK: vendor directory found, adding to Python path", file=sys.stderr)
        sys.path.insert(0, '/backtest/vendor')

        # List vendor contents for debugging
        try:
            vendor_contents = os.listdir('/backtest/vendor')
            print(f"VENDOR_CONTENTS: {vendor_contents}", file=sys.stderr)
        except Exception as e:
            print(f"VENDOR_LIST_ERROR: Could not list vendor directory: {e}", file=sys.stderr)
    else:
        print("VENDOR_CHECK: No vendor directory found", file=sys.stderr)
    
    # Check for requirements.txt
    if os.path.exists('/backtest/requirements.txt'):
        print("REQUIREMENTS_CHECK: requirements.txt found", file=sys.stderr)
        try:
            with open('/backtest/requirements.txt', 'r') as f:
                requirements = f.read().strip()
                print(f"REQUIREMENTS_LIST: {requirements}", file=sys.stderr)
        except Exception as e:
            print(f"REQUIREMENTS_READ_ERROR: Could not read requirements.txt: {e}", file=sys.stderr)
    else:
        print("REQUIREMENTS_CHECK: No requirements.txt found", file=sys.stderr)


    # Configure logging to show info messages
    logging.basicConfig(level=logging.INFO)

def import_main_module(script_path):
    """Import the main module using explicit module loading"""
    print("IMPORT_ATTEMPT: Starting main module import", file=sys.stderr)
    try:
        print("IMPORT_STEP: Importing importlib.util", file=sys.stderr)
        import importlib.util
        
        print(f"IMPORT_STEP: Creating spec for /backtest/{script_path}", file=sys.stderr)
        spec = importlib.util.spec_from_file_location("main", f"/backtest/{script_path}")
        
        if spec and spec.loader:
            print("IMPORT_STEP: Module spec created successfully", file=sys.stderr)
            main_module = importlib.util.module_from_spec(spec)
            
            print("IMPORT_STEP: Executing main module", file=sys.stderr)
            spec.loader.exec_module(main_module)
            
            print("IMPORT_STEP: Getting main function", file=sys.stderr)
            main = getattr(main_module, 'main')
            print("IMPORT_SUCCESS: Main function imported successfully", file=sys.stderr)
            return main
        else:
            raise ImportError(f"Could not load spec for /backtest/{script_path}")
            
    except ImportError as e:
        print(f"IMPORT_ERROR: Import failed: {e}", file=sys.stderr)
        print(f"IMPORT_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"IMPORT_FATAL: Unexpected error during import: {e}", file=sys.stderr)
        print(f"IMPORT_FATAL: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)

def parse_config():
    """Parse backtest configuration from environment variables"""
    print("CONFIG_PARSE: Parsing backtest configuration", file=sys.stderr)
    try:
        config = json.loads(os.environ.get('BACKTEST_CONFIG', '{}'))
        backtest_id = os.environ.get('BACKTEST_ID', '')
        script_path = os.environ.get('SCRIPT_PATH', 'main.py')
        print(f"CONFIG_SUCCESS: Backtest ID: {backtest_id}", file=sys.stderr)
        print(f"CONFIG_SUCCESS: Script path: {script_path}", file=sys.stderr)
        return backtest_id, config, script_path
    except Exception as e:
        print(f"CONFIG_ERROR: Failed to parse configuration: {e}", file=sys.stderr)
        sys.exit(1)

def main():
    """Main entrypoint function"""
    # Setup environment
    setup_environment()
    
    # Parse configuration
    backtest_id, config, script_path = parse_config()
    
    # Import main module
    main_func = import_main_module(script_path)
    
    # Execute the main function
    print("EXECUTE_ATTEMPT: Starting backtest execution", file=sys.stderr)
    try:
        result = run_main_with_callback(main_func, backtest_id, config)
        if shutdown_event.is_set():
            result_data = {"status": "terminated", "message": "Backtest execution terminated by signal"}
            # Write result to file for reliable parsing
            with open("/backtest/result.json", "w") as f:
                json.dump(result_data, f)
            print(json.dumps(result_data))
            sys.exit(0) # Exit gracefully if shutdown event is set
        else:
            # Prepare the result data
            if result is not None:
                result_data = result
            else:
                result_data = {"status": "success", "message": "Backtest executed successfully"}
            
            # Write result to file for reliable parsing by the worker
            print("RESULT_WRITE: Writing result to /backtest/result.json", file=sys.stderr)
            with open("/backtest/result.json", "w") as f:
                json.dump(result_data, f)
            print("RESULT_WRITE: Successfully wrote result file", file=sys.stderr)
            
            # Also output to stdout for backwards compatibility
            print(json.dumps(result_data))
    except Exception as e:
        error_result = {"status": "error", "message": str(e)}
        print(f"EXECUTE_ERROR: Backtest execution failed: {e}", file=sys.stderr)
        print(f"EXECUTE_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        
        # Write error result to file
        try:
            with open("/backtest/result.json", "w") as f:
                json.dump(error_result, f)
            print("RESULT_WRITE: Wrote error result to file", file=sys.stderr)
        except Exception as write_error:
            print(f"RESULT_WRITE_ERROR: Failed to write error result: {write_error}", file=sys.stderr)
        
        print(json.dumps(error_result))
        sys.exit(1)

if __name__ == "__main__":
    main()
`


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

def force_exit():
    print("SIGNAL: Timeout reached, forcing exit", file=sys.stderr)
    sys.exit(0)

def graceful_exit(status, message):
    global shutdown_timer
    if shutdown_timer:
        shutdown_timer.cancel()
    print(json.dumps({"status": status, "message": message}))
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

    print("STARTUP: Python bot wrapper starting", file=sys.stderr)
    print(f"STARTUP: Python version {sys.version}", file=sys.stderr)
    print(f"STARTUP: Current working directory: {os.getcwd()}", file=sys.stderr)
    print("STARTUP: Signal handlers registered for SIGTERM and SIGINT", file=sys.stderr)

    # Get code mount directory from environment (where the code is actually mounted)
    code_mount_dir = os.environ.get('CODE_MOUNT_DIR', 'bot')
    work_dir = f"/{code_mount_dir}"
    
    # Ensure we can access the working directory
    try:
        print(f"CHDIR_ATTEMPT: Changing to {work_dir} directory", file=sys.stderr)
        os.chdir(work_dir)
        current_dir = os.getcwd()
        print(f"CHDIR_SUCCESS: Changed to working directory: {current_dir}", file=sys.stderr)
    except Exception as e:
        print(f"CHDIR_ERROR: Could not change to {work_dir} directory: {e}", file=sys.stderr)
        print(f"CHDIR_ERROR: Current directory: {os.getcwd()}", file=sys.stderr)
        sys.exit(1)

    # Setup Python path
    print(f"PATH_SETUP: Adding {work_dir} to Python path", file=sys.stderr)
    sys.path.insert(0, work_dir)

    # Check for vendor directory exists and add to path
    vendor_dir = f"{work_dir}/vendor"
    if os.path.exists(vendor_dir):
        print("VENDOR_CHECK: vendor directory found, adding to Python path", file=sys.stderr)
        sys.path.insert(0, vendor_dir)

        # List vendor contents for debugging
        try:
            vendor_contents = os.listdir(vendor_dir)
            print(f"VENDOR_CONTENTS: {vendor_contents}", file=sys.stderr)
        except Exception as e:
            print(f"VENDOR_LIST_ERROR: Could not list vendor directory: {e}", file=sys.stderr)
    else:
        print("VENDOR_CHECK: No vendor directory found", file=sys.stderr)
    
    # Check for requirements.txt
    requirements_file = f"{work_dir}/requirements.txt"
    if os.path.exists(requirements_file):
        print("REQUIREMENTS_CHECK: requirements.txt found", file=sys.stderr)
        try:
            with open(requirements_file, 'r') as f:
                requirements = f.read().strip()
                print(f"REQUIREMENTS_LIST: {requirements}", file=sys.stderr)
        except Exception as e:
            print(f"REQUIREMENTS_READ_ERROR: Could not read requirements.txt: {e}", file=sys.stderr)
    else:
        print("REQUIREMENTS_CHECK: No requirements.txt found", file=sys.stderr)


    # Configure logging to show info messages
    logging.basicConfig(level=logging.INFO)

def import_main_module(script_path):
    """Import the main module using explicit module loading"""
    print("IMPORT_ATTEMPT: Starting main module import", file=sys.stderr)
    try:
        print("IMPORT_STEP: Importing importlib.util", file=sys.stderr)
        import importlib.util
        
        # Get code mount directory from environment (where the code is actually mounted)
        code_mount_dir = os.environ.get('CODE_MOUNT_DIR', 'bot')
        work_dir = f"/{code_mount_dir}"
        
        print(f"IMPORT_STEP: Creating spec for {work_dir}/{script_path}", file=sys.stderr)
        spec = importlib.util.spec_from_file_location("main", f"{work_dir}/{script_path}")
        
        if spec and spec.loader:
            print("IMPORT_STEP: Module spec created successfully", file=sys.stderr)
            main_module = importlib.util.module_from_spec(spec)
            
            print("IMPORT_STEP: Executing main module", file=sys.stderr)
            spec.loader.exec_module(main_module)
            
            print("IMPORT_STEP: Getting main function", file=sys.stderr)
            main = getattr(main_module, 'main')
            print("IMPORT_SUCCESS: Main function imported successfully", file=sys.stderr)
            return main
        else:
            raise ImportError(f"Could not load spec for {work_dir}/{script_path}")
            
    except ImportError as e:
        print(f"IMPORT_ERROR: Import failed: {e}", file=sys.stderr)
        print(f"IMPORT_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"IMPORT_FATAL: Unexpected error during import: {e}", file=sys.stderr)
        print(f"IMPORT_FATAL: Traceback: {traceback.format_exc()}", file=sys.stderr)
        sys.exit(1)

def parse_config():
    """Parse bot configuration from environment variables"""
    print("CONFIG_PARSE: Parsing bot configuration", file=sys.stderr)
    try:
        config = json.loads(os.environ.get('BOT_CONFIG', '{}'))
        bot_id = os.environ.get('BOT_ID', '')
        script_path = os.environ.get('SCRIPT_PATH', 'main.py')
        print(f"CONFIG_SUCCESS: Bot ID: {bot_id}", file=sys.stderr)
        print(f"CONFIG_SUCCESS: Script path: {script_path}", file=sys.stderr)
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
    print("EXECUTE_ATTEMPT: Starting bot execution", file=sys.stderr)
    try:
        result = run_main_with_callback(main_func, bot_id, config)
        if shutdown_event.is_set():
            print(json.dumps({"status": "terminated", "message": "Bot execution terminated by signal"}))
            sys.exit(0) # Exit gracefully if shutdown event is set
        else:
            print(json.dumps({"status": "success", "message": "Bot executed successfully"}))
    except Exception as e:
        print(f"EXECUTE_ERROR: Bot execution failed: {e}", file=sys.stderr)
        print(f"EXECUTE_ERROR: Traceback: {traceback.format_exc()}", file=sys.stderr)
        print(json.dumps({"status": "error", "message": str(e)}))
        sys.exit(1)

if __name__ == "__main__":
    main()
`
