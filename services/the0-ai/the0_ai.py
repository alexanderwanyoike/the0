#!/usr/bin/env python3
"""
the0-ai Desktop Application
A cross-platform desktop application for the0 AI Agent Workbench
"""

import os
import sys
import time
import subprocess
import threading
import webbrowser
import argparse
from pathlib import Path

# Try to import pywebview for embedded browser window
try:
    import webview
    HAS_WEBVIEW = True
    print(f"pywebview available")
except ImportError:
    HAS_WEBVIEW = False
    print("pywebview not available - will use browser")

# Add the current directory to Python path for imports
sys.path.insert(0, str(Path(__file__).parent))

def check_dependencies():
    """Check if required dependencies are installed."""
    try:
        import uvicorn
        import fastapi
        return True
    except ImportError as e:
        print(f"Missing dependencies: {e}")
        print("Please install required packages:")
        print("pip install fastapi uvicorn")
        return False

def start_backend():
    """Start the FastAPI backend server."""
    print("Starting backend server...")
    try:
        # Import and run the FastAPI app
        from api.main import app
        import uvicorn
        
        # Run server in a way that doesn't block
        uvicorn.run(
            app, 
            host="127.0.0.1", 
            port=8000, 
            log_level="info",
            access_log=False
        )
    except Exception as e:
        print(f"Failed to start backend: {e}")
        sys.exit(1)

def start_frontend():
    """Serve the built React frontend from the executable."""
    print("Starting frontend server...")
    try:
        from fastapi import FastAPI
        from fastapi.staticfiles import StaticFiles
        from fastapi.responses import FileResponse
        import uvicorn
        
        # Create a simple static file server for the built frontend
        frontend_app = FastAPI()
        
        # Get the bundled frontend dist directory
        if getattr(sys, 'frozen', False):
            # Running as PyInstaller executable
            bundle_dir = Path(sys._MEIPASS)
            static_dir = bundle_dir / "frontend" / "dist"
        else:
            # Running as script
            static_dir = Path(__file__).parent / "frontend" / "dist"
        
        if not static_dir.exists():
            print(f"Frontend dist directory not found at {static_dir}")
            return False
        
        # Mount static files
        frontend_app.mount("/assets", StaticFiles(directory=static_dir / "assets"), name="assets")
        
        # Serve index.html for all routes (SPA routing)
        @frontend_app.get("/{full_path:path}")
        async def serve_spa(full_path: str = ""):
            # Serve the React SPA for all routes
            return FileResponse(static_dir / "index.html")
        
        # Start the frontend server in a separate thread
        def run_frontend():
            uvicorn.run(frontend_app, host="127.0.0.1", port=3000, log_level="error")
        
        frontend_thread = threading.Thread(target=run_frontend, daemon=True)
        frontend_thread.start()
        
        return True
        
    except Exception as e:
        print(f"Failed to start frontend server: {e}")
        return False

def wait_for_server(url, timeout=30):
    """Wait for a server to be ready."""
    import requests
    start_time = time.time()
    
    while time.time() - start_time < timeout:
        try:
            response = requests.get(url, timeout=1)
            if response.status_code == 200:
                return True
        except requests.exceptions.RequestException:
            pass
        time.sleep(0.5)
    
    return False

def open_desktop_app(force_browser=False):
    """Open the application in a desktop window on the main thread."""
    app_url = "http://localhost:3000"
    
    # Wait for frontend to be ready
    print("Waiting for frontend to start...")
    if not wait_for_server(app_url, timeout=60):
        print("Frontend failed to start in time")
        return
    
    print(f"Opening the0 AI Agent Workbench at {app_url}")
    
    if HAS_WEBVIEW and not force_browser:
        # Try to open in desktop window
        try:
            print("Creating desktop window...")
            webview.settings['ALLOW_DOWNLOADS'] = True
            webview.create_window(
                title="the0 AI Agent Workbench",
                url=app_url,
                width=1400,
                height=900,
                min_size=(1000, 700),
                resizable=True,
            )
            print("Starting desktop window...")
            webview.start(debug=False)
            print("Desktop window closed, shutting down...")
        except Exception as e:
            print(f"Desktop window failed: {e}")
            print("Opening in browser instead...")
            webbrowser.open(app_url)
            try:
                print("Press Ctrl+C to stop")
                while True:
                    time.sleep(1)
            except KeyboardInterrupt:
                print("\nShutting down...")
    else:
        if force_browser:
            print("Browser mode requested...")
        else:
            print("Opening in browser...")
        webbrowser.open(app_url)
        try:
            print("Press Ctrl+C to stop")
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            print("\nShutting down...")

def main():
    """Main entry point for the0-ai desktop application."""
    # Parse command line arguments
    parser = argparse.ArgumentParser(description="the0 AI Agent Workbench")
    parser.add_argument("--browser", action="store_true", help="Force browser mode instead of desktop window")
    parser.add_argument("--version", action="store_true", help="Show version information and exit")
    args = parser.parse_args()
    
    # Handle version command
    if args.version:
        from version import get_version, get_build_info, get_update_bucket
        build_info = get_build_info()
        print(f"the0-ai version {build_info['version']}")
        print(f"Build date: {build_info['build_date']}")
        print(f"Commit: {build_info['commit_hash']}")
        print(f"Update stream: {get_update_bucket()}")
        return
    
    print("🤖 Starting the0 AI Agent Workbench...")
    
    # Show update stream info
    from version import get_update_bucket
    update_stream = os.environ.get("THE0_AI_UPDATE_STREAM", "release")
    print(f"📡 Update stream: {update_stream} ({get_update_bucket()})")
    
    # Check dependencies
    if not check_dependencies():
        sys.exit(1)
    
    try:
        # Start backend in a separate thread
        backend_thread = threading.Thread(target=start_backend, daemon=True)
        backend_thread.start()
        
        # Wait for backend to be ready
        print("Waiting for backend to start...")
        if not wait_for_server("http://127.0.0.1:8000/health", timeout=30):
            print("Backend failed to start")
            sys.exit(1)
        
        print("✅ Backend server running at http://127.0.0.1:8000")
        
        # Start frontend
        if not start_frontend():
            print("Failed to start frontend")
            sys.exit(1)
        
        print("✅ Frontend server starting...")
        
        print("\n🚀 the0 AI Agent Workbench is ready!")
        
        # Open the application on the main thread (required for pywebview)
        open_desktop_app(force_browser=args.browser)
            
    except Exception as e:
        print(f"Error starting application: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()