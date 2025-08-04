#!/usr/bin/env python3

import subprocess
import os
import sys
import time
import signal
import threading


class WorkbenchLauncher:
    def __init__(self):
        self.processes = []
        self.base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        
    def cleanup(self, signum=None, frame=None):
        """Clean up running processes."""
        print("\n🛑 Stopping the0 AI Agent Workbench...")
        for process in self.processes:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
        print("👋 All services stopped")
        sys.exit(0)
    
    def start_backend(self):
        """Start the FastAPI backend server."""
        print("🔧 Starting backend server...")
        try:
            process = subprocess.Popen(
                [sys.executable, "scripts/start_server.py"],
                cwd=self.base_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True
            )
            self.processes.append(process)
            return process
        except Exception as e:
            print(f"❌ Failed to start backend: {e}")
            return None
    
    def start_frontend(self):
        """Start the React frontend server."""
        print("🎨 Starting frontend server...")
        frontend_dir = os.path.join(self.base_dir, "frontend")
        try:
            process = subprocess.Popen(
                ["npm", "run", "dev"],
                cwd=frontend_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True
            )
            self.processes.append(process)
            return process
        except Exception as e:
            print(f"❌ Failed to start frontend: {e}")
            return None
    
    def monitor_process(self, process, name):
        """Monitor a process and print its output."""
        try:
            for line in process.stdout:
                print(f"[{name}] {line.rstrip()}")
        except Exception:
            pass
    
    def run(self):
        """Start the complete workbench."""
        print("🚀 Starting the0 AI Agent Workbench")
        print("=" * 60)
        print("🤖 Backend API: http://localhost:8000")
        print("🌐 Frontend UI: http://localhost:3000")  
        print("📚 API Docs: http://localhost:8000/docs")
        print("🔄 Press Ctrl+C to stop all services")
        print("=" * 60)
        
        # Set up signal handlers
        signal.signal(signal.SIGINT, self.cleanup)
        signal.signal(signal.SIGTERM, self.cleanup)
        
        # Start backend
        backend_process = self.start_backend()
        if not backend_process:
            return 1
        
        # Wait a moment for backend to start
        time.sleep(3)
        
        # Start frontend
        frontend_process = self.start_frontend()
        if not frontend_process:
            self.cleanup()
            return 1
        
        # Start monitoring threads
        backend_thread = threading.Thread(
            target=self.monitor_process, 
            args=(backend_process, "BACKEND"),
            daemon=True
        )
        frontend_thread = threading.Thread(
            target=self.monitor_process, 
            args=(frontend_process, "FRONTEND"),
            daemon=True
        )
        
        backend_thread.start()
        frontend_thread.start()
        
        print("\n✅ the0 AI Agent Workbench is running!")
        print("🌐 Open http://localhost:3000 to start building trading bots")
        
        try:
            # Wait for processes to complete
            while True:
                time.sleep(1)
                if backend_process.poll() is not None:
                    print("❌ Backend process stopped unexpectedly")
                    break
                if frontend_process.poll() is not None:
                    print("❌ Frontend process stopped unexpectedly")
                    break
        except KeyboardInterrupt:
            pass
        
        self.cleanup()
        return 0


def main():
    launcher = WorkbenchLauncher()
    return launcher.run()


if __name__ == "__main__":
    sys.exit(main())