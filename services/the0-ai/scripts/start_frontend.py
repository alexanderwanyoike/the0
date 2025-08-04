#!/usr/bin/env python3

import subprocess
import os
import sys
import time


def main():
    """Start the React frontend development server."""
    frontend_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "frontend")
    
    print("🚀 Starting the0 AI Agent Workbench Frontend...")
    print(f"📁 Frontend directory: {frontend_dir}")
    print("🌐 Frontend will be available at: http://localhost:3000")
    print("🔄 Press Ctrl+C to stop the frontend server")
    print("-" * 60)
    
    if not os.path.exists(frontend_dir):
        print(f"❌ Frontend directory not found: {frontend_dir}")
        return 1
    
    package_json = os.path.join(frontend_dir, "package.json")
    if not os.path.exists(package_json):
        print(f"❌ package.json not found: {package_json}")
        return 1
    
    # Check if node_modules exists
    node_modules = os.path.join(frontend_dir, "node_modules")
    if not os.path.exists(node_modules):
        print("📦 Installing dependencies...")
        try:
            subprocess.run(["npm", "install"], cwd=frontend_dir, check=True)
            print("✅ Dependencies installed successfully")
        except subprocess.CalledProcessError as e:
            print(f"❌ Failed to install dependencies: {e}")
            return 1
        except FileNotFoundError:
            print("❌ npm not found. Please install Node.js and npm")
            return 1
    
    try:
        # Start the development server
        print("🔧 Starting development server...")
        subprocess.run(["npm", "run", "dev"], cwd=frontend_dir, check=True)
    except subprocess.CalledProcessError as e:
        print(f"❌ Failed to start frontend server: {e}")
        return 1
    except FileNotFoundError:
        print("❌ npm not found. Please install Node.js and npm")
        return 1
    except KeyboardInterrupt:
        print("\n👋 Frontend server stopped")
        return 0
    
    return 0


if __name__ == "__main__":
    sys.exit(main())