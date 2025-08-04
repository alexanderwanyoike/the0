#!/usr/bin/env python3
"""
Build script for creating standalone executable of the0-ai desktop application
"""

import PyInstaller.__main__
import subprocess
import os
import sys
from pathlib import Path
from datetime import datetime

# Set UTF-8 encoding for Windows console
if sys.platform == 'win32':
    os.environ['PYTHONIOENCODING'] = 'utf-8'

def safe_print(message):
    """Print message with fallback for Unicode encoding issues on Windows."""
    try:
        print(message)
    except UnicodeEncodeError:
        # Remove emojis and Unicode characters for Windows compatibility
        import re
        safe_message = re.sub(r'[^\x00-\x7F]+', '', message)
        print(safe_message)

def build_frontend():
    """Build the React frontend for production."""
    safe_print("🔨 Building frontend...")
    
    frontend_dir = Path("frontend")
    if not frontend_dir.exists():
        safe_print("❌ Frontend directory not found!")
        return False
    
    try:
        # Build for production
        safe_print("🏗️ Building frontend for production...")
        
        # Use yarn.cmd on Windows, yarn on Unix
        yarn_cmd = "yarn.cmd" if sys.platform == "win32" else "yarn"
        subprocess.check_call([yarn_cmd, "build"], cwd=frontend_dir)
        
        dist_dir = frontend_dir / "dist"
        if dist_dir.exists():
            safe_print("✅ Frontend build completed!")
            return True
        else:
            safe_print("❌ Frontend build failed - no dist directory created")
            return False
            
    except subprocess.CalledProcessError as e:
        safe_print(f"❌ Frontend build failed: {e}")
        return False

def update_version_info():
    """Update version.py with build information."""
    safe_print("📝 Updating version information...")
    
    # Get current git commit hash if available
    commit_hash = "dev"
    try:
        result = subprocess.run(['git', 'rev-parse', '--short', 'HEAD'], 
                              capture_output=True, text=True, check=True)
        commit_hash = result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    
    # Get current date
    build_date = datetime.now().strftime("%Y-%m-%d")
    
    # Read current version.py
    version_file = Path("version.py")
    content = version_file.read_text()
    
    # Update build info
    lines = content.split('\n')
    for i, line in enumerate(lines):
        if line.startswith('__build_date__'):
            lines[i] = f'__build_date__ = "{build_date}"'
        elif line.startswith('__commit_hash__'):
            lines[i] = f'__commit_hash__ = "{commit_hash}"'
    
    # Write back
    version_file.write_text('\n'.join(lines))
    safe_print(f"📝 Updated version info: commit={commit_hash}, date={build_date}")

def main():
    """Main build process - follows alyea's minimal approach."""
    safe_print("🚀 Building the0-ai desktop application")
    
    # Update version information (only for local builds, CI handles this separately)
    if not os.environ.get('CI'):
        update_version_info()
    
    # Build frontend first
    if not build_frontend():
        safe_print("❌ Failed to build frontend")
        return
    
    # Clean PyInstaller build using minimal approach like alyea
    safe_print("🔨 Building executable...")
    PyInstaller.__main__.run([
        'the0_ai.py',
        '--name=the0-ai',
        '--onefile',
        '--windowed',
        '--add-data=schemas:schemas',
        '--add-data=frontend/dist:frontend/dist',
        '--add-data=api:api',
        '--add-data=the0:the0',
        '--add-data=version.py:.',
    ])
    
    safe_print("✅ Build completed!")

if __name__ == "__main__":
    main()