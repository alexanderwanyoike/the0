#!/usr/bin/env python3
"""
Installation script for the0-ai desktop application.
Installs the executable to ~/bin/the0-ai (or Windows equivalent).
"""

import os
import sys
import shutil
from pathlib import Path


def get_install_path():
    """Get the installation path for the current platform."""
    if sys.platform == "win32":
        # Windows: %USERPROFILE%\bin\the0-ai.exe
        return Path(os.environ.get('USERPROFILE', Path.home())) / "bin" / "the0-ai.exe"
    else:
        # Linux/macOS: ~/bin/the0-ai
        return Path.home() / "bin" / "the0-ai"


def get_executable_name():
    """Get the platform-specific executable name."""
    if sys.platform == "win32":
        return "the0-ai.exe"
    else:
        return "the0-ai"


def main():
    """Install the0-ai to the user's bin directory."""
    print("🚀 Installing the0-ai...")
    
    # Find the built executable
    dist_dir = Path("dist")
    executable_name = get_executable_name()
    source_path = dist_dir / executable_name
    
    if not source_path.exists():
        print(f"❌ Executable not found at {source_path}")
        print("Please run 'make build' or 'python build.py' first")
        sys.exit(1)
    
    # Get installation path
    install_path = get_install_path()
    
    # Create bin directory if it doesn't exist
    install_path.parent.mkdir(parents=True, exist_ok=True)
    
    try:
        # Copy executable to install location
        print(f"📦 Installing to {install_path}")
        shutil.copy2(source_path, install_path)
        
        # Make executable on Unix systems
        if sys.platform != "win32":
            install_path.chmod(0o755)
        
        print("✅ Installation completed!")
        print(f"🎯 Executable installed at: {install_path}")
        
        # Add to PATH instructions
        if sys.platform == "win32":
            print("\n📝 To use 'the0-ai' from anywhere:")
            print(f"   Add {install_path.parent} to your PATH environment variable")
        else:
            # Check if ~/bin is in PATH
            home_bin = str(Path.home() / "bin")
            path_env = os.environ.get("PATH", "")
            
            if home_bin not in path_env:
                print("\n📝 To use 'the0-ai' from anywhere, add ~/bin to your PATH:")
                print(f"   echo 'export PATH=\"$HOME/bin:$PATH\"' >> ~/.bashrc")
                print(f"   source ~/.bashrc")
            else:
                print(f"\n🎉 You can now run 'the0-ai' from anywhere!")
                
        print(f"\n🚀 To start the application:")
        print(f"   {install_path}")
        print(f"   # or if ~/bin is in PATH:")
        print(f"   the0-ai")
        
    except Exception as e:
        print(f"❌ Installation failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()