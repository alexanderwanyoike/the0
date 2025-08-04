"""Version information for the0-ai desktop application."""

# This version will be updated automatically by the build process
__version__ = "0.1.0"
__build_date__ = "2025-07-16"
__commit_hash__ = "277e3dbf"

# Release configuration
RELEASES_BUCKET_BASE = "https://storage.googleapis.com/the0-ai-releases"
STAGING_BUCKET_BASE = "https://storage.googleapis.com/the0-ai-releases-staging"

def get_update_bucket():
    """Get the update bucket URL based on THE0_AI_UPDATE_STREAM environment variable."""
    import os
    
    update_stream = os.environ.get("THE0_AI_UPDATE_STREAM", "production").lower()
    
    if update_stream == "staging":
        return STAGING_BUCKET_BASE
    else:
        # Default to production bucket
        return RELEASES_BUCKET_BASE

def get_version():
    """Get the current version string."""
    return __version__

def get_build_info():
    """Get complete build information."""
    return {
        "version": __version__,
        "build_date": __build_date__,
        "commit_hash": __commit_hash__
    }

def get_platform_binary_name():
    """Get the platform-specific binary name for updates."""
    import sys
    import platform
    
    if sys.platform == "win32":
        return "the0-ai-windows.exe"
    elif sys.platform == "darwin":
        return "the0-ai-darwin"
    else:  # Linux and other Unix-like
        return "the0-ai-linux"

def get_install_path():
    """Get the expected installation path for the binary."""
    import sys
    import os
    from pathlib import Path
    
    if sys.platform == "win32":
        # Windows: %USERPROFILE%\bin\the0-ai.exe
        return Path(os.environ.get('USERPROFILE', Path.home())) / "bin" / "the0-ai.exe"
    else:
        # Linux/macOS: ~/bin/the0-ai
        return Path.home() / "bin" / "the0-ai"