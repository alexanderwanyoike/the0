"""Auto-update functionality for the0-ai desktop application."""

import asyncio
import json
import os
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Dict, Optional

import httpx
from fastapi import HTTPException

from version import (
    __version__,
    get_update_bucket,
    get_platform_binary_name,
    get_install_path
)


class UpdateManager:
    """Manages application updates."""
    
    def __init__(self):
        self.current_version = __version__
        self.bucket_base = get_update_bucket()
        self.binary_name = get_platform_binary_name()
        self.install_path = get_install_path()
    
    async def check_for_updates(self) -> Dict:
        """Check if a newer version is available."""
        try:
            async with httpx.AsyncClient(timeout=10.0) as client:
                # Check the latest.json file for the newest version
                response = await client.get(f"{self.bucket_base}/latest.json")
                
                if response.status_code != 200:
                    return {
                        "update_available": False,
                        "error": "Could not check for updates"
                    }
                
                latest_info = response.json()
                latest_version = latest_info["version"]
                
                # Simple version comparison (can be improved with semantic versioning)
                update_available = self._is_newer_version(latest_version, self.current_version)
                
                result = {
                    "update_available": update_available,
                    "current_version": self.current_version,
                    "latest_version": latest_version,
                    "release_date": latest_info.get("date"),
                }
                
                if update_available:
                    result["download_url"] = f"{self.bucket_base}/latest/{self.binary_name}"
                
                return result
                
        except Exception as e:
            return {
                "update_available": False,
                "error": f"Update check failed: {str(e)}"
            }
    
    def _is_newer_version(self, latest: str, current: str) -> bool:
        """Compare version strings to determine if latest is newer."""
        # Parse version strings to extract comparable parts
        latest_parsed = self._parse_version(latest)
        current_parsed = self._parse_version(current)
        
        # Local builds should always accept staging builds
        if current_parsed["stream"] == "staging" and latest_parsed["stream"] == "staging":
            # Compare staging builds by date and build number
            if latest_parsed["date"] != current_parsed["date"]:
                return latest_parsed["date"] > current_parsed["date"]
            return latest_parsed["build"] > current_parsed["build"]
        
        # Production builds compare with production builds
        if current_parsed["stream"] == "production" and latest_parsed["stream"] == "production":
            # Compare production builds by date and build number
            if latest_parsed["date"] != current_parsed["date"]:
                return latest_parsed["date"] > current_parsed["date"]
            return latest_parsed["build"] > current_parsed["build"]
        
        # Any built version is newer than local development version
        if current_parsed["stream"] == "local":
            return latest_parsed["stream"] in ["staging", "production"]
        
        # Don't cross streams (production user won't get staging updates)
        return False
    
    def _parse_version(self, version: str) -> dict:
        """Parse version string into components."""
        # Examples: 
        # v2025.01.12-123 -> {stream: "production", date: "2025.01.12", build: 123}
        # develop-2025.01.12-456 -> {stream: "staging", date: "2025.01.12", build: 456}
        # 0.1.0 -> {stream: "local", date: "0.1.0", build: 0}
        
        if version.startswith("v") and "." in version and "-" in version:
            # Production version: v2025.01.12-123
            parts = version[1:].split("-")  # Remove 'v' and split
            return {
                "stream": "production",
                "date": parts[0],
                "build": int(parts[1]) if len(parts) > 1 and parts[1].isdigit() else 0
            }
        elif version.startswith("develop-") and "." in version and "-" in version:
            # Staging version: develop-2025.01.12-123
            parts = version[8:].split("-")  # Remove 'develop-' and split
            return {
                "stream": "staging", 
                "date": parts[0],
                "build": int(parts[1]) if len(parts) > 1 and parts[1].isdigit() else 0
            }
        else:
            # Local/dev version: 0.1.0 -> treat as local, will upgrade to staging
            return {
                "stream": "local",
                "date": version,
                "build": 0
            }
    
    async def download_and_install_update(self, download_url: str) -> Dict:
        """Download and install the update."""
        try:
            # Ensure the install directory exists
            self.install_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Download the new binary to a temporary file
            temp_path = None
            async with httpx.AsyncClient(timeout=300.0) as client:
                response = await client.get(download_url)
                
                if response.status_code != 200:
                    return {
                        "success": False,
                        "error": f"Download failed with status {response.status_code}"
                    }
                
                # Create temporary file
                with tempfile.NamedTemporaryFile(delete=False, suffix=".tmp") as temp_file:
                    temp_path = Path(temp_file.name)
                    temp_file.write(response.content)
            
            if not temp_path or not temp_path.exists():
                return {
                    "success": False,
                    "error": "Failed to download update file"
                }
            
            # Verify download size
            if temp_path.stat().st_size < 1024:  # Less than 1KB seems wrong
                temp_path.unlink(missing_ok=True)
                return {
                    "success": False,
                    "error": "Downloaded file appears to be invalid"
                }
            
            # Backup current executable if it exists
            backup_path = None
            if self.install_path.exists():
                backup_path = self.install_path.with_suffix(self.install_path.suffix + ".backup")
                shutil.copy2(self.install_path, backup_path)
            
            try:
                # Replace the current executable
                shutil.move(str(temp_path), str(self.install_path))
                
                # Make executable on Unix systems
                if sys.platform != "win32":
                    self.install_path.chmod(0o755)
                
                # Clean up backup
                if backup_path and backup_path.exists():
                    backup_path.unlink()
                
                return {
                    "success": True,
                    "message": "Update installed successfully",
                    "install_path": str(self.install_path)
                }
                
            except Exception as e:
                # Restore backup if move failed
                if backup_path and backup_path.exists():
                    shutil.move(str(backup_path), str(self.install_path))
                
                raise e
                
        except Exception as e:
            # Clean up temp file
            if temp_path and temp_path.exists():
                temp_path.unlink(missing_ok=True)
            
            return {
                "success": False,
                "error": f"Update installation failed: {str(e)}"
            }
    
    def get_current_executable_path(self) -> Optional[str]:
        """Get the path of the currently running executable."""
        if getattr(sys, 'frozen', False):
            # Running as PyInstaller executable
            return sys.executable
        else:
            # Running as script - return None as we can't update a script
            return None


# Global update manager instance
update_manager = UpdateManager()