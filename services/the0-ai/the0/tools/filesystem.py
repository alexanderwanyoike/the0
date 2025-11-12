"""
Filesystem Tools for the0 Developer Agent.

Enables listing directory contents for verification and file structure analysis.
"""

import logging
from pathlib import Path
from datetime import datetime
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)

# Maximum items to return to prevent memory issues
MAX_ITEMS = 1000


async def list_directory(
    directory_path: str = ".",
    recursive: bool = False,
    tool_context: ToolContext = None,
) -> dict:
    """
    List files and directories at the specified path.

    Used for verifying bot artifacts are created, checking project structure,
    and exploring file organization during development.

    Args:
        directory_path: Path to directory to list (default: current directory)
        recursive: If True, list subdirectories recursively (default: False)
        tool_context: ADK tool context for session tracking

    Returns:
        dict: {
            "status": "success" | "error",
            "files": [
                {
                    "name": str,  # File/directory name
                    "path": str,  # Full path
                    "type": "file" | "directory",
                    "size_bytes": int,  # Size in bytes (0 for directories)
                    "modified": str,  # ISO 8601 timestamp
                }
            ],
            "total_count": int,  # Total files and directories
            "directory_path": str,  # Directory that was listed
            "recursive": bool,  # Whether listing was recursive
            "truncated": bool,  # True if results limited to MAX_ITEMS
        }

    Security:
        - Results limited to MAX_ITEMS (1000) to prevent memory exhaustion
        - Recursive depth effectively limited by MAX_ITEMS
        - Path validated to ensure directory exists
    """
    try:
        # Get session ID for logging
        session_id = "default"
        if tool_context:
            try:
                session_id = tool_context._invocation_context.session.id
            except AttributeError:
                pass

        logger.info(f"[Session {session_id}] Listing directory: {directory_path} " f"(recursive={recursive})")

        # Resolve and validate directory path
        path = Path(directory_path).resolve()

        if not path.exists():
            return {
                "status": "error",
                "error_message": f"Directory not found: {directory_path}",
                "directory_path": str(path),
            }

        if not path.is_dir():
            return {
                "status": "error",
                "error_message": f"Path is not a directory: {directory_path}",
                "directory_path": str(path),
            }

        # Collect files and directories
        items = []
        truncated = False

        try:
            if recursive:
                # Recursive listing using rglob
                for item in path.rglob("*"):
                    if len(items) >= MAX_ITEMS:
                        truncated = True
                        break

                    try:
                        stat = item.stat()
                        items.append(
                            {
                                "name": item.name,
                                "path": str(item),
                                "type": "directory" if item.is_dir() else "file",
                                "size_bytes": stat.st_size if item.is_file() else 0,
                                "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                            }
                        )
                    except (PermissionError, FileNotFoundError):
                        # Skip files we can't access
                        continue

            else:
                # Non-recursive listing
                for item in path.iterdir():
                    if len(items) >= MAX_ITEMS:
                        truncated = True
                        break

                    try:
                        stat = item.stat()
                        items.append(
                            {
                                "name": item.name,
                                "path": str(item),
                                "type": "directory" if item.is_dir() else "file",
                                "size_bytes": stat.st_size if item.is_file() else 0,
                                "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                            }
                        )
                    except (PermissionError, FileNotFoundError):
                        # Skip files we can't access
                        continue

            # Sort items: directories first, then files, both alphabetically
            items.sort(key=lambda x: (x["type"] == "file", x["name"].lower()))

            logger.info(f"[Session {session_id}] Listed {len(items)} items " f"(truncated={truncated})")

            return {
                "status": "success",
                "files": items,
                "total_count": len(items),
                "directory_path": str(path),
                "recursive": recursive,
                "truncated": truncated,
            }

        except PermissionError:
            return {
                "status": "error",
                "error_message": (f"Permission denied listing directory: {directory_path}"),
                "directory_path": str(path),
            }

    except Exception as e:
        logger.error(f"Unexpected error listing directory: {str(e)}", exc_info=True)
        return {
            "status": "error",
            "error_message": f"Unexpected error: {str(e)}",
            "directory_path": directory_path,
        }
