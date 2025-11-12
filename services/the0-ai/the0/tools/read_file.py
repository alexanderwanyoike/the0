"""
Read File Tool for the0 Developer Agent.

Enables reading file contents from disk for debugging, verification, and analysis.
"""

import logging
from pathlib import Path
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)

# Maximum file size to read (10MB)
MAX_FILE_SIZE_BYTES = 10 * 1024 * 1024


async def read_file(
    file_path: str,
    tool_context: ToolContext = None,
) -> dict:
    """
    Read file contents from disk.

    Used for reading generated bot code, error logs, configuration files,
    and any other text files needed during bot development and debugging.

    Args:
        file_path: Path to the file to read (relative or absolute)
        tool_context: ADK tool context for session tracking

    Returns:
        dict: {
            "status": "success" | "error",
            "content": str,  # File contents (empty string if binary or error)
            "file_path": str,  # Resolved absolute path
            "size_bytes": int,  # File size in bytes
            "lines": int,  # Number of lines in file
            "encoding": str,  # File encoding used (e.g., "utf-8")
        }

    Security:
        - File size limited to 10MB to prevent memory exhaustion
        - Only text files supported (UTF-8 encoding)
        - Path validated to ensure file exists and is readable
    """
    try:
        # Get session ID for logging
        session_id = "default"
        if tool_context:
            try:
                session_id = tool_context._invocation_context.session.id
            except AttributeError:
                pass

        logger.info(f"[Session {session_id}] Reading file: {file_path}")

        # Resolve and validate file path
        path = Path(file_path).resolve()

        if not path.exists():
            return {
                "status": "error",
                "error_message": f"File not found: {file_path}",
                "file_path": str(path),
            }

        if not path.is_file():
            return {
                "status": "error",
                "error_message": (f"Path is not a file (might be a directory): {file_path}"),
                "file_path": str(path),
            }

        # Check file size
        file_size = path.stat().st_size
        if file_size > MAX_FILE_SIZE_BYTES:
            return {
                "status": "error",
                "error_message": (f"File too large ({file_size} bytes, " f"max {MAX_FILE_SIZE_BYTES} bytes)"),
                "file_path": str(path),
                "size_bytes": file_size,
            }

        # Read file with UTF-8 encoding
        try:
            with open(path, "r", encoding="utf-8") as f:
                content = f.read()

            # Count lines
            lines = content.count("\n") + 1 if content else 0

            logger.info(
                f"[Session {session_id}] Successfully read file: {file_path} " f"({file_size} bytes, {lines} lines)"
            )

            return {
                "status": "success",
                "content": content,
                "file_path": str(path),
                "size_bytes": file_size,
                "lines": lines,
                "encoding": "utf-8",
            }

        except UnicodeDecodeError:
            return {
                "status": "error",
                "error_message": (f"File is not a text file " f"(binary file or unsupported encoding): {file_path}"),
                "file_path": str(path),
                "size_bytes": file_size,
            }

        except PermissionError:
            return {
                "status": "error",
                "error_message": f"Permission denied reading file: {file_path}",
                "file_path": str(path),
            }

    except Exception as e:
        logger.error(f"Unexpected error reading file: {str(e)}", exc_info=True)
        return {
            "status": "error",
            "error_message": f"Unexpected error: {str(e)}",
            "file_path": file_path,
        }
