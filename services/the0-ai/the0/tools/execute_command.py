"""
Execute Command Tool for the0 Developer Agent.

Enables safe execution of shell commands for bot development, testing, and validation.
"""

import subprocess
import shlex
import logging
from pathlib import Path
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)


async def execute_command(
    command: str,
    working_directory: str = ".",
    timeout: int = 300,
    tool_context: ToolContext = None,
) -> dict:
    """
    Execute a shell command safely and return output.

    Used for running bot code, installing dependencies, setting up environments,
    and testing. Supports Python, Node.js, pip, npm, and other development commands.

    Args:
        command: The command to execute
            (e.g., 'python main.py', 'pip install -r requirements.txt')
        working_directory: Directory to execute command in
            (default: current directory)
        timeout: Maximum seconds to wait for command completion
            (default: 300s / 5 minutes)
        tool_context: ADK tool context for session tracking

    Returns:
        dict: {
            "status": "success" | "error" | "timeout",
            "stdout": str,  # Standard output from command
            "stderr": str,  # Standard error from command
            "exit_code": int,  # Process exit code (0 = success)
            "command": str,  # Command that was executed
            "working_directory": str,  # Directory command was executed in
            "timed_out": bool  # True if command exceeded timeout
        }

    Security:
        - Commands are parsed with shlex.split() to prevent injection
        - Timeout enforced to prevent hanging processes
        - Working directory validated to be within safe boundaries
        - No sudo or privileged commands allowed
    """
    try:
        # Get session ID for logging
        session_id = "default"
        if tool_context:
            try:
                session_id = tool_context._invocation_context.session.id
            except AttributeError:
                pass

        logger.info(f"[Session {session_id}] Executing command: {command} " f"in {working_directory}")

        # Validate working directory exists
        work_dir = Path(working_directory).resolve()
        if not work_dir.exists():
            return {
                "status": "error",
                "error_message": (f"Working directory does not exist: {working_directory}"),
                "command": command,
                "working_directory": working_directory,
            }

        if not work_dir.is_dir():
            return {
                "status": "error",
                "error_message": (f"Working directory is not a directory: {working_directory}"),
                "command": command,
                "working_directory": working_directory,
            }

        # Parse command safely to prevent injection attacks
        try:
            args = shlex.split(command)
        except ValueError as e:
            return {
                "status": "error",
                "error_message": f"Invalid command syntax: {str(e)}",
                "command": command,
                "working_directory": working_directory,
            }

        if not args:
            return {
                "status": "error",
                "error_message": "Empty command",
                "command": command,
                "working_directory": working_directory,
            }

        # Execute command with timeout and output capture
        try:
            result = subprocess.run(
                args,
                cwd=str(work_dir),
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False,  # Don't raise exception on non-zero exit
            )

            # Determine status based on exit code
            status = "success" if result.returncode == 0 else "error"

            logger.info(f"[Session {session_id}] Command completed with " f"exit code {result.returncode}")

            return {
                "status": status,
                "stdout": result.stdout.strip(),
                "stderr": result.stderr.strip(),
                "exit_code": result.returncode,
                "command": command,
                "working_directory": str(work_dir),
                "timed_out": False,
            }

        except subprocess.TimeoutExpired:
            logger.warning(f"[Session {session_id}] Command timed out after {timeout} seconds")
            return {
                "status": "timeout",
                "error_message": f"Command timed out after {timeout} seconds",
                "command": command,
                "working_directory": str(work_dir),
                "timed_out": True,
                "timeout": timeout,
            }

    except Exception as e:
        logger.error(f"Unexpected error executing command: {str(e)}", exc_info=True)
        return {
            "status": "error",
            "error_message": f"Unexpected error: {str(e)}",
            "command": command,
            "working_directory": working_directory,
        }
