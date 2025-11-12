"""
Unit tests for execute_command tool.
"""

import pytest
from unittest.mock import Mock, patch
import subprocess

from the0.tools.execute_command import execute_command


class TestExecuteCommand:
    """Test suite for execute_command tool."""

    @pytest.mark.asyncio
    async def test_execute_command_success(self):
        """Test successful command execution."""
        result = await execute_command("echo 'hello world'")

        assert result["status"] == "success"
        assert result["exit_code"] == 0
        assert "hello world" in result["stdout"]
        assert result["timed_out"] is False

    @pytest.mark.asyncio
    async def test_execute_command_with_working_directory(self, tmp_path):
        """Test command execution in specific working directory."""
        # Create a test file in tmp directory
        test_file = tmp_path / "test.txt"
        test_file.write_text("test content")

        result = await execute_command("ls test.txt", working_directory=str(tmp_path))

        assert result["status"] == "success"
        assert "test.txt" in result["stdout"]

    @pytest.mark.asyncio
    async def test_execute_command_error_exit_code(self):
        """Test command with non-zero exit code."""
        result = await execute_command("false")

        assert result["status"] == "error"
        assert result["exit_code"] != 0
        assert result["timed_out"] is False

    @pytest.mark.asyncio
    async def test_execute_command_invalid_working_directory(self):
        """Test command with non-existent working directory."""
        result = await execute_command("echo test", working_directory="/nonexistent/directory")

        assert result["status"] == "error"
        assert "does not exist" in result["error_message"]

    @pytest.mark.asyncio
    async def test_execute_command_invalid_syntax(self):
        """Test command with invalid shell syntax."""
        # Test with unclosed quote
        result = await execute_command("echo 'unclosed")

        assert result["status"] == "error"
        assert "Invalid command syntax" in result["error_message"]

    @pytest.mark.asyncio
    async def test_execute_command_empty_command(self):
        """Test empty command."""
        result = await execute_command("")

        assert result["status"] == "error"
        assert "Empty command" in result["error_message"]

    @pytest.mark.asyncio
    async def test_execute_command_python(self):
        """Test Python command execution."""
        result = await execute_command('python -c "print(2 + 2)"')

        assert result["status"] == "success"
        assert "4" in result["stdout"]

    @pytest.mark.asyncio
    async def test_execute_command_with_stderr(self):
        """Test command that outputs to stderr."""
        result = await execute_command("python -c \"import sys; sys.stderr.write('error\\n')\"")

        assert result["status"] == "success"
        assert result["exit_code"] == 0
        assert "error" in result["stderr"]

    @pytest.mark.asyncio
    @patch("subprocess.run")
    async def test_execute_command_timeout(self, mock_run):
        """Test command timeout."""
        mock_run.side_effect = subprocess.TimeoutExpired("test", 1)

        result = await execute_command("sleep 10", timeout=1)

        assert result["status"] == "timeout"
        assert result["timed_out"] is True
        assert "timed out" in result["error_message"]

    @pytest.mark.asyncio
    async def test_execute_command_returns_command_info(self):
        """Test that result includes command and working directory."""
        result = await execute_command("echo test", working_directory=".")

        assert "command" in result
        assert result["command"] == "echo test"
        assert "working_directory" in result

    @pytest.mark.asyncio
    async def test_execute_command_with_tool_context(self):
        """Test command execution with tool context."""
        # Create mock tool context
        mock_context = Mock()
        mock_session = Mock()
        mock_session.id = "test-session-123"
        mock_context._invocation_context = Mock()
        mock_context._invocation_context.session = mock_session

        result = await execute_command("echo 'with context'", tool_context=mock_context)

        assert result["status"] == "success"
        assert "with context" in result["stdout"]
