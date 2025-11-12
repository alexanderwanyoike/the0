"""
Unit tests for read_file tool.
"""

import pytest
from unittest.mock import Mock
from pathlib import Path

from the0.tools.read_file import read_file


class TestReadFile:
    """Test suite for read_file tool."""

    @pytest.mark.asyncio
    async def test_read_file_success(self, tmp_path):
        """Test successful file reading."""
        # Create a test file
        test_file = tmp_path / "test.txt"
        test_content = "Hello, World!\nLine 2\nLine 3"
        test_file.write_text(test_content)

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert result["content"] == test_content
        assert result["lines"] == 3
        assert result["encoding"] == "utf-8"
        assert result["size_bytes"] > 0

    @pytest.mark.asyncio
    async def test_read_file_not_found(self):
        """Test reading non-existent file."""
        result = await read_file("/nonexistent/file.txt")

        assert result["status"] == "error"
        assert "not found" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_read_file_is_directory(self, tmp_path):
        """Test attempting to read a directory."""
        result = await read_file(str(tmp_path))

        assert result["status"] == "error"
        assert "not a file" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_read_file_empty(self, tmp_path):
        """Test reading empty file."""
        test_file = tmp_path / "empty.txt"
        test_file.write_text("")

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert result["content"] == ""
        assert result["lines"] == 0
        assert result["size_bytes"] == 0

    @pytest.mark.asyncio
    async def test_read_file_multiline(self, tmp_path):
        """Test reading file with multiple lines."""
        test_file = tmp_path / "multiline.txt"
        lines = ["Line 1", "Line 2", "Line 3", "Line 4", "Line 5"]
        test_file.write_text("\n".join(lines))

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert result["lines"] == 5
        for line in lines:
            assert line in result["content"]

    @pytest.mark.asyncio
    async def test_read_file_with_special_characters(self, tmp_path):
        """Test reading file with special characters."""
        test_file = tmp_path / "special.txt"
        content = "Special: \n\t\r Hello 世界 🚀"
        test_file.write_text(content, encoding="utf-8")

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert "世界" in result["content"]
        assert "🚀" in result["content"]

    @pytest.mark.asyncio
    async def test_read_file_binary(self, tmp_path):
        """Test attempting to read binary file."""
        test_file = tmp_path / "binary.bin"
        test_file.write_bytes(b"\x00\x01\x02\x03\xff")

        result = await read_file(str(test_file))

        assert result["status"] == "error"
        assert "not a text file" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_read_file_large(self, tmp_path):
        """Test reading file under size limit."""
        test_file = tmp_path / "large.txt"
        # Create 1MB file (under 10MB limit)
        content = "x" * (1024 * 1024)
        test_file.write_text(content)

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert len(result["content"]) == 1024 * 1024

    @pytest.mark.asyncio
    async def test_read_file_too_large(self, tmp_path):
        """Test reading file over size limit."""
        test_file = tmp_path / "toolarge.txt"
        # Create 11MB file (over 10MB limit)
        content = "x" * (11 * 1024 * 1024)
        test_file.write_text(content)

        result = await read_file(str(test_file))

        assert result["status"] == "error"
        assert "too large" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_read_file_with_tool_context(self, tmp_path):
        """Test file reading with tool context."""
        test_file = tmp_path / "context_test.txt"
        test_file.write_text("test content")

        # Create mock tool context
        mock_context = Mock()
        mock_session = Mock()
        mock_session.id = "test-session-456"
        mock_context._invocation_context = Mock()
        mock_context._invocation_context.session = mock_session

        result = await read_file(str(test_file), tool_context=mock_context)

        assert result["status"] == "success"
        assert result["content"] == "test content"

    @pytest.mark.asyncio
    async def test_read_file_resolves_relative_path(self, tmp_path):
        """Test that relative paths are resolved."""
        test_file = tmp_path / "relative.txt"
        test_file.write_text("relative path test")

        result = await read_file(str(test_file))

        assert result["status"] == "success"
        assert Path(result["file_path"]).is_absolute()
