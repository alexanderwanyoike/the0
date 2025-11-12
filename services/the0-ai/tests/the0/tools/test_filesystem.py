"""
Unit tests for filesystem tools (list_directory).
"""

import pytest
from unittest.mock import Mock
from pathlib import Path

from the0.tools.filesystem import list_directory


class TestListDirectory:
    """Test suite for list_directory tool."""

    @pytest.mark.asyncio
    async def test_list_directory_success(self, tmp_path):
        """Test successful directory listing."""
        # Create test files and directories
        (tmp_path / "file1.txt").write_text("test")
        (tmp_path / "file2.py").write_text("code")
        (tmp_path / "subdir").mkdir()

        result = await list_directory(str(tmp_path))

        assert result["status"] == "success"
        assert result["total_count"] == 3
        assert result["recursive"] is False
        assert result["truncated"] is False

        # Check files are present
        file_names = [item["name"] for item in result["files"]]
        assert "file1.txt" in file_names
        assert "file2.py" in file_names
        assert "subdir" in file_names

    @pytest.mark.asyncio
    async def test_list_directory_empty(self, tmp_path):
        """Test listing empty directory."""
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()

        result = await list_directory(str(empty_dir))

        assert result["status"] == "success"
        assert result["total_count"] == 0
        assert len(result["files"]) == 0

    @pytest.mark.asyncio
    async def test_list_directory_not_found(self):
        """Test listing non-existent directory."""
        result = await list_directory("/nonexistent/directory")

        assert result["status"] == "error"
        assert "not found" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_list_directory_not_a_directory(self, tmp_path):
        """Test listing when path is a file, not directory."""
        test_file = tmp_path / "file.txt"
        test_file.write_text("test")

        result = await list_directory(str(test_file))

        assert result["status"] == "error"
        assert "not a directory" in result["error_message"].lower()

    @pytest.mark.asyncio
    async def test_list_directory_file_types(self, tmp_path):
        """Test that files and directories are properly typed."""
        (tmp_path / "regular_file.txt").write_text("test")
        (tmp_path / "directory").mkdir()

        result = await list_directory(str(tmp_path))

        assert result["status"] == "success"

        # Check types
        for item in result["files"]:
            if item["name"] == "regular_file.txt":
                assert item["type"] == "file"
                assert item["size_bytes"] > 0
            elif item["name"] == "directory":
                assert item["type"] == "directory"
                assert item["size_bytes"] == 0

    @pytest.mark.asyncio
    async def test_list_directory_recursive(self, tmp_path):
        """Test recursive directory listing."""
        # Create nested structure
        (tmp_path / "file1.txt").write_text("test")
        subdir = tmp_path / "subdir"
        subdir.mkdir()
        (subdir / "file2.txt").write_text("test")
        deep_dir = subdir / "deep"
        deep_dir.mkdir()
        (deep_dir / "file3.txt").write_text("test")

        result = await list_directory(str(tmp_path), recursive=True)

        assert result["status"] == "success"
        assert result["recursive"] is True
        # Should have: file1.txt, subdir, subdir/file2.txt, subdir/deep,
        # and subdir/deep/file3.txt
        assert result["total_count"] >= 5

        file_names = [item["name"] for item in result["files"]]
        assert "file1.txt" in file_names
        assert "file2.txt" in file_names
        assert "file3.txt" in file_names

    @pytest.mark.asyncio
    async def test_list_directory_sorted(self, tmp_path):
        """Test that results are sorted (directories first, then files)."""
        # Create files and directories
        (tmp_path / "zebra.txt").write_text("test")
        (tmp_path / "apple.txt").write_text("test")
        (tmp_path / "zoo_dir").mkdir()
        (tmp_path / "aaa_dir").mkdir()

        result = await list_directory(str(tmp_path))

        assert result["status"] == "success"

        # Directories should come before files
        types = [item["type"] for item in result["files"]]
        # Find first file index
        first_file_idx = next((i for i, t in enumerate(types) if t == "file"), None)
        if first_file_idx is not None:
            # All items before first file should be directories
            assert all(t == "directory" for t in types[:first_file_idx])

    @pytest.mark.asyncio
    async def test_list_directory_with_metadata(self, tmp_path):
        """Test that file metadata is included."""
        test_file = tmp_path / "metadata_test.txt"
        test_file.write_text("test content")

        result = await list_directory(str(tmp_path))

        assert result["status"] == "success"
        item = result["files"][0]

        # Check required fields
        assert "name" in item
        assert "path" in item
        assert "type" in item
        assert "size_bytes" in item
        assert "modified" in item

        # Check modified is ISO format timestamp
        assert "T" in item["modified"] or "-" in item["modified"]

    @pytest.mark.asyncio
    async def test_list_directory_with_tool_context(self, tmp_path):
        """Test directory listing with tool context."""
        (tmp_path / "test.txt").write_text("test")

        # Create mock tool context
        mock_context = Mock()
        mock_session = Mock()
        mock_session.id = "test-session-789"
        mock_context._invocation_context = Mock()
        mock_context._invocation_context.session = mock_session

        result = await list_directory(str(tmp_path), tool_context=mock_context)

        assert result["status"] == "success"
        assert result["total_count"] == 1

    @pytest.mark.asyncio
    async def test_list_directory_default_current(self):
        """Test listing current directory by default."""
        result = await list_directory()

        assert result["status"] == "success"
        assert "directory_path" in result
        # Should have some files (at least this test file)
        assert result["total_count"] > 0

    @pytest.mark.asyncio
    async def test_list_directory_resolves_path(self, tmp_path):
        """Test that paths are resolved to absolute."""
        (tmp_path / "test.txt").write_text("test")

        result = await list_directory(str(tmp_path))

        assert result["status"] == "success"
        assert Path(result["directory_path"]).is_absolute()
