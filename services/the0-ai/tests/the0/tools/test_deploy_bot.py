"""
Unit tests for the deploy_bot tool.
"""

import pytest
import os
import zipfile
import tempfile
from unittest.mock import AsyncMock, MagicMock, patch
from datetime import datetime

from the0.tools.deploy_bot import deploy_bot


class TestDeployBot:
    """Tests for the deploy_bot function."""

    @pytest.mark.asyncio
    async def test_deploy_bot_success(self, mock_tool_context):
        """Test successful bot deployment with multiple artifacts."""
        bot_name = "trading_bot"

        # Mock artifacts
        artifact_keys = ["bot.py", "config.json", "strategy.py"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        # Mock artifact loading
        def mock_load_artifact(filename):
            mock_artifact = MagicMock()
            if filename == "bot.py":
                mock_artifact.text = "print('Trading Bot')"
            elif filename == "config.json":
                mock_artifact.text = '{"api_key": "test"}'
            else:  # strategy.py
                mock_artifact.text = "def trade(): pass"
            return mock_artifact

        mock_tool_context.load_artifact = AsyncMock(side_effect=mock_load_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            # Change to temp directory for test
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                result = await deploy_bot(bot_name, mock_tool_context)

                assert result["status"] == "success"
                assert result["bot_name"] == bot_name
                assert result["artifacts_count"] == 3
                assert result["artifacts"] == artifact_keys
                assert "deployed successfully" in result["message"]

                # Verify zip file was created
                assert "zip_file" in result
                zip_path = result["zip_file"]
                assert os.path.exists(zip_path)
                assert zip_path.startswith("bots/")
                assert bot_name in zip_path
                assert zip_path.endswith(".zip")

                # Verify zip contents
                with zipfile.ZipFile(zip_path, "r") as zipf:
                    zip_contents = zipf.namelist()
                    assert set(zip_contents) == set(artifact_keys)

                    # Check file contents
                    assert zipf.read("bot.py").decode() == "print('Trading Bot')"
                    assert zipf.read("config.json").decode() == '{"api_key": "test"}'
                    assert zipf.read("strategy.py").decode() == "def trade(): pass"

            finally:
                os.chdir(original_cwd)

    @pytest.mark.asyncio
    async def test_deploy_bot_no_artifacts(self, mock_tool_context):
        """Test deployment when no artifacts exist."""
        bot_name = "empty_bot"

        mock_tool_context.list_artifacts = AsyncMock(return_value=[])

        result = await deploy_bot(bot_name, mock_tool_context)

        assert result["status"] == "error"
        assert result["error_type"] == "no_artifacts"
        assert result["bot_name"] == bot_name
        assert "No artifacts found" in result["message"]

    @pytest.mark.asyncio
    async def test_deploy_bot_artifact_load_error(self, mock_tool_context):
        """Test deployment when artifact loading fails."""
        bot_name = "failing_bot"

        artifact_keys = ["bot.py", "config.json"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        # First artifact loads fine, second fails
        def mock_load_artifact(filename):
            if filename == "bot.py":
                mock_artifact = MagicMock()
                mock_artifact.text = "print('Bot')"
                return mock_artifact
            else:
                raise Exception("Failed to load config.json")

        mock_tool_context.load_artifact = AsyncMock(side_effect=mock_load_artifact)

        result = await deploy_bot(bot_name, mock_tool_context)

        assert result["status"] == "error"
        assert result["error_type"] == "artifact_load_error"
        assert result["bot_name"] == bot_name
        assert "Failed to load artifact 'config.json'" in result["message"]

    @pytest.mark.asyncio
    async def test_deploy_bot_zip_creation_error(self, mock_tool_context):
        """Test deployment when zip file creation fails."""
        bot_name = "zip_error_bot"

        artifact_keys = ["bot.py"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        mock_artifact = MagicMock()
        mock_artifact.text = "print('Bot')"
        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        # Mock zipfile to raise an exception
        with patch("zipfile.ZipFile", side_effect=PermissionError("Cannot create zip file")):
            result = await deploy_bot(bot_name, mock_tool_context)

            assert result["status"] == "error"
            assert result["error_type"] == "deployment_error"
            assert result["bot_name"] == bot_name
            assert "Cannot create zip file" in result["message"]

    @pytest.mark.asyncio
    async def test_deploy_bot_artifact_with_inline_data(self, mock_tool_context):
        """Test deployment with artifacts containing inline binary data."""
        bot_name = "binary_bot"

        artifact_keys = ["image.png"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        # Mock artifact with inline_data (binary content)
        mock_artifact = MagicMock()
        mock_artifact.text = None  # No text content
        mock_artifact.inline_data = MagicMock()
        mock_artifact.inline_data.data = b"binary image data"

        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                result = await deploy_bot(bot_name, mock_tool_context)

                assert result["status"] == "success"
                assert result["artifacts_count"] == 1

                # Verify zip contents
                zip_path = result["zip_file"]
                with zipfile.ZipFile(zip_path, "r") as zipf:
                    content = zipf.read("image.png").decode()
                    assert content == "binary image data"

            finally:
                os.chdir(original_cwd)

    @pytest.mark.asyncio
    async def test_deploy_bot_artifact_fallback_string(self, mock_tool_context):
        """Test deployment with artifacts that need string conversion fallback."""
        bot_name = "fallback_bot"

        artifact_keys = ["data.txt"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        # Mock artifact without text or inline_data
        mock_artifact = MagicMock()
        mock_artifact.text = None
        mock_artifact.inline_data = None
        # str() conversion will be used

        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                result = await deploy_bot(bot_name, mock_tool_context)

                assert result["status"] == "success"
                assert result["artifacts_count"] == 1

                # Verify zip was created (content will be string representation)
                zip_path = result["zip_file"]
                assert os.path.exists(zip_path)

            finally:
                os.chdir(original_cwd)

    @pytest.mark.asyncio
    async def test_deploy_bot_creates_bots_directory(self, mock_tool_context):
        """Test that bots directory is created if it doesn't exist."""
        bot_name = "directory_test_bot"

        artifact_keys = ["test.py"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        mock_artifact = MagicMock()
        mock_artifact.text = "print('test')"
        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                # Ensure bots directory doesn't exist initially
                bots_dir = "bots"
                if os.path.exists(bots_dir):
                    os.rmdir(bots_dir)

                result = await deploy_bot(bot_name, mock_tool_context)

                assert result["status"] == "success"
                # Verify bots directory was created
                assert os.path.exists(bots_dir)
                assert os.path.isdir(bots_dir)

            finally:
                os.chdir(original_cwd)

    @pytest.mark.asyncio
    async def test_deploy_bot_filename_format(self, mock_tool_context):
        """Test that zip filename includes timestamp."""
        bot_name = "timestamp_bot"

        artifact_keys = ["test.py"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        mock_artifact = MagicMock()
        mock_artifact.text = "print('test')"
        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                # Mock datetime to get predictable timestamp
                with patch("the0.tools.deploy_bot.datetime") as mock_datetime:
                    mock_datetime.now.return_value.strftime.return_value = "20240101_120000"

                    result = await deploy_bot(bot_name, mock_tool_context)

                    assert result["status"] == "success"
                    zip_file = result["zip_file"]
                    expected_name = "bots/timestamp_bot_20240101_120000.zip"
                    assert zip_file == expected_name

            finally:
                os.chdir(original_cwd)

    @pytest.mark.asyncio
    async def test_deploy_bot_with_special_characters_in_name(self, mock_tool_context):
        """Test deployment with special characters in bot name."""
        bot_name = "my-trading_bot.v2"

        artifact_keys = ["bot.py"]
        mock_tool_context.list_artifacts = AsyncMock(return_value=artifact_keys)

        mock_artifact = MagicMock()
        mock_artifact.text = "print('Trading Bot v2')"
        mock_tool_context.load_artifact = AsyncMock(return_value=mock_artifact)

        with tempfile.TemporaryDirectory() as temp_dir:
            original_cwd = os.getcwd()
            os.chdir(temp_dir)

            try:
                result = await deploy_bot(bot_name, mock_tool_context)

                assert result["status"] == "success"
                assert result["bot_name"] == bot_name
                assert bot_name in result["zip_file"]

                # Verify zip file was created successfully
                zip_path = result["zip_file"]
                assert os.path.exists(zip_path)

            finally:
                os.chdir(original_cwd)
