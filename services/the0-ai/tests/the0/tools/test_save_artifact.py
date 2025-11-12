"""
Unit tests for the save_artifact tool.
"""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch

from the0.tools.save_artifact import save_artifact, get_mime_type_from_filename


class TestGetMimeTypeFromFilename:
    """Tests for MIME type detection function."""

    def test_python_file(self):
        """Test MIME type for Python files."""
        assert get_mime_type_from_filename("bot.py") == "text/x-python"
        assert get_mime_type_from_filename("script.PY") == "text/x-python"  # Case insensitive

    def test_javascript_file(self):
        """Test MIME type for JavaScript files."""
        assert get_mime_type_from_filename("script.js") == "text/javascript"
        assert get_mime_type_from_filename("app.JS") == "text/javascript"

    def test_typescript_file(self):
        """Test MIME type for TypeScript files."""
        assert get_mime_type_from_filename("component.ts") == "text/typescript"
        assert get_mime_type_from_filename("types.TS") == "text/typescript"

    def test_json_file(self):
        """Test MIME type for JSON files."""
        assert get_mime_type_from_filename("config.json") == "application/json"
        assert get_mime_type_from_filename("package.JSON") == "application/json"

    def test_yaml_files(self):
        """Test MIME type for YAML files."""
        assert get_mime_type_from_filename("config.yaml") == "text/yaml"
        assert get_mime_type_from_filename("docker-compose.yml") == "text/yaml"
        assert get_mime_type_from_filename("config.YAML") == "text/yaml"

    def test_markdown_file(self):
        """Test MIME type for Markdown files."""
        assert get_mime_type_from_filename("README.md") == "text/markdown"
        assert get_mime_type_from_filename("docs.MD") == "text/markdown"

    def test_text_files(self):
        """Test MIME type for text files."""
        assert get_mime_type_from_filename("notes.txt") == "text/plain"
        assert get_mime_type_from_filename("log.TXT") == "text/plain"

    def test_csv_file(self):
        """Test MIME type for CSV files."""
        assert get_mime_type_from_filename("data.csv") == "text/csv"
        assert get_mime_type_from_filename("export.CSV") == "text/csv"

    def test_xml_file(self):
        """Test MIME type for XML files."""
        assert get_mime_type_from_filename("config.xml") == "text/xml"
        assert get_mime_type_from_filename("data.XML") == "text/xml"

    def test_html_file(self):
        """Test MIME type for HTML files."""
        assert get_mime_type_from_filename("index.html") == "text/html"
        assert get_mime_type_from_filename("page.HTML") == "text/html"

    def test_css_file(self):
        """Test MIME type for CSS files."""
        assert get_mime_type_from_filename("styles.css") == "text/css"
        assert get_mime_type_from_filename("theme.CSS") == "text/css"

    def test_unknown_extension(self):
        """Test MIME type for unknown file extensions."""
        assert get_mime_type_from_filename("unknown.zzz") == "text/plain"
        assert get_mime_type_from_filename("file.unknown123") == "text/plain"

    def test_no_extension(self):
        """Test MIME type for files without extension."""
        assert get_mime_type_from_filename("Dockerfile") == "text/plain"
        assert get_mime_type_from_filename("Makefile") == "text/plain"
        assert get_mime_type_from_filename("README") == "text/plain"


class TestSaveArtifact:
    """Tests for the save_artifact function."""

    @pytest.mark.asyncio
    async def test_save_artifact_success_new_file(
        self,
        mock_tool_context,
        mock_storage_service,
        mock_db_session,
        mock_chat_repository,
    ):
        """Test saving a new artifact successfully."""
        code = "print('Hello World')"
        filename = "hello.py"

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            # Mock context manager
            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_tool_context)

            assert result["status"] == "success"
            assert result["filename"] == filename
            assert result["action"] == "created"
            assert "hello.py" in result["message"]
            assert result["file_path"] == "/path/to/artifact.py"
            assert result["version"] == 1

            # Verify calls
            mock_storage_service.save_artifact.assert_called_once_with("test-session-123", filename, code)
            mock_chat_repository.get_artifact.assert_called_once_with("test-session-123", filename)
            mock_chat_repository.save_artifact.assert_called_once()
            mock_tool_context.save_artifact.assert_called_once()

    @pytest.mark.asyncio
    async def test_save_artifact_success_update_file(
        self,
        mock_tool_context,
        mock_storage_service,
        mock_db_session,
        mock_chat_repository,
    ):
        """Test updating an existing artifact successfully."""
        code = "print('Hello Updated World')"
        filename = "hello.py"

        # Mock existing artifact
        existing_artifact = MagicMock()
        mock_chat_repository.get_artifact.return_value = existing_artifact

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_tool_context)

            assert result["status"] == "success"
            assert result["action"] == "updated"
            assert "updated" in result["message"].lower()

    @pytest.mark.asyncio
    async def test_save_artifact_session_fallback(self, mock_storage_service, mock_db_session, mock_chat_repository):
        """Test save_artifact with session ID fallback."""
        code = "print('Test')"
        filename = "test.py"

        # Mock tool context without session ID in expected location
        mock_context = MagicMock()
        mock_context._invocation_context = MagicMock()
        mock_context._invocation_context.session = None
        mock_context.state = {"current_session_id": "fallback-session"}
        mock_context.save_artifact = AsyncMock(return_value=1)

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_context)

            assert result["status"] == "success"
            mock_storage_service.save_artifact.assert_called_once_with("fallback-session", filename, code)

    @pytest.mark.asyncio
    async def test_save_artifact_default_session(self, mock_storage_service, mock_db_session, mock_chat_repository):
        """Test save_artifact with default session when no session ID found."""
        code = "print('Test')"
        filename = "test.py"

        # Mock tool context without any session ID
        mock_context = MagicMock()
        mock_context._invocation_context = MagicMock()
        mock_context._invocation_context.session = None
        mock_context.state = {}
        mock_context.save_artifact = AsyncMock(return_value=1)

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_context)

            assert result["status"] == "success"
            mock_storage_service.save_artifact.assert_called_once_with("default", filename, code)

    @pytest.mark.asyncio
    async def test_save_artifact_storage_error(self, mock_tool_context):
        """Test save_artifact handling storage service error."""
        code = "print('Test')"
        filename = "test.py"

        mock_storage_service = AsyncMock()
        mock_storage_service.save_artifact.side_effect = Exception("Storage error")

        mock_chat_repo = MagicMock()
        mock_chat_repo.get_artifact = AsyncMock(return_value=None)

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repo):

            # Mock context manager
            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=MagicMock())
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_tool_context)

            assert result["status"] == "error"
            assert result["error_type"] == "unexpected_error"
            assert "Storage error" in result["message"]
            assert result["filename"] == filename

    @pytest.mark.asyncio
    async def test_save_artifact_value_error(self, mock_tool_context):
        """Test save_artifact handling ValueError (configuration error)."""
        code = "print('Test')"
        filename = "test.py"

        # Mock tool context that raises ValueError
        mock_context = MagicMock()
        mock_context._invocation_context = MagicMock()
        mock_context._invocation_context.session = MagicMock()
        mock_context._invocation_context.session.id = "test-session"
        mock_context.save_artifact = AsyncMock(side_effect=ValueError("Config error"))

        # Mock the other services to avoid them interfering
        mock_storage_service = MagicMock()
        mock_storage_service.save_artifact = AsyncMock(return_value="/path/to/artifact.py")

        mock_chat_repo = MagicMock()
        mock_chat_repo.get_artifact = AsyncMock(return_value=None)

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repo):

            # Mock context manager
            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=MagicMock())
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_context)

            assert result["status"] == "error"
            assert result["error_type"] == "configuration_error"
            assert "ArtifactService" in result["message"]
            assert result["filename"] == filename

    @pytest.mark.asyncio
    async def test_save_artifact_database_error(
        self,
        mock_tool_context,
        mock_storage_service,
        mock_db_session,
        mock_chat_repository,
    ):
        """Test save_artifact handling database error during metadata save."""
        code = "print('Test')"
        filename = "test.py"

        # Make chat repository raise an exception
        mock_chat_repository.save_artifact.side_effect = Exception("Database error")

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            # Should still succeed since database error is caught but logged
            result = await save_artifact(code, filename, mock_tool_context)

            assert result["status"] == "success"  # Storage succeeded, DB metadata failed but caught
            assert result["filename"] == filename

    @pytest.mark.asyncio
    async def test_save_artifact_with_special_characters(
        self,
        mock_tool_context,
        mock_storage_service,
        mock_db_session,
        mock_chat_repository,
    ):
        """Test saving artifact with special characters in filename and content."""
        code = "# Trading bot with émojis 🤖\nprint('Hello, 世界!')"
        filename = "bot-émojis_🤖.py"

        with patch("api.storage.storage_service", mock_storage_service), patch(
            "api.database.get_db_session"
        ) as mock_get_db, patch("api.repositories.get_chat_repository", return_value=mock_chat_repository):

            mock_get_db.return_value.__aenter__ = AsyncMock(return_value=mock_db_session)
            mock_get_db.return_value.__aexit__ = AsyncMock(return_value=False)

            result = await save_artifact(code, filename, mock_tool_context)

            assert result["status"] == "success"
            assert result["filename"] == filename
            mock_storage_service.save_artifact.assert_called_once_with("test-session-123", filename, code)
