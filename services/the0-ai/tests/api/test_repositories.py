"""
Unit tests for repository classes.
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import AsyncMock, MagicMock
from sqlalchemy.ext.asyncio import AsyncSession

from api.repositories import ChatRepository, SettingsRepository
from api.models.database import ChatSession, ChatMessage, Artifact, Setting


class TestChatRepository:
    """Tests for ChatRepository class."""

    @pytest.fixture
    def mock_session(self):
        """Create a mock database session."""
        return AsyncMock(spec=AsyncSession)

    @pytest.fixture
    def chat_repository(self, mock_session):
        """Create a ChatRepository instance with mock session."""
        return ChatRepository(mock_session)

    @pytest.mark.asyncio
    async def test_create_session(self, chat_repository, mock_session):
        """Test creating a new chat session."""
        session_id = "test-session-123"
        user_id = "test-user"

        # Mock the session add and commit methods
        mock_session.add = MagicMock()
        mock_session.commit = AsyncMock()

        result = await chat_repository.create_session(session_id, user_id)

        assert result is not None
        assert result.id == session_id
        assert result.user_id == user_id
        mock_session.add.assert_called_once()
        mock_session.commit.assert_called_once()

        # Verify the session object was created correctly
        call_args = mock_session.add.call_args[0][0]
        assert isinstance(call_args, ChatSession)
        assert call_args.id == session_id
        assert call_args.user_id == user_id
        assert call_args.is_active is True

    @pytest.mark.asyncio
    async def test_create_session_error(self, chat_repository, mock_session):
        """Test creating a session with database error."""
        session_id = "test-session-123"
        user_id = "test-user"

        mock_session.add = MagicMock()
        mock_session.commit = AsyncMock(side_effect=Exception("DB error"))
        mock_session.rollback = AsyncMock()

        with pytest.raises(Exception, match="DB error"):
            await chat_repository.create_session(session_id, user_id)

        mock_session.rollback.assert_called_once()

    @pytest.mark.asyncio
    async def test_add_message(self, chat_repository, mock_session):
        """Test adding a message to a session."""
        session_id = "test-session"
        role = "user"
        content = "Hello, create a trading bot"

        mock_session.add = MagicMock()
        mock_session.commit = AsyncMock()
        mock_session.execute = AsyncMock()

        result = await chat_repository.add_message(session_id, role, content)

        # Verify message was added and returned
        assert result is not None
        assert isinstance(result, ChatMessage)
        assert result.session_id == session_id
        assert result.role == role
        assert result.content == content

        # Verify database operations
        mock_session.add.assert_called()
        assert mock_session.commit.call_count == 2  # Called twice in add_message

    @pytest.mark.asyncio
    async def test_save_artifact(self, chat_repository, mock_session):
        """Test saving artifact metadata."""
        session_id = "test-session"
        filename = "trading_bot.py"
        file_path = "/path/to/trading_bot.py"
        mime_type = "text/x-python"
        version = 1

        # Mock existing artifact check
        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = None
        mock_session.execute.return_value = mock_result
        mock_session.add = MagicMock()
        mock_session.commit = AsyncMock()

        result = await chat_repository.save_artifact(
            session_id=session_id,
            filename=filename,
            file_path=file_path,
            mime_type=mime_type,
            version=version,
        )

        # Verify artifact was created and returned
        assert result is not None
        assert isinstance(result, Artifact)
        assert result.session_id == session_id
        assert result.filename == filename
        assert result.file_path == file_path
        assert result.mime_type == mime_type
        assert result.version == version

        mock_session.add.assert_called_once()
        mock_session.commit.assert_called_once()


class TestSettingsRepository:
    """Tests for SettingsRepository class."""

    @pytest.fixture
    def mock_session(self):
        """Create a mock database session."""
        return AsyncMock(spec=AsyncSession)

    @pytest.fixture
    def settings_repository(self, mock_session):
        """Create a SettingsRepository instance with mock session."""
        return SettingsRepository(mock_session)

    @pytest.mark.asyncio
    async def test_set_setting_new(self, settings_repository, mock_session):
        """Test setting a new configuration value."""
        key = "test_setting"
        value = "test_value"

        # Mock no existing setting
        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = None
        mock_session.execute.return_value = mock_result
        mock_session.add = MagicMock()
        mock_session.commit = AsyncMock()

        await settings_repository.set_setting(key, value)

        mock_session.add.assert_called_once()
        mock_session.commit.assert_called_once()

        # Check setting object
        setting_call = mock_session.add.call_args[0][0]
        assert isinstance(setting_call, Setting)
        assert setting_call.key == key
        assert setting_call.value == value

    @pytest.mark.asyncio
    async def test_update_existing_setting(self, settings_repository, mock_session):
        """Test updating an existing setting."""
        key = "existing_setting"
        new_value = "updated_value"

        # Mock existing setting
        mock_setting = MagicMock()
        mock_setting.value = "old_value"

        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = mock_setting
        mock_session.execute.return_value = mock_result
        mock_session.commit = AsyncMock()

        await settings_repository.set_setting(key, new_value)

        # Should update existing setting, not add new one
        assert mock_setting.value == new_value
        mock_session.commit.assert_called_once()

    @pytest.mark.asyncio
    async def test_get_setting_exists(self, settings_repository, mock_session):
        """Test getting an existing setting."""
        key = "test_setting"
        expected_value = "test_value"

        mock_setting = MagicMock()
        mock_setting.value = expected_value

        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = mock_setting
        mock_session.execute.return_value = mock_result

        value = await settings_repository.get_setting(key)

        assert value == expected_value
        mock_session.execute.assert_called_once()

    @pytest.mark.asyncio
    async def test_get_setting_not_found(self, settings_repository, mock_session):
        """Test getting a non-existent setting."""
        key = "nonexistent_setting"

        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = None
        mock_session.execute.return_value = mock_result

        value = await settings_repository.get_setting(key)

        assert value is None

    @pytest.mark.asyncio
    async def test_has_api_key_true(self, settings_repository, mock_session):
        """Test checking for API key when it exists."""
        mock_setting = MagicMock()
        mock_setting.value = "some-key"

        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = mock_setting
        mock_session.execute.return_value = mock_result

        has_key = await settings_repository.has_api_key()

        assert has_key is True

    @pytest.mark.asyncio
    async def test_has_api_key_false(self, settings_repository, mock_session):
        """Test checking for API key when it doesn't exist."""
        mock_result = MagicMock()
        mock_result.scalar_one_or_none.return_value = None
        mock_session.execute.return_value = mock_result

        has_key = await settings_repository.has_api_key()

        assert has_key is False
