"""
Unit tests for chat session management API endpoints.
"""

import pytest
from unittest.mock import patch, AsyncMock, MagicMock
from datetime import datetime


class TestSessionEndpoints:
    """Tests for chat session management endpoints."""

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_list_chat_sessions_empty(self, mock_get_repo, mock_get_session, test_client):
        """Test listing chat sessions when none exist."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.list_sessions = AsyncMock(return_value=[])

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/chat/sessions")

        assert response.status_code == 200
        data = response.json()
        assert isinstance(data, list)
        assert len(data) == 0

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_list_chat_sessions_with_data(self, mock_get_repo, mock_get_session, test_client):
        """Test listing chat sessions with existing sessions."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()

        # Create mock session objects
        mock_session_1 = MagicMock()
        mock_session_1.id = "session-1"
        mock_session_1.title = "Chat 1"
        mock_session_1.created_at = datetime(2024, 1, 1, 12, 0, 0)
        mock_session_1.updated_at = datetime(2024, 1, 1, 12, 30, 0)
        mock_session_1.is_active = True

        mock_session_2 = MagicMock()
        mock_session_2.id = "session-2"
        mock_session_2.title = None  # Test fallback title
        mock_session_2.created_at = datetime(2024, 1, 2, 10, 0, 0)
        mock_session_2.updated_at = datetime(2024, 1, 2, 11, 0, 0)
        mock_session_2.is_active = True

        mock_repo.list_sessions = AsyncMock(return_value=[mock_session_1, mock_session_2])

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/chat/sessions?user_id=test-user")

        assert response.status_code == 200
        data = response.json()
        assert len(data) == 2

        # Check first session
        assert data[0]["id"] == "session-1"
        assert data[0]["title"] == "Chat 1"
        assert data[0]["is_active"] is True

        # Check second session (with fallback title)
        assert data[1]["id"] == "session-2"
        assert data[1]["title"] == "Chat session-"  # Fallback title with truncated ID
        assert data[1]["is_active"] is True

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_list_chat_sessions_error(self, mock_get_repo, mock_get_session, test_client):
        """Test listing chat sessions with database error."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.list_sessions = AsyncMock(side_effect=Exception("Database error"))

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/chat/sessions")

        assert response.status_code == 500
        assert "Database error" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_get_chat_session_not_found(self, mock_get_repo, mock_get_session, test_client):
        """Test getting non-existent chat session."""
        session_id = "nonexistent-session"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.get_session_with_messages = AsyncMock(return_value=None)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get(f"/chat/sessions/{session_id}")

        assert response.status_code == 404
        assert "Session not found" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_update_session_title_missing_title(self, mock_get_repo, mock_get_session, test_client):
        """Test updating session title without providing title."""
        session_id = "test-session"

        response = test_client.put(f"/chat/sessions/{session_id}/title")

        assert response.status_code == 400
        assert "Title is required" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_update_session_title_success(self, mock_get_repo, mock_get_session, test_client):
        """Test successfully updating session title."""
        session_id = "test-session"
        new_title = "My Trading Bot Chat"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.update_session_title = AsyncMock(return_value=True)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.put(f"/chat/sessions/{session_id}/title", params={"title": new_title})

        assert response.status_code == 200
        data = response.json()
        assert data["message"] == "Title updated successfully"

        mock_repo.update_session_title.assert_called_once_with(session_id, new_title)

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_update_session_title_not_found(self, mock_get_repo, mock_get_session, test_client):
        """Test updating title for non-existent session."""
        session_id = "nonexistent-session"
        new_title = "New Title"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.update_session_title = AsyncMock(return_value=False)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.put(f"/chat/sessions/{session_id}/title", params={"title": new_title})

        assert response.status_code == 404
        assert "Session not found" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_delete_chat_session_success(self, mock_get_repo, mock_get_session, test_client):
        """Test successfully deleting a session."""
        session_id = "test-session"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_session = AsyncMock(return_value=True)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete(f"/chat/sessions/{session_id}")

        assert response.status_code == 200
        data = response.json()
        assert data["message"] == "Session deleted successfully"

        mock_repo.delete_session.assert_called_once_with(session_id)

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_delete_chat_session_not_found(self, mock_get_repo, mock_get_session, test_client):
        """Test deleting non-existent session."""
        session_id = "nonexistent-session"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_session = AsyncMock(return_value=False)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete(f"/chat/sessions/{session_id}")

        assert response.status_code == 404
        assert "Session not found" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_chat_repository")
    def test_delete_chat_session_error(self, mock_get_repo, mock_get_session, test_client):
        """Test deleting session with database error."""
        session_id = "test-session"

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_session = AsyncMock(side_effect=Exception("Database error"))

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete(f"/chat/sessions/{session_id}")

        assert response.status_code == 500
        assert "Database error" in response.json()["detail"]
