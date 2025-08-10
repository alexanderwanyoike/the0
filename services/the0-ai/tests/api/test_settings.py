"""
Unit tests for settings management API endpoints.
"""

import pytest
from unittest.mock import patch, AsyncMock


class TestSettingsEndpoints:
    """Tests for settings management endpoints."""

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_check_api_key_status_false(
        self, mock_get_repo, mock_get_session, test_client
    ):
        """Test checking API key status when no key exists."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.has_api_key = AsyncMock(return_value=False)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/settings/api-key/status")

        assert response.status_code == 200
        data = response.json()
        assert data["has_api_key"] is False

        mock_repo.has_api_key.assert_called_once()

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_check_api_key_status_true(
        self, mock_get_repo, mock_get_session, test_client
    ):
        """Test checking API key status when key exists."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.has_api_key = AsyncMock(return_value=True)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/settings/api-key/status")

        assert response.status_code == 200
        data = response.json()
        assert data["has_api_key"] is True

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_check_api_key_status_error(
        self, mock_get_repo, mock_get_session, test_client
    ):
        """Test checking API key status with database error."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.has_api_key = AsyncMock(side_effect=Exception("Database error"))

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.get("/settings/api-key/status")

        assert response.status_code == 500
        assert "Database error" in response.json()["detail"]

    @patch("api.main.agent_service")
    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_set_api_key_success(
        self, mock_get_repo, mock_get_session, mock_agent, test_client
    ):
        """Test setting API key successfully."""
        api_key_data = {"api_key": "test-api-key-12345"}

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.set_api_key = AsyncMock()
        mock_agent.refresh_api_key = AsyncMock()

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.post("/settings/api-key", json=api_key_data)

        assert response.status_code == 200
        data = response.json()
        assert data["message"] == "API key saved successfully"

        mock_repo.set_api_key.assert_called_once_with(api_key_data["api_key"])
        mock_agent.refresh_api_key.assert_called_once()

    def test_set_api_key_missing(self, test_client):
        """Test setting API key without providing key."""
        response = test_client.post("/settings/api-key", json={})

        assert response.status_code == 400
        assert "API key is required" in response.json()["detail"]

    def test_set_api_key_empty_string(self, test_client):
        """Test setting API key with empty string."""
        api_key_data = {"api_key": ""}

        response = test_client.post("/settings/api-key", json=api_key_data)

        assert response.status_code == 400
        assert "API key is required" in response.json()["detail"]

    def test_set_api_key_none_value(self, test_client):
        """Test setting API key with None value."""
        api_key_data = {"api_key": None}

        response = test_client.post("/settings/api-key", json=api_key_data)

        assert response.status_code == 400
        assert "API key is required" in response.json()["detail"]

    @patch("api.main.agent_service")
    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_set_api_key_database_error(
        self, mock_get_repo, mock_get_session, mock_agent, test_client
    ):
        """Test setting API key with database error."""
        api_key_data = {"api_key": "test-api-key-12345"}

        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.set_api_key = AsyncMock(side_effect=Exception("Database error"))

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.post("/settings/api-key", json=api_key_data)

        assert response.status_code == 500
        assert "Database error" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_reset_api_key_success(self, mock_get_repo, mock_get_session, test_client):
        """Test resetting API key successfully."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_setting = AsyncMock(return_value=True)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete("/settings/api-key")

        assert response.status_code == 200
        data = response.json()
        assert data["message"] == "API key reset successfully"

        mock_repo.delete_setting.assert_called_once_with("google_ai_api_key")

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_reset_api_key_not_found(
        self, mock_get_repo, mock_get_session, test_client
    ):
        """Test resetting API key when none exists."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_setting = AsyncMock(return_value=False)

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete("/settings/api-key")

        assert response.status_code == 404
        assert "No API key found to reset" in response.json()["detail"]

    @patch("api.main.get_db_session")
    @patch("api.main.get_settings_repository")
    def test_reset_api_key_error(self, mock_get_repo, mock_get_session, test_client):
        """Test resetting API key with database error."""
        mock_session = AsyncMock()
        mock_repo = AsyncMock()
        mock_repo.delete_setting = AsyncMock(side_effect=Exception("Database error"))

        mock_get_session.return_value.__aenter__ = AsyncMock(return_value=mock_session)
        mock_get_session.return_value.__aexit__ = AsyncMock(return_value=False)
        mock_get_repo.return_value = mock_repo

        response = test_client.delete("/settings/api-key")

        assert response.status_code == 500
        assert "Database error" in response.json()["detail"]
