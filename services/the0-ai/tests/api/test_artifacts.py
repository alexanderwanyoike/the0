"""
Unit tests for artifact-related API endpoints.
"""

import pytest
from unittest.mock import patch, AsyncMock


class TestArtifactEndpoints:
    """Tests for artifact-related endpoints."""

    @patch("api.main.agent_service")
    def test_list_artifacts(self, mock_agent, test_client):
        """Test listing all artifacts."""
        mock_agent.list_artifact_keys = AsyncMock(return_value=["bot1.py", "config.yaml"])

        response = test_client.get("/artifacts")

        assert response.status_code == 200
        data = response.json()
        assert data == ["bot1.py", "config.yaml"]
        mock_agent.list_artifact_keys.assert_called_once()

    @patch("api.main.agent_service")
    def test_list_artifacts_error(self, mock_agent, test_client):
        """Test listing artifacts with service error."""
        mock_agent.list_artifact_keys = AsyncMock(side_effect=Exception("Service error"))

        response = test_client.get("/artifacts")

        assert response.status_code == 500
        assert "Service error" in response.json()["detail"]

    @patch("api.main.agent_service")
    def test_list_session_artifacts(self, mock_agent, test_client):
        """Test listing session artifacts."""
        session_id = "test-session-123"
        mock_agent.list_session_artifact_keys = AsyncMock(return_value=["session_bot.py"])

        response = test_client.get(f"/artifacts/session/{session_id}")

        assert response.status_code == 200
        data = response.json()
        assert data == ["session_bot.py"]

        mock_agent.list_session_artifact_keys.assert_called_once_with(session_id)

    @patch("api.main.agent_service")
    def test_list_session_artifacts_error(self, mock_agent, test_client):
        """Test listing session artifacts with error."""
        session_id = "test-session-123"
        mock_agent.list_session_artifact_keys = AsyncMock(side_effect=Exception("Session error"))

        response = test_client.get(f"/artifacts/session/{session_id}")

        assert response.status_code == 500
        assert "Session error" in response.json()["detail"]

    @patch("api.main.agent_service")
    def test_get_artifact_success(self, mock_agent, test_client, sample_artifact_response):
        """Test getting an artifact successfully."""
        filename = "trading_bot.py"
        session_id = "test-session"
        mock_agent.get_artifact = AsyncMock(return_value=sample_artifact_response)

        response = test_client.get(f"/artifacts/{filename}?session_id={session_id}")

        assert response.status_code == 200
        data = response.json()
        assert data["filename"] == sample_artifact_response.filename
        assert data["content"] == sample_artifact_response.content
        assert data["version"] == sample_artifact_response.version

        mock_agent.get_artifact.assert_called_once_with(filename, session_id)

    @patch("api.main.agent_service")
    def test_get_artifact_not_found(self, mock_agent, test_client):
        """Test getting non-existent artifact."""
        filename = "nonexistent.py"
        mock_agent.get_artifact = AsyncMock(return_value=None)

        response = test_client.get(f"/artifacts/{filename}")

        assert response.status_code == 404
        assert "Artifact not found" in response.json()["detail"]

    @patch("api.main.agent_service")
    def test_get_artifact_service_error(self, mock_agent, test_client):
        """Test getting artifact with service error."""
        filename = "error_file.py"
        mock_agent.get_artifact = AsyncMock(side_effect=Exception("Storage error"))

        response = test_client.get(f"/artifacts/{filename}")

        assert response.status_code == 500
        assert "Storage error" in response.json()["detail"]

    @patch("api.main.agent_service")
    def test_get_session_artifact(self, mock_agent, test_client, sample_artifact_response):
        """Test getting session-specific artifact."""
        session_id = "test-session"
        filename = "bot.py"
        mock_agent.get_artifact = AsyncMock(return_value=sample_artifact_response)

        response = test_client.get(f"/artifacts/session/{session_id}/{filename}")

        assert response.status_code == 200
        data = response.json()
        assert data["filename"] == sample_artifact_response.filename

        mock_agent.get_artifact.assert_called_once_with(filename, session_id)

    @patch("api.main.agent_service")
    def test_download_artifacts_success(self, mock_agent, test_client):
        """Test downloading artifacts as ZIP."""
        import tempfile
        import os

        # Create a temporary file
        temp_fd, temp_path = tempfile.mkstemp(suffix=".zip")
        os.write(temp_fd, b"fake zip content")
        os.close(temp_fd)

        try:
            mock_agent.create_artifacts_zip = AsyncMock(return_value=temp_path)

            response = test_client.get("/artifacts/download")

            assert response.status_code == 200
            assert response.headers["content-type"] == "application/zip"
            mock_agent.create_artifacts_zip.assert_called_once()
        finally:
            # Clean up temp file
            if os.path.exists(temp_path):
                os.unlink(temp_path)

    @patch("api.main.agent_service")
    def test_download_artifacts_not_found(self, mock_agent, test_client):
        """Test downloading when no artifacts exist."""
        mock_agent.create_artifacts_zip = AsyncMock(return_value=None)

        response = test_client.get("/artifacts/download")

        assert response.status_code == 404
        assert "No artifacts found" in response.json()["detail"]
        mock_agent.create_artifacts_zip.assert_called_once()

    @patch("api.main.agent_service")
    def test_download_artifacts_error(self, mock_agent, test_client):
        """Test download artifacts with service error."""
        mock_agent.create_artifacts_zip = AsyncMock(side_effect=Exception("ZIP error"))

        response = test_client.get("/artifacts/download")

        assert response.status_code == 500
        assert "ZIP error" in response.json()["detail"]
        mock_agent.create_artifacts_zip.assert_called_once()
