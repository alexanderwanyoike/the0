"""
Unit tests for main API endpoints.
"""

import pytest
from unittest.mock import patch, AsyncMock
from fastapi import HTTPException


class TestHealthEndpoint:
    """Tests for health check endpoint."""

    def test_health_check(self, test_client):
        """Test health check returns correct response."""
        response = test_client.get("/health")
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert data["message"] == "the0 AI Agent API is running"


class TestChatEndpoints:
    """Tests for chat-related endpoints."""

    @patch("api.main.agent_service")
    def test_chat_endpoint_success(self, mock_agent, test_client, sample_chat_request, sample_chat_response):
        """Test successful chat request."""
        mock_agent.chat = AsyncMock(return_value=sample_chat_response)

        response = test_client.post("/chat", json=sample_chat_request)

        assert response.status_code == 200
        data = response.json()
        assert data["response"] == sample_chat_response.response
        assert data["session_id"] == sample_chat_response.session_id
        assert data["artifacts"] == sample_chat_response.artifacts

        mock_agent.chat.assert_called_once_with(
            message=sample_chat_request["message"],
            session_id=sample_chat_request["session_id"],
        )

    @patch("api.main.agent_service")
    def test_chat_endpoint_error(self, mock_agent, test_client, sample_chat_request):
        """Test chat endpoint error handling."""
        mock_agent.chat = AsyncMock(side_effect=Exception("Service error"))

        response = test_client.post("/chat", json=sample_chat_request)

        assert response.status_code == 500
        assert "Service error" in response.json()["detail"]

    @patch("api.main.agent_service")
    def test_chat_stream_endpoint_success(self, mock_agent, test_client, sample_chat_request):
        """Test successful streaming chat request."""
        from api.schemas import StreamChunk

        # Mock async generator
        async def mock_stream():
            yield StreamChunk(type="content", content="Hello", session_id="test-session")
            yield StreamChunk(type="complete", session_id="test-session")

        mock_agent.chat_stream = AsyncMock(return_value=mock_stream())

        response = test_client.post("/chat/stream", json=sample_chat_request)

        assert response.status_code == 200
        assert response.headers["content-type"] == "text/event-stream; charset=utf-8"

        # Check that stream is called
        mock_agent.chat_stream.assert_called_once_with(
            message=sample_chat_request["message"],
            session_id=sample_chat_request["session_id"],
        )


class TestVersionEndpoint:
    """Tests for version endpoint."""

    def test_get_version(self, test_client):
        """Test getting application version."""
        with patch("version.get_build_info") as mock_get_build_info:
            mock_build_info = {
                "version": "1.0.0",
                "build_date": "2024-01-01",
                "commit_hash": "abc123",
            }
            mock_get_build_info.return_value = mock_build_info

            response = test_client.get("/version")

            assert response.status_code == 200
            data = response.json()
            assert data == mock_build_info

    def test_get_version_error(self, test_client):
        """Test version endpoint error handling."""
        with patch("version.get_build_info", side_effect=Exception("Version error")):
            response = test_client.get("/version")

            assert response.status_code == 500
            assert "Version error" in response.json()["detail"]
