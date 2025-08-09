"""
Test configuration - minimal setup for running tests.
"""

import os
import pytest
from unittest.mock import patch, MagicMock, AsyncMock

# Set minimal environment for imports to work
os.environ.setdefault("DATABASE_URL", "postgresql://test:test@localhost:5432/test")
os.environ.setdefault("MINIO_ENDPOINT", "localhost:9000")
os.environ.setdefault("MINIO_ACCESS_KEY", "test")
os.environ.setdefault("MINIO_SECRET_KEY", "test")
os.environ.setdefault("MINIO_BUCKET_NAME", "test")

# Mock storage service module completely
import sys

mock_storage = MagicMock()
mock_storage.storage_service = MagicMock()
sys.modules["api.storage"] = mock_storage

from fastapi.testclient import TestClient
from api.main import app


@pytest.fixture
def test_client():
    """Test client for API testing."""
    return TestClient(app)


@pytest.fixture
def mock_tool_context():
    """Mock tool context for testing."""
    context = MagicMock()
    context.session_id = "test-session-123"
    context.save_artifact = AsyncMock(return_value=1)
    # Set up the proper nested structure for session ID access
    context._invocation_context.session.id = "test-session-123"
    return context


@pytest.fixture
def mock_storage_service():
    """Mock storage service for testing."""
    service = MagicMock()
    service.save_artifact = AsyncMock(return_value="/path/to/artifact.py")
    return service


@pytest.fixture
def mock_db_session():
    """Mock database session for testing."""
    return MagicMock()


@pytest.fixture
def mock_chat_repository():
    """Mock chat repository for testing."""
    repo = MagicMock()
    repo.get_artifact = AsyncMock(return_value=None)  # New artifact by default
    repo.save_artifact = AsyncMock(return_value=MagicMock(version=1))
    return repo


@pytest.fixture
def sample_artifact_response():
    """Sample artifact response for testing."""
    from api.schemas import ArtifactResponse

    return ArtifactResponse(
        filename="trading_bot.py",
        content="# Trading bot code\nprint('Hello, world!')",
        version=1,
    )


@pytest.fixture
def sample_chat_request():
    """Sample chat request for testing."""
    return {"message": "Create a simple trading bot", "session_id": "test-session-123"}


@pytest.fixture
def sample_chat_response():
    """Sample chat response for testing."""
    from api.schemas import ChatResponse

    return ChatResponse(
        response="I'll create a trading bot for you.",
        session_id="test-session-123",
        artifacts=["trading_bot.py"],
    )
