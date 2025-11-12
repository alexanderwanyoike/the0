"""
Tests for state management tools.

Verifies that state management tools correctly store and retrieve data
from session state using SessionStateManager.
"""

import pytest
from unittest.mock import Mock
from the0.tools.state_management import (
    store_research_data,
    get_research_data,
    store_bot_metadata,
    get_bot_metadata,
    get_state_summary,
)


class TestStoreResearchData:
    """Test store_research_data tool."""

    @pytest.mark.asyncio
    async def test_store_research_data_success(self):
        """Test successful research data storage."""
        # Mock tool context
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        # Call tool
        result = await store_research_data(
            query="Research Binance API",
            summary="Binance provides REST and WebSocket APIs",
            findings=[
                {
                    "point": "WebSocket supported",
                    "source": "https://binance-docs.github.io/",
                    "confidence": "high",
                }
            ],
            recommendations=["Use WebSocket for real-time data"],
            sources=[
                {
                    "title": "Binance Docs",
                    "url": "https://binance-docs.github.io/",
                    "relevance": "Official documentation",
                }
            ],
            tool_context=mock_context,
        )

        # Verify response
        assert result["status"] == "success"
        assert result["findings_count"] == 1
        assert result["recommendations_count"] == 1
        assert result["sources_count"] == 1

        # Verify data stored in session state
        assert "research_data" in session_state

    @pytest.mark.asyncio
    async def test_store_research_data_with_notes(self):
        """Test research data storage with optional notes."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await store_research_data(
            query="Test query",
            summary="Test summary",
            findings=[],
            recommendations=[],
            sources=[],
            researcher_notes="Additional context",
            tool_context=mock_context,
        )

        assert result["status"] == "success"

    @pytest.mark.asyncio
    async def test_store_research_data_invalid_confidence(self):
        """Test that invalid confidence level fails validation."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await store_research_data(
            query="Test",
            summary="Test",
            findings=[
                {
                    "point": "Test finding",
                    "source": "https://test.com",
                    "confidence": "invalid",  # Invalid confidence
                }
            ],
            recommendations=[],
            sources=[],
            tool_context=mock_context,
        )

        assert result["status"] == "error"
        assert "confidence" in result["message"].lower()


class TestGetResearchData:
    """Test get_research_data tool."""

    @pytest.mark.asyncio
    async def test_get_research_data_success(self):
        """Test successful research data retrieval."""
        # Setup: store data first
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        await store_research_data(
            query="Test query",
            summary="Test summary",
            findings=[
                {
                    "point": "Test finding",
                    "source": "https://test.com",
                    "confidence": "high",
                }
            ],
            recommendations=["Test recommendation"],
            sources=[{"title": "Test", "url": "https://test.com"}],
            tool_context=mock_context,
        )

        # Retrieve data
        result = await get_research_data(tool_context=mock_context)

        assert result["status"] == "success"
        assert result["data"]["query"] == "Test query"
        assert result["data"]["summary"] == "Test summary"
        assert len(result["data"]["findings"]) == 1
        assert result["data"]["findings"][0]["point"] == "Test finding"

    @pytest.mark.asyncio
    async def test_get_research_data_not_found(self):
        """Test retrieval when no research data exists."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await get_research_data(tool_context=mock_context)

        assert result["status"] == "not_found"
        assert result["data"] is None


class TestStoreBotMetadata:
    """Test store_bot_metadata tool."""

    @pytest.mark.asyncio
    async def test_store_bot_metadata_success(self):
        """Test successful bot metadata storage."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await store_bot_metadata(
            bot_name="momentum_btc_binance",
            language="python",
            files_created=["main.py", "bot-config.yaml"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
            tool_context=mock_context,
        )

        assert result["status"] == "success"
        assert result["bot_name"] == "momentum_btc_binance"
        assert result["execution_verified"] is True
        assert result["backtest_verified"] is True

        # Verify data stored
        assert "bot_metadata" in session_state

    @pytest.mark.asyncio
    async def test_store_bot_metadata_with_test_results(self):
        """Test bot metadata storage with test results."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await store_bot_metadata(
            bot_name="test_bot",
            language="javascript",
            files_created=["index.js"],
            strategy_type="DCA",
            platform="kraken",
            status="needs_fixes",
            execution_verified=False,
            backtest_verified=False,
            test_results={
                "bot_execution": "failed",
                "error_message": "Module not found",
            },
            tool_context=mock_context,
        )

        assert result["status"] == "success"

    @pytest.mark.asyncio
    async def test_store_bot_metadata_invalid_language(self):
        """Test that invalid language fails validation."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await store_bot_metadata(
            bot_name="test",
            language="ruby",  # Invalid - only python/javascript allowed
            files_created=[],
            strategy_type="test",
            platform="test",
            status="test",
            tool_context=mock_context,
        )

        assert result["status"] == "error"
        assert "language" in result["message"].lower()


class TestGetBotMetadata:
    """Test get_bot_metadata tool."""

    @pytest.mark.asyncio
    async def test_get_bot_metadata_success(self):
        """Test successful bot metadata retrieval."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        # Store first
        await store_bot_metadata(
            bot_name="test_bot",
            language="python",
            files_created=["main.py"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            tool_context=mock_context,
        )

        # Retrieve
        result = await get_bot_metadata(tool_context=mock_context)

        assert result["status"] == "success"
        assert result["data"]["bot_name"] == "test_bot"
        assert result["data"]["language"] == "python"

    @pytest.mark.asyncio
    async def test_get_bot_metadata_not_found(self):
        """Test retrieval when no bot metadata exists."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await get_bot_metadata(tool_context=mock_context)

        assert result["status"] == "not_found"
        assert result["data"] is None


class TestGetStateSummary:
    """Test get_state_summary tool."""

    @pytest.mark.asyncio
    async def test_get_state_summary_empty(self):
        """Test state summary with no data."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        result = await get_state_summary(tool_context=mock_context)

        assert result["status"] == "success"
        assert result["summary"]["has_research_data"] is False
        assert result["summary"]["has_bot_metadata"] is False

    @pytest.mark.asyncio
    async def test_get_state_summary_with_data(self):
        """Test state summary with both research and bot data."""
        mock_context = Mock()
        session_state = {}
        mock_context._invocation_context.session.state = session_state

        # Store research
        await store_research_data(
            query="Test",
            summary="Test",
            findings=[],
            recommendations=[],
            sources=[],
            tool_context=mock_context,
        )

        # Store bot metadata
        await store_bot_metadata(
            bot_name="test_bot",
            language="python",
            files_created=[],
            strategy_type="test",
            platform="test",
            status="test",
            tool_context=mock_context,
        )

        # Get summary
        result = await get_state_summary(tool_context=mock_context)

        assert result["status"] == "success"
        assert result["summary"]["has_research_data"] is True
        assert result["summary"]["has_bot_metadata"] is True
        assert result["summary"]["bot_name"] == "test_bot"


class TestEndToEndWorkflow:
    """Test complete workflow with state management tools."""

    @pytest.mark.asyncio
    async def test_researcher_to_developer_workflow(self):
        """Simulate Researcher → Developer workflow."""
        # Shared session state
        session_state = {}
        mock_context = Mock()
        mock_context._invocation_context.session.state = session_state

        # Researcher stores research
        research_result = await store_research_data(
            query="Research Binance API for momentum trading",
            summary="Binance provides comprehensive API support",
            findings=[
                {
                    "point": "WebSocket recommended for real-time data",
                    "source": "https://binance-docs.github.io/",
                    "confidence": "high",
                }
            ],
            recommendations=["Use python-binance library"],
            sources=[{"title": "Binance Docs", "url": "https://binance-docs.github.io/"}],
            tool_context=mock_context,
        )
        assert research_result["status"] == "success"

        # Developer reads research
        get_research_result = await get_research_data(tool_context=mock_context)
        assert get_research_result["status"] == "success"
        assert get_research_result["data"]["query"] == "Research Binance API for momentum trading"

        # Developer creates bot based on research
        bot_result = await store_bot_metadata(
            bot_name="momentum_btc_binance",
            language="python",
            files_created=["main.py", "bot-config.yaml", "requirements.txt"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
            libraries_used=["python-binance==1.0.19"],
            tool_context=mock_context,
        )
        assert bot_result["status"] == "success"

        # Supervisor can access all data
        summary_result = await get_state_summary(tool_context=mock_context)
        assert summary_result["status"] == "success"
        assert summary_result["summary"]["has_research_data"] is True
        assert summary_result["summary"]["has_bot_metadata"] is True
