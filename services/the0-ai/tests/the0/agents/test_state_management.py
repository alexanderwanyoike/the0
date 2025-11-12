"""
Comprehensive tests for state management infrastructure.

This module tests the Pydantic schemas and SessionStateManager utility
for cross-agent state sharing in the Multi-Agent System (MAS).

Test Coverage:
    - ResearchFinding validation and serialization
    - ResearchData validation and serialization
    - BotMetadata validation and serialization
    - SessionStateManager CRUD operations
    - Cross-agent state sharing workflows
    - Schema versioning
    - Error handling and logging
"""

import pytest
from datetime import datetime
from the0.agents.state_schema import (
    ResearchData,
    ResearchFinding,
    BotMetadata,
    SessionStateManager,
)


class TestResearchFinding:
    """Test ResearchFinding Pydantic model."""

    def test_valid_finding(self):
        """Test creating valid ResearchFinding."""
        finding = ResearchFinding(
            point="Test finding",
            source="https://example.com",
            confidence="high",
        )
        assert finding.point == "Test finding"
        assert finding.source == "https://example.com"
        assert finding.confidence == "high"
        assert isinstance(finding.timestamp, datetime)

    def test_invalid_confidence(self):
        """Test that invalid confidence raises ValidationError."""
        with pytest.raises(ValueError):
            ResearchFinding(
                point="Test",
                source="https://example.com",
                confidence="invalid",  # Should be high/medium/low
            )

    def test_finding_serialization(self):
        """Test finding serializes to dict correctly."""
        finding = ResearchFinding(point="Test", source="https://example.com", confidence="high")
        data = finding.model_dump()
        assert data["point"] == "Test"
        assert data["source"] == "https://example.com"
        assert data["confidence"] == "high"
        assert "timestamp" in data

    def test_all_confidence_levels(self):
        """Test all valid confidence levels."""
        for level in ["high", "medium", "low"]:
            finding = ResearchFinding(point="Test", source="https://example.com", confidence=level)
            assert finding.confidence == level

    def test_empty_point_raises_error(self):
        """Test that empty point raises ValidationError."""
        with pytest.raises(ValueError):
            ResearchFinding(point="", source="https://example.com", confidence="high")

    def test_empty_source_raises_error(self):
        """Test that empty source raises ValidationError."""
        with pytest.raises(ValueError):
            ResearchFinding(point="Test", source="", confidence="high")


class TestResearchData:
    """Test ResearchData Pydantic model."""

    def test_valid_research_data(self):
        """Test creating valid ResearchData."""
        data = ResearchData(
            query="Test query",
            summary="Test summary",
            findings=[],
            recommendations=["rec1", "rec2"],
            sources=[],
        )
        assert data.query == "Test query"
        assert data.summary == "Test summary"
        assert data.version == "1.0"
        assert len(data.recommendations) == 2
        assert isinstance(data.timestamp, datetime)

    def test_research_data_with_findings(self):
        """Test ResearchData with nested findings."""
        finding = ResearchFinding(
            point="Test finding",
            source="https://example.com",
            confidence="high",
        )
        data = ResearchData(
            query="Test",
            summary="Summary",
            findings=[finding],
            recommendations=[],
            sources=[],
        )
        assert len(data.findings) == 1
        assert data.findings[0].point == "Test finding"
        assert data.findings[0].confidence == "high"

    def test_research_data_serialization(self):
        """Test research data serializes correctly."""
        data = ResearchData(
            query="Test",
            summary="Summary",
            findings=[],
            recommendations=["rec1"],
            sources=[{"title": "Test", "url": "https://example.com"}],
        )
        serialized = data.model_dump()
        assert serialized["query"] == "Test"
        assert serialized["summary"] == "Summary"
        assert serialized["version"] == "1.0"
        assert len(serialized["sources"]) == 1
        assert len(serialized["recommendations"]) == 1

    def test_research_data_defaults(self):
        """Test that default values are set correctly."""
        data = ResearchData(query="Test", summary="Summary")
        assert data.version == "1.0"
        assert data.findings == []
        assert data.recommendations == []
        assert data.sources == []
        assert data.researcher_notes is None
        assert isinstance(data.timestamp, datetime)

    def test_research_data_with_notes(self):
        """Test ResearchData with researcher notes."""
        data = ResearchData(
            query="Test",
            summary="Summary",
            researcher_notes="Additional context",
        )
        assert data.researcher_notes == "Additional context"

    def test_empty_query_raises_error(self):
        """Test that empty query raises ValidationError."""
        with pytest.raises(ValueError):
            ResearchData(query="", summary="Summary")

    def test_empty_summary_raises_error(self):
        """Test that empty summary raises ValidationError."""
        with pytest.raises(ValueError):
            ResearchData(query="Test", summary="")


class TestBotMetadata:
    """Test BotMetadata Pydantic model."""

    def test_valid_bot_metadata(self):
        """Test creating valid BotMetadata."""
        metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            files_created=["main.py"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
        )
        assert metadata.bot_name == "test_bot"
        assert metadata.language == "python"
        assert metadata.version == "1.0"
        assert metadata.execution_verified is False  # default
        assert metadata.backtest_verified is False  # default

    def test_invalid_language(self):
        """Test that invalid language raises ValidationError."""
        with pytest.raises(ValueError):
            BotMetadata(
                bot_name="test",
                language="java",  # Should be python or javascript
                files_created=[],
                strategy_type="momentum",
                platform="binance",
                status="in_progress",
            )

    def test_valid_languages(self):
        """Test both valid languages."""
        for lang in ["python", "javascript"]:
            metadata = BotMetadata(
                bot_name="test",
                language=lang,
                files_created=[],
                strategy_type="momentum",
                platform="binance",
                status="in_progress",
            )
            assert metadata.language == lang

    def test_bot_metadata_with_test_results(self):
        """Test BotMetadata with test results."""
        metadata = BotMetadata(
            bot_name="test",
            language="python",
            files_created=["main.py"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
            test_results={
                "bot_execution": "success",
                "backtest_execution": "success",
                "backtest_trades": 42,
                "backtest_pnl": -2.3,
            },
        )
        assert metadata.execution_verified is True
        assert metadata.backtest_verified is True
        assert metadata.test_results["backtest_trades"] == 42
        assert metadata.test_results["backtest_pnl"] == -2.3

    def test_bot_metadata_defaults(self):
        """Test that default values are set correctly."""
        metadata = BotMetadata(
            bot_name="test",
            language="python",
            strategy_type="momentum",
            platform="binance",
            status="in_progress",
        )
        assert metadata.version == "1.0"
        assert metadata.files_created == []
        assert metadata.execution_verified is False
        assert metadata.backtest_verified is False
        assert metadata.libraries_used == []
        assert metadata.test_results == {}
        assert metadata.developer_notes is None
        assert isinstance(metadata.timestamp, datetime)

    def test_bot_metadata_with_libraries(self):
        """Test BotMetadata with libraries."""
        metadata = BotMetadata(
            bot_name="test",
            language="python",
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            libraries_used=["ccxt==4.1.0", "pandas-ta==0.3.14b0"],
        )
        assert len(metadata.libraries_used) == 2
        assert "ccxt==4.1.0" in metadata.libraries_used

    def test_bot_metadata_serialization(self):
        """Test bot metadata serializes correctly."""
        metadata = BotMetadata(
            bot_name="test",
            language="python",
            files_created=["main.py", "bot-config.yaml"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
        )
        data = metadata.model_dump()
        assert data["bot_name"] == "test"
        assert data["language"] == "python"
        assert len(data["files_created"]) == 2
        assert data["version"] == "1.0"


class TestSessionStateManager:
    """Test SessionStateManager utility class."""

    def test_store_and_retrieve_research(self):
        """Test storing and retrieving research data."""
        state = {}
        research = ResearchData(
            query="Test query",
            summary="Test summary",
            findings=[],
            recommendations=["rec1"],
            sources=[],
        )

        # Store
        SessionStateManager.store_research(state, research)
        assert SessionStateManager.RESEARCH_KEY in state

        # Retrieve
        retrieved = SessionStateManager.get_research(state)
        assert retrieved is not None
        assert retrieved.query == "Test query"
        assert retrieved.summary == "Test summary"
        assert len(retrieved.recommendations) == 1

    def test_get_research_returns_none_when_not_found(self):
        """Test that get_research returns None when no data."""
        state = {}
        result = SessionStateManager.get_research(state)
        assert result is None

    def test_store_and_retrieve_bot_metadata(self):
        """Test storing and retrieving bot metadata."""
        state = {}
        metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            files_created=["main.py"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
        )

        # Store
        SessionStateManager.store_bot_metadata(state, metadata)
        assert SessionStateManager.BOT_METADATA_KEY in state

        # Retrieve
        retrieved = SessionStateManager.get_bot_metadata(state)
        assert retrieved is not None
        assert retrieved.bot_name == "test_bot"
        assert retrieved.language == "python"

    def test_get_bot_metadata_returns_none_when_not_found(self):
        """Test that get_bot_metadata returns None when no data."""
        state = {}
        result = SessionStateManager.get_bot_metadata(state)
        assert result is None

    def test_clear_state(self):
        """Test clearing all MAS state."""
        state = {
            SessionStateManager.RESEARCH_KEY: {"query": "test"},
            SessionStateManager.BOT_METADATA_KEY: {"bot_name": "test"},
            "other_key": "should_remain",
        }

        SessionStateManager.clear_state(state)

        assert SessionStateManager.RESEARCH_KEY not in state
        assert SessionStateManager.BOT_METADATA_KEY not in state
        assert "other_key" in state  # Other keys preserved

    def test_get_state_summary_empty(self):
        """Test getting state summary for empty state."""
        state = {}

        summary = SessionStateManager.get_state_summary(state)
        assert summary["has_research_data"] is False
        assert summary["has_bot_metadata"] is False
        assert summary["state_keys"] == []

    def test_get_state_summary_with_research(self):
        """Test getting state summary with research data."""
        state = {}

        # Add research data
        research = ResearchData(query="Test query", summary="Summary", findings=[], recommendations=[], sources=[])
        SessionStateManager.store_research(state, research)

        summary = SessionStateManager.get_state_summary(state)
        assert summary["has_research_data"] is True
        assert summary["research_findings_count"] == 0
        assert "research_timestamp" in summary
        assert "research_query" in summary

    def test_get_state_summary_with_bot(self):
        """Test getting state summary with bot metadata."""
        state = {}

        # Add bot metadata
        metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            files_created=["main.py", "bot-config.yaml"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
        )
        SessionStateManager.store_bot_metadata(state, metadata)

        summary = SessionStateManager.get_state_summary(state)
        assert summary["has_bot_metadata"] is True
        assert summary["bot_name"] == "test_bot"
        assert summary["bot_status"] == "ready_for_deploy"
        assert summary["bot_files_count"] == 2
        assert summary["bot_execution_verified"] is True
        assert summary["bot_backtest_verified"] is True

    def test_store_research_with_invalid_data_raises_error(self):
        """Test that storing invalid research raises error."""
        state = {}

        # Try to store non-ResearchData object
        with pytest.raises(ValueError):
            SessionStateManager.store_research(state, "not a ResearchData object")

    def test_store_bot_metadata_with_invalid_data_raises_error(self):
        """Test that storing invalid metadata raises error."""
        state = {}

        # Try to store non-BotMetadata object
        with pytest.raises(ValueError):
            SessionStateManager.store_bot_metadata(state, "not a BotMetadata object")

    def test_logging_on_store_research(self, caplog):
        """Test that store_research logs operations."""
        import logging

        caplog.set_level(logging.INFO)

        state = {}
        research = ResearchData(
            query="Test query",
            summary="Summary",
            findings=[],
            recommendations=[],
            sources=[],
        )

        SessionStateManager.store_research(state, research)

        assert "Storing research data" in caplog.text
        assert "Successfully stored research data" in caplog.text

    def test_logging_on_get_research_not_found(self, caplog):
        """Test that get_research logs when data not found."""
        import logging

        caplog.set_level(logging.INFO)

        state = {}
        SessionStateManager.get_research(state)

        assert "No research data found" in caplog.text

    def test_logging_on_store_bot_metadata(self, caplog):
        """Test that store_bot_metadata logs operations."""
        import logging

        caplog.set_level(logging.INFO)

        state = {}
        metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            strategy_type="momentum",
            platform="binance",
            status="in_progress",
        )

        SessionStateManager.store_bot_metadata(state, metadata)

        assert "Storing bot metadata" in caplog.text
        assert "Successfully stored bot metadata" in caplog.text

    def test_logging_on_get_bot_metadata_not_found(self, caplog):
        """Test that get_bot_metadata logs when data not found."""
        import logging

        caplog.set_level(logging.INFO)

        state = {}
        SessionStateManager.get_bot_metadata(state)

        assert "No bot metadata found" in caplog.text

    def test_research_data_persistence_through_serialization(self):
        """Test that research data survives serialization/deserialization."""
        state = {}
        finding = ResearchFinding(
            point="WebSocket available",
            source="https://binance-docs.github.io/",
            confidence="high",
        )
        research = ResearchData(
            query="Research Binance API",
            summary="API provides REST and WebSocket",
            findings=[finding],
            recommendations=["Use WebSocket"],
            sources=[
                {
                    "title": "Binance Docs",
                    "url": "https://binance-docs.github.io/",
                }
            ],
        )

        # Store and retrieve
        SessionStateManager.store_research(state, research)
        retrieved = SessionStateManager.get_research(state)

        # Verify all data preserved
        assert retrieved is not None
        assert retrieved.query == research.query
        assert retrieved.summary == research.summary
        assert len(retrieved.findings) == 1
        assert retrieved.findings[0].point == "WebSocket available"
        assert retrieved.findings[0].confidence == "high"
        assert len(retrieved.recommendations) == 1
        assert len(retrieved.sources) == 1


def test_cross_agent_state_sharing():
    """
    Integration test: Researcher writes state, Developer reads it.

    Simulates the MAS workflow where Researcher agent completes research
    and stores findings, then Developer agent reads those findings.
    """
    # Simulate shared session state
    session_state = {}

    # === Researcher Agent Workflow ===
    # Researcher completes research and stores findings
    researcher_output = ResearchData(
        query="Research Binance API for momentum strategy",
        summary="Binance provides REST and WebSocket APIs with comprehensive documentation",
        findings=[
            ResearchFinding(
                point="WebSocket available for real-time data",
                source="https://binance-docs.github.io/",
                confidence="high",
            ),
            ResearchFinding(
                point="python-binance library recommended",
                source="https://pypi.org/project/python-binance/",
                confidence="high",
            ),
        ],
        recommendations=[
            "Use WebSocket for real-time data",
            "Implement python-binance library",
        ],
        sources=[
            {
                "title": "Binance API Docs",
                "url": "https://binance-docs.github.io/",
                "relevance": "Official API documentation",
            }
        ],
    )

    # Store research data in state
    SessionStateManager.store_research(session_state, researcher_output)

    # === Developer Agent Workflow ===
    # Developer reads research data from state
    research = SessionStateManager.get_research(session_state)

    # Verify data was successfully shared
    assert research is not None
    assert research.query == "Research Binance API for momentum strategy"
    assert len(research.findings) == 2
    assert "WebSocket" in research.findings[0].point
    assert "python-binance" in research.recommendations[1]

    # Developer creates bot based on research
    bot = BotMetadata(
        bot_name="momentum_btc_binance",
        language="python",
        files_created=["main.py", "bot-config.yaml", "requirements.txt"],
        strategy_type="momentum",
        platform="binance",
        status="ready_for_deploy",
        execution_verified=True,
        backtest_verified=True,
        libraries_used=["python-binance==1.0.19", "pandas-ta==0.3.14b0"],
    )

    # Store bot metadata in state
    SessionStateManager.store_bot_metadata(session_state, bot)

    # === Supervisor Reads Both ===
    # Supervisor can access both research and bot metadata
    final_research = SessionStateManager.get_research(session_state)
    final_bot = SessionStateManager.get_bot_metadata(session_state)

    assert final_research is not None
    assert final_bot is not None
    assert final_bot.bot_name == "momentum_btc_binance"
    assert final_bot.execution_verified is True

    # Get state summary
    summary = SessionStateManager.get_state_summary(session_state)
    assert summary["has_research_data"] is True
    assert summary["has_bot_metadata"] is True
    assert summary["research_findings_count"] == 2
    assert summary["bot_name"] == "momentum_btc_binance"


def test_schema_versioning():
    """Test that schema versioning is supported for evolution."""
    state = {}

    # Store research with v1.0 schema
    research_v1 = ResearchData(
        version="1.0",
        query="Test",
        summary="Summary",
        findings=[],
        recommendations=[],
        sources=[],
    )
    SessionStateManager.store_research(state, research_v1)

    # Retrieve and verify version
    retrieved = SessionStateManager.get_research(state)
    assert retrieved is not None
    assert retrieved.version == "1.0"

    # Store bot with v1.0 schema
    bot_v1 = BotMetadata(
        version="1.0",
        bot_name="test",
        language="python",
        strategy_type="momentum",
        platform="binance",
        status="in_progress",
    )
    SessionStateManager.store_bot_metadata(state, bot_v1)

    # Retrieve and verify version
    retrieved_bot = SessionStateManager.get_bot_metadata(state)
    assert retrieved_bot is not None
    assert retrieved_bot.version == "1.0"


def test_state_isolation_between_sessions():
    """Test that state is properly isolated between different sessions."""
    # Simulate two different session states
    session1 = {}
    session2 = {}

    # Store different data in each session
    research1 = ResearchData(query="Session 1 query", summary="Session 1 summary")
    research2 = ResearchData(query="Session 2 query", summary="Session 2 summary")

    SessionStateManager.store_research(session1, research1)
    SessionStateManager.store_research(session2, research2)

    # Verify isolation
    retrieved1 = SessionStateManager.get_research(session1)
    retrieved2 = SessionStateManager.get_research(session2)

    assert retrieved1.query == "Session 1 query"
    assert retrieved2.query == "Session 2 query"
    assert retrieved1.query != retrieved2.query


def test_handling_corrupted_state():
    """Test that corrupted state data is handled gracefully."""
    state = {}

    # Manually insert invalid data (simulate corruption)
    state[SessionStateManager.RESEARCH_KEY] = {
        "query": "Test",
        # Missing required 'summary' field
    }

    # Should return None instead of raising exception
    result = SessionStateManager.get_research(state)
    assert result is None

    # Same for bot metadata
    state[SessionStateManager.BOT_METADATA_KEY] = {
        "bot_name": "test",
        # Missing required fields
    }

    result = SessionStateManager.get_bot_metadata(state)
    assert result is None
