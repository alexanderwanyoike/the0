"""
State management schemas and utilities for Multi-Agent System (MAS).

This module provides Pydantic models and utilities for managing shared state
across agents in the MAS architecture. Agents use session state to communicate
structured data through validated schemas.

Key Components:
    - ResearchFinding: Individual research finding with source and confidence
    - ResearchData: Complete research output from Researcher agent
    - BotMetadata: Bot creation metadata from Developer agent
    - SessionStateManager: Utility class for type-safe state operations

Usage Example:
    # Researcher stores findings
    research = ResearchData(
        query="Research Binance API",
        summary="API provides REST and WebSocket endpoints",
        findings=[],
        recommendations=["Use WebSocket for real-time data"]
    )
    SessionStateManager.store_research(session.state, research)

    # Developer retrieves findings
    research = SessionStateManager.get_research(session.state)
    if research:
        print(f"Found {len(research.findings)} findings")
"""

from typing import List, Dict, Optional
from pydantic import BaseModel, Field
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class ResearchFinding(BaseModel):
    """
    Single research finding with source and confidence.

    Represents one discrete piece of information discovered during research,
    including the source URL and confidence level assessment.

    Attributes:
        point: Key finding or insight
        source: Source URL where information was found
        confidence: Confidence level (high, medium, or low)
        timestamp: When this finding was recorded
    """

    point: str = Field(..., description="Key finding or insight", min_length=1)
    source: str = Field(
        ...,
        description="Source URL where information was found",
        min_length=1,
    )
    confidence: str = Field(
        ...,
        description="Confidence level: high, medium, or low",
        pattern="^(high|medium|low)$",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this finding was recorded",
    )

    class Config:
        json_schema_extra = {
            "example": {
                "point": "Binance API supports WebSocket for real-time data",
                "source": "https://binance-docs.github.io/apidocs/spot/en/",
                "confidence": "high",
                "timestamp": "2025-11-12T15:00:00Z",
            }
        }


class ResearchData(BaseModel):
    """
    Structured research data stored in session state.

    Complete research output from Researcher agent, including findings,
    recommendations, and sources. Stored in session.state['research_data'].

    Attributes:
        version: Schema version for evolution support
        query: Original research request from user or Supervisor
        summary: Executive summary of findings (2-3 sentences)
        findings: List of key findings with sources and confidence
        recommendations: Actionable recommendations based on research
        sources: All sources referenced (title, url, relevance, published)
        timestamp: When research was completed
        researcher_notes: Additional notes or context from researcher
    """

    version: str = Field(default="1.0", description="Schema version for evolution support")
    query: str = Field(
        ...,
        description="Original research request from user or Supervisor",
        min_length=1,
    )
    summary: str = Field(
        ...,
        description="Executive summary of findings (2-3 sentences)",
        min_length=1,
    )
    findings: List[ResearchFinding] = Field(
        default_factory=list,
        description="List of key findings with sources and confidence",
    )
    recommendations: List[str] = Field(
        default_factory=list,
        description="Actionable recommendations based on research",
    )
    sources: List[Dict[str, str]] = Field(
        default_factory=list,
        description=("All sources referenced (title, url, relevance, published)"),
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When research was completed",
    )
    researcher_notes: Optional[str] = Field(
        default=None,
        description="Additional notes or context from researcher",
    )

    class Config:
        json_schema_extra = {
            "example": {
                "version": "1.0",
                "query": "Research Binance Spot API for real-time data",
                "summary": ("Binance Spot API v3 provides comprehensive real-time " "market data"),
                "findings": [
                    {
                        "point": "WebSocket streams available for real-time data",
                        "source": "https://binance-docs.github.io/",
                        "confidence": "high",
                        "timestamp": "2025-11-12T15:00:00Z",
                    }
                ],
                "recommendations": [
                    "Use WebSocket for real-time data to avoid rate limits",
                    "Implement python-binance library for easier integration",
                ],
                "sources": [
                    {
                        "title": "Binance API Docs",
                        "url": "https://binance-docs.github.io/",
                        "relevance": "Official API documentation",
                        "published": "2025-01-15",
                    }
                ],
                "timestamp": "2025-11-12T15:00:00Z",
                "researcher_notes": "API is well-documented and stable",
            }
        }


class BotMetadata(BaseModel):
    """
    Bot metadata stored in session state.

    Complete bot creation metadata from Developer agent, including files,
    status, and test results. Stored in session.state['bot_metadata'].

    Attributes:
        version: Schema version for evolution support
        bot_name: Bot name/identifier (e.g., momentum_btc_binance)
        language: Programming language (python or javascript)
        files_created: List of files created for the bot
        strategy_type: Strategy type (e.g., momentum, arbitrage, DCA)
        platform: Trading platform (e.g., binance, alpaca)
        status: Development status (e.g., ready_for_deploy, in_progress)
        execution_verified: Whether bot execution was tested and passed
        backtest_verified: Whether backtest execution was tested and passed
        libraries_used: Dependencies with versions (e.g., ccxt==4.1.0)
        test_results: Execution test results (bot_execution, backtest_execution, etc.)
        timestamp: When bot was created
        developer_notes: Additional notes from developer
    """

    version: str = Field(default="1.0", description="Schema version for evolution support")
    bot_name: str = Field(
        ...,
        description="Bot name/identifier (e.g., momentum_btc_binance)",
        min_length=1,
    )
    language: str = Field(
        ...,
        description="Programming language (python or javascript)",
        pattern="^(python|javascript)$",
    )
    files_created: List[str] = Field(
        default_factory=list,
        description="List of files created for the bot",
    )
    strategy_type: str = Field(
        ...,
        description="Strategy type (e.g., momentum, arbitrage, DCA)",
        min_length=1,
    )
    platform: str = Field(
        ...,
        description="Trading platform (e.g., binance, alpaca)",
        min_length=1,
    )
    status: str = Field(
        ...,
        description="Development status (e.g., ready_for_deploy, in_progress)",
        min_length=1,
    )
    execution_verified: bool = Field(
        default=False,
        description="Whether bot execution was tested and passed",
    )
    backtest_verified: bool = Field(
        default=False,
        description="Whether backtest execution was tested and passed",
    )
    libraries_used: List[str] = Field(
        default_factory=list,
        description="Dependencies with versions (e.g., ccxt==4.1.0)",
    )
    test_results: Dict = Field(
        default_factory=dict,
        description=("Execution test results (bot_execution, backtest_execution, etc.)"),
    )
    timestamp: datetime = Field(default_factory=datetime.utcnow, description="When bot was created")
    developer_notes: Optional[str] = Field(default=None, description="Additional notes from developer")

    class Config:
        json_schema_extra = {
            "example": {
                "version": "1.0",
                "bot_name": "momentum_btc_binance",
                "language": "python",
                "files_created": [
                    "main.py",
                    "bot-config.yaml",
                    "requirements.txt",
                    "bot-schema.json",
                    "README.md",
                    "backtest.py",
                    "backtest-schema.json",
                ],
                "strategy_type": "momentum",
                "platform": "binance",
                "status": "ready_for_deploy",
                "execution_verified": True,
                "backtest_verified": True,
                "libraries_used": [
                    "ccxt==4.1.0",
                    "pandas-ta==0.3.14b0",
                    "pandas==2.1.4",
                ],
                "test_results": {
                    "bot_execution": "success",
                    "backtest_execution": "success",
                    "backtest_trades": 42,
                    "backtest_pnl": -2.3,
                    "used_mock_data": False,
                },
                "timestamp": "2025-11-12T16:00:00Z",
                "developer_notes": "Bot tested with paper trading credentials",
            }
        }


class SessionStateManager:
    """
    Utilities for managing session state across agents.

    Provides type-safe methods for storing and retrieving structured data
    in ADK session state. Handles validation, serialization, and logging.

    This class uses static methods to operate on ADK session.state dictionaries.
    All state operations are validated through Pydantic schemas and logged
    for debugging purposes.

    State Keys:
        RESEARCH_KEY: "research_data" - ResearchData from Researcher agent
        BOT_METADATA_KEY: "bot_metadata" - BotMetadata from Developer agent

    Usage:
        # Store research data
        research = ResearchData(query="...", summary="...", findings=[])
        SessionStateManager.store_research(session.state, research)

        # Retrieve research data
        research = SessionStateManager.get_research(session.state)
        if research:
            print(research.summary)
    """

    # State keys - matches constants in base.py
    RESEARCH_KEY = "research_data"
    BOT_METADATA_KEY = "bot_metadata"

    @staticmethod
    def store_research(state: dict, research: ResearchData) -> None:
        """
        Store research data in session state.

        Validates the ResearchData object, serializes it to a dict,
        logs the operation, and stores it in session.state['research_data'].

        Args:
            state: Session state dictionary (session.state)
            research: ResearchData instance to store

        Raises:
            ValueError: If research data is invalid or serialization fails

        Example:
            research = ResearchData(
                query="Research Binance API",
                summary="API provides REST and WebSocket endpoints",
                findings=[],
                recommendations=["Use WebSocket for real-time data"]
            )
            SessionStateManager.store_research(session.state, research)
        """
        try:
            # Validate and serialize
            data = research.model_dump()

            # Log operation
            logger.info(
                f"Storing research data: "
                f"query='{research.query[:50]}...', "
                f"findings={len(research.findings)}, "
                f"recommendations={len(research.recommendations)}, "
                f"size={len(str(data))} bytes"
            )

            # Store in state
            state[SessionStateManager.RESEARCH_KEY] = data

            logger.info("Successfully stored research data")

        except Exception as e:
            logger.error(f"Error storing research data: {e}", exc_info=True)
            raise ValueError(f"Failed to store research: {e}")

    @staticmethod
    def get_research(state: dict) -> Optional[ResearchData]:
        """
        Retrieve research data from session state.

        Reads from session.state['research_data'], validates the schema,
        and returns a ResearchData object if found.

        Args:
            state: Session state dictionary (session.state)

        Returns:
            ResearchData instance if found and valid, None otherwise

        Example:
            research = SessionStateManager.get_research(session.state)
            if research:
                print(f"Found {len(research.findings)} findings")
                for rec in research.recommendations:
                    print(f"- {rec}")
            else:
                print("No research data available")
        """
        data = state.get(SessionStateManager.RESEARCH_KEY)

        if not data:
            logger.info("No research data found in session state")
            return None

        try:
            research = ResearchData(**data)
            logger.info(
                f"Retrieved research data: "
                f"query='{research.query[:50]}...', "
                f"findings={len(research.findings)}, "
                f"age={(datetime.utcnow() - research.timestamp).seconds}s"
            )
            return research
        except Exception as e:
            logger.error(f"Error parsing research data: {e}", exc_info=True)
            return None

    @staticmethod
    def store_bot_metadata(state: dict, metadata: BotMetadata) -> None:
        """
        Store bot metadata in session state.

        Validates the BotMetadata object, serializes it to a dict,
        logs the operation, and stores it in session.state['bot_metadata'].

        Args:
            state: Session state dictionary (session.state)
            metadata: BotMetadata instance to store

        Raises:
            ValueError: If metadata is invalid or serialization fails

        Example:
            metadata = BotMetadata(
                bot_name="momentum_btc_binance",
                language="python",
                files_created=["main.py", "bot-config.yaml"],
                strategy_type="momentum",
                platform="binance",
                status="ready_for_deploy",
                execution_verified=True
            )
            SessionStateManager.store_bot_metadata(session.state, metadata)
        """
        try:
            # Validate and serialize
            data = metadata.model_dump()

            # Log operation
            logger.info(
                f"Storing bot metadata: "
                f"bot_name='{metadata.bot_name}', "
                f"language={metadata.language}, "
                f"files={len(metadata.files_created)}, "
                f"status={metadata.status}, "
                f"execution_verified={metadata.execution_verified}, "
                f"backtest_verified={metadata.backtest_verified}"
            )

            # Store in state
            state[SessionStateManager.BOT_METADATA_KEY] = data

            logger.info("Successfully stored bot metadata")

        except Exception as e:
            logger.error(f"Error storing bot metadata: {e}", exc_info=True)
            raise ValueError(f"Failed to store metadata: {e}")

    @staticmethod
    def get_bot_metadata(state: dict) -> Optional[BotMetadata]:
        """
        Retrieve bot metadata from session state.

        Reads from session.state['bot_metadata'], validates the schema,
        and returns a BotMetadata object if found.

        Args:
            state: Session state dictionary (session.state)

        Returns:
            BotMetadata instance if found and valid, None otherwise

        Example:
            metadata = SessionStateManager.get_bot_metadata(session.state)
            if metadata:
                print(f"Bot: {metadata.bot_name}")
                print(f"Status: {metadata.status}")
                print(f"Files: {len(metadata.files_created)}")
            else:
                print("No bot metadata available")
        """
        data = state.get(SessionStateManager.BOT_METADATA_KEY)

        if not data:
            logger.info("No bot metadata found in session state")
            return None

        try:
            metadata = BotMetadata(**data)
            logger.info(
                f"Retrieved bot metadata: "
                f"bot_name='{metadata.bot_name}', "
                f"status={metadata.status}, "
                f"age={(datetime.utcnow() - metadata.timestamp).seconds}s"
            )
            return metadata
        except Exception as e:
            logger.error(f"Error parsing bot metadata: {e}", exc_info=True)
            return None

    @staticmethod
    def clear_state(state: dict) -> None:
        """
        Clear all MAS-related state (for new conversations).

        Removes both research_data and bot_metadata from session state.
        Useful when starting a new bot creation workflow.

        Args:
            state: Session state dictionary (session.state)

        Example:
            # Clear state at start of new conversation
            SessionStateManager.clear_state(session.state)
        """
        logger.info("Clearing MAS state (research_data, bot_metadata)")
        state.pop(SessionStateManager.RESEARCH_KEY, None)
        state.pop(SessionStateManager.BOT_METADATA_KEY, None)
        logger.info("Successfully cleared MAS state")

    @staticmethod
    def get_state_summary(state: dict) -> dict:
        """
        Get summary of current state for debugging.

        Returns a dict with information about what's currently stored
        in session state, useful for logging and debugging.

        Args:
            state: Session state dictionary (session.state)

        Returns:
            Dictionary with state summary information

        Example:
            summary = SessionStateManager.get_state_summary(session.state)
            print(f"Has research: {summary['has_research_data']}")
            print(f"Has bot: {summary['has_bot_metadata']}")
            print(f"Total size: {summary['state_size_bytes']} bytes")
        """
        has_research = SessionStateManager.RESEARCH_KEY in state
        has_bot = SessionStateManager.BOT_METADATA_KEY in state

        summary = {
            "has_research_data": has_research,
            "has_bot_metadata": has_bot,
            "state_keys": list(state.keys()),
            "state_size_bytes": len(str(state)),
        }

        if has_research:
            research = SessionStateManager.get_research(state)
            if research:
                summary["research_findings_count"] = len(research.findings)
                summary["research_timestamp"] = research.timestamp.isoformat()
                query_preview = research.query[:50] + "..." if len(research.query) > 50 else research.query
                summary["research_query"] = query_preview

        if has_bot:
            metadata = SessionStateManager.get_bot_metadata(state)
            if metadata:
                summary["bot_name"] = metadata.bot_name
                summary["bot_status"] = metadata.status
                summary["bot_files_count"] = len(metadata.files_created)
                summary["bot_execution_verified"] = metadata.execution_verified
                summary["bot_backtest_verified"] = metadata.backtest_verified

        logger.debug(f"State summary: {summary}")
        return summary
