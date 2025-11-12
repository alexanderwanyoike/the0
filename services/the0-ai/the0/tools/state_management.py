"""
State Management Tools for Multi-Agent System.

Provides tools for agents to store and retrieve structured data from session state.
Uses SessionStateManager for type-safe operations with Pydantic validation.
"""

from typing import Optional, List, Dict, Any
from google.adk.tools.tool_context import ToolContext
from the0.agents.state_schema import (
    SessionStateManager,
    ResearchData,
    ResearchFinding,
    BotMetadata,
)


async def store_research_data(
    query: str,
    summary: str,
    findings: List[Dict[str, str]],
    recommendations: List[str],
    sources: List[Dict[str, str]],
    researcher_notes: Optional[str] = None,
    tool_context: ToolContext = None,
) -> Dict[str, Any]:
    """
    Store research findings in session state for use by other agents.

    This tool allows the Researcher agent to persist research findings in a structured
    format that can be accessed by the Developer agent and Supervisor.

    Args:
        query: The original research request/question
        summary: Executive summary of findings (2-3 sentences)
        findings: List of key findings, each with:
            - point: The finding description
            - source: Source URL
            - confidence: "high" | "medium" | "low"
        recommendations: List of actionable recommendations
        sources: List of all sources used, each with:
            - title: Source title
            - url: Source URL
            - relevance: Why this source matters (optional)
            - published: Publication date if available (optional)
        researcher_notes: Optional additional context or caveats
        tool_context: ADK tool context (automatically provided)

    Returns:
        Dictionary with storage status and metadata

    Example:
        findings = [
            {
                "point": "Binance supports WebSocket for real-time data",
                "source": "https://binance-docs.github.io/",
                "confidence": "high"
            }
        ]
        sources = [
            {
                "title": "Binance API Docs",
                "url": "https://binance-docs.github.io/",
                "relevance": "Official API documentation",
                "published": "2025-01-15"
            }
        ]
        result = await store_research_data(
            query="Research Binance API for trading bot",
            summary="Binance provides REST and WebSocket APIs...",
            findings=findings,
            recommendations=["Use WebSocket for real-time data"],
            sources=sources
        )
    """
    if tool_context is None:
        return {"status": "error", "message": "No tool context provided"}

    try:
        # Convert findings dicts to ResearchFinding objects
        finding_objects = [
            ResearchFinding(
                point=f["point"],
                source=f["source"],
                confidence=f.get("confidence", "medium"),
            )
            for f in findings
        ]

        # Create ResearchData object (validates with Pydantic)
        research = ResearchData(
            query=query,
            summary=summary,
            findings=finding_objects,
            recommendations=recommendations,
            sources=sources,
            researcher_notes=researcher_notes,
        )

        # Store in session state
        session_state = tool_context._invocation_context.session.state
        SessionStateManager.store_research(session_state, research)

        return {
            "status": "success",
            "message": "Research data stored successfully",
            "findings_count": len(finding_objects),
            "recommendations_count": len(recommendations),
            "sources_count": len(sources),
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to store research data: {str(e)}",
        }


async def get_research_data(
    tool_context: ToolContext = None,
) -> Dict[str, Any]:
    """
    Retrieve research findings from session state.

    This tool allows agents (typically Developer) to access research findings
    stored by the Researcher agent.

    Args:
        tool_context: ADK tool context (automatically provided)

    Returns:
        Dictionary containing research data or None if not found

    Example:
        result = await get_research_data()
        if result["status"] == "success":
            research = result["data"]
            print(research["summary"])
            for finding in research["findings"]:
                print(f"- {finding['point']} ({finding['confidence']})")
    """
    if tool_context is None:
        return {"status": "error", "message": "No tool context provided"}

    try:
        session_state = tool_context._invocation_context.session.state
        research = SessionStateManager.get_research(session_state)

        if research is None:
            return {
                "status": "not_found",
                "message": "No research data found in session state",
                "data": None,
            }

        # Convert to dict for LLM consumption
        return {
            "status": "success",
            "message": "Research data retrieved successfully",
            "data": {
                "query": research.query,
                "summary": research.summary,
                "findings": [
                    {
                        "point": f.point,
                        "source": f.source,
                        "confidence": f.confidence,
                    }
                    for f in research.findings
                ],
                "recommendations": research.recommendations,
                "sources": research.sources,
                "researcher_notes": research.researcher_notes,
            },
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to retrieve research data: {str(e)}",
        }


async def store_bot_metadata(
    bot_name: str,
    language: str,
    files_created: List[str],
    strategy_type: str,
    platform: str,
    status: str,
    execution_verified: bool = False,
    backtest_verified: bool = False,
    libraries_used: Optional[List[str]] = None,
    test_results: Optional[Dict[str, Any]] = None,
    developer_notes: Optional[str] = None,
    tool_context: ToolContext = None,
) -> Dict[str, Any]:
    """
    Store bot metadata in session state after bot creation.

    This tool allows the Developer agent to persist bot creation results
    for the Supervisor and future reference.

    Args:
        bot_name: Name of the bot (e.g., "momentum_btc_binance")
        language: Programming language ("python" or "javascript")
        files_created: List of all files created
        strategy_type: Trading strategy type (e.g., "momentum", "DCA")
        platform: Trading platform (e.g., "binance", "alpaca")
        status: Bot status (e.g., "ready_for_deploy", "needs_fixes")
        execution_verified: Whether bot execution was verified
        backtest_verified: Whether backtest execution was verified
        libraries_used: List of libraries with versions
        test_results: Dictionary of test execution results
        developer_notes: Optional notes about implementation
        tool_context: ADK tool context (automatically provided)

    Returns:
        Dictionary with storage status

    Example:
        result = await store_bot_metadata(
            bot_name="momentum_btc_binance",
            language="python",
            files_created=["main.py", "bot-config.yaml", "requirements.txt"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
            libraries_used=["ccxt==4.1.0", "pandas-ta==0.3.14b0"],
            test_results={
                "bot_execution": "success",
                "backtest_execution": "success",
                "backtest_trades": 42,
                "backtest_pnl": -2.3
            }
        )
    """
    if tool_context is None:
        return {"status": "error", "message": "No tool context provided"}

    try:
        # Create BotMetadata object (validates with Pydantic)
        metadata = BotMetadata(
            bot_name=bot_name,
            language=language,
            files_created=files_created,
            strategy_type=strategy_type,
            platform=platform,
            status=status,
            execution_verified=execution_verified,
            backtest_verified=backtest_verified,
            libraries_used=libraries_used or [],
            test_results=test_results or {},
            developer_notes=developer_notes,
        )

        # Store in session state
        session_state = tool_context._invocation_context.session.state
        SessionStateManager.store_bot_metadata(session_state, metadata)

        return {
            "status": "success",
            "message": f"Bot metadata stored for '{bot_name}'",
            "bot_name": bot_name,
            "execution_verified": execution_verified,
            "backtest_verified": backtest_verified,
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to store bot metadata: {str(e)}",
        }


async def get_bot_metadata(
    tool_context: ToolContext = None,
) -> Dict[str, Any]:
    """
    Retrieve bot metadata from session state.

    This tool allows agents to access bot creation results stored by
    the Developer agent.

    Args:
        tool_context: ADK tool context (automatically provided)

    Returns:
        Dictionary containing bot metadata or None if not found
    """
    if tool_context is None:
        return {"status": "error", "message": "No tool context provided"}

    try:
        session_state = tool_context._invocation_context.session.state
        metadata = SessionStateManager.get_bot_metadata(session_state)

        if metadata is None:
            return {
                "status": "not_found",
                "message": "No bot metadata found in session state",
                "data": None,
            }

        return {
            "status": "success",
            "message": "Bot metadata retrieved successfully",
            "data": {
                "bot_name": metadata.bot_name,
                "language": metadata.language,
                "files_created": metadata.files_created,
                "strategy_type": metadata.strategy_type,
                "platform": metadata.platform,
                "status": metadata.status,
                "execution_verified": metadata.execution_verified,
                "backtest_verified": metadata.backtest_verified,
                "libraries_used": metadata.libraries_used,
                "test_results": metadata.test_results,
                "developer_notes": metadata.developer_notes,
            },
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to retrieve bot metadata: {str(e)}",
        }


async def get_state_summary(
    tool_context: ToolContext = None,
) -> Dict[str, Any]:
    """
    Get a summary of all data stored in session state.

    Useful for debugging and understanding what information is available
    across agents.

    Args:
        tool_context: ADK tool context (automatically provided)

    Returns:
        Dictionary with state summary
    """
    if tool_context is None:
        return {"status": "error", "message": "No tool context provided"}

    try:
        session_state = tool_context._invocation_context.session.state
        summary = SessionStateManager.get_state_summary(session_state)

        return {
            "status": "success",
            "message": "State summary retrieved",
            "summary": summary,
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Failed to get state summary: {str(e)}",
        }
