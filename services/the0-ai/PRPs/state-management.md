# PRP: Agent State Management Infrastructure

**Story**: Story 5 - Agent Communication & State Management
**Epic**: MAS Orchestration
**Estimated Effort**: 6-8 hours
**Dependencies**: Story 2 (Researcher Agent), Story 3 (Developer Agent)

---

## Goal

Implement robust agent-to-agent communication infrastructure using shared session state with Pydantic schemas, validation, utilities, and comprehensive testing. This enables Researcher and Developer agents to share structured data seamlessly through Google ADK's session state mechanism.

**End State:**
- Pydantic schemas defined for `research_data` and `bot_metadata` state keys
- `SessionStateManager` utility class for type-safe state operations
- State validation, logging, and error handling
- Cross-agent state sharing working reliably
- Comprehensive unit and integration tests (>80% coverage)
- Documentation complete

---

## Why

- **Reliable Communication**: Structured data prevents errors and misunderstandings between agents
- **Type Safety**: Pydantic validation catches schema violations before they corrupt state
- **Debugging**: Logging all state operations aids in troubleshooting agent workflows
- **Maintainability**: Clear schemas and utilities make the codebase easier to understand and extend
- **Evolution**: Schema versioning supports future changes without breaking existing workflows
- **Quality**: Validation gates ensure data integrity throughout agent interactions

---

## What

**User-Visible Behavior:**
- Agents communicate seamlessly - Researcher findings flow to Developer automatically
- State corruption prevented through validation
- Clear error messages when state operations fail

**Technical Requirements:**
- Pydantic models: `ResearchFinding`, `ResearchData`, `BotMetadata`
- Utility class: `SessionStateManager` with CRUD operations
- State validation using Pydantic's validation engine
- Comprehensive logging for all state operations
- Unit tests achieving >80% coverage
- Integration tests for cross-agent workflows

### Success Criteria

- [x] `state_schema.py` created with all Pydantic models
- [x] `ResearchData` schema with `findings`, `recommendations`, `sources`
- [x] `BotMetadata` schema with `files_created`, `test_results`, `execution_verified`
- [x] `SessionStateManager` class with store/retrieve methods
- [x] State validation prevents corrupt data
- [x] Logging captures all operations (store, retrieve, validation errors)
- [x] Cross-agent state sharing tested end-to-end
- [x] Schema versioning supports evolution
- [x] Unit tests pass with >80% coverage
- [x] Integration tests pass
- [x] Code formatted (`make format`)
- [x] Linting passes (`make lint`)

---

## All Needed Context

### Documentation & References

```yaml
# MUST READ - Core ADK Documentation
- url: https://google.github.io/adk-docs/sessions/state/
  why: Session state API, access patterns, state_delta, scoping, injection
  key_points:
    - Access state via session.state['key']
    - Use state_delta in EventActions for updates
    - Prefixes: no prefix (session), user: (user), app: (app), temp: (temp)
    - Never modify session.state directly outside managed contexts
    - Use context.state in callbacks/tools for safe updates
    - Inject state in instructions with {key} or {key?}

- url: https://google.github.io/adk-docs/sessions/session/
  why: Session lifecycle and management

- url: https://docs.pydantic.dev/latest/
  why: Pydantic model patterns, validation, serialization
  key_points:
    - BaseModel for all schemas
    - Field() for descriptions and defaults
    - model_dump() for serialization
    - Validation happens automatically on instantiation

- url: https://docs.pytest.org/en/stable/how-to/asyncio.html
  why: Async testing patterns with pytest-asyncio

# CRITICAL - Project Files (Read These First!)
- file: CLAUDE.md
  why: Architecture overview, MAS design, state schemas, conventions
  sections:
    - "Multi-Agent System (MAS) Architecture"
    - "State Management" (research_data, bot_metadata schemas)
    - "Communication Patterns"
    - "Code Conventions & Best Practices"

- file: the0/agents/researcher.py
  why: Researcher agent structure, STATE_KEY_RESEARCH usage, output schema
  key_points:
    - Uses STATE_KEY_RESEARCH = "research_data"
    - Documents expected state schema in docstring
    - Instruction mentions state storage (not yet implemented)
    - Tools: tavily_search, browse_url, list_documentation, get_documentation

- file: the0/agents/developer.py
  why: Developer agent structure, STATE_KEY_BOT_METADATA usage, input/output schemas
  key_points:
    - Uses STATE_KEY_BOT_METADATA = "bot_metadata"
    - Documents expected state schema in docstring
    - Reads research_data from state (not yet implemented)
    - Stores bot_metadata (not yet implemented)
    - Tools: save_artifact, deploy_bot, execute_command, read_file, list_directory

- file: the0/agents/base.py
  why: Shared agent constants and utilities
  key_points:
    - STATE_KEY_RESEARCH = "research_data"
    - STATE_KEY_BOT_METADATA = "bot_metadata"
    - DEFAULT_MODEL = "gemini-2.5-flash"
    - format_citations() utility function

- file: the0/agent.py
  why: Supervisor agent orchestration patterns
  key_points:
    - Mentions reading research_data from session.state
    - Mentions reading bot_metadata from session.state
    - Shows expected workflow and data flow

- file: api/agent_service.py
  why: AgentService integration with ADK Runner
  key_points:
    - Uses DatabaseSessionService (persists state)
    - Fallback to InMemorySessionService
    - Session management with ADK Runner
    - Lazy initialization pattern

- file: tests/the0/agents/test_researcher.py
  why: Testing patterns for agents
  key_points:
    - Test agent configuration (name, model, description, instruction)
    - Test tool assignment
    - Test constant references
    - Test instruction completeness

- file: tests/the0/agents/test_developer.py
  why: Testing patterns for agents (similar to researcher)

# Story File (Requirements Source)
- file: stories/story-5-state-management.md
  why: Complete requirements, acceptance criteria, implementation details
```

### Current Codebase Structure

```
the0-ai/
├── api/                      # FastAPI application layer
│   ├── main.py              # REST endpoints
│   ├── agent_service.py     # Core agent business logic
│   ├── schemas.py           # Pydantic request/response models
│   ├── database.py          # Database connection management
│   ├── repositories.py      # Data access layer
│   ├── storage.py           # MinIO storage service
│   ├── config.py            # Configuration management
│   └── models/
│       └── database.py      # SQLAlchemy ORM models
├── the0/                    # Google ADK agent implementation
│   ├── agent.py            # Supervisor agent definition
│   ├── agents/             # Specialized agents (MAS)
│   │   ├── __init__.py     # Agent exports
│   │   ├── base.py         # Shared utilities (STATE_KEY_* constants)
│   │   ├── researcher.py   # Research specialist agent
│   │   └── developer.py    # Development specialist agent
│   └── tools/              # Custom agent tools
│       ├── save_artifact.py
│       ├── web_browser.py
│       ├── documentation.py
│       ├── deploy_bot.py
│       ├── execute_command.py
│       ├── read_file.py
│       └── filesystem.py
├── tests/                   # Pytest test suite
│   ├── api/
│   │   ├── test_main.py
│   │   ├── test_repositories.py
│   │   ├── test_sessions.py
│   │   └── test_artifacts.py
│   └── the0/
│       ├── agents/
│       │   ├── test_researcher.py
│       │   └── test_developer.py
│       └── tools/
│           ├── test_save_artifact.py
│           ├── test_web_browser.py
│           └── test_execute_command.py
├── alembic/                # Database migrations
├── requirements.txt        # Python dependencies
├── Makefile               # Development commands
└── pytest.ini             # Pytest configuration
```

### Desired Codebase Structure (After Implementation)

```
the0-ai/
├── the0/
│   ├── agents/
│   │   ├── __init__.py         # Export state_schema
│   │   ├── base.py             # STATE_KEY_* constants (no changes)
│   │   ├── researcher.py       # Update to use SessionStateManager
│   │   ├── developer.py        # Update to use SessionStateManager
│   │   └── state_schema.py     # ⭐ NEW: Pydantic schemas + SessionStateManager
├── tests/
│   └── the0/
│       └── agents/
│           └── test_state_management.py  # ⭐ NEW: State management tests
```

**File Responsibilities:**

1. **`the0/agents/state_schema.py`** (NEW - ~430 lines):
   - `ResearchFinding` Pydantic model (single finding with source/confidence)
   - `ResearchData` Pydantic model (complete research output)
   - `BotMetadata` Pydantic model (bot creation metadata)
   - `SessionStateManager` class (utility methods for state CRUD)
   - Logging for all state operations
   - Version fields for schema evolution

2. **`tests/the0/agents/test_state_management.py`** (NEW - ~200 lines):
   - Unit tests for Pydantic model validation
   - Tests for SessionStateManager methods
   - Cross-agent state sharing integration tests
   - Error handling and validation tests
   - Schema versioning tests

3. **`the0/agents/__init__.py`** (UPDATED):
   - Export state_schema module

4. **`the0/agents/researcher.py`** (UPDATED - minor):
   - Import SessionStateManager
   - Update instruction to reference usage
   - No functional changes yet (Story 6 will integrate)

5. **`the0/agents/developer.py`** (UPDATED - minor):
   - Import SessionStateManager
   - Update instruction to reference usage
   - No functional changes yet (Story 6 will integrate)

---

## Known Gotchas & Library Quirks

```yaml
google_adk_state:
  - "CRITICAL: Never modify session.state directly - use state_delta or context.state"
  - "State keys are always strings, values must be JSON-serializable"
  - "Datetime objects must be serialized to ISO 8601 strings"
  - "Use session.state.get('key', default) for safe retrieval"
  - "State updates via state_delta are atomic and properly persisted"
  - "Pydantic model_dump() returns dict suitable for state storage"
  - "Session state persists only with DatabaseSessionService, not InMemorySessionService"

pydantic:
  - "CRITICAL: Use BaseModel for all schemas"
  - "Field() for descriptions, defaults, and validation rules"
  - "model_dump() serializes to dict (for JSON/state storage)"
  - "model_dump_json() serializes to JSON string"
  - "Validation happens automatically on __init__"
  - "datetime fields serialize to ISO 8601 strings by default"
  - "Use default_factory=list for mutable defaults (never default=[])"
  - "Use default_factory=datetime.utcnow for timestamps"
  - "Optional[T] for nullable fields"
  - "Config.json_schema_extra for examples in docs"

python:
  - "CRITICAL: Use logging.getLogger(__name__) for module-level logger"
  - "Use type hints everywhere"
  - "Use @staticmethod for utility methods that don't need self"
  - "Docstrings with Args, Returns, Raises sections"

testing:
  - "CRITICAL: pytest-asyncio for async tests"
  - "Use pytest.mark.asyncio for async test functions"
  - "Mock session state as plain dict: state = {}"
  - "Test both success and error paths"
  - "Test validation with invalid data"
  - "Achieve >80% coverage"

the0_conventions:
  - "Follow existing patterns in researcher.py and developer.py"
  - "State keys defined as constants in base.py"
  - "Use descriptive variable names"
  - "Comprehensive logging for debugging"
  - "Error messages should be actionable"
```

---

## Implementation Blueprint

### Overview

This implementation creates the state management foundation for the MAS. The key insight is that ADK's `session.state` is a simple dict-like object, and we're adding:
1. **Structure** via Pydantic schemas
2. **Validation** via Pydantic's validation engine
3. **Utilities** via SessionStateManager class
4. **Logging** for observability
5. **Testing** for reliability

### Data Models and Structure

```python
# the0/agents/state_schema.py

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
    """
    point: str = Field(
        ...,
        description="Key finding or insight",
        min_length=1
    )
    source: str = Field(
        ...,
        description="Source URL where information was found",
        min_length=1
    )
    confidence: str = Field(
        ...,
        description="Confidence level: high, medium, or low",
        pattern="^(high|medium|low)$"
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this finding was recorded"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "point": "Binance API supports WebSocket for real-time data",
                "source": "https://binance-docs.github.io/apidocs/spot/en/",
                "confidence": "high",
                "timestamp": "2025-11-12T15:00:00Z"
            }
        }


class ResearchData(BaseModel):
    """
    Structured research data stored in session state.

    Complete research output from Researcher agent, including findings,
    recommendations, and sources. Stored in session.state['research_data'].
    """
    version: str = Field(
        default="1.0",
        description="Schema version for evolution support"
    )
    query: str = Field(
        ...,
        description="Original research request from user or Supervisor",
        min_length=1
    )
    summary: str = Field(
        ...,
        description="Executive summary of findings (2-3 sentences)",
        min_length=1
    )
    findings: List[ResearchFinding] = Field(
        default_factory=list,
        description="List of key findings with sources and confidence"
    )
    recommendations: List[str] = Field(
        default_factory=list,
        description="Actionable recommendations based on research"
    )
    sources: List[Dict[str, str]] = Field(
        default_factory=list,
        description="All sources referenced (title, url, relevance, published)"
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When research was completed"
    )
    researcher_notes: Optional[str] = Field(
        default=None,
        description="Additional notes or context from researcher"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "version": "1.0",
                "query": "Research Binance Spot API for real-time data",
                "summary": "Binance Spot API v3 provides comprehensive real-time market data",
                "findings": [
                    {
                        "point": "WebSocket streams available for real-time data",
                        "source": "https://binance-docs.github.io/",
                        "confidence": "high",
                        "timestamp": "2025-11-12T15:00:00Z"
                    }
                ],
                "recommendations": [
                    "Use WebSocket for real-time data to avoid rate limits",
                    "Implement python-binance library for easier integration"
                ],
                "sources": [
                    {
                        "title": "Binance API Docs",
                        "url": "https://binance-docs.github.io/",
                        "relevance": "Official API documentation",
                        "published": "2025-01-15"
                    }
                ],
                "timestamp": "2025-11-12T15:00:00Z",
                "researcher_notes": "API is well-documented and stable"
            }
        }


class BotMetadata(BaseModel):
    """
    Bot metadata stored in session state.

    Complete bot creation metadata from Developer agent, including files,
    status, and test results. Stored in session.state['bot_metadata'].
    """
    version: str = Field(
        default="1.0",
        description="Schema version for evolution support"
    )
    bot_name: str = Field(
        ...,
        description="Bot name/identifier (e.g., momentum_btc_binance)",
        min_length=1
    )
    language: str = Field(
        ...,
        description="Programming language (python or javascript)",
        pattern="^(python|javascript)$"
    )
    files_created: List[str] = Field(
        default_factory=list,
        description="List of files created for the bot"
    )
    strategy_type: str = Field(
        ...,
        description="Strategy type (e.g., momentum, arbitrage, DCA)",
        min_length=1
    )
    platform: str = Field(
        ...,
        description="Trading platform (e.g., binance, alpaca)",
        min_length=1
    )
    status: str = Field(
        ...,
        description="Development status (e.g., ready_for_deploy, in_progress)",
        min_length=1
    )
    execution_verified: bool = Field(
        default=False,
        description="Whether bot execution was tested and passed"
    )
    backtest_verified: bool = Field(
        default=False,
        description="Whether backtest execution was tested and passed"
    )
    libraries_used: List[str] = Field(
        default_factory=list,
        description="Dependencies with versions (e.g., ccxt==4.1.0)"
    )
    test_results: Dict = Field(
        default_factory=dict,
        description="Execution test results (bot_execution, backtest_execution, etc.)"
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When bot was created"
    )
    developer_notes: Optional[str] = Field(
        default=None,
        description="Additional notes from developer"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "version": "1.0",
                "bot_name": "momentum_btc_binance",
                "language": "python",
                "files_created": [
                    "main.py", "bot-config.yaml", "requirements.txt",
                    "bot-schema.json", "README.md", "backtest.py",
                    "backtest-schema.json"
                ],
                "strategy_type": "momentum",
                "platform": "binance",
                "status": "ready_for_deploy",
                "execution_verified": True,
                "backtest_verified": True,
                "libraries_used": [
                    "ccxt==4.1.0",
                    "pandas-ta==0.3.14b0",
                    "pandas==2.1.4"
                ],
                "test_results": {
                    "bot_execution": "success",
                    "backtest_execution": "success",
                    "backtest_trades": 42,
                    "backtest_pnl": -2.3,
                    "used_mock_data": False
                },
                "timestamp": "2025-11-12T16:00:00Z",
                "developer_notes": "Bot tested with paper trading credentials"
            }
        }


class SessionStateManager:
    """
    Utilities for managing session state across agents.

    Provides type-safe methods for storing and retrieving structured data
    in ADK session state. Handles validation, serialization, and logging.

    Usage:
        # Store research data
        research = ResearchData(query="...", summary="...", findings=[])
        SessionStateManager.store_research(session.state, research)

        # Retrieve research data
        research = SessionStateManager.get_research(session.state)
        if research:
            print(research.summary)
    """

    # State keys - imported from base.py
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

            logger.info(f"Successfully stored research data")

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

            logger.info(f"Successfully stored bot metadata")

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
            "state_size_bytes": len(str(state))
        }

        if has_research:
            research = SessionStateManager.get_research(state)
            if research:
                summary["research_findings_count"] = len(research.findings)
                summary["research_timestamp"] = research.timestamp.isoformat()
                summary["research_query"] = research.query[:50] + "..." if len(research.query) > 50 else research.query

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
```

---

### List of Tasks (In Order)

```yaml
Task 1: Create State Schema Module
  file: the0/agents/state_schema.py
  actions:
    - CREATE new file
    - IMPORT: typing (List, Dict, Optional), pydantic (BaseModel, Field), datetime, logging
    - DEFINE ResearchFinding(BaseModel) with point, source, confidence, timestamp
    - DEFINE ResearchData(BaseModel) with version, query, summary, findings, recommendations, sources, timestamp, researcher_notes
    - DEFINE BotMetadata(BaseModel) with version, bot_name, language, files_created, strategy_type, platform, status, execution_verified, backtest_verified, libraries_used, test_results, timestamp, developer_notes
    - ADD Config.json_schema_extra examples to all models
    - CREATE SessionStateManager class with RESEARCH_KEY and BOT_METADATA_KEY constants
    - IMPLEMENT store_research(state, research) method with validation and logging
    - IMPLEMENT get_research(state) method with parsing and error handling
    - IMPLEMENT store_bot_metadata(state, metadata) method with validation and logging
    - IMPLEMENT get_bot_metadata(state) method with parsing and error handling
    - IMPLEMENT clear_state(state) method
    - IMPLEMENT get_state_summary(state) method for debugging
  validation:
    - Python syntax valid: python -m py_compile the0/agents/state_schema.py
    - Imports resolve correctly
    - All classes have docstrings
    - All methods have Args, Returns, Raises sections

Task 2: Update Agents Package Exports
  file: the0/agents/__init__.py
  actions:
    - READ current file
    - ADD: from the0.agents.state_schema import (ResearchData, ResearchFinding, BotMetadata, SessionStateManager)
    - VERIFY __all__ includes new exports
  validation:
    - Import works: python -c "from the0.agents import SessionStateManager; print('OK')"

Task 3: Update Agent Instructions (Documentation)
  files: the0/agents/researcher.py, the0/agents/developer.py
  actions:
    - UPDATE docstrings to mention SessionStateManager
    - ADD import statement: from the0.agents.state_schema import SessionStateManager, ResearchData
    - NO functional changes (actual usage in Story 6)
  validation:
    - Agents still pass existing tests: pytest tests/the0/agents/test_researcher.py tests/the0/agents/test_developer.py

Task 4: Create Unit Tests for State Schemas
  file: tests/the0/agents/test_state_management.py
  actions:
    - CREATE new test file
    - IMPORT pytest, datetime, state_schema classes
    - TEST ResearchFinding validation (valid data, invalid confidence)
    - TEST ResearchData validation (required fields, defaults, timestamps)
    - TEST BotMetadata validation (required fields, defaults, booleans)
    - TEST model serialization with model_dump()
    - TEST model examples from Config.json_schema_extra
  validation:
    - Tests run: pytest tests/the0/agents/test_state_management.py -v
    - All validation tests pass

Task 5: Create Unit Tests for SessionStateManager
  file: tests/the0/agents/test_state_management.py (continued)
  actions:
    - TEST store_research() with valid data
    - TEST get_research() with existing data
    - TEST get_research() returns None when no data
    - TEST store_bot_metadata() with valid data
    - TEST get_bot_metadata() with existing data
    - TEST get_bot_metadata() returns None when no data
    - TEST clear_state() removes both keys
    - TEST get_state_summary() returns correct info
    - TEST error handling for invalid data
    - TEST logging output (with caplog fixture)
  validation:
    - Tests run: pytest tests/the0/agents/test_state_management.py::TestSessionStateManager -v
    - All manager tests pass

Task 6: Create Integration Tests (Cross-Agent)
  file: tests/the0/agents/test_state_management.py (continued)
  actions:
    - TEST cross-agent workflow: Researcher writes → Developer reads
    - TEST state persistence across multiple operations
    - TEST state isolation between sessions (use different state dicts)
    - TEST schema versioning (store v1.0, verify retrieval)
    - MOCK session.state as plain dict
  validation:
    - Integration tests run: pytest tests/the0/agents/test_state_management.py::test_cross_agent -v
    - All integration tests pass

Task 7: Run Full Test Suite
  actions:
    - RUN: pytest tests/the0/agents/test_state_management.py -v --cov=the0/agents/state_schema --cov-report=term
    - VERIFY: >80% code coverage
    - FIX any failing tests
  validation:
    - Coverage >80%
    - All tests pass

Task 8: Code Quality Checks
  actions:
    - RUN: make format (or black the0/ tests/)
    - RUN: make lint (or flake8 the0/ tests/)
    - FIX any linting issues
  validation:
    - Formatting applied
    - No linting errors

Task 9: Update Documentation
  file: CLAUDE.md
  actions:
    - UPDATE "State Management" section with SessionStateManager usage
    - ADD code examples for storing/retrieving state
    - DOCUMENT schema versioning approach
  validation:
    - CLAUDE.md is readable and accurate

Task 10: Final Validation
  actions:
    - RUN: pytest (all tests)
    - RUN: make format
    - RUN: make lint
    - VERIFY: All acceptance criteria met
  validation:
    - All tests pass
    - Code formatted
    - No linting errors
```

---

## Validation Loop

### Level 1: Syntax & Type Checking

```bash
# Python syntax validation
python -m py_compile the0/agents/state_schema.py

# Import validation
python -c "from the0.agents.state_schema import SessionStateManager; print('OK')"
python -c "from the0.agents import SessionStateManager, ResearchData, BotMetadata; print('OK')"

# Type checking (if mypy is configured)
mypy the0/agents/state_schema.py
```

### Level 2: Unit Tests

```bash
# Run state management tests
pytest tests/the0/agents/test_state_management.py -v

# Run with coverage
pytest tests/the0/agents/test_state_management.py -v \
  --cov=the0/agents/state_schema \
  --cov-report=term \
  --cov-report=html

# Verify >80% coverage
# Expected output: Coverage: 80-95% for state_schema.py

# Run specific test classes
pytest tests/the0/agents/test_state_management.py::TestResearchData -v
pytest tests/the0/agents/test_state_management.py::TestBotMetadata -v
pytest tests/the0/agents/test_state_management.py::TestSessionStateManager -v
```

### Level 3: Integration Tests

```bash
# Run cross-agent tests
pytest tests/the0/agents/test_state_management.py::test_cross_agent -v

# Run all agent tests (ensure no regressions)
pytest tests/the0/agents/ -v
```

### Level 4: Code Quality

```bash
# Format code
make format
# OR: black the0/ tests/

# Lint code
make lint
# OR: flake8 the0/ tests/

# Should have ZERO errors
```

### Level 5: Full Test Suite

```bash
# Run ALL tests to ensure no regressions
pytest

# Should see all tests passing, including:
# - tests/the0/agents/test_state_management.py (NEW)
# - tests/the0/agents/test_researcher.py (existing)
# - tests/the0/agents/test_developer.py (existing)
# - All other existing tests
```

---

## Test Pseudocode

```python
# tests/the0/agents/test_state_management.py

import pytest
from datetime import datetime
from the0.agents.state_schema import (
    ResearchData,
    ResearchFinding,
    BotMetadata,
    SessionStateManager
)


class TestResearchFinding:
    """Test ResearchFinding Pydantic model."""

    def test_valid_finding(self):
        """Test creating valid ResearchFinding."""
        finding = ResearchFinding(
            point="Test finding",
            source="https://example.com",
            confidence="high"
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
                confidence="invalid"  # Should be high/medium/low
            )

    def test_finding_serialization(self):
        """Test finding serializes to dict correctly."""
        finding = ResearchFinding(
            point="Test",
            source="https://example.com",
            confidence="high"
        )
        data = finding.model_dump()
        assert data["point"] == "Test"
        assert "timestamp" in data


class TestResearchData:
    """Test ResearchData Pydantic model."""

    def test_valid_research_data(self):
        """Test creating valid ResearchData."""
        data = ResearchData(
            query="Test query",
            summary="Test summary",
            findings=[],
            recommendations=["rec1", "rec2"],
            sources=[]
        )
        assert data.query == "Test query"
        assert data.version == "1.0"
        assert len(data.recommendations) == 2
        assert isinstance(data.timestamp, datetime)

    def test_research_data_with_findings(self):
        """Test ResearchData with nested findings."""
        finding = ResearchFinding(
            point="Test finding",
            source="https://example.com",
            confidence="high"
        )
        data = ResearchData(
            query="Test",
            summary="Summary",
            findings=[finding],
            recommendations=[],
            sources=[]
        )
        assert len(data.findings) == 1
        assert data.findings[0].point == "Test finding"

    def test_research_data_serialization(self):
        """Test research data serializes correctly."""
        data = ResearchData(
            query="Test",
            summary="Summary",
            findings=[],
            recommendations=["rec1"],
            sources=[{"title": "Test", "url": "https://example.com"}]
        )
        serialized = data.model_dump()
        assert serialized["query"] == "Test"
        assert serialized["version"] == "1.0"
        assert len(serialized["sources"]) == 1


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
            status="ready_for_deploy"
        )
        assert metadata.bot_name == "test_bot"
        assert metadata.language == "python"
        assert metadata.version == "1.0"
        assert metadata.execution_verified is False  # default

    def test_invalid_language(self):
        """Test that invalid language raises ValidationError."""
        with pytest.raises(ValueError):
            BotMetadata(
                bot_name="test",
                language="java",  # Should be python or javascript
                files_created=[],
                strategy_type="momentum",
                platform="binance",
                status="in_progress"
            )

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
                "backtest_pnl": -2.3
            }
        )
        assert metadata.execution_verified is True
        assert metadata.test_results["backtest_trades"] == 42


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
            sources=[]
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
            status="ready_for_deploy"
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
            "other_key": "should_remain"
        }

        SessionStateManager.clear_state(state)

        assert SessionStateManager.RESEARCH_KEY not in state
        assert SessionStateManager.BOT_METADATA_KEY not in state
        assert "other_key" in state  # Other keys preserved

    def test_get_state_summary(self):
        """Test getting state summary for debugging."""
        state = {}

        # Empty state
        summary = SessionStateManager.get_state_summary(state)
        assert summary["has_research_data"] is False
        assert summary["has_bot_metadata"] is False

        # With research data
        research = ResearchData(
            query="Test query",
            summary="Summary",
            findings=[],
            recommendations=[],
            sources=[]
        )
        SessionStateManager.store_research(state, research)

        summary = SessionStateManager.get_state_summary(state)
        assert summary["has_research_data"] is True
        assert summary["research_findings_count"] == 0
        assert "research_timestamp" in summary

    def test_store_research_with_invalid_data_raises_error(self):
        """Test that storing invalid research raises ValueError."""
        state = {}

        # Create invalid object (this would fail Pydantic validation)
        # We can't create invalid ResearchData directly, so test TypeError
        with pytest.raises(TypeError):
            SessionStateManager.store_research(state, "not a ResearchData object")

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
            sources=[]
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
                confidence="high"
            ),
            ResearchFinding(
                point="python-binance library recommended",
                source="https://pypi.org/project/python-binance/",
                confidence="high"
            )
        ],
        recommendations=[
            "Use WebSocket for real-time data",
            "Implement python-binance library"
        ],
        sources=[
            {
                "title": "Binance API Docs",
                "url": "https://binance-docs.github.io/",
                "relevance": "Official API documentation"
            }
        ]
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
        libraries_used=["python-binance==1.0.19", "pandas-ta==0.3.14b0"]
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
        sources=[]
    )
    SessionStateManager.store_research(state, research_v1)

    # Retrieve and verify version
    retrieved = SessionStateManager.get_research(state)
    assert retrieved is not None
    assert retrieved.version == "1.0"

    # Future: When v2.0 is released, migration logic would go here
    # For now, we just verify that version field exists and is preserved
```

---

## Final Validation Checklist

- [ ] **Syntax**: Python syntax valid (`python -m py_compile`)
- [ ] **Imports**: All imports resolve correctly
- [ ] **Schemas**: ResearchData, BotMetadata, ResearchFinding defined
- [ ] **Manager**: SessionStateManager with all methods implemented
- [ ] **Validation**: Pydantic validation working (test invalid data)
- [ ] **Logging**: All operations logged (test with caplog)
- [ ] **Tests**: Unit tests for all models and manager methods
- [ ] **Integration**: Cross-agent test passes
- [ ] **Coverage**: >80% code coverage achieved
- [ ] **Formatting**: Code formatted with Black (`make format`)
- [ ] **Linting**: No flake8 errors (`make lint`)
- [ ] **Documentation**: Docstrings complete, CLAUDE.md updated
- [ ] **Exports**: state_schema exported from `the0/agents/__init__.py`
- [ ] **Backward Compat**: Existing agent tests still pass

---

## Anti-Patterns to Avoid

- ❌ **Don't modify session.state directly in tools** - Use proper patterns
- ❌ **Don't use mutable defaults** - Use `default_factory=list` not `default=[]`
- ❌ **Don't skip validation** - Always validate through Pydantic
- ❌ **Don't ignore logging** - Log all state operations for debugging
- ❌ **Don't hardcode state keys** - Use constants from base.py
- ❌ **Don't skip error handling** - Catch and log all exceptions
- ❌ **Don't test only happy path** - Test error cases too
- ❌ **Don't forget datetime serialization** - Use ISO 8601 strings
- ❌ **Don't create complex nested structures** - Keep state flat and simple
- ❌ **Don't skip docstrings** - Document all classes and methods

---

## Technical Considerations

### State Size Limits

- Session state persists in PostgreSQL
- Keep total state <100KB for performance
- Monitor state sizes in production using `get_state_summary()`
- Implement truncation if needed (truncate findings to top N, etc.)

### JSON Serialization

- Pydantic handles serialization automatically via `model_dump()`
- Datetime fields serialize to ISO 8601 format
- All models must be JSON-compatible (no custom objects)
- Test serialization with `model_dump()` and `model_dump_json()`

### Schema Evolution

- Version field enables migration logic
- Backward-compatible changes preferred (add optional fields)
- For breaking changes: Write migration functions that detect version and transform
- Example: If v2.0 adds required field, migration reads v1.0, adds default, saves as v2.0

### Concurrency

- ADK handles session locking automatically
- Multiple agents won't corrupt state (ADK guarantees)
- Document thread-safety assumptions in comments
- Use state_delta for atomic updates

### Validation Performance

- Pydantic validation is fast but not free
- Avoid validating same data multiple times
- Consider caching if validation becomes bottleneck
- Profile with `pytest --profile` if needed

---

## Known Risks & Mitigations

### Low Risk: State Corruption

**Risk:** Invalid data written to state breaks agent communication
**Impact:** Agents can't read each other's data, workflow fails
**Mitigation:**
- Pydantic validation catches schema violations
- Try/except in all SessionStateManager methods
- Comprehensive logging for debugging
- Return None on parse errors (graceful degradation)

**Contingency:** Clear state and restart workflow

### Low Risk: State Size Exceeds Limits

**Risk:** State grows too large, performance degrades
**Impact:** Slow response times, database issues
**Mitigation:**
- Monitor state sizes with `get_state_summary()`
- Set reasonable limits (e.g., max 100 findings)
- Summarize when needed (top N findings)
- Log warnings when state exceeds thresholds

**Contingency:** Implement state truncation logic

### Low Risk: Schema Version Conflicts

**Risk:** Agent writes v2.0, another agent expects v1.0
**Impact:** Parse errors, missing fields
**Mitigation:**
- Version field in all schemas
- Migration functions for version upgrades
- Default to v1.0 for compatibility
- Log version mismatches as warnings

**Contingency:** Migration function transforms old schemas to new

---

## Success Metrics

- **Code Coverage**: >80% for state_schema.py
- **Test Pass Rate**: 100% (all tests must pass)
- **Linting**: Zero flake8 errors
- **Documentation**: All classes/methods have docstrings
- **Integration**: Cross-agent state sharing test passes
- **Performance**: State operations <10ms (validate with benchmarks if needed)

---

## PRP Confidence Score

**8.5 / 10** - High confidence for one-pass implementation

**Reasoning:**

**Strengths:**
- Clear, well-defined schemas with examples from story file
- Existing patterns in researcher.py and developer.py to follow
- Comprehensive test pseudocode with expected behaviors
- Simple, focused scope (schemas + utilities, no API changes)
- Strong documentation (ADK state docs, Pydantic docs, existing code)
- No external service dependencies (pure Python logic)
- Executable validation commands at each step

**Risks:**
- ADK session.state behavior might have edge cases not covered in docs (-0.5)
- Pydantic validation patterns might need adjustment based on actual usage (-0.5)
- Integration with actual agents happens in Story 6, not fully testable now (-0.5)

**Mitigation:**
- Comprehensive error handling in SessionStateManager
- Extensive unit tests cover edge cases
- Integration test simulates cross-agent workflow accurately

This PRP provides sufficient context, patterns, and validation steps for Claude Code to implement the feature successfully in one pass.

---

## Next Steps (After Implementation)

1. **Story 6**: Update Researcher and Developer agents to actually USE SessionStateManager
2. **Story 7**: Document state management patterns for future agents
3. **Monitoring**: Add metrics for state operations (size, frequency, errors)
4. **Optimization**: Profile state operations if performance issues arise

---

## PR Message Template

```markdown
# Feature: Agent State Management Infrastructure

Implement robust state management for Multi-Agent System (MAS) communication.

## Background

Enables Researcher and Developer agents to share structured data through session state using Pydantic schemas and a utility manager class. Foundation for reliable agent-to-agent communication.

## Changes Made

### Core Files

- **`the0/agents/state_schema.py`** (NEW): Pydantic schemas and SessionStateManager
  - `ResearchFinding`: Single finding with source and confidence
  - `ResearchData`: Complete research output (query, summary, findings, recommendations, sources)
  - `BotMetadata`: Bot creation metadata (name, files, status, test results)
  - `SessionStateManager`: Utility class with store/retrieve/clear methods

### Tests

- **`tests/the0/agents/test_state_management.py`** (NEW): Comprehensive test suite
  - Unit tests for all Pydantic models
  - Unit tests for SessionStateManager methods
  - Integration test for cross-agent state sharing
  - Schema versioning tests
  - Error handling and validation tests

### Updates

- **`the0/agents/__init__.py`**: Export state_schema classes
- **`the0/agents/researcher.py`**: Import SessionStateManager (documentation only)
- **`the0/agents/developer.py`**: Import SessionStateManager (documentation only)

## Validation Steps

### Code Quality

- [x] Python syntax validation passed
- [x] Code formatted with Black
- [x] Linting passed with flake8 (zero errors)
- [x] All imports resolve correctly

### Testing

- [x] Unit tests passing (20+ tests)
- [x] Integration tests passing (cross-agent workflow)
- [x] Code coverage >80% for state_schema.py
- [x] Existing agent tests still pass (no regressions)

## Test Results

```
tests/the0/agents/test_state_management.py::TestResearchFinding PASSED
tests/the0/agents/test_state_management.py::TestResearchData PASSED
tests/the0/agents/test_state_management.py::TestBotMetadata PASSED
tests/the0/agents/test_state_management.py::TestSessionStateManager PASSED
tests/the0/agents/test_state_management.py::test_cross_agent_state_sharing PASSED
tests/the0/agents/test_state_management.py::test_schema_versioning PASSED

Coverage: 85% for the0/agents/state_schema.py
```

## Known Issues

None

## Follow-up Tasks

- Story 6: Update Researcher and Developer to USE SessionStateManager
- Story 7: Document state management patterns in developer guide

## Notes

- Actual state usage integrated in Story 6 (this PR is foundation only)
- Schemas match CLAUDE.md specifications exactly
- Logging added for all state operations (debugging support)
```

Save as: `PR_MSG/state-management.md`
