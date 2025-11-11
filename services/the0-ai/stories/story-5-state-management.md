# Story 5: Agent Communication & State Management

**Epic:** MAS Orchestration
**Status:** Not Started
**Estimated Effort:** 6-8 hours
**Dependencies:** Story 2 (Researcher Agent), Story 3 (Developer Agent)

---

## Description

Implement robust agent-to-agent communication using shared session state with proper data structures, validation, versioning, and utilities. This enables Researcher and Developer agents to share information seamlessly.

---

## Why

- **Communication**: Enable agents to share data reliably
- **Structure**: Standardized data formats prevent errors
- **Validation**: Pydantic schemas ensure data integrity
- **Debugging**: Logging and utilities aid troubleshooting
- **Evolution**: Versioning supports schema changes

---

## What

Create state management infrastructure:
- Define Pydantic schemas for research_data and bot_metadata
- Create utility functions for reading/writing state
- Implement state validation
- Add state logging for debugging
- Support schema versioning for future evolution
- Provide comprehensive testing

---

## Tasks

### 1. Define State Schemas
- [ ] Create `the0/agents/state_schema.py`
- [ ] Define `ResearchFinding` Pydantic model
- [ ] Define `ResearchData` Pydantic model
- [ ] Define `BotMetadata` Pydantic model
- [ ] Add version fields for schema evolution
- [ ] Add timestamps for freshness tracking

### 2. Create State Management Utilities
- [ ] Create `SessionStateManager` class
- [ ] Implement `store_research()` method
- [ ] Implement `get_research()` method
- [ ] Implement `store_bot_metadata()` method
- [ ] Implement `get_bot_metadata()` method
- [ ] Implement `clear_state()` method
- [ ] Add validation and error handling

### 3. Add State Logging
- [ ] Log state writes (agent, key, size)
- [ ] Log state reads (agent, key, found/not found)
- [ ] Log validation errors
- [ ] Log state transitions

### 4. Update Agent Integration
- [ ] Update Researcher agent to use SessionStateManager
- [ ] Update Developer agent to use SessionStateManager
- [ ] Verify state sharing works between agents

### 5. Testing
- [ ] Create `tests/the0/agents/test_state_management.py`
- [ ] Test ResearchData schema validation
- [ ] Test BotMetadata schema validation
- [ ] Test SessionStateManager store/retrieve
- [ ] Test cross-agent state sharing
- [ ] Test error handling for corrupt state
- [ ] Test schema versioning
- [ ] Achieve >80% code coverage

### 6. Documentation
- [ ] Document state schemas
- [ ] Document SessionStateManager API
- [ ] Provide usage examples
- [ ] Update CLAUDE.md with state management patterns

---

## Acceptance Criteria

- [ ] `state_schema.py` created with Pydantic models
- [ ] ResearchData model defined with version field
- [ ] BotMetadata model defined with version field
- [ ] SessionStateManager class implemented
- [ ] All CRUD methods working (store, get, clear)
- [ ] Validation prevents corrupt state
- [ ] Logging captures all state operations
- [ ] Cross-agent state sharing tested
- [ ] Schema versioning supports evolution
- [ ] Unit tests achieve >80% coverage
- [ ] Integration tests pass
- [ ] Documentation complete
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### State Schemas

```python
# the0/agents/state_schema.py

from typing import List, Dict, Optional
from pydantic import BaseModel, Field
from datetime import datetime

class ResearchFinding(BaseModel):
    """Single research finding with source and confidence."""
    point: str = Field(..., description="Key finding or insight")
    source: str = Field(..., description="Source URL")
    confidence: str = Field(
        ...,
        description="Confidence level: high, medium, or low"
    )
    timestamp: datetime = Field(default_factory=datetime.utcnow)

    class Config:
        json_schema_extra = {
            "example": {
                "point": "Binance API supports WebSocket for real-time data",
                "source": "https://binance-docs.github.io/apidocs/spot/en/",
                "confidence": "high",
                "timestamp": "2025-11-10T15:00:00Z"
            }
        }

class ResearchData(BaseModel):
    """Structured research data stored in session state."""
    version: str = Field(
        default="1.0",
        description="Schema version for evolution support"
    )
    query: str = Field(..., description="Original research request")
    summary: str = Field(..., description="Executive summary of findings")
    findings: List[ResearchFinding] = Field(
        default_factory=list,
        description="List of key findings with sources"
    )
    recommendations: List[str] = Field(
        default_factory=list,
        description="Actionable recommendations"
    )
    sources: List[Dict[str, str]] = Field(
        default_factory=list,
        description="All sources referenced"
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When research was completed"
    )
    researcher_notes: Optional[str] = Field(
        default=None,
        description="Additional notes from researcher"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "version": "1.0",
                "query": "Research Binance Spot API for real-time data",
                "summary": "Binance Spot API v3 provides comprehensive real-time market data",
                "findings": [
                    {
                        "point": "WebSocket streams available",
                        "source": "https://binance-docs.github.io/",
                        "confidence": "high"
                    }
                ],
                "recommendations": [
                    "Use WebSocket for real-time data",
                    "Implement python-binance library"
                ],
                "sources": [
                    {
                        "title": "Binance API Docs",
                        "url": "https://binance-docs.github.io/",
                        "relevance": "Official documentation"
                    }
                ],
                "timestamp": "2025-11-10T15:00:00Z"
            }
        }

class BotMetadata(BaseModel):
    """Bot metadata stored in session state."""
    version: str = Field(
        default="1.0",
        description="Schema version for evolution support"
    )
    bot_name: str = Field(..., description="Bot name/identifier")
    language: str = Field(
        ...,
        description="Programming language (python or javascript)"
    )
    files_created: List[str] = Field(
        default_factory=list,
        description="List of files created for the bot"
    )
    strategy_type: str = Field(
        ...,
        description="Strategy type (e.g., momentum, arbitrage, DCA)"
    )
    platform: str = Field(
        ...,
        description="Trading platform (e.g., binance, alpaca)"
    )
    status: str = Field(
        ...,
        description="Development status (e.g., ready_for_deploy, in_progress)"
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
                    "main.py",
                    "bot-config.yaml",
                    "requirements.txt",
                    "bot-schema.json",
                    "README.md",
                    "backtest.py",
                    "backtest-schema.json"
                ],
                "strategy_type": "momentum",
                "platform": "binance",
                "status": "ready_for_deploy",
                "timestamp": "2025-11-10T16:00:00Z"
            }
        }
```

### State Management Utilities

```python
# the0/agents/state_schema.py (continued)

import logging

logger = logging.getLogger(__name__)

class SessionStateManager:
    """Utilities for managing session state across agents."""

    # State keys
    RESEARCH_KEY = "research_data"
    BOT_METADATA_KEY = "bot_metadata"

    @staticmethod
    def store_research(state: dict, research: ResearchData) -> None:
        """
        Store research data in session state.

        Args:
            state: Session state dictionary
            research: ResearchData instance to store

        Raises:
            ValueError: If research data is invalid
        """
        try:
            # Validate
            data = research.model_dump()

            # Log
            logger.info(
                f"Storing research data: query='{research.query}', "
                f"findings={len(research.findings)}, "
                f"size={len(str(data))} bytes"
            )

            # Store
            state[SessionStateManager.RESEARCH_KEY] = data

        except Exception as e:
            logger.error(f"Error storing research data: {e}")
            raise ValueError(f"Failed to store research: {e}")

    @staticmethod
    def get_research(state: dict) -> Optional[ResearchData]:
        """
        Retrieve research data from session state.

        Args:
            state: Session state dictionary

        Returns:
            ResearchData instance if found, None otherwise
        """
        data = state.get(SessionStateManager.RESEARCH_KEY)

        if not data:
            logger.info("No research data found in session state")
            return None

        try:
            research = ResearchData(**data)
            logger.info(
                f"Retrieved research data: query='{research.query}', "
                f"findings={len(research.findings)}"
            )
            return research
        except Exception as e:
            logger.error(f"Error parsing research data: {e}")
            return None

    @staticmethod
    def store_bot_metadata(state: dict, metadata: BotMetadata) -> None:
        """
        Store bot metadata in session state.

        Args:
            state: Session state dictionary
            metadata: BotMetadata instance to store

        Raises:
            ValueError: If metadata is invalid
        """
        try:
            # Validate
            data = metadata.model_dump()

            # Log
            logger.info(
                f"Storing bot metadata: bot_name='{metadata.bot_name}', "
                f"files={len(metadata.files_created)}, "
                f"status={metadata.status}"
            )

            # Store
            state[SessionStateManager.BOT_METADATA_KEY] = data

        except Exception as e:
            logger.error(f"Error storing bot metadata: {e}")
            raise ValueError(f"Failed to store metadata: {e}")

    @staticmethod
    def get_bot_metadata(state: dict) -> Optional[BotMetadata]:
        """
        Retrieve bot metadata from session state.

        Args:
            state: Session state dictionary

        Returns:
            BotMetadata instance if found, None otherwise
        """
        data = state.get(SessionStateManager.BOT_METADATA_KEY)

        if not data:
            logger.info("No bot metadata found in session state")
            return None

        try:
            metadata = BotMetadata(**data)
            logger.info(
                f"Retrieved bot metadata: bot_name='{metadata.bot_name}', "
                f"status={metadata.status}"
            )
            return metadata
        except Exception as e:
            logger.error(f"Error parsing bot metadata: {e}")
            return None

    @staticmethod
    def clear_state(state: dict) -> None:
        """
        Clear all MAS-related state (for new conversations).

        Args:
            state: Session state dictionary
        """
        logger.info("Clearing MAS state")
        state.pop(SessionStateManager.RESEARCH_KEY, None)
        state.pop(SessionStateManager.BOT_METADATA_KEY, None)

    @staticmethod
    def get_state_summary(state: dict) -> dict:
        """
        Get summary of current state for debugging.

        Args:
            state: Session state dictionary

        Returns:
            Dictionary with state summary
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

        if has_bot:
            metadata = SessionStateManager.get_bot_metadata(state)
            if metadata:
                summary["bot_name"] = metadata.bot_name
                summary["bot_status"] = metadata.status
                summary["files_count"] = len(metadata.files_created)

        return summary
```

---

## Technical Considerations

### State Size Limits
- Session state persists in PostgreSQL
- Keep total state <100KB for performance
- Monitor state sizes in production
- Implement truncation if needed

### JSON Serialization
- Pydantic handles serialization automatically
- Datetime fields serialize to ISO format
- All models must be JSON-compatible

### Schema Evolution
- Version field enables migration logic
- Backward-compatible changes preferred
- Migration functions for breaking changes

### Concurrency
- ADK handles session locking
- Multiple agents won't corrupt state
- Document thread-safety assumptions

---

## Risks & Mitigations

### Low Risk: State Corruption
**Risk:** Invalid data written to state
**Impact:** Agent communication breaks
**Mitigation:**
- Pydantic validation
- Try/except in all methods
- Logging for debugging

**Contingency:** Clear state, restart workflow

### Low Risk: State Size Exceeds Limits
**Risk:** State grows too large
**Impact:** Performance degradation
**Mitigation:**
- Monitor state sizes
- Set reasonable limits
- Summarize when needed

**Contingency:** Implement state truncation

---

## Testing Strategy

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

def test_research_finding_validation():
    """Test ResearchFinding model validation."""
    finding = ResearchFinding(
        point="Test finding",
        source="https://example.com",
        confidence="high"
    )
    assert finding.point == "Test finding"
    assert finding.confidence == "high"

def test_research_data_validation():
    """Test ResearchData model validation."""
    data = ResearchData(
        query="Test query",
        summary="Test summary",
        findings=[],
        recommendations=["rec1", "rec2"],
        sources=[]
    )
    assert data.version == "1.0"
    assert len(data.recommendations) == 2

def test_bot_metadata_validation():
    """Test BotMetadata model validation."""
    metadata = BotMetadata(
        bot_name="test_bot",
        language="python",
        files_created=["main.py"],
        strategy_type="momentum",
        platform="binance",
        status="ready_for_deploy"
    )
    assert metadata.bot_name == "test_bot"
    assert metadata.version == "1.0"

def test_state_manager_store_retrieve_research():
    """Test storing and retrieving research data."""
    state = {}
    research = ResearchData(
        query="Test",
        summary="Summary",
        findings=[],
        recommendations=[],
        sources=[]
    )

    SessionStateManager.store_research(state, research)
    retrieved = SessionStateManager.get_research(state)

    assert retrieved is not None
    assert retrieved.query == "Test"
    assert retrieved.summary == "Summary"

def test_state_manager_store_retrieve_metadata():
    """Test storing and retrieving bot metadata."""
    state = {}
    metadata = BotMetadata(
        bot_name="test",
        language="python",
        files_created=[],
        strategy_type="momentum",
        platform="binance",
        status="in_progress"
    )

    SessionStateManager.store_bot_metadata(state, metadata)
    retrieved = SessionStateManager.get_bot_metadata(state)

    assert retrieved is not None
    assert retrieved.bot_name == "test"

def test_state_manager_clear():
    """Test clearing state."""
    state = {
        SessionStateManager.RESEARCH_KEY: {},
        SessionStateManager.BOT_METADATA_KEY: {}
    }

    SessionStateManager.clear_state(state)

    assert SessionStateManager.RESEARCH_KEY not in state
    assert SessionStateManager.BOT_METADATA_KEY not in state

@pytest.mark.asyncio
async def test_cross_agent_state_sharing():
    """Test state sharing between agents."""
    # Researcher writes state
    state = {}
    research = ResearchData(
        query="API research",
        summary="Findings",
        findings=[],
        recommendations=["Use WebSocket"],
        sources=[]
    )
    SessionStateManager.store_research(state, research)

    # Developer reads state
    retrieved = SessionStateManager.get_research(state)
    assert retrieved is not None
    assert "WebSocket" in retrieved.recommendations[0]
```

---

## Related Stories

**Depends On:**
- Story 2: Researcher Agent
- Story 3: Developer Agent

**Blocks:**
- Story 6: Testing & Validation (validates state management)

**Related:**
- Story 4: Supervisor Transformation
- Story 7: Documentation

---

## Notes

- Keep state schemas simple and focused
- Add fields incrementally as needed
- Log everything for debugging
- Future: Add state caching, compression if needed

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Pydantic schemas defined and validated
- [ ] SessionStateManager fully implemented
- [ ] Logging captures all operations
- [ ] Unit tests achieve >80% coverage
- [ ] Integration tests pass
- [ ] Cross-agent state sharing working
- [ ] Documentation complete
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
