# Story 5 Part 2: Agent State Management Integration

**Epic:** MAS Orchestration
**Status:** Not Started
**Estimated Effort:** 2-3 hours
**Dependencies:** Story 5 (State Management Infrastructure)

---

## Description

Integrate the SessionStateManager infrastructure into Researcher and Developer agents so they automatically use validated state sharing during execution. This completes the state management implementation by wiring the infrastructure (from Story 5) into the actual agent workflows.

---

## Why

- **Completion**: Finalize state management by integrating infrastructure into agents
- **Automation**: Agents automatically share data without manual intervention
- **Reliability**: Type-safe state operations prevent data corruption
- **Workflow**: Enable seamless Researcher → Developer data flow
- **Observability**: State operations are logged for debugging

---

## What

Update agent instructions to use SessionStateManager:
- Researcher agent stores research findings using SessionStateManager
- Developer agent reads research data using SessionStateManager
- Developer agent stores bot metadata using SessionStateManager
- Integration tests verify cross-agent state sharing
- Documentation reflects active state usage

---

## Tasks

### 1. Update Researcher Agent Instructions
**File**: `the0/agents/researcher.py`

- [ ] Update "Session State Storage" section (lines 197-229)
- [ ] Remove placeholder note: "State writing will be implemented in Story 5..."
- [ ] Add explicit instruction to use `SessionStateManager.store_research()`
- [ ] Add code example showing how to construct ResearchData object
- [ ] Add instruction to store after completing research
- [ ] Verify agent imports already include SessionStateManager (added in Story 5)

**Example instruction to add**:
```
After completing research, store findings in session state:

1. Import required classes (already imported at top of file)
2. Construct ResearchData object with all findings
3. Call SessionStateManager.store_research(session.state, research_data)
4. Verify storage with logging output
5. Return to Supervisor to present results

Example:
from the0.agents.state_schema import SessionStateManager, ResearchData, ResearchFinding

research = ResearchData(
    query=original_request,
    summary="2-3 sentence summary",
    findings=[
        ResearchFinding(
            point="Key finding",
            source="https://source.com",
            confidence="high"
        )
    ],
    recommendations=["rec1", "rec2"],
    sources=[{
        "title": "Source Title",
        "url": "https://source.com",
        "relevance": "Why relevant",
        "published": "2025-01-15"
    }]
)

SessionStateManager.store_research(session.state, research)
```

### 2. Update Developer Agent Instructions

**File**: `the0/agents/developer.py`

#### 2.1 Update Step 3: Analyze Research Findings (lines 107-126)

- [ ] Replace raw dict access with SessionStateManager.get_research()
- [ ] Update code example to use typed ResearchData object
- [ ] Add null check and graceful handling if no research data
- [ ] Add logging when research data is read

**Replace**:
```python
research = session.state.get('{STATE_KEY_RESEARCH}', {})
if research:
    summary = research.get('summary', '')
    findings = research.get('findings', [])
    recommendations = research.get('recommendations', [])
    sources = research.get('sources', [])
```

**With**:
```python
from the0.agents.state_schema import SessionStateManager

research = SessionStateManager.get_research(session.state)
if research:
    summary = research.summary
    findings = research.findings  # List[ResearchFinding]
    recommendations = research.recommendations
    sources = research.sources

    # Log research data retrieval
    logger.info(f"Retrieved research: {len(findings)} findings, "
                f"{len(recommendations)} recommendations")
```

#### 2.2 Update Step 8.5: Document Results (lines 317-324)

- [ ] Add explicit instruction to create BotMetadata object
- [ ] Add call to SessionStateManager.store_bot_metadata()
- [ ] Include all required metadata fields
- [ ] Add logging for metadata storage

**Add after existing documentation**:
```
Store bot metadata in session state:

from the0.agents.state_schema import SessionStateManager, BotMetadata

metadata = BotMetadata(
    bot_name=bot_name,
    language=language,  # "python" or "javascript"
    files_created=all_files_created,  # List of filenames
    strategy_type=strategy_type,  # e.g., "momentum"
    platform=platform,  # e.g., "binance"
    status="ready_for_deploy" if all_verified else "needs_fixes",
    execution_verified=bot_execution_passed,
    backtest_verified=backtest_execution_passed,
    libraries_used=dependencies_list,  # With versions
    test_results={
        "bot_execution": "success" or "failed",
        "backtest_execution": "success" or "failed",
        "backtest_trades": trade_count,
        "backtest_pnl": pnl_value,
        "used_mock_data": mock_data_used
    }
)

SessionStateManager.store_bot_metadata(session.state, metadata)
logger.info(f"Stored bot metadata for {bot_name}")
```

#### 2.3 Update Step 9: Package for Deployment (line 332)

- [ ] Update instruction to confirm metadata already stored
- [ ] Add redundancy check (optional re-store if needed)
- [ ] Verify metadata before reporting to Supervisor

**Update line 332**:
```
4. Verify bot_metadata stored in session state (already done in Step 8.5)
   - Optional: Re-store if any final updates needed
   - Verify using SessionStateManager.get_state_summary(session.state)
```

### 3. Create Integration Tests

**File**: `tests/the0/agents/test_agent_state_integration.py` (NEW)

- [ ] Create new test file for state integration
- [ ] Test Researcher stores research data correctly
- [ ] Test Developer reads research data correctly
- [ ] Test Developer stores bot metadata correctly
- [ ] Test end-to-end workflow: Researcher → Developer
- [ ] Test null handling when no research data available
- [ ] Test state summary reflects stored data
- [ ] Achieve >80% coverage for new code paths

**Test Structure**:
```python
class TestResearcherStateIntegration:
    """Test Researcher agent state storage."""
    def test_researcher_stores_research_data(self):
        """Test that researcher stores data using SessionStateManager."""
        # Test implementation

class TestDeveloperStateIntegration:
    """Test Developer agent state operations."""
    def test_developer_reads_research_data(self):
        """Test that developer reads research data correctly."""
        # Test implementation

    def test_developer_stores_bot_metadata(self):
        """Test that developer stores bot metadata correctly."""
        # Test implementation

def test_end_to_end_state_workflow():
    """Integration test: Researcher stores → Developer reads → Developer stores."""
    # Test implementation
```

### 4. Update Documentation

**File**: `CLAUDE.md`

- [ ] Update Researcher Agent Details section
  - Add note: "Stores research in session state using SessionStateManager"
  - Update State Output description

- [ ] Update Developer Agent Details section
  - Add note: "Reads research from session state using SessionStateManager"
  - Add note: "Stores bot metadata using SessionStateManager"
  - Update State Input/Output description

- [ ] Update State Management section (if needed)
  - Confirm agents now actively use state management
  - Add "✅ Integrated" status marker

### 5. Validation

- [ ] Run existing agent tests to ensure no regressions
- [ ] Run new integration tests
- [ ] Run `make format` to format all code
- [ ] Run `make lint` to verify no linting errors
- [ ] Manual test: Run agent and check logs for state operations
- [ ] Verify state persistence in database (if using DatabaseSessionService)

---

## Acceptance Criteria

- [ ] Researcher agent instructions updated to use SessionStateManager
- [ ] Researcher stores ResearchData after completing research
- [ ] Developer agent reads research using SessionStateManager.get_research()
- [ ] Developer agent stores BotMetadata using SessionStateManager.store_bot_metadata()
- [ ] All raw dict access (`session.state.get()`) replaced with SessionStateManager
- [ ] Integration tests pass with >80% coverage
- [ ] Existing agent tests pass (no regressions)
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)
- [ ] CLAUDE.md updated with integration status
- [ ] State operations appear in logs during execution
- [ ] End-to-end workflow test passes

---

## Implementation Notes

### Key Locations

**Researcher Agent** (`the0/agents/researcher.py`):
- Lines 197-229: Session State Storage section (update instructions)
- Line 41: Import already exists (added in Story 5)

**Developer Agent** (`the0/agents/developer.py`):
- Lines 107-126: Step 3 - Analyze Research Findings (replace dict access)
- Lines 317-324: Step 8.5 - Document Results (add metadata storage)
- Line 332: Step 9 - Package for Deployment (verify metadata stored)
- Lines 40-44: Imports already exist (added in Story 5)

### Integration Approach

This is an **instruction-only update** - no code changes to agent definitions themselves. The agents are Google ADK agents that execute based on their instruction prompts. By updating the instructions, the LLM will automatically call SessionStateManager methods during execution.

### Testing Strategy

1. **Unit Tests**: Verify instruction content mentions SessionStateManager
2. **Integration Tests**: Mock session.state dict and verify the workflow
3. **Manual Tests**: Run actual agent with logging to observe state operations

### Gotchas

- **Import Verification**: Imports were already added in Story 5 (with noqa comments)
- **ADK Context**: Ensure instructions show correct context usage (session.state available)
- **Logging**: State operations are already logged by SessionStateManager
- **Backwards Compatibility**: Old agents without state still work (graceful None handling)

---

## Success Metrics

- **Code Coverage**: >80% for new test file
- **Test Pass Rate**: 100% (all tests must pass)
- **Linting**: Zero errors
- **Integration**: End-to-end workflow test passes
- **Observability**: State operations visible in logs

---

## Related Stories

- **Story 5**: State Management Infrastructure (completed) - provided SessionStateManager
- **Story 2**: Researcher Agent (completed) - agent to be updated
- **Story 3**: Developer Agent (completed) - agent to be updated
- **Story 4**: Supervisor Transformation (completed) - orchestrates the agents
- **Story 6**: Testing & Validation (not started) - comprehensive testing strategy

---

## Next Steps (After Completion)

1. **Manual Testing**: Run full bot creation workflow and verify state in logs
2. **Story 6**: Comprehensive testing strategy can build on these integration tests
3. **Monitoring**: Consider adding metrics for state operation frequency/size
4. **Optimization**: Profile state operations if performance becomes an issue
