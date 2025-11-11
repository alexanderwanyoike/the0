# Story 6: Testing & Validation Strategy

**Epic:** Quality Assurance
**Status:** Not Started
**Estimated Effort:** 12-16 hours
**Dependencies:** Stories 1-5 (all previous implementation stories)

---

## Description

Comprehensive testing strategy for the Multi-Agent System including unit tests, integration tests, end-to-end tests, and validation of agent behaviors. Ensures MAS works reliably and meets quality standards.

---

## Why

- **Quality**: Prevent bugs and regressions
- **Confidence**: Deploy with confidence
- **Documentation**: Tests serve as usage examples
- **Maintenance**: Catch issues early
- **Validation**: Verify MAS behaves as designed

---

## What

Create comprehensive test suite covering:
- Unit tests for each component (agents, tools, state management)
- Integration tests for agent interactions
- End-to-end tests for complete workflows
- Mock infrastructure for external services
- Performance benchmarks
- Test documentation

---

## Tasks

### 1. Unit Tests - Tavily Integration
- [ ] Create/update `tests/the0/tools/test_tavily_search.py`
- [ ] Test basic search functionality (mocked)
- [ ] Test advanced search functionality (mocked)
- [ ] Test error handling (no API key, API failure, timeout)
- [ ] Test result formatting and citations
- [ ] Achieve >80% coverage for tavily_search.py

### 2. Unit Tests - Agent Configurations
- [ ] Create `tests/the0/agents/test_researcher.py`
- [ ] Test researcher configuration (name, model, tools)
- [ ] Test researcher instructions present
- [ ] Create `tests/the0/agents/test_developer.py`
- [ ] Test developer configuration (name, model, tools)
- [ ] Test developer instructions present
- [ ] Create/update `tests/the0/test_supervisor.py`
- [ ] Test supervisor configuration (name, model, sub_agents)
- [ ] Test supervisor tools reduced to docs only

### 3. Unit Tests - State Management
- [ ] Test ResearchData validation
- [ ] Test BotMetadata validation
- [ ] Test SessionStateManager store/retrieve
- [ ] Test state schema versioning
- [ ] Test error handling for corrupt state
- [ ] Achieve >80% coverage for state_schema.py

### 4. Integration Tests - Agent Interactions
- [ ] Create `tests/integration/test_mas_workflow.py`
- [ ] Test Supervisor → Researcher delegation
- [ ] Test Supervisor → Developer delegation
- [ ] Test Researcher writes state
- [ ] Test Developer reads state
- [ ] Test state persistence across transitions

### 5. End-to-End Tests
- [ ] Test full bot creation workflow
- [ ] Test research-heavy scenarios
- [ ] Test development-heavy scenarios
- [ ] Test error recovery scenarios
- [ ] Test multiple iterations
- [ ] Test streaming responses

### 6. Mock Infrastructure
- [ ] Create mock Tavily client
- [ ] Create mock ADK components
- [ ] Create mock documentation API
- [ ] Create mock storage service
- [ ] Create test fixtures

### 7. Performance Tests
- [ ] Benchmark response times
- [ ] Validate state size limits
- [ ] Monitor API usage (Tavily)
- [ ] Test concurrent requests

### 8. Test Documentation
- [ ] Document test strategy
- [ ] Create test coverage reports
- [ ] Write testing guidelines
- [ ] Document mock usage

---

## Acceptance Criteria

- [ ] Unit test coverage >80% for new code
- [ ] All agent configurations tested
- [ ] Integration tests for all agent interactions working
- [ ] End-to-end test for complete workflow passing
- [ ] Mock services for external dependencies
- [ ] Performance benchmarks established
- [ ] Test documentation complete
- [ ] All tests passing
- [ ] CI/CD integration (if applicable)
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### Mock Infrastructure

```python
# tests/conftest.py (additions)

import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent
from the0.agent import supervisor_agent

@pytest.fixture
def mock_tavily_client():
    """Mock Tavily client for testing."""
    client = AsyncMock()
    client.search = AsyncMock(return_value={
        "answer": "Test AI-generated answer",
        "results": [
            {
                "title": "Test Result 1",
                "url": "https://example.com/1",
                "content": "Test content 1",
                "score": 0.95
            },
            {
                "title": "Test Result 2",
                "url": "https://example.com/2",
                "content": "Test content 2",
                "score": 0.85
            }
        ]
    })
    return client

@pytest.fixture
def mock_session_state():
    """Mock session state for testing."""
    return {}

@pytest.fixture
def sample_research_data():
    """Sample research data for testing."""
    from the0.agents.state_schema import ResearchData, ResearchFinding

    return ResearchData(
        query="Test research query",
        summary="Test summary of findings",
        findings=[
            ResearchFinding(
                point="Finding 1",
                source="https://example.com/1",
                confidence="high"
            )
        ],
        recommendations=["Recommendation 1", "Recommendation 2"],
        sources=[
            {
                "title": "Source 1",
                "url": "https://example.com/1",
                "relevance": "Test relevance"
            }
        ]
    )

@pytest.fixture
def sample_bot_metadata():
    """Sample bot metadata for testing."""
    from the0.agents.state_schema import BotMetadata

    return BotMetadata(
        bot_name="test_bot",
        language="python",
        files_created=["main.py", "bot-config.yaml", "requirements.txt"],
        strategy_type="momentum",
        platform="binance",
        status="ready_for_deploy"
    )
```

### Integration Tests

```python
# tests/integration/test_mas_workflow.py

import pytest
from unittest.mock import AsyncMock, patch
from the0.agent import supervisor_agent
from the0.agents.state_schema import SessionStateManager

@pytest.mark.asyncio
async def test_full_bot_creation_workflow(
    mock_tavily_client,
    mock_session_state,
    sample_research_data
):
    """
    End-to-end test: User requests bot → Research → Development → Delivery
    """
    # Simulate user request
    user_message = "Build a momentum trading bot for Binance"

    # 1. Supervisor should delegate to Researcher
    # Mock: Supervisor invokes researcher
    # Researcher performs search (mocked)
    # Researcher stores research data

    SessionStateManager.store_research(mock_session_state, sample_research_data)
    research = SessionStateManager.get_research(mock_session_state)
    assert research is not None
    assert research.query == "Test research query"

    # 2. Supervisor should delegate to Developer
    # Developer reads research data
    research_for_dev = SessionStateManager.get_research(mock_session_state)
    assert research_for_dev is not None

    # Developer creates artifacts (mocked)
    # Developer stores bot metadata
    from the0.agents.state_schema import BotMetadata
    bot_metadata = BotMetadata(
        bot_name="momentum_binance",
        language="python",
        files_created=["main.py", "bot-config.yaml"],
        strategy_type="momentum",
        platform="binance",
        status="ready_for_deploy"
    )
    SessionStateManager.store_bot_metadata(mock_session_state, bot_metadata)

    # 3. Supervisor presents results
    metadata = SessionStateManager.get_bot_metadata(mock_session_state)
    assert metadata is not None
    assert metadata.bot_name == "momentum_binance"
    assert "main.py" in metadata.files_created

@pytest.mark.asyncio
async def test_research_delegation():
    """Test supervisor correctly delegates research tasks."""
    # This test would verify:
    # 1. Supervisor receives query requiring research
    # 2. Supervisor calls transfer_to_agent(agent_name='researcher')
    # 3. Researcher executes and returns findings
    # 4. State contains research_data
    pass

@pytest.mark.asyncio
async def test_development_delegation():
    """Test supervisor correctly delegates development tasks."""
    # This test would verify:
    # 1. Session state has research_data
    # 2. Supervisor receives development request
    # 3. Supervisor calls transfer_to_agent(agent_name='developer')
    # 4. Developer reads research, creates files
    # 5. State contains bot_metadata
    pass

@pytest.mark.asyncio
async def test_error_recovery():
    """Test system handles agent failures gracefully."""
    # Simulate researcher failure
    # Verify supervisor handles error
    # Verify user receives informative message
    # Verify system doesn't crash
    pass

@pytest.mark.asyncio
async def test_state_persistence():
    """Test state persists across agent transitions."""
    state = {}

    # Researcher stores data
    from the0.agents.state_schema import ResearchData
    research = ResearchData(
        query="Test",
        summary="Summary",
        findings=[],
        recommendations=[],
        sources=[]
    )
    SessionStateManager.store_research(state, research)

    # Verify developer can read it
    retrieved = SessionStateManager.get_research(state)
    assert retrieved is not None
    assert retrieved.query == "Test"

    # Verify supervisor can read both
    assert SessionStateManager.get_research(state) is not None
```

### Unit Tests - Agents

```python
# tests/the0/agents/test_researcher.py

def test_researcher_configuration():
    """Test researcher agent properly configured."""
    from the0.agents.researcher import researcher_agent

    assert researcher_agent.name == "researcher"
    assert researcher_agent.model == "gemini-2.5-flash"
    assert len(researcher_agent.tools) == 4  # tavily, browse, 2x docs
    assert "research" in researcher_agent.description.lower()
    assert "citation" in researcher_agent.instruction.lower()

# tests/the0/agents/test_developer.py

def test_developer_configuration():
    """Test developer agent properly configured."""
    from the0.agents.developer import developer_agent

    assert developer_agent.name == "developer"
    assert developer_agent.model == "gemini-2.5-flash"
    assert len(developer_agent.tools) == 4  # save, deploy, 2x docs
    assert "development" in developer_agent.description.lower()
    assert "artifact" in developer_agent.instruction.lower()

# tests/the0/test_supervisor.py

def test_supervisor_configuration():
    """Test supervisor agent properly configured."""
    from the0.agent import supervisor_agent

    assert supervisor_agent.name == "the0"
    assert supervisor_agent.model == "gemini-2.5-flash"
    assert len(supervisor_agent.sub_agents) == 2
    assert len(supervisor_agent.tools) == 2  # 2x docs only
    assert "orchestrate" in supervisor_agent.description.lower()
    assert "Alfred" in supervisor_agent.instruction

def test_supervisor_maintains_personality():
    """Verify Alfred personality in instructions."""
    from the0.agent import supervisor_agent

    assert "Sir" in supervisor_agent.instruction or "Madam" in supervisor_agent.instruction
    assert "butler" in supervisor_agent.instruction.lower()
```

---

## Technical Considerations

### Async Testing
- Use `pytest-asyncio` throughout
- All async functions need `@pytest.mark.asyncio`
- Mock async functions with `AsyncMock`

### Test Isolation
- Each test should be independent
- Use fixtures for shared setup
- Clean up state after tests

### Mock Management
- Create reusable mock fixtures
- Keep mocks simple and focused
- Validate mock behavior matches real implementations

### Coverage Tools
- Use `pytest-cov` for coverage reporting
- Generate HTML reports for review
- Set minimum coverage thresholds

---

## Risks & Mitigations

### Low Risk: Flaky Tests
**Risk:** Async timing issues cause intermittent failures
**Impact:** Reduced confidence in test suite
**Mitigation:**
- Proper use of async fixtures
- Avoid sleep(), use proper await
- Deterministic mocks

**Contingency:** Identify and fix flaky tests immediately

### Low Risk: Mock Drift
**Risk:** Mocks don't match real implementations
**Impact:** Tests pass but real code fails
**Mitigation:**
- Regular validation against real services
- Integration tests with real APIs (staging)
- Document mock assumptions

**Contingency:** Integration tests catch drift

---

## Testing Strategy Summary

### Test Pyramid

```
           ┌─────────────┐
          │   E2E Tests  │  (Few, high-value workflows)
         └───────────────┘
        ┌────────────────────┐
       │ Integration Tests   │  (Agent interactions)
      └─────────────────────┘
    ┌─────────────────────────────┐
   │      Unit Tests              │  (Components, utilities)
  └──────────────────────────────┘
```

### Coverage Goals
- **Unit Tests**: >80% coverage
- **Integration Tests**: All agent interaction patterns
- **E2E Tests**: Critical user workflows
- **Performance Tests**: Baseline benchmarks

### Test Execution
```bash
# Run all tests
make test

# Run with coverage
pytest --cov=the0 --cov=api --cov-report=html

# Run specific test category
pytest tests/unit/
pytest tests/integration/
pytest tests/the0/agents/

# Run with verbose output
pytest -v

# Run specific test
pytest tests/the0/test_supervisor.py::test_supervisor_configuration
```

---

## Related Stories

**Depends On:**
- All previous stories (0-5)

**Blocks:**
- Story 7: Documentation (test docs)

---

## Notes

- Tests are living documentation - keep them clear
- Don't test implementation details, test behaviors
- Integration tests can be slower - that's OK
- Future: Add load testing, chaos testing

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Unit tests >80% coverage
- [ ] Integration tests all passing
- [ ] E2E test demonstrates full workflow
- [ ] Mock infrastructure complete
- [ ] Performance benchmarks established
- [ ] All tests passing
- [ ] Test documentation complete
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
