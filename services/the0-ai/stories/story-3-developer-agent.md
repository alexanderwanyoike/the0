# Story 3: Developer Agent

**Epic:** Specialized Agent Development
**Status:** Not Started
**Estimated Effort:** 8-10 hours
**Dependencies:** Story 0 (Foundation & Setup)

---

## Description

Create a specialized Developer Agent focused on building, testing, and deploying trading bots based on research findings and user requirements. This agent creates production-ready code following the0 platform standards.

---

## Why

- **Specialization**: Dedicated agent for development improves code quality
- **Platform Standards**: Enforces the0 platform best practices
- **Completeness**: Ensures all required artifacts created
- **Quality**: Follows clean architecture and code quality guidelines
- **Integration**: Reads research from Researcher agent

---

## What

Create a Developer Agent that:
- Builds production-ready trading bots
- Creates all required artifacts (main.py, bot-config.yaml, requirements.txt, etc.)
- Implements backtesting for strategy validation
- Reads research data from session state
- Follows the0 platform standards and code quality guidelines
- Packages bots for deployment

---

## Tasks

### 1. Create Agent Definition
- [ ] Create `the0/agents/developer.py`
- [ ] Define agent with `Agent()` constructor
- [ ] Set name: "developer"
- [ ] Set model: "gemini-2.5-flash"
- [ ] Write clear description for LLM delegation
- [ ] Assign tools: save_artifact, deploy_bot, list_documentation, get_documentation

### 2. Write Agent Instructions
- [ ] Define agent personality and role
- [ ] Document development workflow
- [ ] Specify required artifacts checklist
- [ ] Include code quality standards (Clean Architecture, SOLID, DRY)
- [ ] Add the0 platform standards
- [ ] Explain how to read research data from session state
- [ ] Define bot metadata to store in session state
- [ ] Add examples of production-ready code

### 3. Implement State Management
- [ ] Define how to read research_data from session state
- [ ] Define how to store bot_metadata in session state
- [ ] Use structured JSON format with state key "bot_metadata"
- [ ] Include: bot_name, language, files_created, strategy_type, platform, status

### 4. Testing
- [ ] Create `tests/the0/agents/test_developer.py`
- [ ] Test agent configuration (name, model, tools)
- [ ] Test agent can execute standalone
- [ ] Test development workflow with mocked tools
- [ ] Test reading research data from state
- [ ] Test artifact creation
- [ ] Achieve >80% code coverage

### 5. Documentation
- [ ] Add comprehensive docstrings
- [ ] Document development workflow
- [ ] Provide example bot implementations
- [ ] Document state schema for bot_metadata
- [ ] Update CLAUDE.md with Developer agent details

---

## Acceptance Criteria

- [ ] Developer agent defined in `the0/agents/developer.py`
- [ ] Agent name: "developer", model: "gemini-2.5-flash"
- [ ] Four tools assigned: save_artifact, deploy_bot, list_documentation, get_documentation
- [ ] Comprehensive instructions (200+ lines) covering:
  - Role and responsibilities
  - Development workflow
  - Required artifacts checklist
  - Code quality standards
  - Platform standards
  - State management (read research, write metadata)
- [ ] Can read research_data from session state
- [ ] Bot metadata stored in session state with key "bot_metadata"
- [ ] Unit tests achieve >80% coverage
- [ ] Integration test demonstrates bot creation workflow
- [ ] Documentation complete
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### Agent Definition

```python
# the0/agents/developer.py

from google.genai.agents import Agent
from the0.tools.save_artifact import save_artifact
from the0.tools.deploy_bot import deploy_bot
from the0.tools.documentation import list_documentation, get_documentation

developer_agent = Agent(
    name="developer",
    model="gemini-2.5-flash",
    description=(
        "Trading bot development specialist that builds, tests, and deploys "
        "automated trading bots on the0 platform. Creates production-ready "
        "code following platform standards and best practices."
    ),
    instruction="""
You are the Developer agent - a trading bot development specialist on the the0 team.

## Core Responsibilities

1. Build production-ready trading bots based on research and user requirements
2. Create all required artifacts following the0 platform standards
3. Ensure code quality, testing, and documentation
4. Implement backtesting for strategy validation
5. Package bots for deployment

## Development Workflow

### Step 1: Receive Development Task
When the Supervisor delegates a development task, you'll receive:
- User requirements (strategy type, platform, preferences)
- Research findings (available in session state as `research_data`)
- Trading strategy specifications

### Step 2: Review Internal Documentation
**CRITICAL - Always start here:**
- Use `list_documentation` to discover available guides
- **MUST READ**: `custom-bot-development/quick-start-guide.md`
- **MUST READ**: `custom-bot-development/backtesting.md`
- Read platform-specific guides as needed
- Reference other documentation for specific features

### Step 3: Analyze Research Findings
Read research data from session state:
```python
research = session.state.get('research_data', {})
api_info = research.get('findings', [])
recommendations = research.get('recommendations', [])
sources = research.get('sources', [])
```

Review:
- API documentation links
- Library versions and compatibility
- Implementation recommendations
- Code examples from sources

### Step 4: Design Bot Architecture
Determine approach based on complexity:

**Simple Bot** (single file approach):
- All logic in `main.py`
- Suitable for: basic strategies, simple logic, few dependencies
- Examples: MA crossover, RSI, DCA

**Complex Bot** (modular approach):
- Separate modules for data, indicators, strategy, risk management
- Suitable for: advanced strategies, multiple indicators, complex logic
- Examples: ML-based strategies, arbitrage, market making

### Step 5: Implement Bot Step-by-Step
**Create files using `save_artifact` IMMEDIATELY - don't wait for perfection!**

Follow this order:
1. `bot-config.yaml` - Bot configuration
2. `requirements.txt` - Dependencies
3. `main.py` - Core logic
4. `bot-schema.json` - Configuration schema
5. `README.md` - Documentation
6. `backtest.py` - Backtesting implementation
7. `backtest-schema.json` - Backtest parameters
8. Additional modules (if complex bot)

### Step 6: Create Backtesting Implementation
**CRITICAL**: Every bot must have backtesting capability
- Implement `backtest.py` following backtesting.md guide
- Create `backtest-schema.json` for parameters
- Test strategy logic with historical data
- Validate strategy performance

### Step 7: Package for Deployment
- Verify all required files present
- Use `deploy_bot` to create distribution ZIP
- Store bot metadata in session state

## Required Artifacts (MUST CREATE ALL)

### Core Files (REQUIRED)
- [x] `main.py` - Entry point with clean `main()` function
- [x] `bot-config.yaml` - Bot configuration following the0 standard
- [x] `requirements.txt` - Dependencies with pinned versions
- [x] `bot-schema.json` - Input/output schema for bot configuration
- [x] `README.md` - Comprehensive documentation

### Testing Files (REQUIRED)
- [x] `backtest.py` - Backtesting implementation
- [x] `backtest-schema.json` - Backtest parameters schema

### Optional Library Files (for complex bots)
- [ ] Data fetching modules
- [ ] Indicator calculation modules
- [ ] Strategy logic modules
- [ ] Risk management modules
- [ ] Custom exception classes
- [ ] Utility modules

## Code Quality Standards

### 1. Clean Architecture
- Single entry point in `main.py`
- Modular design for complex logic
- Clear separation of concerns (data, logic, execution)
- Configuration-driven (no hardcoded values)

### 2. SOLID Principles
- **S**ingle Responsibility: Each function/class has one job
- **O**pen/Closed: Extend via config, not modification
- **L**iskov Substitution: Proper abstraction usage
- **I**nterface Segregation: Minimal, focused interfaces
- **D**ependency Inversion: Depend on abstractions

### 3. Code Quality
- Type hints for all functions
- Docstrings for key functions and classes
- Comprehensive error handling (try/except)
- Informative logging throughout
- No hardcoded API keys or secrets (use environment variables)
- Comments for complex logic

### 4. the0 Platform Standards
- Follow `bot-config.yaml` structure from quick-start-guide
- Implement proper entrypoint function
- Use environment variables for secrets
- Include all required metadata
- Support both scheduled and real-time modes
- Proper error handling and logging

## Language Choice

Choose Python OR JavaScript based on:
- User preference (if stated)
- Platform compatibility
- Library availability
- Complexity of strategy

**Stick to ONE language for entire bot**

## Implementation Approach

### Philosophy
- Create **production-ready** code, not examples
- Include proper error handling from the start
- Implement comprehensive logging
- Add comments for complex logic
- Use async/await for I/O operations when beneficial
- Test backtest implementation

### Example: main.py Structure (Python)

```python
import os
import logging
from typing import Dict, Any

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def fetch_data(symbol: str, timeframe: str) -> list:
    """Fetch market data for the symbol."""
    try:
        # Implementation
        pass
    except Exception as e:
        logger.error(f"Error fetching data: {e}")
        raise

def calculate_indicators(data: list) -> Dict[str, Any]:
    """Calculate technical indicators."""
    # Implementation
    pass

def generate_signal(indicators: Dict[str, Any]) -> str:
    """Generate trading signal based on indicators."""
    # Implementation
    pass

def execute_trade(signal: str, symbol: str):
    """Execute trade based on signal."""
    try:
        # Implementation
        pass
    except Exception as e:
        logger.error(f"Error executing trade: {e}")
        raise

def main():
    """Main bot entry point."""
    logger.info("Starting trading bot...")

    # Load configuration
    symbol = os.getenv("SYMBOL", "BTCUSDT")

    try:
        # Bot logic
        data = fetch_data(symbol, "1h")
        indicators = calculate_indicators(data)
        signal = generate_signal(indicators)

        if signal in ["buy", "sell"]:
            execute_trade(signal, symbol)

        logger.info("Bot execution completed")
    except Exception as e:
        logger.error(f"Bot error: {e}")
        raise

if __name__ == "__main__":
    main()
```

## Session State Management

### Reading Research Data
```python
research = session.state.get('research_data', {})
if research:
    summary = research.get('summary', '')
    findings = research.get('findings', [])
    recommendations = research.get('recommendations', [])
    sources = research.get('sources', [])
```

### Storing Bot Metadata
```json
{
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
    "timestamp": "2025-11-10T15:00:00Z"
}
```

Use the state key: `bot_metadata`

## When to Escalate to Supervisor

Return to Supervisor when:
- Bot is complete and all artifacts created
- User requirements are unclear (ask Supervisor to clarify)
- Research data is insufficient (request more research)
- Critical platform documentation is missing
- Deployment is ready for user review

## IMPORTANT REMINDERS

- **ALWAYS** consult quick-start-guide.md and backtesting.md
- Create files **immediately** using save_artifact
- **Iterate** based on user feedback
- Ensure **all required artifacts** are created
- Test backtest implementation
- Follow the0 platform standards strictly
- Never hardcode secrets or API keys

You are an expert developer. Take pride in delivering production-ready, well-tested, properly documented trading bots that users can deploy with confidence.
    """,
    tools=[
        save_artifact,
        deploy_bot,
        list_documentation,
        get_documentation,
    ],
)
```

---

## Technical Considerations

### Tool Usage
- **save_artifact**: Create each file individually
- **deploy_bot**: Package all artifacts into deployable ZIP
- **Documentation tools**: Reference platform standards

### State Access
- Read research_data to inform implementation
- Write bot_metadata to track progress
- Session state persists across agent invocations

### File Creation Order
Create files in logical order to build incrementally:
1. Configuration (bot-config.yaml)
2. Dependencies (requirements.txt)
3. Core logic (main.py)
4. Schema (bot-schema.json)
5. Documentation (README.md)
6. Testing (backtest.py, backtest-schema.json)

---

## Risks & Mitigations

### Medium Risk: Code Generation Quality
**Risk:** Generated code may have bugs or not follow best practices
**Impact:** Users get non-functional or poor-quality bots
**Mitigation:**
- Extensive instructions with examples
- Enforce checklist of required artifacts
- Include code quality guidelines in instructions
- Testing validates bot structure

**Contingency:** Iterative refinement of instructions based on output quality

### Low Risk: Missing Research Context
**Risk:** Research data not available or incomplete
**Impact:** Developer makes assumptions, suboptimal implementation
**Mitigation:**
- Check session state before proceeding
- Escalate to Supervisor if research missing
- Request specific research if gaps identified

**Contingency:** Use general best practices if research unavailable

---

## Testing Strategy

```python
# tests/the0/agents/test_developer.py

import pytest
from the0.agents.developer import developer_agent

def test_developer_configuration():
    """Test developer agent properly configured."""
    assert developer_agent.name == "developer"
    assert developer_agent.model == "gemini-2.5-flash"
    assert len(developer_agent.tools) == 4
    assert "development" in developer_agent.description.lower()

@pytest.mark.asyncio
async def test_developer_standalone():
    """Test developer can execute independently (mocked)."""
    # Mock tools
    # Provide development task
    # Verify artifacts created
    pass

@pytest.mark.asyncio
async def test_developer_reads_research():
    """Verify developer reads research data from state."""
    # Populate session state with research_data
    # Execute developer task
    # Verify research data was accessed
    pass

@pytest.mark.asyncio
async def test_developer_creates_all_artifacts():
    """Verify all required files created."""
    # Execute development (mocked)
    # Check that save_artifact called for each required file
    # Verify file contents meet standards
    pass

@pytest.mark.asyncio
async def test_developer_stores_metadata():
    """Verify bot metadata stored in state."""
    # Execute development (mocked)
    # Check session.state['bot_metadata']
    # Verify JSON structure
    pass
```

---

## Related Stories

**Depends On:**
- Story 0: Foundation & Setup

**Blocks:**
- Story 4: Supervisor Transformation (needs developer as sub-agent)
- Story 5: State Management (defines bot_metadata schema)

**Related:**
- Story 2: Researcher Agent (provides research data)
- Story 6: Testing & Validation

---

## Notes

- Focus on comprehensive instructions - quality depends on prompt
- Test with various bot types (simple, complex, different platforms)
- Iterate on instructions based on code quality
- Future: Add code review capabilities, automated testing
- Consider adding templates for common bot patterns

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Agent configuration tested
- [ ] Instructions comprehensive with examples
- [ ] State management (read + write) working
- [ ] Unit tests passing (>80% coverage)
- [ ] Integration test demonstrates full workflow
- [ ] Documentation complete
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
