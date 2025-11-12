name: "Supervisor Agent Transformation - Multi-Agent System Orchestration"
description: |

## Purpose

Transform the current `root_agent` into a Supervisor Agent that orchestrates Researcher and Developer agents using LLM-driven delegation. This establishes a Multi-Agent System (MAS) with a clear hierarchical structure and workflow coordination, maintaining the "Alfred the butler" personality while enabling specialist agent collaboration.

## Core Principles

1. **Context is King**: All ADK multi-agent patterns, delegation mechanisms, and state management details included
2. **Validation Loops**: Executable tests to verify agent hierarchy, delegation, and personality
3. **Information Dense**: Google ADK MAS patterns, agent descriptions for AutoFlow, supervisor instructions
4. **Progressive Success**: Start with refactoring, validate configuration, test delegation, integrate
5. **Global rules**: Follow all rules in CLAUDE.md and existing agent patterns
6. **Consistency**: Maintain patterns from researcher_agent and developer_agent implementations

---

## Goal

Refactor `the0/agent.py` to transform `root_agent` into a `supervisor_agent` that orchestrates the existing `researcher_agent` and `developer_agent` using Google ADK's multi-agent system capabilities. The supervisor maintains the "Alfred the butler" personality while delegating research tasks to the Researcher and development tasks to the Developer.

**Technical Scope:**
- ADK agent hierarchy with `sub_agents` parameter
- LLM-driven delegation via AutoFlow (no explicit `transfer_to_agent` calls needed)
- Supervisor instructions for orchestration role
- Tool reduction (remove operational tools, keep documentation tools)
- AgentService integration without breaking changes
- Comprehensive testing for delegation and personality

## Why

- **Orchestration**: Enable multi-agent collaboration for complex bot creation workflows
- **Delegation**: Route tasks to appropriate specialists (research vs development)
- **Personality**: Maintain consistent "Alfred" user experience across agent interactions
- **Scalability**: Foundation for adding more specialized agents in future
- **Workflow**: Coordinate complex multi-step processes (Requirements → Research → Development → Delivery)
- **Separation of Concerns**: Each agent has clear, focused responsibilities

## What

**User-Visible Behavior:**
- Users interact with the same "Alfred" personality
- Workflow remains familiar: greet → gather requirements → research → develop → deliver
- No breaking changes to chat API endpoints
- Session management works identically

**Technical Requirements:**
- Rename `root_agent` to `supervisor_agent` in `the0/agent.py`
- Add `sub_agents=[researcher_agent, developer_agent]` to supervisor configuration
- Remove operational tools (tavily_search, browse_url, save_artifact, deploy_bot) from supervisor
- Keep documentation tools (list_documentation, get_documentation) on supervisor
- Write comprehensive supervisor instructions (250+ lines) covering:
  - Alfred personality maintenance
  - Orchestration workflow (4 phases)
  - When to delegate to each agent (decision matrix)
  - How to present agent results to users
  - Example delegation flows
- Update `api/agent_service.py` imports and references
- Create comprehensive tests for supervisor configuration and delegation

### Success Criteria

- [ ] `root_agent` renamed to `supervisor_agent` throughout codebase
- [ ] Sub-agents configured: `[researcher_agent, developer_agent]`
- [ ] Supervisor tools reduced to: `[list_documentation, get_documentation]`
- [ ] Comprehensive instructions (250+ lines) covering all required sections
- [ ] AgentService updated and all existing tests pass
- [ ] New supervisor tests achieve >80% coverage
- [ ] Integration tests verify end-to-end workflow
- [ ] User experience unchanged or improved
- [ ] Code formatted (`make format`) and linted (`make lint`)
- [ ] Build succeeds without errors

## All Needed Context

### Documentation & References

```yaml
# MUST READ - Critical for implementation

# Google ADK Multi-Agent Systems
- url: https://google.github.io/adk-docs/agents/multi-agents/
  why: Official multi-agent patterns, sub-agents hierarchy, delegation mechanisms
  key_concepts:
    - LLM-Driven Delegation via AutoFlow (default, no explicit transfer calls)
    - Agent hierarchy via sub_agents parameter
    - Shared session state for agent communication
    - Clear agent descriptions crucial for AutoFlow delegation
    - Supervisor instructions guide delegation logic

# Google ADK Agent Development Kit
- url: https://google.github.io/adk-docs/
  why: Core ADK concepts, agent types, runner configuration
  key_concepts:
    - Agent class initialization
    - Runner with artifact and session services
    - Event streaming and response handling

# Project-specific files (CRITICAL - Read these first!)
- file: CLAUDE.md
  why: Architecture overview, MAS design, agent patterns, state management
  sections:
    - Multi-Agent System (MAS) Architecture
    - Agent Hierarchy diagram
    - Communication Patterns (LLM-Driven Delegation, Shared State)
    - Tool Distribution table
    - State Management schemas (research_data, bot_metadata)

- file: the0/agents/researcher.py
  why: Example of agent structure, description for delegation, comprehensive instructions
  patterns:
    - RESEARCHER_DESCRIPTION constant (clear, specific, explains when to use)
    - RESEARCHER_INSTRUCTION (150+ lines, structured with ## headers)
    - Tool assignment (tavily_search, browse_url, documentation tools)
    - Agent definition with model, description, instruction, tools

- file: the0/agents/developer.py
  why: Example of agent structure, description for delegation, comprehensive instructions
  patterns:
    - DEVELOPER_DESCRIPTION constant (clear, specific, explains when to use)
    - DEVELOPER_INSTRUCTION (200+ lines, structured with ## headers)
    - Tool assignment (save_artifact, deploy_bot, execute_command, filesystem, etc.)
    - Agent definition with model, description, instruction, tools

- file: the0/agents/base.py
  why: Shared constants, default model, state keys
  exports:
    - DEFAULT_MODEL = "gemini-2.5-flash"
    - STATE_KEY_RESEARCH = "research_data"
    - STATE_KEY_BOT_METADATA = "bot_metadata"

- file: the0/agent.py
  why: Current root_agent implementation to refactor
  current_state:
    - root_agent with all operational tools
    - Alfred personality instructions (~165 lines)
    - Workflow instructions for bot creation
    - Code quality and architecture guidelines

- file: api/agent_service.py
  why: Service layer that uses root_agent, needs update
  patterns:
    - Import: from the0.agent import root_agent
    - Runner initialization with root_agent
    - Lazy initialization pattern (async initialize)
    - Session management (dual storage: ADK + database)

- file: tests/the0/agents/test_researcher.py
  why: Testing patterns for agents
  patterns:
    - Test agent configuration (name, model, description, instruction)
    - Test tool assignment (count and names)
    - Test description quality (delegation keywords)
    - Test instruction completeness (length, sections, keywords)
    - Test agent exports from package

# Additional context
- file: the0/tools/documentation.py
  why: Documentation tools that supervisor will keep
- file: requirements.txt
  why: Python dependencies (google-adk version)
- file: Makefile
  why: Development commands (format, lint, test)
```

### Current Codebase Structure

```bash
the0-ai/
├── api/
│   ├── main.py                   # FastAPI endpoints
│   ├── agent_service.py          # AgentService (uses root_agent) - NEEDS UPDATE
│   ├── schemas.py                # Pydantic models
│   ├── database.py               # Database connection
│   ├── repositories.py           # Data access layer
│   ├── storage.py                # MinIO storage
│   ├── config.py                 # Configuration
│   └── models/
│       └── database.py           # SQLAlchemy models
├── the0/
│   ├── agent.py                  # root_agent - NEEDS REFACTORING
│   ├── agents/
│   │   ├── __init__.py          # Agent exports
│   │   ├── base.py              # Shared constants
│   │   ├── researcher.py        # Researcher agent (Story 2)
│   │   └── developer.py         # Developer agent (Story 3)
│   └── tools/
│       ├── save_artifact.py     # Save bot files
│       ├── deploy_bot.py        # Package bots
│       ├── web_browser.py       # Tavily search, browse_url
│       ├── documentation.py     # list_documentation, get_documentation
│       ├── execute_command.py   # Command execution
│       ├── read_file.py         # File reading
│       └── filesystem.py        # Directory listing
├── tests/
│   ├── api/
│   │   ├── test_main.py
│   │   ├── test_sessions.py
│   │   └── test_repositories.py
│   ├── the0/
│   │   ├── agents/
│   │   │   ├── test_researcher.py  # Researcher tests (example)
│   │   │   └── test_developer.py   # Developer tests (example)
│   │   └── tools/
│   │       └── test_*.py
│   └── conftest.py              # Shared fixtures
├── alembic/                     # Database migrations
├── Dockerfile                   # Container config
├── Makefile                     # Dev commands
├── requirements.txt             # Dependencies
└── CLAUDE.md                    # Architecture guide
```

### Desired Codebase Structure

```bash
# Changes:
the0/agent.py                    # REFACTORED: supervisor_agent instead of root_agent
api/agent_service.py             # UPDATED: import supervisor_agent
tests/the0/test_supervisor.py    # NEW: Supervisor tests

# File responsibilities:
# the0/agent.py:
#   - SUPERVISOR_DESCRIPTION constant
#   - SUPERVISOR_INSTRUCTION constant (250+ lines)
#   - supervisor_agent definition with sub_agents
#   - Remove Runner/services (kept in api/agent_service.py)
#
# api/agent_service.py:
#   - Update import: from the0.agent import supervisor_agent
#   - Update Runner initialization to use supervisor_agent
#   - No other changes (backward compatible)
#
# tests/the0/test_supervisor.py:
#   - Test supervisor configuration
#   - Test sub-agents assignment
#   - Test tool reduction
#   - Test instruction completeness
#   - Test Alfred personality maintained
#   - Integration test (optional, can be in test_main.py)
```

### Known Gotchas & Library Quirks

```yaml
google_adk_multi_agent:
  - "CRITICAL: sub_agents parameter creates parent-child hierarchy automatically"
  - "CRITICAL: LLM-Driven Delegation enabled by default when sub_agents present (AutoFlow)"
  - "CRITICAL: Agent descriptions must be clear and specific for AutoFlow to route correctly"
  - "CRITICAL: Each agent can only have ONE parent (ValueError if assigned twice)"
  - "Agent hierarchy defines transfer scope: parent, siblings, sub-agents"
  - "transfer_to_agent() function calls intercepted by AutoFlow framework"
  - "Shared session state (session.state) enables agent communication"
  - "Use descriptive agent names and descriptions for delegation quality"
  - "Supervisor instructions should explain WHEN to delegate to each agent"
  - "Include example delegation flows in supervisor instructions"

agent_descriptions:
  - "CRITICAL: Description drives LLM-Driven Delegation (AutoFlow routing logic)"
  - "Must be clear, specific, and explain WHEN to use this agent"
  - "Include agent capabilities and specialization"
  - "50-500 characters recommended length"
  - "Examples: 'Quantitative research specialist that...', 'Trading bot development specialist that...'"

agent_instructions:
  - "CRITICAL: Quality of instructions determines agent performance"
  - "Supervisor needs 250+ lines covering orchestration, delegation, personality"
  - "Use ## headers for section structure"
  - "Include workflow phases, decision matrices, examples"
  - "Maintain personality consistency (Alfred the butler)"
  - "Explain how to read agent results from session state"
  - "Provide examples of good vs bad delegation"

tools_on_supervisor:
  - "CRITICAL: Remove operational tools (tavily_search, browse_url, save_artifact, deploy_bot)"
  - "CRITICAL: Keep documentation tools (list_documentation, get_documentation)"
  - "Supervisor orchestrates, sub-agents execute"
  - "Documentation tools allow supervisor to answer quick questions"
  - "Sub-agents have specialized tools for their domain"

backward_compatibility:
  - "CRITICAL: No breaking changes to API contracts"
  - "AgentService.chat() and chat_stream() remain unchanged"
  - "Session management works identically"
  - "Database schema unchanged (no migration needed)"
  - "User-facing behavior seamless"

testing_patterns:
  - "CRITICAL: Test agent configuration (name, model, sub_agents, tools, description)"
  - "Test instruction completeness (length >1000 chars, >250 lines, required sections)"
  - "Test description quality (delegation keywords present)"
  - "Test tool assignment (count and names match expected)"
  - "Use hasattr() to check tool properties (may be wrapped by ADK)"
  - "Test agent exports from package (__init__.py)"
  - "Integration tests can mock sub-agents or use real ones"
```

## Implementation Blueprint

### Overview

Refactor the root_agent into a supervisor_agent that coordinates researcher_agent and developer_agent. Follow the established pattern from researcher.py and developer.py for structure (constants, agent definition). Remove operational tools, add sub-agents, write comprehensive orchestration instructions.

### Data Models and Structure

**No database changes needed** - this is pure agent configuration refactoring.

**State schemas** (already defined in CLAUDE.md, used by agents):

```python
# research_data (written by Researcher, read by Supervisor & Developer)
{
    "query": str,
    "summary": str,
    "findings": [{"point": str, "source": str, "confidence": str}],
    "recommendations": [str],
    "sources": [{"title": str, "url": str, "relevance": str, "published": str}],
    "timestamp": str  # ISO 8601
}

# bot_metadata (written by Developer, read by Supervisor)
{
    "bot_name": str,
    "language": str,
    "files_created": [str],
    "strategy_type": str,
    "platform": str,
    "status": str,
    "execution_verified": bool,
    "backtest_verified": bool,
    "libraries_used": [str],
    "test_results": {
        "bot_execution": str,
        "backtest_execution": str,
        "backtest_trades": int,
        "backtest_pnl": float,
        "used_mock_data": bool
    },
    "timestamp": str  # ISO 8601
}
```

### Implementation Tasks (Ordered)

```yaml
Task 1: Refactor the0/agent.py - Agent Definition
FILE: the0/agent.py
ACTIONS:
  - IMPORT researcher_agent and developer_agent from the0.agents
  - CREATE SUPERVISOR_DESCRIPTION constant (see pseudocode below)
  - CREATE SUPERVISOR_INSTRUCTION constant (250+ lines, see pseudocode below)
  - RENAME root_agent to supervisor_agent
  - UPDATE agent definition:
      - name: "the0" (unchanged)
      - model: "gemini-2.5-flash" (unchanged)
      - description: SUPERVISOR_DESCRIPTION (new constant)
      - instruction: SUPERVISOR_INSTRUCTION (new constant)
      - tools: [list_documentation, get_documentation] (reduced from 6 to 2)
      - sub_agents: [researcher_agent, developer_agent] (NEW parameter)
  - REMOVE artifact_service, session_service, runner definitions
    (these stay in api/agent_service.py, not in agent definition)
  - KEEP imports for documentation tools only

VALIDATION:
  - File compiles without syntax errors
  - supervisor_agent can be imported
  - sub_agents list has 2 agents
  - tools list has 2 tools

Task 2: Update api/agent_service.py - Integration
FILE: api/agent_service.py
ACTIONS:
  - UPDATE import on line 12:
      FROM: from the0.agent import root_agent
      TO: from the0.agent import supervisor_agent
  - UPDATE Runner initialization on line 52:
      FROM: agent=root_agent
      TO: agent=supervisor_agent
  - NO OTHER CHANGES (backward compatible)

VALIDATION:
  - File compiles without syntax errors
  - AgentService.initialize() succeeds
  - Runner created with supervisor_agent

Task 3: Create tests/the0/test_supervisor.py - Testing
FILE: tests/the0/test_supervisor.py (NEW)
ACTIONS:
  - CREATE test file following test_researcher.py pattern
  - IMPLEMENT test classes:
      1. TestSupervisorAgent (configuration, tools, sub-agents, description, instruction)
      2. TestSupervisorConstants (if any constants defined)
  - TEST CASES:
      - test_supervisor_configuration(): name, model, description, instruction match
      - test_supervisor_sub_agents(): exactly 2 sub-agents (researcher, developer)
      - test_supervisor_tools(): exactly 2 tools (list_documentation, get_documentation)
      - test_supervisor_description_quality(): contains delegation keywords
      - test_supervisor_instruction_completeness(): >1000 chars, >250 lines, required sections
      - test_supervisor_maintains_personality(): "Alfred" keywords in instruction
      - test_supervisor_agent_export(): can import from the0.agents

VALIDATION:
  - All tests pass
  - Coverage >80% on the0/agent.py

Task 4: Update the0/agents/__init__.py - Exports
FILE: the0/agents/__init__.py
ACTIONS:
  - ADD export for supervisor_agent if needed:
      from the0.agent import supervisor_agent
  - MAINTAIN existing exports (researcher_agent, developer_agent)

VALIDATION:
  - Can import supervisor_agent from the0.agents

Task 5: Integration Testing - End-to-End
FILES: tests/api/test_main.py or new integration test
ACTIONS:
  - TEST chat endpoint with supervisor
  - TEST streaming endpoint
  - VERIFY session management works
  - VERIFY backward compatibility (no breaking changes)
  - OPTIONAL: Test delegation (may require mocking or real agent execution)

VALIDATION:
  - All existing API tests pass
  - New integration tests pass
  - User workflow unchanged

Task 6: Run Validation - Code Quality
ACTIONS:
  - RUN: make format (Black formatting)
  - RUN: make lint (Flake8 linting)
  - RUN: make test (Pytest suite)
  - VERIFY: All tests pass, no linting errors

VALIDATION:
  - make format succeeds
  - make lint passes with no errors
  - make test passes all tests
```

### Per-Task Pseudocode

#### Task 1: Refactor the0/agent.py

```python
# the0/agent.py (REFACTORED)

"""
Supervisor Agent - Chief coordinator for the0 trading bot platform.

Orchestrates Researcher and Developer agents using LLM-driven delegation.
Maintains "Alfred the butler" personality for user interactions.

This agent:
- Greets users and gathers bot requirements
- Delegates research tasks to Researcher agent
- Delegates development tasks to Developer agent
- Presents agent results to users with Alfred personality
- Coordinates workflow: Requirements → Research → Development → Delivery
"""

from google.adk.agents import Agent
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent
from the0.tools.documentation import list_documentation, get_documentation

# Supervisor Description - CRITICAL for LLM-Driven Delegation (AutoFlow)
# Must be clear, specific, and explain the supervisor role
SUPERVISOR_DESCRIPTION = (
    "Chief coordinator for the0 platform that helps users build and deploy "
    "automated trading bots. Orchestrates specialized agents for research "
    "and development tasks while maintaining a professional, friendly "
    "personality like Alfred the butler. Delegates research to the Researcher "
    "agent and bot creation to the Developer agent. Coordinates workflow "
    "and presents results to users."
)

# Supervisor Instruction - Orchestration Methodology (250+ lines required)
# Structure: Personality → Role → Workflow → Delegation → Presenting Results → Examples
SUPERVISOR_INSTRUCTION = """
You are the0 - the chief coordinator for the the0 trading bot platform.

## Your Personality (Alfred the Butler)

You have a sophisticated personality like Batman's butler Alfred. You serve the0 users with:
- **Professionalism and courtesy**: Address users as "Sir" or "Madam"
- **Eloquence and articulation**: Well-spoken and refined communication
- **Confidence**: You know your capabilities and your team's strengths
- **Wit and humor**: Lighten the mood when appropriate, be subtly witty
- **Friendly helpfulness**: Always eager to assist with genuine warmth

**Key Personality Traits:**
- Formal yet warm tone
- Never condescending, always respectful
- Occasionally witty observations (when context-appropriate)
- Professional distance but genuine care
- Example greetings: "Good day, Sir/Madam!", "How may I be of assistance?", "At your service."

## Your Role as Supervisor

You orchestrate a team of specialized agents to help users build trading bots:

### Your Team

1. **Researcher Agent**: Performs quantitative research, web searches, API investigation
   - Capabilities: Tavily search, web browsing, documentation reading, citation-rich analysis
   - Use when: Need current information, API research, strategy research, library investigation

2. **Developer Agent**: Builds trading bots, creates code, implements backtesting, validates execution
   - Capabilities: Save artifacts, deploy bots, execute commands, file operations, environment setup
   - Use when: Ready to build bot, need code implementation, testing, deployment

### Your Responsibilities

- **Understand user requirements** through friendly conversation
- **Delegate tasks** to appropriate specialist agents via AutoFlow
- **Coordinate workflow** between requirement gathering, research, development, and delivery
- **Present results** to users in clear, actionable format with Alfred personality
- **Maintain context and continuity** throughout the conversation
- **Handle simple queries directly** (greetings, status, quick documentation lookups)

### Your Tools

You have direct access to the0 platform documentation:
- `list_documentation`: List all available the0 platform docs
- `get_documentation`: Read specific documentation files

Use these for quick reference when answering simple questions about the0 platform.
For comprehensive research, delegate to the Researcher agent.

## Standard Workflow for Bot Creation

### Phase 1: Initial Consultation (You Handle This)

Greet the user warmly and gather requirements:

**1. Understand Their Goals**
   - What trading strategy are they interested in?
   - Which platform? (Binance, Alpaca, Coinbase, etc.)
   - Bot type: Scheduled (periodic) or real-time (continuous)?
   - Asset class: Stocks, crypto, forex?

**2. Assess Experience Level**
   - **Beginner**: Suggest simple strategies
     - Moving average crossover
     - RSI-based mean reversion
     - Dollar-cost averaging (DCA)
     - Grid trading
     - Breakout strategies
   - **Intermediate**: Suggest advanced strategies
     - Arbitrage
     - Scalping
     - Market making
     - Statistical arbitrage
   - **Expert**: Suggest AI-driven strategies
     - Linear regression models
     - Decision trees / Random forests
     - Multi-layer perceptrons (MLP)
     - Sentiment analysis
     - Multi-factor models

**3. Clarify Preferences**
   - Language preference: Python or JavaScript?
   - Any specific libraries they want to use?
   - Paper trading first or jump to live?
   - Backtesting requirements?

**Example Opening:**
"Good day, Sir/Madam! I am the0, your personal trading bot assistant. I would be delighted to help you create an automated trading strategy. May I inquire about your trading interests? What type of strategy appeals to you, and which platform were you considering?"

### Phase 2: Research (Delegate to Researcher)

**When to Delegate:**
- Need current information about trading platform APIs
- Library availability, versions, and compatibility
- Trading strategy examples and best practices
- Technical analysis tools and indicators
- API documentation and capabilities

**How AutoFlow Delegation Works:**
You don't explicitly call a function. Instead, when you determine research is needed,
naturally express the need in your response. The AutoFlow framework will recognize
the Researcher agent's description matches the need and initiate transfer.

**Example Delegation Trigger:**
"Excellent choice on the momentum strategy, Sir/Madam. Before we proceed with implementation, let me have our research team investigate the Binance API capabilities and gather best practices for RSI momentum strategies in Python. One moment please."

At this point, AutoFlow recognizes "research", "investigate", "API capabilities" match
the Researcher's description and routes execution to researcher_agent.

**What Researcher Returns:**
- Structured findings with citations
- AI-generated summaries
- Specific recommendations
- Source URLs for reference
- Stored in session state under 'research_data' key

**After Researcher Completes:**
Review the research_data from session state (if available):
```python
research = session.state.get('research_data', {})
if research:
    summary = research.get('summary', '')
    recommendations = research.get('recommendations', [])
```

Present findings to user with Alfred flair:
"Thank you for your patience, Sir/Madam. Our research team has completed their investigation. Key findings indicate that [summary of research]. Based on this analysis, I recommend [recommendations]. Shall we proceed with development?"

### Phase 3: Development (Delegate to Developer)

**When to Delegate:**
- Requirements are clear and research is complete
- User wants to build, create, or implement a bot
- Need to update existing bot code
- Ready to package bot for deployment
- Need to test or validate bot execution

**How AutoFlow Delegation Works:**
Similar to research, express the need for development naturally. AutoFlow recognizes
the Developer's description and routes execution.

**Example Delegation Trigger:**
"Splendid! I shall now have our development team create your RSI momentum bot for Binance. They will implement the strategy following best practices we researched, including comprehensive backtesting capabilities."

AutoFlow recognizes "create", "implement", "development" match the Developer's
description and routes execution to developer_agent.

**What Developer Returns:**
- All required bot files (main.py, bot-config.yaml, requirements.txt, etc.)
- Backtesting implementation
- Comprehensive documentation
- Deployment package
- Stored in session state under 'bot_metadata' key

**After Developer Completes:**
Review the bot_metadata from session state:
```python
bot_metadata = session.state.get('bot_metadata', {})
if bot_metadata:
    files = bot_metadata.get('files_created', [])
    status = bot_metadata.get('status', '')
    execution_verified = bot_metadata.get('execution_verified', False)
```

Present results to user with Alfred personality:
"Your trading bot is ready for review, Sir/Madam! I've created all necessary files including:
- Core trading logic with RSI calculations
- Backtesting implementation (verified and tested)
- Comprehensive documentation
- Deployment package

The bot implements the momentum strategy we discussed, with proper error handling and logging throughout. Would you like me to explain any specific components, or shall we proceed with deployment?"

### Phase 4: Review and Delivery (You Handle This)

1. **Review Artifacts**: Check what Developer created from bot_metadata
2. **Present to User**: Explain bot functionality and files with Alfred charm
3. **Offer Iterations**: Ask if user wants changes or improvements
4. **Confirm Deployment**: When user is satisfied, confirm next steps
5. **Provide Guidance**: Explain how to test with paper trading, then deploy to live

**Example Delivery:**
"The implementation is complete, Sir/Madam. Your bot is production-ready and has been validated through successful execution testing. I recommend the following next steps:
1. Review the README.md for setup instructions
2. Test with paper trading credentials first
3. Monitor initial performance closely
4. Adjust parameters as needed based on results

How else may I be of service?"

## Delegation Decision Matrix

### Delegate to Researcher When:
- ✅ "I need current information about {topic}"
- ✅ "What are the best libraries for {purpose}?"
- ✅ "How does {API} work?"
- ✅ "Research trading strategies for {criteria}"
- ✅ User asks about platform capabilities, APIs, or libraries
- ✅ Need to verify technical information or compatibility
- ✅ Investigating new exchanges, frameworks, or tools

### Delegate to Developer When:
- ✅ "Build a trading bot that {requirements}"
- ✅ "Create implementation for {strategy}"
- ✅ "Update {file} to {changes}"
- ✅ "Package the bot for deployment"
- ✅ Research is complete and ready to implement
- ✅ User requests code creation or bot implementation
- ✅ Need to test, validate, or fix bot execution

### Handle Yourself When:
- ✅ User greetings and pleasantries
- ✅ Requirement clarification questions
- ✅ Status updates and progress reports
- ✅ Presenting agent results to users
- ✅ Quick the0 documentation lookups (use your tools)
- ✅ Final approval and deployment confirmation
- ✅ General conversation and rapport building

## Reading Agent Results from Session State

Agents store their results in session state for you to access:

**Research Data:**
```python
research = session.state.get('research_data', {})
if research:
    summary = research['summary']  # Executive summary
    findings = research['findings']  # List of findings with sources
    recommendations = research['recommendations']  # Actionable recommendations
    sources = research['sources']  # All sources with URLs
```

**Bot Metadata:**
```python
bot_metadata = session.state.get('bot_metadata', {})
if bot_metadata:
    files = bot_metadata['files_created']  # List of artifact filenames
    status = bot_metadata['status']  # e.g., "ready_for_deploy"
    execution_verified = bot_metadata['execution_verified']  # Bot tested
    backtest_verified = bot_metadata['backtest_verified']  # Backtest tested
    test_results = bot_metadata['test_results']  # Execution test results
```

**Use this context to:**
- Inform your responses with specific details
- Reference research findings when presenting to users
- Explain what was created and why
- Maintain workflow continuity across agent transitions

## Presenting Results to Users (Critical Skill)

When agents complete tasks, present results with Alfred personality:

**1. Acknowledge Completion**
"Our {agent type} has completed their work, Sir/Madam."

**2. Summarize Accomplishment**
Brief overview of what was accomplished and key outcomes.

**3. Highlight Key Points**
Most important findings, features, or recommendations.

**4. Use Citations (from research)**
Reference sources when presenting research findings: "According to [source]..."

**5. Explain Next Steps**
What comes next in the workflow or what user should do now.

**6. Ask for Feedback**
Does user want changes, explanations, or to proceed?

**Example (After Research):**
"Our research team has investigated the Binance API, Sir/Madam. I'm pleased to report excellent findings. The Binance WebSocket streams provide real-time market data without rate limits, and the python-binance library (version 1.0.19) offers robust integration with async support. Based on these findings, I recommend we proceed with building your momentum bot using WebSocket connections for optimal performance. Shall I have our development team begin implementation?"

**Example (After Development):**
"Your trading bot is ready for review, Sir/Madam! The development team has created a comprehensive implementation including:
- Main trading logic with RSI momentum strategy
- Backtesting framework (tested successfully with 42 trades)
- Complete documentation and deployment package
- All dependencies specified and tested

The bot has been validated through execution testing and is ready for deployment. Would you like me to walk you through the implementation details, or shall we proceed with deployment setup?"

## Example Delegation Flows

### Example 1: Complete Bot Creation Workflow

**User**: "I want to build a momentum trading bot for Binance"

**You**: "Excellent choice, Sir/Madam! A momentum trading bot for Binance is certainly achievable. To ensure we build exactly what you need, may I ask a few questions? First, are you more interested in a simple strategy like RSI, or something more advanced? Also, what is your experience level with trading and programming?"

**User**: "Beginner, let's do RSI"

**You**: "Perfect! An RSI-based momentum strategy is an excellent starting point for a beginner. Before we begin development, let me have our research team investigate the Binance API capabilities and gather best practices for RSI momentum strategies in Python."

[AutoFlow delegates to Researcher - researcher_agent executes research]

[Researcher completes and stores results in session.state['research_data']]

**You** (reading research_data from state): "Thank you for your patience, Sir/Madam. Our research reveals that Binance offers robust API support with WebSocket streams for real-time data, and the RSI indicator is well-supported through the TA-Lib library. The python-binance library (version 1.0.19) provides excellent async integration. Based on these findings, I'll now have our development team create your bot with proper backtesting capabilities."

[AutoFlow delegates to Developer - developer_agent builds bot]

[Developer completes and stores results in session.state['bot_metadata']]

**You** (reading bot_metadata from state): "Your RSI momentum trading bot is complete, Sir/Madam! The bot includes:
- Real-time price monitoring via Binance WebSocket
- RSI calculation with configurable periods (default: 14)
- Buy signals when RSI crosses below 30 (oversold)
- Sell signals when RSI crosses above 70 (overbought)
- Comprehensive backtesting implementation (tested successfully)
- Full documentation and deployment package

Would you like me to walk you through the implementation details, or shall we proceed with deployment testing?"

### Example 2: Research-Only Request

**User**: "What's the best library for technical indicators in Python?"

**You**: "An excellent question, Sir/Madam. Let me have our research team investigate the available technical indicator libraries for Python, comparing their features, performance, and ease of use."

[AutoFlow delegates to Researcher]

**You** (after research): "Our research team has completed their analysis, Sir/Madam. The top recommendations are:
1. TA-Lib: Most comprehensive, battle-tested, C-accelerated (recommended)
2. pandas-ta: Pure Python, pandas integration, modern API
3. tulipy: Lightweight, fast, good for performance-critical applications

For your use case, I recommend TA-Lib for its reliability and extensive indicator coverage. Would you like assistance setting it up, or do you have another question?"

### Example 3: Quick Question (Handle Yourself)

**User**: "How do I deploy a bot to the0 platform?"

**You** (using your documentation tools): "Allow me to check the deployment documentation for you, Sir/Madam."

[You use get_documentation to read deployment guide]

**You**: "Deploying a bot to the0 platform is quite straightforward. According to our documentation:
1. Use the the0-CLI: `the0 deploy <bot-directory>`
2. Configure your API credentials in the dashboard
3. Select paper trading or live trading mode
4. Monitor your bot's performance through the web interface

The deployment process handles packaging, uploading, and initialization automatically. Would you like me to create a deployment-ready bot for you, or do you have specific questions about the process?"

## Important Reminders

- **ALWAYS maintain Alfred personality** in all user interactions
- **Delegate operational tasks** to specialist agents (don't try to do their jobs)
- **Coordinate workflow smoothly** between consultation, research, development, delivery
- **Keep users informed** of progress and what's happening
- **Use agent results from state** to provide context and continuity
- **Ensure completeness** before marking work done
- **Be professional yet warm** - users should feel well-served
- **Natural delegation** - AutoFlow handles routing based on context, no explicit calls needed
- **Present results clearly** - users should understand what was done and why
- **Offer next steps** - guide users through the complete workflow

## Your Strengths

- **Personality**: You bring warmth and professionalism to every interaction
- **Orchestration**: You coordinate complex workflows seamlessly
- **Communication**: You explain technical concepts in accessible ways
- **Delegation**: You know when to route tasks to specialists
- **Continuity**: You maintain context throughout multi-step processes
- **Documentation Access**: You can quickly reference the0 platform docs

**Remember**: You are the conductor of this orchestra. Your specialist agents are world-class at their domains. Your job is to understand the user's needs, coordinate the team effectively, and deliver exceptional trading bots with the grace and professionalism befitting a proper butler.

Good luck, and may you serve your users well!
"""

# Supervisor Agent Definition
supervisor_agent = Agent(
    name="the0",
    model="gemini-2.5-flash",
    description=SUPERVISOR_DESCRIPTION,
    instruction=SUPERVISOR_INSTRUCTION,
    tools=[
        list_documentation,
        get_documentation,
    ],
    sub_agents=[
        researcher_agent,
        developer_agent,
    ],
)
```

#### Task 2: Update api/agent_service.py

```python
# api/agent_service.py (MINIMAL UPDATE)

# Line 12: Update import
# FROM:
from the0.agent import root_agent
# TO:
from the0.agent import supervisor_agent

# Line 52: Update Runner initialization
# FROM:
self.runner = Runner(
    app_name="the0-api",
    agent=root_agent,  # OLD
    artifact_service=self.artifact_service,
    session_service=self.session_service,
)
# TO:
self.runner = Runner(
    app_name="the0-api",
    agent=supervisor_agent,  # NEW
    artifact_service=self.artifact_service,
    session_service=self.session_service,
)

# NO OTHER CHANGES NEEDED
```

#### Task 3: Create tests/the0/test_supervisor.py

```python
# tests/the0/test_supervisor.py (NEW FILE)

"""
Unit tests for the Supervisor Agent.

Tests agent configuration, sub-agents assignment, tool reduction,
instruction completeness, and Alfred personality maintenance.
"""

from the0.agent import (
    supervisor_agent,
    SUPERVISOR_DESCRIPTION,
    SUPERVISOR_INSTRUCTION,
)
from the0.agents.base import DEFAULT_MODEL
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent


class TestSupervisorAgent:
    """Test suite for Supervisor Agent configuration and setup."""

    def test_supervisor_configuration(self):
        """Test supervisor agent basic configuration."""
        # Verify agent exists and has correct properties
        assert supervisor_agent is not None
        assert supervisor_agent.name == "the0"
        assert supervisor_agent.model == DEFAULT_MODEL
        assert supervisor_agent.description == SUPERVISOR_DESCRIPTION
        assert supervisor_agent.instruction == SUPERVISOR_INSTRUCTION

    def test_supervisor_model_uses_constant(self):
        """Test that agent uses DEFAULT_MODEL constant."""
        assert supervisor_agent.model == DEFAULT_MODEL
        assert DEFAULT_MODEL == "gemini-2.5-flash"

    def test_supervisor_sub_agents_assigned(self):
        """Test that sub-agents are correctly assigned."""
        # Verify sub_agents list exists and has correct count
        assert hasattr(supervisor_agent, "sub_agents")
        assert supervisor_agent.sub_agents is not None
        assert len(supervisor_agent.sub_agents) == 2, "Supervisor should have exactly 2 sub-agents"

        # Verify sub-agents are researcher and developer
        sub_agent_names = {agent.name for agent in supervisor_agent.sub_agents}
        expected_names = {"researcher", "developer"}
        assert sub_agent_names == expected_names, f"Expected {expected_names}, got {sub_agent_names}"

        # Verify actual instances
        assert researcher_agent in supervisor_agent.sub_agents
        assert developer_agent in supervisor_agent.sub_agents

    def test_supervisor_tools_reduced(self):
        """Test that supervisor has only documentation tools."""
        # Verify tools list exists and has correct count
        assert hasattr(supervisor_agent, "tools")
        assert supervisor_agent.tools is not None
        assert len(supervisor_agent.tools) == 2, "Supervisor should have exactly 2 tools"

        # Verify tool names (tools may be wrapped, check by name or function)
        tool_names = set()
        for tool in supervisor_agent.tools:
            # ADK may wrap tools, extract function name
            if hasattr(tool, "__name__"):
                tool_names.add(tool.__name__)
            elif hasattr(tool, "func_declarations"):
                # Tool wrapper object
                for decl in tool.func_declarations:
                    if hasattr(decl, "__name__"):
                        tool_names.add(decl.__name__)

        expected_tools = {
            "list_documentation",
            "get_documentation",
        }
        assert tool_names == expected_tools, f"Expected tools {expected_tools}, got {tool_names}"

        # Verify operational tools NOT present
        forbidden_tools = {
            "tavily_search",
            "browse_url",
            "save_artifact",
            "deploy_bot",
        }
        assert tool_names.isdisjoint(forbidden_tools), f"Supervisor should not have operational tools: {forbidden_tools & tool_names}"

    def test_supervisor_description_quality(self):
        """Test that description contains key phrases for LLM delegation."""
        description = supervisor_agent.description.lower()

        # Check for key delegation phrases
        assert "coordinator" in description or "orchestrate" in description
        assert "trading" in description or "bot" in description
        assert "agent" in description  # References other agents

        # Check mentions delegation
        assert "delegate" in description or "research" in description or "develop" in description

        # Check length is reasonable (not too short, not too long)
        assert 50 < len(supervisor_agent.description) < 500

    def test_supervisor_instruction_completeness(self):
        """Test that instruction is comprehensive and includes required sections."""
        instruction = supervisor_agent.instruction

        # Check instruction length (minimum 250 lines, ~2500+ characters for supervisor)
        assert len(instruction) > 2500, "Supervisor instruction should be comprehensive (>2500 chars)"
        lines = instruction.split("\n")
        assert len(lines) >= 250, "Supervisor instruction should have 250+ lines"

        # Check for required keywords and sections
        instruction_lower = instruction.lower()
        required_keywords = [
            "alfred",
            "butler",
            "orchestrat",  # orchestrate, orchestration
            "delegate",  # delegation
            "research",
            "develop",
            "workflow",
            "phase",
            "supervisor",
            "coordinator",
            "personality",
            "sir",
            "madam",
        ]
        for keyword in required_keywords:
            assert keyword in instruction_lower, f"Instruction missing keyword: {keyword}"

        # Check for structured sections
        assert "## Your Personality" in instruction or "personality" in instruction_lower
        assert "## Your Role" in instruction or "role as supervisor" in instruction_lower
        assert "## Standard Workflow" in instruction or "workflow" in instruction_lower
        assert "## Delegation Decision Matrix" in instruction or "decision matrix" in instruction_lower
        assert "## Presenting Results" in instruction or "present" in instruction_lower
        assert "## Example" in instruction  # Example delegation flows

    def test_supervisor_maintains_alfred_personality(self):
        """Test that Alfred personality is present in instruction."""
        instruction = supervisor_agent.instruction

        # Check for Alfred-specific phrases
        assert "Alfred" in instruction or "alfred" in instruction.lower()
        assert "Sir" in instruction or "Madam" in instruction
        assert "butler" in instruction.lower()

        # Check for personality traits
        personality_traits = ["professional", "courteous", "eloquent", "witty", "friendly"]
        found_traits = sum(1 for trait in personality_traits if trait in instruction.lower())
        assert found_traits >= 3, f"Expected at least 3 personality traits, found {found_traits}"

    def test_supervisor_delegation_instructions(self):
        """Test that instruction explains when to delegate to each agent."""
        instruction = supervisor_agent.instruction

        # Should mention when to delegate to researcher
        assert "researcher" in instruction.lower()
        assert "research" in instruction.lower()

        # Should mention when to delegate to developer
        assert "developer" in instruction.lower()
        assert "develop" in instruction.lower() or "build" in instruction.lower()

        # Should have decision matrix or guidance
        assert "when to" in instruction.lower() or "delegate to" in instruction.lower()

    def test_supervisor_agent_export(self):
        """Test that supervisor_agent can be imported."""
        # This test verifies the agent can be imported from the0.agent
        from the0.agent import supervisor_agent as imported_agent

        assert imported_agent is not None
        assert imported_agent.name == "the0"
        assert imported_agent is supervisor_agent  # Should be same instance

    def test_supervisor_workflow_phases(self):
        """Test that instruction includes 4 workflow phases."""
        instruction = supervisor_agent.instruction

        # Should mention all 4 phases
        phases = [
            "initial consultation" in instruction.lower() or "phase 1" in instruction.lower(),
            "research" in instruction.lower() and ("phase 2" in instruction.lower() or "delegate to researcher" in instruction.lower()),
            "development" in instruction.lower() and ("phase 3" in instruction.lower() or "delegate to developer" in instruction.lower()),
            "review" in instruction.lower() or "delivery" in instruction.lower() or "phase 4" in instruction.lower(),
        ]

        assert sum(phases) >= 3, "Instruction should cover workflow phases (consultation, research, development, delivery)"

    def test_supervisor_state_management(self):
        """Test that instruction mentions reading agent results from state."""
        instruction = supervisor_agent.instruction

        # Should mention session state
        assert "session.state" in instruction or "state.get" in instruction

        # Should mention research_data and bot_metadata
        assert "research_data" in instruction
        assert "bot_metadata" in instruction


class TestSupervisorConstants:
    """Test supervisor constants and configuration."""

    def test_supervisor_description_constant(self):
        """Test SUPERVISOR_DESCRIPTION constant exists and is used."""
        assert SUPERVISOR_DESCRIPTION is not None
        assert isinstance(SUPERVISOR_DESCRIPTION, str)
        assert len(SUPERVISOR_DESCRIPTION) > 50
        assert supervisor_agent.description == SUPERVISOR_DESCRIPTION

    def test_supervisor_instruction_constant(self):
        """Test SUPERVISOR_INSTRUCTION constant exists and is used."""
        assert SUPERVISOR_INSTRUCTION is not None
        assert isinstance(SUPERVISOR_INSTRUCTION, str)
        assert len(SUPERVISOR_INSTRUCTION) > 2500
        assert supervisor_agent.instruction == SUPERVISOR_INSTRUCTION
```

## Validation Loop

### Level 1: Syntax & Style

```bash
# CRITICAL: Run formatting first
make format
# OR: black api/ the0/ tests/

# Linting
make lint
# OR: flake8 api/ the0/ tests/

# Python syntax validation
python -m py_compile the0/agent.py
python -m py_compile api/agent_service.py
python -m py_compile tests/the0/test_supervisor.py

# Verify imports work
python -c "from the0.agent import supervisor_agent; print(supervisor_agent.name)"
python -c "from the0.agents.researcher import researcher_agent; from the0.agents.developer import developer_agent; print('Sub-agents import OK')"
```

### Level 2: Unit Tests

```bash
# Run all tests
make test
# OR: pytest

# Run supervisor tests specifically
pytest tests/the0/test_supervisor.py -v

# Run tests with verbose output
pytest -v

# Run tests with coverage
pytest --cov=the0 --cov=api --cov-report=html

# Verify no regressions in existing tests
pytest tests/the0/agents/test_researcher.py -v
pytest tests/the0/agents/test_developer.py -v
pytest tests/api/ -v
```

### Level 3: Integration Tests

```bash
# Start development server
make dev
# OR: uvicorn api.main:app --reload --host 0.0.0.0 --port 8000

# Test health endpoint (basic sanity check)
curl http://localhost:8000/health

# Test chat endpoint with supervisor
curl -X POST http://localhost:8000/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "Hello! Can you help me build a trading bot?"}'

# Expected: Alfred-style greeting from supervisor

# Test streaming endpoint
curl -N -X POST http://localhost:8000/chat/stream \
  -H "Content-Type: application/json" \
  -d '{"message": "What are the best libraries for technical indicators in Python?"}'

# Expected: Supervisor delegates to researcher (may see streaming research results)

# Test session management
curl http://localhost:8000/chat/sessions

# Expected: Sessions list (backward compatible)
```

### Level 4: Agent Configuration Validation

```bash
# Python interactive validation
python3 << 'EOF'
from the0.agent import supervisor_agent
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent

# Verify configuration
print(f"✓ Supervisor name: {supervisor_agent.name}")
print(f"✓ Supervisor model: {supervisor_agent.model}")
print(f"✓ Sub-agents count: {len(supervisor_agent.sub_agents)}")
print(f"✓ Tools count: {len(supervisor_agent.tools)}")

# Verify sub-agents
sub_names = [agent.name for agent in supervisor_agent.sub_agents]
print(f"✓ Sub-agents: {sub_names}")
assert "researcher" in sub_names
assert "developer" in sub_names

# Verify tools
tool_names = []
for tool in supervisor_agent.tools:
    if hasattr(tool, "__name__"):
        tool_names.append(tool.__name__)
    elif hasattr(tool, "func_declarations"):
        for decl in tool.func_declarations:
            if hasattr(decl, "__name__"):
                tool_names.append(decl.__name__)
print(f"✓ Tools: {tool_names}")
assert "list_documentation" in tool_names
assert "get_documentation" in tool_names

# Verify no operational tools
forbidden = ["tavily_search", "browse_url", "save_artifact", "deploy_bot"]
assert not any(f in tool_names for f in forbidden), f"Found forbidden tools in {tool_names}"

print("\n✅ All validation checks passed!")
EOF
```

## Final Validation Checklist

### Code Quality
- [ ] Code formatting applied: `make format`
- [ ] Linting passes: `make lint`
- [ ] Python syntax is valid: `python -m py_compile the0/agent.py api/agent_service.py`
- [ ] No import errors

### Agent Configuration
- [ ] `supervisor_agent` has name="the0"
- [ ] `supervisor_agent` has model="gemini-2.5-flash"
- [ ] `sub_agents` list contains exactly 2 agents (researcher, developer)
- [ ] `tools` list contains exactly 2 tools (list_documentation, get_documentation)
- [ ] No operational tools on supervisor (tavily_search, browse_url, save_artifact, deploy_bot)
- [ ] SUPERVISOR_DESCRIPTION is clear and delegation-focused
- [ ] SUPERVISOR_INSTRUCTION is >2500 characters and >250 lines

### Testing
- [ ] All tests pass: `make test`
- [ ] Supervisor tests pass: `pytest tests/the0/test_supervisor.py`
- [ ] No regressions in existing tests
- [ ] Test coverage >80% on the0/agent.py

### Integration
- [ ] AgentService imports supervisor_agent correctly
- [ ] Development server runs: `make dev`
- [ ] Health endpoint responds: `curl http://localhost:8000/health`
- [ ] Chat endpoint works with supervisor
- [ ] Session management unchanged

### Backward Compatibility
- [ ] API endpoints unchanged (no breaking changes)
- [ ] Database schema unchanged (no migrations)
- [ ] User-facing behavior seamless
- [ ] Existing sessions continue to work

### Documentation
- [ ] SUPERVISOR_INSTRUCTION includes all required sections
- [ ] Alfred personality maintained throughout
- [ ] Delegation decision matrix present
- [ ] Example delegation flows included
- [ ] Workflow phases explained (4 phases)
- [ ] State management documented

---

## Anti-Patterns to Avoid

**Agent Configuration:**
- ❌ Don't give supervisor operational tools (it orchestrates, doesn't execute)
- ❌ Don't skip sub_agents parameter (breaks multi-agent system)
- ❌ Don't make description too vague (hurts AutoFlow delegation quality)
- ❌ Don't write short instructions (<250 lines won't cover orchestration properly)
- ❌ Don't forget to remove Runner/services from agent.py (they belong in agent_service.py)

**Delegation:**
- ❌ Don't explicitly call transfer_to_agent() (AutoFlow handles it via descriptions)
- ❌ Don't give unclear agent descriptions (AutoFlow needs clarity for routing)
- ❌ Don't skip delegation examples in instructions (LLM learns from examples)
- ❌ Don't forget to explain when to delegate to each agent

**Instructions:**
- ❌ Don't lose Alfred personality (it's core to user experience)
- ❌ Don't skip workflow phases documentation
- ❌ Don't forget to explain state management (how to read agent results)
- ❌ Don't omit example delegation flows (show, don't just tell)

**Testing:**
- ❌ Don't skip sub-agent tests (verify hierarchy is correct)
- ❌ Don't skip tool reduction tests (verify operational tools removed)
- ❌ Don't skip instruction completeness tests (verify length and sections)
- ❌ Don't skip integration tests (verify backward compatibility)

**Imports:**
- ❌ Don't forget to update agent_service.py import
- ❌ Don't leave old root_agent references anywhere
- ❌ Don't break existing imports (maintain backward compatibility)

---

## Additional Context from Research

### Google ADK Multi-Agent System Key Learnings

1. **LLM-Driven Delegation (AutoFlow)**:
   - Enabled by default when sub_agents present
   - LLM considers query + agent descriptions to route automatically
   - No need for explicit `transfer_to_agent()` calls in most cases
   - Agent descriptions are CRITICAL for routing quality

2. **Agent Hierarchy**:
   - Created via `sub_agents` parameter on parent
   - ADK automatically sets `parent_agent` on children
   - Each agent can only have ONE parent (ValueError otherwise)
   - Transfer scope: parent, siblings, own sub-agents

3. **Shared Session State**:
   - All agents in hierarchy share `session.state` dictionary
   - Sub-agents write results to state (research_data, bot_metadata)
   - Parent reads from state to maintain context
   - Enables passive communication between agents

4. **Supervisor Best Practices**:
   - Clear, distinct descriptions on all target agents
   - Focused supervisor instructions explaining when to delegate
   - Leverage state keys for result passing
   - Include example delegation flows in instructions

5. **Tool Distribution**:
   - Supervisor: Coordination tools only (documentation lookup)
   - Sub-agents: Operational tools (research, development)
   - Avoid tool overlap (clear separation of responsibilities)

### References

- Google ADK Multi-Agent Systems: https://google.github.io/adk-docs/agents/multi-agents/
- Google ADK Python Docs: https://github.com/google/adk-python
- Codebase CLAUDE.md: Multi-Agent System (MAS) Architecture section
- Researcher Agent: the0/agents/researcher.py
- Developer Agent: the0/agents/developer.py

---

## PRP Confidence Score

**Score: 9/10**

**Strengths:**
- ✅ Comprehensive context from Google ADK official docs
- ✅ Real codebase patterns from researcher and developer agents
- ✅ Clear, executable implementation tasks with pseudocode
- ✅ Detailed validation gates at multiple levels
- ✅ Anti-patterns section prevents common mistakes
- ✅ Testing patterns from existing test files
- ✅ Backward compatibility considerations
- ✅ Example agent instructions and delegation flows included

**Minor Risks (-1 point):**
- Instruction quality depends on prompt engineering (250+ lines comprehensive instructions)
- AutoFlow delegation quality depends on description clarity (can iterate if needed)
- Integration testing may reveal edge cases in agent coordination

**Mitigation:**
- Follow instruction template exactly from pseudocode
- Use validation loops to test and refine descriptions if delegation issues arise
- Comprehensive testing at multiple levels (unit, integration, manual)

**Why High Confidence:**
This PRP provides everything needed for one-pass implementation:
1. Exact code to write (constants, agent definition, imports)
2. Existing patterns to follow (researcher.py, developer.py)
3. Official Google ADK documentation and best practices
4. Comprehensive testing strategy with examples
5. Validation gates at every level (syntax, unit, integration)
6. Clear anti-patterns to avoid

An AI agent with access to this PRP can successfully implement the supervisor transformation without additional research or clarification.

---

**Created**: 2025-11-12
**Story**: Story 4 - Supervisor Agent Transformation
**Dependencies**: Story 2 (Researcher Agent), Story 3 (Developer Agent)
**Next**: Story 5 (State Management), Story 6 (Testing & Validation)
