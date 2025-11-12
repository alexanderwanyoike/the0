# PRP: Researcher Agent - Quantitative Research Specialist

**Status:** Ready for Implementation
**Epic:** Specialized Agent Development
**Estimated Effort:** 8-10 hours
**Dependencies:** Story 1 (Tavily Integration - COMPLETED)
**Target:** Story 2

---

## Goal

Create a specialized Researcher Agent that performs comprehensive quantitative research and information gathering for trading bot development. The agent will use Tavily search and web browsing to research trading strategies, APIs, libraries, and technical documentation, returning structured findings with citations stored in session state for other agents to consume.

**Specific End State:**
- New agent file `the0/agents/researcher.py` with complete agent definition
- Agent integrated into ADK multi-agent system (ready for Story 4 supervisor integration)
- Comprehensive instructions (150+ lines) covering research methodology, tool usage, output format, and citation requirements
- Session state management for research findings using state key `research_data`
- Comprehensive test suite (`tests/the0/agents/test_researcher.py`) with >80% coverage
- Documentation updated in CLAUDE.md

---

## Why

- **Specialization**: Dedicated research agent improves research quality through focused expertise and optimized prompts
- **Clear Responsibility**: Separates research concerns from bot development, enabling parallel development and clearer debugging
- **Expertise**: Research-specific prompts and workflows produce better-sourced, more accurate findings
- **Scalability**: Foundation for Multi-Agent System (MAS) enables future addition of Developer, Optimizer, and Monitor agents
- **Quality**: Structured output with citations ensures downstream agents have verifiable, traceable information
- **Integration**: Sets architectural pattern for all future specialized agents in the system

---

## What

Create a Researcher Agent (`the0/agents/researcher.py`) that:

1. **Research Capabilities:**
   - Performs comprehensive web research using Tavily AI search
   - Analyzes and verifies information from multiple authoritative sources
   - Researches trading strategies, APIs, libraries, and technical documentation
   - Uses browse_url for deep-dive documentation reading
   - Consults internal the0 platform documentation

2. **Output & Quality:**
   - Returns structured findings with markdown formatting
   - Provides citations for all factual claims using footnote format `[^1]`
   - Includes AI-generated summary, key findings, detailed analysis, recommendations, and sources
   - Assesses source authority (official docs > academic > blogs > forums)
   - Flags information freshness and version numbers

3. **State Management:**
   - Stores research findings in session state using key `research_data`
   - Uses structured JSON format with query, summary, findings, recommendations, sources, timestamp
   - Enables downstream agents (Developer, Optimizer) to consume research without re-searching

4. **Agent Configuration:**
   - Name: "researcher"
   - Model: "gemini-2.5-flash"
   - Tools: `tavily_search`, `browse_url`, `list_documentation`, `get_documentation`
   - Description optimized for LLM-driven delegation (Story 4)
   - Instructions covering research workflow, quality standards, and output format

### Success Criteria

- [x] Agent file created at `the0/agents/researcher.py`
- [x] Agent uses `gemini-2.5-flash` model as per codebase standards
- [x] Four tools assigned: `tavily_search`, `browse_url`, `list_documentation`, `get_documentation`
- [x] Comprehensive instructions (150+ lines minimum) covering:
  - Role and core responsibilities
  - Research workflow (7 steps: understand → formulate → search → deep-dive → consult → verify → structure)
  - Tool usage guidelines (when to use Tavily vs browse_url vs documentation)
  - Output format requirements (Summary, Findings, Analysis, Recommendations, Sources)
  - Citation requirements (all claims must have [^N] citations)
  - Quality standards (source authority hierarchy, information freshness, completeness)
  - Example research outputs (API research, strategy research)
- [x] Research findings stored in session state with key `research_data`
- [x] Structured JSON format defined (query, summary, findings, recommendations, sources, timestamp)
- [x] Unit tests achieve >80% coverage
- [x] Agent configuration tests (name, model, tools, description, instruction completeness)
- [x] Documentation complete and CLAUDE.md updated
- [x] Code formatted with `make format`
- [x] Linting passes with `make lint`
- [x] All tests pass with `make test`

---

## All Needed Context

### Documentation & References

**CRITICAL - MUST READ BEFORE IMPLEMENTATION:**

```yaml
# Google ADK Documentation (PRIMARY)
- url: https://google.github.io/adk-docs/agents/multi-agents/
  why: Multi-agent system architecture, sub_agents, agent transfer, state sharing
  key_sections:
    - Agent Hierarchy & Composition (parent-child relationships)
    - LLM-Driven Delegation (transfer_to_agent pattern)
    - Shared Session State (communication between agents)
    - Common Multi-Agent Patterns (Coordinator/Dispatcher, Sequential Pipeline)

- url: https://google.github.io/adk-docs/sessions/state/
  why: Session state management, reading/writing state, state injection, persistence
  key_sections:
    - State Prefixes & Scope (user:, app:, temp:)
    - Reading & Writing State (output_key, EventActions, context.state)
    - State Injection in Instructions ({key} templating)
    - State Sharing Between Agents (shared invocation context)
    - Critical Warning (avoid direct session.state modification)

- url: https://google.github.io/adk-docs/agents/
  why: Agent creation, configuration, and best practices
  key_sections:
    - Agent initialization with name, model, description, instruction, tools
    - Tool assignment and tool context patterns

# Tavily AI Search Documentation
- url: https://docs.tavily.com
  why: Tavily API usage, search parameters, response format, best practices
  note: Already integrated in Story 1, review web_browser.py for implementation

# Project Documentation (LOCAL - READ THESE FIRST)
- file: /home/alexander/Code/Apps/the0/services/the0-ai/CLAUDE.md
  why: Architecture overview, MAS section, agent patterns, state management schema
  key_sections:
    - Multi-Agent System (MAS) Architecture
    - Agent Hierarchy (Supervisor → Researcher → Developer)
    - State Management (research_data schema)
    - Tool Distribution table
    - Design Principles for Agents

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/agent.py
  why: Current supervisor agent (will become supervisor in Story 4)
  note: Study instruction format, tool usage patterns, citation requirements

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/agents/base.py
  why: Shared constants and utilities for all agents
  items_to_use:
    - DEFAULT_MODEL = "gemini-2.5-flash"
    - STATE_KEY_RESEARCH = "research_data"
    - format_citations() utility function

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/tools/web_browser.py
  why: Tavily search and browse_url tool implementations
  note: These tools are already created, just import and use

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/tools/documentation.py
  why: Internal documentation tool implementations
  note: list_documentation and get_documentation already created

- file: /home/alexander/Code/Apps/the0/services/the0-ai/tests/the0/tools/test_web_browser.py
  why: Testing patterns for async tools with mocking
  patterns_to_follow:
    - pytest.mark.asyncio for async tests
    - AsyncMock for mocking async functions
    - patch decorators for external dependencies
    - Test both success and error scenarios

# Story File (Implementation Requirements)
- file: /home/alexander/Code/Apps/the0/services/the0-ai/stories/story-2-researcher-agent.md
  why: Detailed implementation requirements, acceptance criteria, examples
  critical_sections:
    - Agent Definition example (lines 105-358)
    - State Storage Format example (lines 360-405)
    - Testing Strategy (lines 460-499)
```

---

### Current Codebase Structure

```bash
the0-ai/
├── the0/
│   ├── agent.py                    # Current supervisor agent (Story 0)
│   ├── agents/
│   │   ├── __init__.py            # Package init, exports base
│   │   ├── base.py                # Shared constants (DEFAULT_MODEL, STATE_KEY_RESEARCH)
│   │   └── researcher.py          # ← CREATE THIS (Story 2)
│   └── tools/
│       ├── __init__.py
│       ├── save_artifact.py       # Bot file creation tool
│       ├── web_browser.py         # Tavily search + browse_url (Story 1)
│       ├── documentation.py       # Internal docs tools
│       └── deploy_bot.py          # Bot deployment tool
├── tests/
│   └── the0/
│       ├── agents/
│       │   └── test_researcher.py  # ← CREATE THIS (Story 2)
│       └── tools/
│           ├── test_web_browser.py # Reference for testing patterns
│           ├── test_save_artifact.py
│           └── test_deploy_bot.py
├── api/                           # FastAPI layer (not modified in this story)
├── alembic/                       # Database migrations (not modified)
├── CLAUDE.md                      # ← UPDATE with Researcher agent docs
├── requirements.txt               # Dependencies (no new deps needed)
├── Makefile                       # Dev commands (format, lint, test)
└── pytest.ini                     # Test configuration
```

---

### Desired Structure After Implementation

```bash
the0-ai/
├── the0/
│   ├── agents/
│   │   ├── __init__.py            # UPDATE: Export researcher_agent
│   │   ├── base.py                # (no changes)
│   │   └── researcher.py          # ✨ NEW: Researcher agent definition
│   │       # Responsibilities:
│   │       # - Agent definition with google.adk.agents.Agent()
│   │       # - 150+ line instruction prompt covering research workflow
│   │       # - Tool imports: tavily_search, browse_url, list_documentation, get_documentation
│   │       # - State management examples in docstrings
│   │       # - Export: researcher_agent instance
│   └── tools/
│       └── (no changes - tools already exist)
├── tests/
│   └── the0/
│       └── agents/
│           └── test_researcher.py  # ✨ NEW: Comprehensive test suite
│               # Test Functions:
│               # - test_researcher_configuration() - verify name, model, tools, description
│               # - test_researcher_instruction_completeness() - check instruction length, keywords
│               # - test_researcher_tools_available() - verify all 4 tools attached
│               # - test_researcher_state_key_constant() - verify STATE_KEY_RESEARCH usage
│               # - test_researcher_exports() - verify agent can be imported
│               # Target: >80% coverage
├── CLAUDE.md                      # ✨ UPDATE: Add Researcher agent to MAS section
│   # Updates:
│   # - Multi-Agent System section: Add Researcher agent description
│   # - Tool Distribution table: Update with Researcher tools
│   # - State Management: Document research_data schema
│   # - Agent Hierarchy diagram: Include Researcher
└── (no other files modified)
```

---

### Known Gotchas & Library Quirks

```yaml
google_adk:
  - "CRITICAL: Agent constructor is google.adk.agents.Agent (not LlmAgent)"
  - "Import: from google.adk.agents import Agent"
  - "Agent automatically gets LLM capabilities when model parameter is set"
  - "CRITICAL: Tools must be callable objects/functions, not Tool instances (ADK wraps them)"
  - "Tool assignment: tools=[tavily_search, browse_url, ...] (pass functions directly)"
  - "Agent description is critical - LLM uses it to decide delegation in Story 4"
  - "Description should be clear, specific, and explain when to use this agent"
  - "Instruction is a multi-line string - use triple quotes for readability"
  - "CRITICAL: State management in instructions - do NOT use state injection yet (Story 5)"
  - "Agent name must be unique within the agent hierarchy"
  - "Model 'gemini-2.5-flash' is the standard - use DEFAULT_MODEL constant"

state_management:
  - "CRITICAL: Use STATE_KEY_RESEARCH constant from base.py"
  - "State key: 'research_data' for research findings"
  - "DO NOT implement state writing yet - Story 5 will add that"
  - "Document the expected state schema in instruction and docstrings"
  - "State is shared across all agents in same session (future Story 4)"
  - "JSON structure must be serializable (no datetime objects - use ISO strings)"
  - "Keep state size reasonable (<50KB) - summarize when needed"

tools:
  - "CRITICAL: Do NOT create new tools - all 4 tools already exist"
  - "Import from: the0.tools.web_browser (tavily_search, browse_url)"
  - "Import from: the0.tools.documentation (list_documentation, get_documentation)"
  - "Tools are already async and handle ToolContext correctly"
  - "Tavily search returns markdown with footnote citations [^1], [^2]"
  - "browse_url returns markdown with source citation at top"
  - "list_documentation returns formatted list with paths and descriptions"
  - "get_documentation returns full markdown content with metadata"

testing:
  - "CRITICAL: Use pytest with pytest.mark.asyncio for async tests"
  - "Agent configuration tests are SYNCHRONOUS (no async needed)"
  - "Test agent properties: name, model, description, instruction, tools"
  - "Use assert statements, not mock tool invocations (unit tests only)"
  - "Test instruction completeness: len(agent.instruction) > 150 characters"
  - "Test instruction keywords: 'research', 'citation', 'Tavily', 'sources'"
  - "Mock external dependencies if needed (should not be needed for config tests)"
  - "Target: >80% code coverage on researcher.py"
  - "Run tests: make test or pytest tests/the0/agents/test_researcher.py"

code_quality:
  - "CRITICAL: Run make format FIRST before make lint"
  - "Black formatting with 88 character line length"
  - "Flake8 linting must pass (no warnings)"
  - "Use triple-quoted strings for long instructions"
  - "Add comprehensive docstrings to all public functions/classes"
  - "Type hints not strictly required but encouraged"
  - "Follow existing patterns in the0/agent.py for instruction structure"

common_mistakes:
  - "❌ DO NOT create tools - they already exist, just import them"
  - "❌ DO NOT implement state writing logic yet - that's Story 5"
  - "❌ DO NOT modify the0/agent.py - that's Story 4 (Supervisor Transformation)"
  - "❌ DO NOT use LlmAgent - use Agent with model parameter"
  - "❌ DO NOT wrap tools in Tool() - pass functions directly"
  - "❌ DO NOT create sub_agents parameter - that's Story 4"
  - "❌ DO NOT implement transfer_to_agent - that's Story 4"
  - "❌ DO NOT add new dependencies to requirements.txt - everything exists"
```

---

## Implementation Blueprint

### Task List - Execute in Order

```yaml
Task 1: Create Researcher Agent Definition File
FILE: the0/agents/researcher.py
ACTIONS:
  1. CREATE new file the0/agents/researcher.py
  2. ADD imports:
     - from google.adk.agents import Agent
     - from the0.agents.base import DEFAULT_MODEL, STATE_KEY_RESEARCH
     - from the0.tools.web_browser import tavily_search, browse_url
     - from the0.tools.documentation import list_documentation, get_documentation
  3. DEFINE researcher_agent using Agent() constructor
  4. SET agent properties:
     - name: "researcher"
     - model: DEFAULT_MODEL (gemini-2.5-flash)
     - description: Clear, specific description for LLM delegation
     - instruction: 150+ line research methodology prompt (see pseudocode)
     - tools: [tavily_search, browse_url, list_documentation, get_documentation]
  5. ADD comprehensive module docstring explaining agent purpose
  6. DOCUMENT research_data state schema in docstrings
  7. EXPORT researcher_agent at module level
VALIDATION:
  - File exists: the0/agents/researcher.py
  - Can import: from the0.agents.researcher import researcher_agent
  - Agent has all required properties
  - Instruction length >= 150 lines

Task 2: Update Agents Package Init
FILE: the0/agents/__init__.py
ACTIONS:
  1. ADD import: from the0.agents.researcher import researcher_agent
  2. ADD to __all__: "researcher_agent"
  3. KEEP existing exports: "base"
VALIDATION:
  - Can import: from the0.agents import researcher_agent
  - No import errors when importing package

Task 3: Create Comprehensive Test Suite
FILE: tests/the0/agents/test_researcher.py
ACTIONS:
  1. CREATE new file tests/the0/agents/test_researcher.py
  2. ADD imports:
     - import pytest
     - from the0.agents.researcher import researcher_agent
     - from the0.agents.base import DEFAULT_MODEL, STATE_KEY_RESEARCH
  3. CREATE test class: TestResearcherAgent
  4. IMPLEMENT test functions:
     - test_researcher_configuration(): name, model, description exist
     - test_researcher_instruction_completeness(): length, keywords
     - test_researcher_tools_assigned(): verify 4 tools
     - test_researcher_model_constant(): verify DEFAULT_MODEL usage
     - test_researcher_state_key(): verify STATE_KEY_RESEARCH in docstring
     - test_researcher_description_quality(): check for key delegation phrases
  5. ADD comprehensive docstrings to all test functions
  6. TARGET: >80% coverage of researcher.py
VALIDATION:
  - pytest tests/the0/agents/test_researcher.py passes
  - Coverage: pytest --cov=the0/agents/researcher --cov-report=term
  - All tests green, >80% coverage achieved

Task 4: Update CLAUDE.md Documentation
FILE: CLAUDE.md
ACTIONS:
  1. LOCATE "Multi-Agent System (MAS) Architecture" section
  2. UPDATE Agent Hierarchy diagram to include Researcher
  3. UPDATE Tool Distribution table:
     - Add Researcher column
     - Mark tavily_search, browse_url, list_documentation, get_documentation as ✅
     - Mark save_artifact, deploy_bot as ❌
  4. ADD State Management subsection for research_data schema
  5. UPDATE Design Principles to reference Researcher as example
  6. ADD entry to Future Agents section noting Researcher is now complete
VALIDATION:
  - CLAUDE.md mentions Researcher agent
  - Tool Distribution table updated correctly
  - research_data schema documented
  - No broken markdown formatting

Task 5: Code Quality Validation
COMMANDS:
  1. RUN: make format (Black formatting)
  2. RUN: make lint (Flake8 linting)
  3. FIX any linting errors
  4. VERIFY no warnings remain
VALIDATION:
  - make format exits successfully
  - make lint shows 0 errors, 0 warnings
  - Code follows Black style (88 char lines)

Task 6: Test Execution Validation
COMMANDS:
  1. RUN: make test (full test suite)
  2. RUN: pytest tests/the0/agents/test_researcher.py -v (specific tests)
  3. RUN: pytest --cov=the0/agents/researcher --cov-report=html
  4. VERIFY coverage >80%
VALIDATION:
  - All tests pass
  - No test failures or errors
  - Coverage report shows >80% for researcher.py
  - Coverage HTML report generated successfully

Task 7: Integration Verification
COMMANDS:
  1. TEST import: python -c "from the0.agents import researcher_agent; print(researcher_agent.name)"
  2. VERIFY agent properties accessible
  3. VERIFY tools list has 4 items
  4. CHECK instruction completeness
VALIDATION:
  - Import succeeds without errors
  - Agent name prints "researcher"
  - Tools list length == 4
  - Instruction is comprehensive (>1000 characters)
```

---

### Pseudocode - Agent Definition

```python
# FILE: the0/agents/researcher.py
"""
Researcher Agent - Quantitative research specialist for trading bot development.

This agent performs comprehensive web research using Tavily AI search and web browsing
to research trading strategies, APIs, libraries, and technical documentation.

Research findings are stored in session state for consumption by other agents.

State Schema (research_data):
{
    "query": str,                    # Original research request
    "summary": str,                  # Executive summary (2-3 sentences)
    "findings": [                    # Key findings list
        {
            "point": str,            # Finding description
            "source": str,           # Source URL
            "confidence": str        # "high"|"medium"|"low"
        }
    ],
    "recommendations": [str],        # Actionable recommendations
    "sources": [                     # All sources used
        {
            "title": str,
            "url": str,
            "relevance": str,        # Why this source matters
            "published": str         # Publication date (if available)
        }
    ],
    "timestamp": str                 # ISO 8601 timestamp
}

Example Usage:
    from the0.agents.researcher import researcher_agent

    # Agent will be integrated into supervisor in Story 4
    # For now, can be tested independently with ADK Runner
"""

from google.adk.agents import Agent
from the0.agents.base import DEFAULT_MODEL, STATE_KEY_RESEARCH
from the0.tools.web_browser import tavily_search, browse_url
from the0.tools.documentation import list_documentation, get_documentation


# Agent Description - Critical for LLM-driven delegation (Story 4)
# Must be clear, specific, and explain when to use this agent
RESEARCHER_DESCRIPTION = (
    "Quantitative research specialist that performs web searches, "
    "analyzes market data, and researches trading strategies, APIs, "
    "and technical documentation. Returns structured research findings "
    "with citations. Use this agent when you need to gather information "
    "about trading platforms, strategies, libraries, or market analysis "
    "before building a bot."
)


# Agent Instruction - Research Methodology (150+ lines required)
# Structure: Role → Workflow → Tools → Output Format → Quality Standards → Examples
RESEARCHER_INSTRUCTION = """
You are the Researcher agent - a quantitative research specialist on the the0 team.

## Core Responsibilities

1. Perform comprehensive web research using Tavily search
2. Analyze and verify information from multiple sources
3. Research trading strategies, market data, and technical documentation
4. Investigate API capabilities and library features
5. Provide structured, citation-rich research findings

## Research Workflow

### Step 1: Understand the Research Request
When you receive a research task from the Supervisor, carefully analyze:
- What information is being requested?
- What is the specific scope (strategy type, platform, time period)?
- What level of depth is needed?
- Who is the end user (beginner, intermediate, expert)?

### Step 2: Formulate Effective Search Queries
Create targeted search queries that:
- Include relevant context and intent
- Specify technical requirements (language, framework, version)
- Target recent information when time-sensitive
- Focus on authoritative sources

Examples:
- ✅ "Python momentum trading strategy implementation 2025 Binance API"
- ✅ "Alpaca API v2 real-time market data WebSocket tutorial"
- ❌ "trading bot" (too vague)
- ❌ "how to trade" (not technical enough)

### Step 3: Use Tavily Search for Broad Research
Use the `tavily_search` tool as your primary research method:
- For comprehensive research: `search_depth="advanced"`
- For quick lookups: `search_depth="basic"`
- Request AI-generated answers: `include_answer=True`
- Typical max_results: 5-10 depending on topic breadth

Tavily provides:
- Relevance-ranked results
- AI-generated summaries
- Clean, structured content
- Automatic citation URLs with footnote format [^1], [^2]

### Step 4: Deep Dive with browse_url
When Tavily results mention important documentation or tutorials, use `browse_url` to:
- Read full documentation pages
- Extract code examples and API specifications
- Verify version numbers and compatibility
- Get detailed implementation guidance

### Step 5: Consult Internal Documentation
**ALWAYS check the0 platform documentation first:**
- Use `list_documentation` to see available guides
- **CRITICAL**: For bot development, prioritize:
  - `custom-bot-development/quick-start-guide.md`
  - `custom-bot-development/backtesting.md`
  - Platform-specific guides
- Use `get_documentation` to read specific documents

### Step 6: Verify and Cross-Reference
For critical information:
- Verify across multiple sources
- Check publication dates (prefer recent)
- Note version numbers and compatibility
- Flag conflicting information with context
- Assess source authority (official docs > blog posts)

### Step 7: Structure Your Findings
Format research output as follows:

## Research Summary
[2-3 sentence executive summary of key findings]

## Key Findings
- Finding 1: [Specific insight] [^1]
- Finding 2: [Specific insight] [^2]
- Finding 3: [Specific insight] [^3]
[Continue as needed]

## Detailed Analysis
[In-depth analysis organized by topic]

### Topic 1: [e.g., API Capabilities]
[Detailed information with citations]

### Topic 2: [e.g., Implementation Approach]
[Detailed information with citations]

## Recommendations
Based on the research, I recommend:
1. [Actionable recommendation with rationale]
2. [Actionable recommendation with rationale]
3. [Actionable recommendation with rationale]

## References
[^1]: [Source Title](URL) - Brief description and relevance
[^2]: [Source Title](URL) - Brief description and relevance
[Continue for all sources]

---

## Citation Requirements

**CRITICAL**: Every factual claim MUST have a citation:
- Use footnote format: [^1], [^2], [^3]
- Cite immediately after claim: "Binance supports WebSockets[^1]"
- Include source URLs in References section at the end
- Prefer official documentation and authoritative sources
- Note version numbers: "as of Python 3.11[^2]"
- Group citations: "Multiple studies confirm this[^1][^2][^3]"

## Research Quality Standards

### Source Authority (Prioritize in this order)
1. Official documentation (API docs, library docs)
2. Academic papers and research publications
3. Established technical blogs (Medium, Dev.to by experts)
4. Stack Overflow for specific technical issues
5. General forums and discussions (use cautiously)

### Information Freshness
- Check publication dates
- Prefer recent sources (last 1-2 years for fast-moving tech)
- Note when using older sources: "As of 2023..."
- Flag deprecated information
- Verify current library versions

### Completeness
- Answer all aspects of the research request
- If information is unavailable, explicitly state gaps
- Provide alternatives when preferred option unavailable
- Include pros/cons for important decisions
- Suggest next steps for further research if needed

## Session State Storage

After completing research, store findings in structured format using state key: `{STATE_KEY_RESEARCH}`

Expected JSON structure:
{{
    "query": "Original research request from Supervisor",
    "summary": "Executive summary of findings",
    "findings": [
        {{
            "point": "Key finding or insight",
            "source": "https://source-url.com",
            "confidence": "high|medium|low"
        }}
    ],
    "recommendations": [
        "Actionable recommendation 1",
        "Actionable recommendation 2"
    ],
    "sources": [
        {{
            "title": "Source Title",
            "url": "https://source-url.com",
            "relevance": "Why this source matters",
            "published": "2025-01-15" (if available)
        }}
    ],
    "timestamp": "ISO 8601 timestamp"
}}

NOTE: State writing will be implemented in Story 5. For now, document findings in text output.

## When to Escalate to Supervisor

Return to Supervisor when:
- Research is complete and findings are structured
- Research scope is too broad (ask for clarification)
- Critical information is completely unavailable (report gaps)
- Conflicting information requires user decision
- Additional deep investigation needed beyond scope

## Examples of Good Research

### Example 1: API Research
**Request**: "Research Binance Spot API for real-time price data"

**Good Response**:
## Research Summary
Binance Spot API v3 provides comprehensive real-time market data via REST and WebSocket APIs. Python bindings available via python-binance library (v1.0.19). Rate limits: 1200 requests/min for REST, unlimited for WebSocket.

## Key Findings
- REST API endpoint: `/api/v3/ticker/price` for current prices[^1]
- WebSocket stream: `wss://stream.binance.com:9443/ws/<symbol>@trade` for real-time trades[^2]
- Python library: `python-binance` recommended, actively maintained[^3]
- Rate limits: 1200 weight per minute for REST API, no limit for WebSocket[^1]

## Detailed Analysis
### API Architecture
Binance provides both REST and WebSocket APIs for different use cases. REST is suitable for periodic data fetching (backtesting, position checks), while WebSocket excels at real-time streaming (live price monitoring, order execution).[^1][^2]

### Python Integration
The `python-binance` library (v1.0.19 as of January 2025) provides a clean async/await interface for both REST and WebSocket endpoints.[^3] It handles authentication, reconnection logic, and rate limiting automatically.

## Recommendations
1. Use WebSocket for real-time data to avoid rate limits
2. Implement python-binance library for easier integration
3. Add reconnection logic for WebSocket disconnections
4. Store API credentials securely in environment variables

## References
[^1]: [Binance API Documentation - Spot](https://binance-docs.github.io/apidocs/spot/en/) - Official API specification
[^2]: [Binance WebSocket Streams](https://binance-docs.github.io/apidocs/spot/en/#websocket-market-streams) - Real-time data streaming guide
[^3]: [python-binance PyPI](https://pypi.org/project/python-binance/) - Python library package information

### Example 2: Strategy Research
**Request**: "Research momentum trading strategies suitable for crypto"

**Good Response**:
## Research Summary
Momentum trading strategies for crypto typically use RSI, MACD, and moving averages to identify trend strength. Studies show 65-70% win rates in trending markets but perform poorly in sideways markets.[^1][^2] Key risk: high volatility requires strict stop-losses.

## Key Findings
- RSI Momentum: Buy when RSI crosses above 30, sell above 70[^3]
- MACD Crossover: Signal line crosses indicate momentum shifts[^4]
- Moving Average: 50-day/200-day crossovers for long-term trends[^5]
- Crypto-specific: 24/7 markets require automated execution[^1]
- Volatility: Crypto volatility 3-5x higher than traditional markets[^2]

[Continue with Detailed Analysis, Recommendations, References...]

## Tools Available

1. **tavily_search**: Primary research tool
   - Use for: Broad information gathering, current data, comparisons
   - Best for: Technical documentation, API research, strategy overviews
   - Returns: AI-generated summary + ranked results with footnote citations

2. **browse_url**: Deep dive tool
   - Use for: Reading full documentation pages, detailed tutorials
   - Best for: Implementation details, code examples, specifications
   - Returns: Markdown content with source citation

3. **list_documentation**: Internal docs discovery
   - Use for: Finding relevant the0 platform guides
   - Best for: Understanding platform capabilities and standards
   - Returns: Formatted list with paths and descriptions

4. **get_documentation**: Internal docs reading
   - Use for: Reading specific the0 platform documentation
   - Best for: Platform-specific implementation guidance
   - Returns: Full markdown content with metadata

## Important Notes

- ALWAYS cite sources - no citation means no credibility
- Quality over quantity - 3 authoritative sources > 10 mediocre ones
- Be specific - vague research is not helpful
- Structure matters - use the prescribed output format
- Time-sensitive info - note dates and versions
- When in doubt, verify across multiple sources
- Prioritize official documentation and recent sources
- Flag any conflicting information with context

You are an expert researcher. Take pride in delivering thorough, accurate, well-cited research that enables the team to build exceptional trading bots.
""".format(STATE_KEY_RESEARCH=STATE_KEY_RESEARCH)


# Agent Definition
researcher_agent = Agent(
    name="researcher",
    model=DEFAULT_MODEL,
    description=RESEARCHER_DESCRIPTION,
    instruction=RESEARCHER_INSTRUCTION,
    tools=[
        tavily_search,
        browse_url,
        list_documentation,
        get_documentation,
    ],
)
```

---

### Pseudocode - Test Suite

```python
# FILE: tests/the0/agents/test_researcher.py
"""
Unit tests for the Researcher Agent.

Tests agent configuration, instruction completeness, tool assignment,
and integration with the agents package.
"""

import pytest
from the0.agents.researcher import researcher_agent, RESEARCHER_DESCRIPTION, RESEARCHER_INSTRUCTION
from the0.agents.base import DEFAULT_MODEL, STATE_KEY_RESEARCH


class TestResearcherAgent:
    """Test suite for Researcher Agent configuration and setup."""

    def test_researcher_configuration(self):
        """Test researcher agent basic configuration."""
        # Verify agent exists and has correct properties
        assert researcher_agent is not None
        assert researcher_agent.name == "researcher"
        assert researcher_agent.model == DEFAULT_MODEL
        assert researcher_agent.description == RESEARCHER_DESCRIPTION
        assert researcher_agent.instruction == RESEARCHER_INSTRUCTION

    def test_researcher_model_uses_constant(self):
        """Test that agent uses DEFAULT_MODEL constant."""
        assert researcher_agent.model == DEFAULT_MODEL
        assert DEFAULT_MODEL == "gemini-2.5-flash"

    def test_researcher_description_quality(self):
        """Test that description contains key phrases for LLM delegation."""
        description = researcher_agent.description.lower()

        # Check for key delegation phrases
        assert "research" in description
        assert "specialist" in description or "expert" in description
        assert "trading" in description or "market" in description

        # Check length is reasonable (not too short, not too long)
        assert 50 < len(researcher_agent.description) < 500

    def test_researcher_instruction_completeness(self):
        """Test that instruction is comprehensive and includes required sections."""
        instruction = researcher_agent.instruction

        # Check instruction length (minimum 150 lines, ~1000+ characters)
        assert len(instruction) > 1000, "Instruction should be comprehensive (>1000 chars)"
        lines = instruction.split('\n')
        assert len(lines) >= 150, "Instruction should have 150+ lines"

        # Check for required keywords and sections
        instruction_lower = instruction.lower()
        required_keywords = [
            "research", "tavily", "search", "citation", "sources",
            "findings", "recommendations", "quality", "workflow",
            "browse_url", "documentation", "verify", "authority"
        ]
        for keyword in required_keywords:
            assert keyword in instruction_lower, f"Instruction missing keyword: {keyword}"

        # Check for structured sections
        assert "## Core Responsibilities" in instruction or "core responsibilit" in instruction_lower
        assert "## Research Workflow" in instruction or "workflow" in instruction_lower
        assert "## Citation Requirements" in instruction or "citation" in instruction_lower
        assert "## Quality Standards" in instruction or "quality standard" in instruction_lower
        assert "## Examples" in instruction or "example" in instruction_lower

    def test_researcher_tools_assigned(self):
        """Test that all required tools are assigned to the agent."""
        # Verify tools list exists and has correct count
        assert hasattr(researcher_agent, 'tools')
        assert researcher_agent.tools is not None
        assert len(researcher_agent.tools) == 4, "Researcher should have exactly 4 tools"

        # Verify tool names (tools may be wrapped, check by name or function)
        tool_names = set()
        for tool in researcher_agent.tools:
            # ADK may wrap tools, extract function name
            if hasattr(tool, '__name__'):
                tool_names.add(tool.__name__)
            elif hasattr(tool, 'func_declarations'):
                # Tool wrapper object
                for decl in tool.func_declarations:
                    if hasattr(decl, '__name__'):
                        tool_names.add(decl.__name__)

        expected_tools = {'tavily_search', 'browse_url', 'list_documentation', 'get_documentation'}
        assert tool_names == expected_tools, f"Expected tools {expected_tools}, got {tool_names}"

    def test_researcher_state_key_reference(self):
        """Test that instruction references STATE_KEY_RESEARCH constant."""
        # Check that state key constant is used in instruction or documentation
        instruction = researcher_agent.instruction

        # Should reference the state key or research_data
        assert STATE_KEY_RESEARCH in instruction or "research_data" in instruction

    def test_researcher_agent_export(self):
        """Test that researcher_agent can be imported from package."""
        # This test verifies the __init__.py export
        from the0.agents import researcher_agent as exported_agent

        assert exported_agent is not None
        assert exported_agent.name == "researcher"
        assert exported_agent is researcher_agent  # Should be same instance

    def test_researcher_instruction_examples(self):
        """Test that instruction includes research examples."""
        instruction = researcher_agent.instruction

        # Should include example research outputs
        assert "Example" in instruction or "example" in instruction.lower()

        # Should show good vs bad patterns
        assert "✅" in instruction or "good" in instruction.lower()
        assert "❌" in instruction or "bad" in instruction.lower() or "not" in instruction.lower()


class TestResearcherConstants:
    """Test agent constants and configuration."""

    def test_default_model_constant(self):
        """Test DEFAULT_MODEL constant value."""
        assert DEFAULT_MODEL == "gemini-2.5-flash"

    def test_state_key_constant(self):
        """Test STATE_KEY_RESEARCH constant value."""
        assert STATE_KEY_RESEARCH == "research_data"
```

---

### CLAUDE.md Update - Pseudocode

```markdown
# FILE: CLAUDE.md (locate and update these sections)

## Multi-Agent System (MAS) Architecture

the0-ai uses a Multi-Agent System with specialized agents for different aspects of bot creation. The architecture follows a supervisor pattern where a central coordinator delegates tasks to specialist agents.

### Agent Hierarchy

```
┌─────────────────────────────────────────────────┐
│         Supervisor Agent (the0)                 │
│         - Orchestrates workflow                 │
│         - Routes tasks to specialists           │
│         - Maintains personality & context       │
│         - LLM-driven delegation                 │
└────────────┬──────────────────┬─────────────────┘
             │                  │
    ┌────────▼────────┐  ┌─────▼──────────────┐
    │  Researcher      │  │  Developer         │
    │  Agent           │  │  Agent (Story 3)   │
    │  ✅ COMPLETE     │  │  - Build bots      │
    │  - Tavily search │  │  - Backtest        │
    │  - Web browsing  │  │  - Deploy          │
    │  - Documentation │  │                    │
    │  - Analysis      │  │                    │
    └──────────────────┘  └────────────────────┘
```

### Tool Distribution

| Tool | Supervisor | Researcher | Developer (Story 3) |
|------|-----------|-----------|---------------------|
| tavily_search | ❌ | ✅ | ❌ |
| browse_url | ❌ | ✅ | ❌ |
| list_documentation | ✅ | ✅ | ✅ |
| get_documentation | ✅ | ✅ | ✅ |
| save_artifact | ❌ | ❌ | ✅ (Story 3) |
| deploy_bot | ❌ | ❌ | ✅ (Story 3) |

### State Management

Agents use session state for data sharing:

**research_data** (written by Researcher in Story 5):
```json
{
    "query": "original research request",
    "summary": "executive summary",
    "findings": [{"point": "...", "source": "...", "confidence": "high"}],
    "recommendations": ["...", "..."],
    "sources": [{"title": "...", "url": "...", "relevance": "..."}],
    "timestamp": "2025-01-15T10:30:00Z"
}
```

### Researcher Agent Details

**File**: `the0/agents/researcher.py`
**Purpose**: Quantitative research specialist for trading bot development
**Model**: Gemini 2.5 Flash

**Capabilities:**
- Performs comprehensive web research using Tavily AI search
- Analyzes and verifies information from multiple authoritative sources
- Researches trading strategies, APIs, libraries, and technical documentation
- Uses browse_url for deep-dive documentation reading
- Consults internal the0 platform documentation
- Returns structured findings with markdown formatting and citations

**Research Workflow:**
1. Understand research request (scope, depth, audience)
2. Formulate effective search queries (targeted, technical, recent)
3. Use Tavily search for broad research (basic or advanced depth)
4. Deep dive with browse_url for detailed documentation
5. Consult internal the0 documentation (prioritize quick-start, backtesting)
6. Verify and cross-reference across multiple sources
7. Structure findings with citations (Summary, Findings, Analysis, Recommendations, References)

**Quality Standards:**
- Source authority hierarchy: Official docs > Academic > Technical blogs > Forums
- Information freshness: Prefer recent sources, note deprecation
- Completeness: Answer all aspects, state gaps explicitly
- Citations: All factual claims must have footnote citations [^1]

**State Output**: Stores research in `research_data` state key (Story 5) for consumption by Developer agent
```

---

## Validation Loop

### Level 1: Syntax & Style (MUST RUN FIRST)

```bash
# STEP 1: Code formatting (CRITICAL - Run BEFORE lint)
make format
# OR: black the0/agents/ tests/the0/agents/
# Expected output: "All done! ✨ 🍰 ✨"
# Expected output: "N files reformatted" or "N files left unchanged"

# STEP 2: Linting
make lint
# OR: flake8 the0/agents/ tests/the0/agents/
# Expected output: 0 errors, 0 warnings
# If errors: Fix them, then re-run make format && make lint

# STEP 3: Python syntax validation (optional but recommended)
python -m py_compile the0/agents/researcher.py
python -m py_compile tests/the0/agents/test_researcher.py
# Expected output: No output = success

# SUCCESS CRITERIA:
# - ✅ make format completes without errors
# - ✅ make lint shows 0 errors, 0 warnings
# - ✅ All files follow Black style (88 char lines)
```

---

### Level 2: Unit Tests

```bash
# STEP 1: Run specific researcher tests
pytest tests/the0/agents/test_researcher.py -v
# Expected output: All tests pass (green checkmarks)
# Expected output: "N passed in X.XXs"

# STEP 2: Run all tests (ensure no regressions)
make test
# OR: pytest
# Expected output: All existing tests still pass
# Expected output: New tests pass

# STEP 3: Check test coverage
pytest --cov=the0/agents/researcher --cov-report=term tests/the0/agents/test_researcher.py
# Expected output: Coverage >80%
# TARGET: 100% coverage since this is a simple agent definition file

# STEP 4: Generate HTML coverage report (optional)
pytest --cov=the0/agents/researcher --cov-report=html tests/the0/agents/test_researcher.py
# Open htmlcov/index.html in browser to see detailed coverage

# SUCCESS CRITERIA:
# - ✅ All tests pass (0 failures, 0 errors)
# - ✅ Coverage >80% for the0/agents/researcher.py
# - ✅ No regressions in existing tests
# - ✅ Test names descriptive and clear
```

---

### Level 3: Integration Tests

```bash
# STEP 1: Test agent import from package
python -c "from the0.agents import researcher_agent; print(f'Agent: {researcher_agent.name}, Model: {researcher_agent.model}')"
# Expected output: "Agent: researcher, Model: gemini-2.5-flash"

# STEP 2: Verify agent properties
python -c "
from the0.agents.researcher import researcher_agent
assert researcher_agent.name == 'researcher'
assert len(researcher_agent.tools) == 4
assert len(researcher_agent.instruction) > 1000
print('✅ Agent properties verified')
"
# Expected output: "✅ Agent properties verified"

# STEP 3: Verify tools are callable
python -c "
from the0.agents.researcher import researcher_agent
for tool in researcher_agent.tools:
    assert callable(tool) or hasattr(tool, 'func_declarations')
print(f'✅ All {len(researcher_agent.tools)} tools are valid')
"
# Expected output: "✅ All 4 tools are valid"

# STEP 4: Verify constants usage
python -c "
from the0.agents.base import DEFAULT_MODEL, STATE_KEY_RESEARCH
from the0.agents.researcher import researcher_agent
assert researcher_agent.model == DEFAULT_MODEL
assert STATE_KEY_RESEARCH in researcher_agent.instruction
print('✅ Constants correctly used')
"
# Expected output: "✅ Constants correctly used"

# SUCCESS CRITERIA:
# - ✅ Agent can be imported from package
# - ✅ All properties accessible
# - ✅ Tools are valid callables
# - ✅ Constants correctly referenced
```

---

### Level 4: Documentation Validation

```bash
# STEP 1: Verify CLAUDE.md updates
grep -i "researcher" CLAUDE.md | head -5
# Expected output: Multiple matches including "Researcher Agent"

# STEP 2: Check tool distribution table
grep -A 10 "Tool Distribution" CLAUDE.md
# Expected output: Table includes Researcher column

# STEP 3: Verify research_data schema documented
grep -A 15 "research_data" CLAUDE.md
# Expected output: JSON schema for research_data state

# STEP 4: Check markdown formatting
python -c "
with open('CLAUDE.md', 'r') as f:
    content = f.read()
    # Check for broken markdown (mismatched code fences)
    assert content.count('\`\`\`') % 2 == 0, 'Mismatched code fences'
    print('✅ Markdown formatting valid')
"
# Expected output: "✅ Markdown formatting valid"

# SUCCESS CRITERIA:
# - ✅ CLAUDE.md mentions Researcher agent
# - ✅ Tool Distribution table updated
# - ✅ State schema documented
# - ✅ No broken markdown formatting
```

---

## Final Validation Checklist

**Before Marking Story as Complete:**

- [ ] **Code Created:**
  - [ ] File created: `the0/agents/researcher.py`
  - [ ] File created: `tests/the0/agents/test_researcher.py`
  - [ ] File updated: `the0/agents/__init__.py`
  - [ ] File updated: `CLAUDE.md`

- [ ] **Agent Configuration:**
  - [ ] Agent name: "researcher"
  - [ ] Agent model: "gemini-2.5-flash" (using DEFAULT_MODEL)
  - [ ] Agent description clear and specific (50-500 chars)
  - [ ] Agent instruction comprehensive (>150 lines, >1000 chars)
  - [ ] Four tools assigned: tavily_search, browse_url, list_documentation, get_documentation

- [ ] **Instruction Quality:**
  - [ ] Core Responsibilities section present
  - [ ] Research Workflow (7 steps) documented
  - [ ] Tool usage guidelines explained
  - [ ] Output format specified (Summary, Findings, Analysis, Recommendations, References)
  - [ ] Citation requirements detailed (footnote format [^N])
  - [ ] Quality standards defined (source authority, freshness, completeness)
  - [ ] Research examples included (API research, strategy research)

- [ ] **State Management:**
  - [ ] STATE_KEY_RESEARCH constant used
  - [ ] research_data schema documented in docstring
  - [ ] JSON structure defined (query, summary, findings, recommendations, sources, timestamp)
  - [ ] State key referenced in instruction

- [ ] **Testing:**
  - [ ] All tests pass: `make test`
  - [ ] Coverage >80%: `pytest --cov=the0/agents/researcher`
  - [ ] Tests verify: configuration, instruction completeness, tools, constants, exports

- [ ] **Code Quality:**
  - [ ] Formatted: `make format` passes
  - [ ] Linted: `make lint` shows 0 errors/warnings
  - [ ] Imports clean and organized
  - [ ] Docstrings comprehensive

- [ ] **Documentation:**
  - [ ] CLAUDE.md updated with Researcher agent details
  - [ ] Tool Distribution table updated
  - [ ] State schema documented
  - [ ] No broken markdown formatting

- [ ] **Integration:**
  - [ ] Can import: `from the0.agents import researcher_agent`
  - [ ] Properties accessible
  - [ ] Tools callable
  - [ ] Constants referenced correctly

---

## Anti-Patterns to Avoid

**During Implementation - DO NOT:**

- ❌ **Create new tools** - All 4 tools already exist in `the0/tools/`
- ❌ **Modify existing tools** - Tools are shared, modifications affect supervisor
- ❌ **Implement state writing** - That's Story 5, just document the schema
- ❌ **Modify the0/agent.py** - That's Story 4 (Supervisor Transformation)
- ❌ **Use LlmAgent** - Use `Agent` from `google.adk.agents`
- ❌ **Wrap tools in Tool()** - Pass function references directly
- ❌ **Create sub_agents** - That's Story 4, agent works standalone for now
- ❌ **Implement transfer_to_agent** - That's Story 4, delegation logic
- ❌ **Add new dependencies** - Everything needed already in requirements.txt
- ❌ **Use async def for tests** - Agent config tests are synchronous
- ❌ **Mock tools in tests** - Test agent configuration only, not tool invocation
- ❌ **Skip make format** - Always format before linting
- ❌ **Ignore flake8 warnings** - Fix all warnings for clean build

**Code Quality - AVOID:**

- ❌ Lines longer than 88 characters (Black will error)
- ❌ Missing docstrings on functions/classes
- ❌ Hardcoded strings instead of constants (use DEFAULT_MODEL, STATE_KEY_RESEARCH)
- ❌ Inconsistent indentation or formatting
- ❌ Trailing whitespace or multiple blank lines
- ❌ Missing type hints on function parameters (optional but encouraged)

**Testing - AVOID:**

- ❌ Brittle tests that depend on exact string matching (use `in` for keywords)
- ❌ Tests without descriptive docstrings
- ❌ Over-testing implementation details (test behavior, not internals)
- ❌ Skipping edge cases (empty instruction, missing tools, wrong model)

---

## PR Message Template

When implementation is complete, use this template:

```markdown
## Feature: Researcher Agent - Quantitative Research Specialist

Implements Story 2 of the Multi-Agent System (MAS) development. Creates a specialized Researcher Agent that performs comprehensive web research using Tavily AI search and web browsing capabilities.

## Background

Part of the MAS architecture refactoring (Epic: Specialized Agent Development). The Researcher Agent is the first specialized sub-agent to be created, focusing exclusively on quantitative research and information gathering for trading bot development. This sets the foundation for:
- Story 3: Developer Agent
- Story 4: Supervisor Transformation (agent delegation)
- Story 5: State Management (research_data sharing)

## Changes Made

### New Agent Definition

**File**: `the0/agents/researcher.py`
- Agent name: "researcher"
- Model: Gemini 2.5 Flash (gemini-2.5-flash)
- Tools: tavily_search, browse_url, list_documentation, get_documentation
- Comprehensive 150+ line instruction covering:
  - Research workflow (7 steps: understand → formulate → search → deep-dive → consult → verify → structure)
  - Tool usage guidelines (when to use Tavily vs browse_url vs documentation)
  - Output format requirements (Summary, Findings, Analysis, Recommendations, References)
  - Citation requirements (footnote format [^N] for all factual claims)
  - Quality standards (source authority hierarchy, information freshness, completeness)
  - Research examples (API research, strategy research with sample outputs)

### Package Updates

**File**: `the0/agents/__init__.py`
- Added export for researcher_agent
- Maintains existing base module export

### Comprehensive Test Suite

**File**: `tests/the0/agents/test_researcher.py`
- Test configuration (name, model, description, instruction)
- Test instruction completeness (length, keywords, sections)
- Test tool assignment (4 tools: tavily_search, browse_url, list_documentation, get_documentation)
- Test constants usage (DEFAULT_MODEL, STATE_KEY_RESEARCH)
- Test agent export from package
- Test instruction examples and patterns
- **Coverage**: >80% (target met)

### Documentation Updates

**File**: `CLAUDE.md`
- Updated Multi-Agent System (MAS) Architecture section
- Added Researcher agent to Agent Hierarchy diagram
- Updated Tool Distribution table with Researcher column
- Documented research_data state schema (for Story 5)
- Added detailed Researcher Agent section with capabilities, workflow, quality standards

## Validation Steps

### Code Quality & Safety

- [x] Python syntax validation passed
- [x] Code formatting with `make format` applied
- [x] Linting with `make lint` passed (0 errors, 0 warnings)
- [x] Black style enforced (88 character lines)

### Testing

- [x] All unit tests passing: `pytest tests/the0/agents/test_researcher.py`
- [x] No regressions: `make test` (all existing tests still pass)
- [x] Coverage >80%: `pytest --cov=the0/agents/researcher`
- [x] Integration testing: Agent can be imported and properties accessible

### Integration

- [x] Agent imports successfully: `from the0.agents import researcher_agent`
- [x] All 4 tools assigned and callable
- [x] Constants correctly referenced (DEFAULT_MODEL, STATE_KEY_RESEARCH)
- [x] CLAUDE.md updates verified

## State Schema (For Story 5)

The Researcher agent documents research findings in structured JSON format using state key `research_data`:

```json
{
    "query": "original research request",
    "summary": "executive summary (2-3 sentences)",
    "findings": [
        {
            "point": "key finding or insight",
            "source": "https://source-url.com",
            "confidence": "high|medium|low"
        }
    ],
    "recommendations": ["actionable recommendation 1", "..."],
    "sources": [
        {
            "title": "Source Title",
            "url": "https://source-url.com",
            "relevance": "Why this source matters",
            "published": "2025-01-15"
        }
    ],
    "timestamp": "2025-01-15T10:30:00Z"
}
```

**Note**: State writing logic will be implemented in Story 5. This story only defines the agent and documents the schema.

## Known Limitations

1. **Standalone Agent**: Agent can be tested independently but is not yet integrated into supervisor (Story 4)
2. **No State Writing**: Agent documents state schema but doesn't write to state yet (Story 5)
3. **No Delegation**: Supervisor doesn't delegate to Researcher yet (Story 4)
4. **Manual Testing**: Requires ADK Runner setup for manual execution testing

## Follow-up Tasks

**Story 3**: Developer Agent
- Create developer specialist agent
- Assign save_artifact, deploy_bot tools
- Define bot_metadata state schema

**Story 4**: Supervisor Transformation
- Transform current agent into supervisor
- Implement LLM-driven delegation (transfer_to_agent)
- Set up sub_agents relationship (Supervisor → Researcher → Developer)
- Configure delegation based on agent descriptions

**Story 5**: State Management
- Implement state writing logic in agents
- Test state sharing between Researcher and Developer
- Validate research_data and bot_metadata schemas
- Implement state injection in instructions

## Testing Notes

All tests pass with >80% coverage. Manual testing can be performed using ADK Runner:

```python
from google.adk.runners import Runner
from google.adk.sessions import InMemorySessionService
from google.adk.artifacts import InMemoryArtifactService
from the0.agents.researcher import researcher_agent

runner = Runner(
    app_name="researcher-test",
    agent=researcher_agent,
    session_service=InMemorySessionService(),
    artifact_service=InMemoryArtifactService()
)

# Test query
response = runner.run("Research Binance Spot API for Python trading bots")
print(response)
```

## Agent Performance

**Instruction Quality**: 150+ lines, comprehensive coverage of research methodology
**Tool Coverage**: 100% of required tools (4/4)
**Test Coverage**: >80% (100% on agent definition file)
**Code Quality**: 0 linting errors/warnings

---

**Files Changed**:
- `the0/agents/researcher.py` (NEW)
- `tests/the0/agents/test_researcher.py` (NEW)
- `the0/agents/__init__.py` (UPDATED)
- `CLAUDE.md` (UPDATED)

**Lines Added**: ~900 (agent definition + tests + documentation)
**Lines Modified**: ~50 (CLAUDE.md updates, __init__.py export)
```

---

## PRP Confidence Score

**Score: 9/10** - High confidence in one-pass implementation success

### Justification:

**Strengths (Why 9/10):**

1. **Complete Context** ✅
   - All necessary files documented and linked
   - Google ADK documentation fetched and summarized
   - Existing patterns identified and explained
   - Tool implementations reviewed

2. **Clear Implementation Path** ✅
   - Step-by-step task list with validation gates
   - Complete pseudocode for both agent and tests
   - Example outputs and patterns provided
   - Known gotchas explicitly listed

3. **Executable Validation** ✅
   - All validation commands are runnable
   - Success criteria clearly defined
   - Multiple levels of validation (syntax → tests → integration → docs)
   - Coverage targets specified (>80%)

4. **Pattern Matching** ✅
   - Follows existing codebase patterns (the0/agent.py, tools)
   - Uses established testing patterns (test_web_browser.py)
   - Imports and exports match package structure
   - Constants usage consistent with base.py

5. **Comprehensive Documentation** ✅
   - Research gathered from authoritative sources (Google ADK docs)
   - State schema fully defined
   - CLAUDE.md update template provided
   - Anti-patterns explicitly listed

**Weaknesses (Why not 10/10):**

1. **Agent Instruction Length** ⚠️
   - 150+ line instruction is long and may require iteration for quality
   - Examples provided but AI may need to adjust tone/style
   - Risk: Instruction may be too verbose or miss nuances

2. **Test Coverage Uncertainty** ⚠️
   - Test implementation relies on understanding ADK tool wrapping behavior
   - Tool name extraction logic may need adjustment based on actual ADK version
   - Risk: Tool verification tests may need debugging

**Mitigation:**
- Story file has complete instruction example (lines 105-358) to copy/adapt
- Test pseudocode accounts for tool wrapping with multiple checks
- Validation loop includes multiple test runs to catch issues early

**Overall Assessment:**
PRP provides sufficient context and guidance for one-pass implementation. Minor adjustments may be needed for instruction tone and test assertions, but core structure is solid. Expected outcome: Agent implemented, tests passing, documentation updated, ready for Story 4 integration.

---

**END OF PRP**

Generated: 2025-11-12
Version: 1.0
Status: Ready for Implementation
