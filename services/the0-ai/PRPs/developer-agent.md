# PRP: Developer Agent - Trading Bot Development Specialist

**Status:** Ready for Implementation
**Epic:** Specialized Agent Development
**Estimated Effort:** 8-10 hours
**Dependencies:** Story 0 (Foundation & Setup - COMPLETED)
**Target:** Story 3

---

## Goal

Create a specialized Developer Agent that builds production-ready trading bots based on user requirements and research findings. The agent creates all required artifacts following the0 platform standards, implements backtesting, and packages bots for deployment.

**Specific End State:**
- New agent file `the0/agents/developer.py` with complete agent definition
- Agent name: "developer", model: "gemini-2.5-flash"
- Four tools assigned: save_artifact, deploy_bot, list_documentation, get_documentation
- Comprehensive instructions (200+ lines minimum) covering:
  - Role and responsibilities
  - Development workflow (9 steps - includes environment setup and execution validation)
  - Required artifacts checklist
  - Code quality standards (Clean Architecture, SOLID, DRY)
  - Platform standards
  - State management (read research_data, write bot_metadata)
  - Production-ready code examples
  - **CRITICAL: Executable validation workflow**
  - **CRITICAL: Environment setup (venv/node_modules)**
  - **CRITICAL: Credentials handling via user questions (paper API keys)**
  - **CRITICAL: Backtest execution verification**
  - Library preference guidelines (use established libraries for calculations)
- Session state management for reading research and writing metadata
- Comprehensive test suite (`tests/the0/agents/test_developer.py`) with >80% coverage
- Documentation updated in CLAUDE.md with Developer agent details

---

## Why

- **Specialization**: Dedicated development agent improves code quality through focused expertise and development-specific prompts
- **Platform Standards**: Enforces the0 platform best practices, bot-config structure, and required artifacts
- **Completeness**: Ensures all required files created (main.py, config, requirements, schema, README, backtest)
- **Quality**: Follows clean architecture principles (SOLID, DRY, modularity) from the start
- **Integration**: Reads research findings from Researcher agent via session state
- **Scalability**: Foundation for Multi-Agent System enables future Optimizer, Backtester, and Monitor agents
- **Consistency**: Sets development standards pattern for all future bot-building capabilities

---

## What

Create a Developer Agent (`the0/agents/developer.py`) that:

1. **Development Capabilities:**
   - Builds production-ready, **EXECUTABLE** trading bots based on user requirements
   - Creates all required artifacts following the0 platform standards
   - Implements backtesting for strategy validation
   - **VALIDATES bot and backtest execution** before completion
   - Supports both simple (single-file) and complex (modular) architectures
   - Follows code quality guidelines (SOLID, Clean Architecture, DRY)
   - Uses environment variables for secrets (no hardcoded API keys)
   - **Prefers established libraries** for calculations (ta-lib, pandas-ta, ccxt, etc.)

2. **Research Integration:**
   - Reads research findings from session state (key: `research_data`)
   - Leverages API documentation, library versions, and recommendations
   - Uses research sources for implementation decisions

3. **Platform Integration:**
   - Consults internal the0 documentation (CRITICAL: quick-start-guide.md, backtesting.md)
   - Follows bot-config.yaml structure from platform standards
   - Implements proper entrypoint functions
   - Supports both scheduled and real-time execution modes

4. **Artifact Creation:**
   - Core files: main.py, bot-config.yaml, requirements.txt, bot-schema.json, README.md
   - Testing files: backtest.py, backtest-schema.json
   - Optional modules: Data fetching, indicators, strategy logic, risk management
   - Uses save_artifact tool for each file
   - Uses deploy_bot tool to package into deployment ZIP

5. **Executable Validation (CRITICAL):**
   - Sets up development environment (venv for Python, node_modules for JavaScript)
   - Installs dependencies from requirements.txt or package.json
   - **Asks user for paper API credentials** if needed for testing
   - Executes main.py to verify bot runs without errors
   - Executes backtest.py to verify backtesting works correctly
   - **Backtest results must be correct** (even if not profitable)
   - Fixes any execution errors before marking bot complete
   - Documents execution results in bot metadata

6. **Credentials Handling:**
   - **Asks user** if they want to provide paper API credentials for testing
   - Uses credentials ONLY in memory for current session
   - **Does NOT persist credentials** to files or artifacts
   - Sets credentials as temporary environment variables for testing
   - Documents in README.md how user should provide real credentials
   - Falls back to mock data if no credentials provided

7. **Library Preferences:**
   - Prefers established libraries over custom implementations
   - Technical indicators: ta-lib, pandas-ta, tulipy
   - Exchange APIs: ccxt (Python/JS), binance-api-node (JS)
   - Data manipulation: pandas (Python), lodash (JS)
   - Backtesting frameworks: backtrader (Python), ccxt (built-in)
   - Researches latest stable versions before implementation

8. **State Management:**
   - Reads research_data from session state (written by Researcher)
   - Stores bot_metadata in session state with key "bot_metadata"
   - Includes: bot_name, language, files_created, strategy_type, platform, status, execution_verified, backtest_verified, timestamp

9. **Agent Configuration:**
   - Name: "developer"
   - Model: "gemini-2.5-flash"
   - Tools: save_artifact, deploy_bot, list_documentation, get_documentation
   - Description optimized for LLM-driven delegation (Story 4)
   - Instructions covering workflow, standards, quality, and executable validation

### Success Criteria

- [ ] Agent file created at `the0/agents/developer.py`
- [ ] Agent name: "developer", model: "gemini-2.5-flash"
- [ ] Four tools assigned: save_artifact, deploy_bot, list_documentation, get_documentation
- [ ] Comprehensive instructions (200+ lines) covering:
  - [ ] Role and responsibilities
  - [ ] Development workflow (9 steps - includes environment setup and validation)
  - [ ] Required artifacts checklist (core + testing files)
  - [ ] Code quality standards (Clean Architecture, SOLID, DRY)
  - [ ] the0 platform standards
  - [ ] State management (read research, write metadata)
  - [ ] Language choice criteria (Python vs JavaScript)
  - [ ] Implementation approach and philosophy
  - [ ] **CRITICAL: Executable validation workflow**
  - [ ] **CRITICAL: Environment setup instructions (venv/node_modules)**
  - [ ] **CRITICAL: Credentials handling (ask user, don't persist)**
  - [ ] **CRITICAL: Execution verification (bot + backtest must run)**
  - [ ] Library preference guidelines (prefer established libraries)
  - [ ] Example main.py structure (production-ready)
  - [ ] Session state schemas (input: research_data, output: bot_metadata)
  - [ ] Escalation conditions
  - [ ] Important reminders
- [ ] Can read research_data from session state
- [ ] Bot metadata stored in session state with key "bot_metadata"
- [ ] Unit tests achieve >80% coverage
- [ ] Integration test demonstrates bot creation workflow
- [ ] Documentation complete and CLAUDE.md updated
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)
- [ ] All tests pass (`make test`)

---

## All Needed Context

### Documentation & References

**CRITICAL - MUST READ BEFORE IMPLEMENTATION:**

```yaml
# Google ADK Documentation (PRIMARY)
- url: https://google.github.io/adk-docs/agents/multi-agents/
  why: Multi-agent system architecture, sub_agents, LLM-driven delegation, state sharing
  key_sections:
    - Agent Hierarchy (parent-child relationships, single parent rule)
    - LLM-Driven Delegation (transfer_to_agent pattern for Story 4)
    - Shared Session State (agents access identical state via InvocationContext)
    - Coordinator/Dispatcher Pattern (Supervisor routes to specialists)
  key_insights:
    - "An agent instance can only be added as a sub-agent once"
    - Shared state is primary communication channel
    - LLM agents generate transfer_to_agent() calls dynamically
    - Clear agent descriptions critical for delegation

- url: https://google.github.io/adk-docs/sessions/state/
  why: Session state management, reading/writing, persistence, best practices
  key_sections:
    - State Structure (keys=strings, values=serializable)
    - State Prefixes (no prefix=session, user:=cross-session, app:=global, temp:=invocation-only)
    - Reading/Writing (output_key, EventActions.state_delta, context.state)
    - State Injection ({key} templating in instructions)
  critical_warnings:
    - "State should ALWAYS be updated as part of adding an Event to session history"
    - "Avoid directly modifying session.state outside managed contexts"
    - Use CallbackContext.state or ToolContext.state for reliable updates
    - Direct modifications bypass event history and break persistence

- url: https://google.github.io/adk-docs/agents/
  why: Agent creation, configuration, tool assignment
  key_sections:
    - Agent() constructor (name, model, description, instruction, tools)
    - Tool assignment patterns
    - Instruction writing fundamentals

# ADK Prompting Best Practices (2025)
- url: https://medium.com/google-cloud/the-adk-prompting-pattern-static-vs-turn-instructions-7a1e5b25eeef
  why: Static vs turn instructions pattern for cacheable, efficient prompts
  key_insights:
    - Static instructions: invariant header (policy, persona, tools, examples)
    - Turn instructions: per-request steering (goal, style, constraints)
    - Static instructions cached by ADK for performance
    - Turn instructions injected as user-role message before query

- url: https://medium.com/@george_6906/prompt-engineering-with-googles-agent-development-kit-adk-d748ba212440
  why: Prompt engineering techniques for ADK agents
  best_practices:
    - Clear and specific instructions
    - Provide necessary context
    - Use dynamic placeholders ({state.key}, {artifact.filename})
    - Include explicit output format instructions
    - Show good/bad examples

# Gemini 2.5 Flash for Code Generation
- url: https://developers.google.com/gemini-code-assist/docs/agent-mode
  why: Gemini 2.5 Flash capabilities for code generation and agent automation
  key_capabilities:
    - Complex multi-step task execution
    - Code generation from design documents
    - 128K token context window (free tier)
    - Thinking budget for fine-grained reasoning control
  best_practices:
    - Use agent mode for complex tasks (refactoring, test gen, architecture)
    - Provide context from multiple sources
    - Set project-specific rules for conventions
    - Show examples of expected code patterns

# Project Documentation (LOCAL - READ THESE FIRST!)
- file: /home/alexander/Code/Apps/the0/services/the0-ai/CLAUDE.md
  why: Architecture overview, MAS section, Developer agent requirements
  key_sections:
    - Multi-Agent System (MAS) Architecture
    - Agent Hierarchy (Supervisor → Researcher → Developer)
    - State Management (bot_metadata schema definition)
    - Tool Distribution table
    - Design Principles for Agents
    - Code Quality Guidelines (SOLID, Clean Architecture, DRY)
  critical_info:
    - Developer agent description (lines 217-243)
    - bot_metadata schema (lines 353-371)
    - Required artifacts list (lines 205-223)
    - Code quality standards (lines 225-256)
    - Platform standards (lines 248-256)

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/agents/researcher.py
  why: Reference implementation for agent structure, instruction format, tool usage
  patterns_to_follow:
    - Agent definition structure
    - Comprehensive instruction format (150+ lines)
    - Clear section headers (## Core Responsibilities, ## Workflow, etc.)
    - Good/bad examples with ✅ and ❌
    - State management documentation
    - Escalation conditions
    - Important reminders section

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/agents/base.py
  why: Shared constants and utilities for all agents
  items_to_use:
    - DEFAULT_MODEL = "gemini-2.5-flash"
    - STATE_KEY_BOT_METADATA = "bot_metadata"
    - STATE_KEY_RESEARCH = "research_data"
    - format_citations() utility (if needed)

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/tools/save_artifact.py
  why: Artifact creation tool implementation and usage pattern
  usage_pattern:
    - Accept code (str), filename (str), tool_context (ToolContext)
    - Extract session_id from tool_context._invocation_context.session.id
    - Returns dict with status, message, filename, version, file_path, action
    - Handles both MinIO storage and database metadata
    - Detects MIME types automatically

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/tools/deploy_bot.py
  why: Bot deployment tool implementation
  usage_pattern:
    - Accept bot_name (str), tool_context (ToolContext)
    - Lists all artifacts in session
    - Creates ZIP file in bots/ directory
    - Returns dict with status, bot_name, zip_file, artifacts_count, artifacts

- file: /home/alexander/Code/Apps/the0/services/the0-ai/the0/tools/documentation.py
  why: Internal documentation tools (list_documentation, get_documentation)
  usage_pattern:
    - list_documentation: Returns formatted markdown list of all docs
    - get_documentation(path): Returns full markdown content
    - Connects to DOCS_ENDPOINT environment variable

- file: /home/alexander/Code/Apps/the0/services/the0-ai/tests/the0/agents/test_researcher.py
  why: Testing patterns for agent configuration and completeness
  patterns_to_follow:
    - test_*_configuration: name, model, description, instruction
    - test_*_description_quality: length, key phrases, delegation words
    - test_*_instruction_completeness: length (>1000 chars, 150+ lines), keywords, sections
    - test_*_tools_assigned: tool count, tool names extraction
    - test_*_state_key_reference: state key in instruction
    - test_*_agent_export: package __init__.py export verification
    - test_*_instruction_examples: good/bad patterns present

# Story File (Implementation Requirements)
- file: /home/alexander/Code/Apps/the0/services/the0-ai/stories/story-3-developer-agent.md
  why: Detailed implementation requirements, acceptance criteria, agent definition example
  critical_sections:
    - Agent Definition example (lines 106-403)
    - Complete instruction template with all sections
    - State management schemas (lines 340-371)
    - Testing strategy (lines 457-501)
    - Technical considerations (lines 407-427)
```

---

### Current Codebase Structure

```bash
the0-ai/
├── the0/
│   ├── agent.py                    # Current supervisor agent (Story 0)
│   ├── agents/
│   │   ├── __init__.py            # Package init, exports base, researcher_agent
│   │   ├── base.py                # Shared constants (DEFAULT_MODEL, STATE_KEY_*)
│   │   ├── researcher.py          # ✅ COMPLETE - Researcher agent (Story 2)
│   │   └── developer.py           # ← CREATE THIS (Story 3)
│   └── tools/
│       ├── __init__.py
│       ├── save_artifact.py       # ✅ Bot file creation tool
│       ├── deploy_bot.py          # ✅ Bot deployment/packaging tool
│       ├── web_browser.py         # ✅ Tavily search + browse_url (Story 1)
│       └── documentation.py       # ✅ Internal docs tools
├── tests/
│   └── the0/
│       ├── agents/
│       │   ├── test_researcher.py  # ✅ Researcher tests (reference)
│       │   └── test_developer.py   # ← CREATE THIS (Story 3)
│       └── tools/
│           ├── test_save_artifact.py    # ✅ Tool tests (reference)
│           ├── test_deploy_bot.py       # ✅ Tool tests (reference)
│           └── test_web_browser.py      # ✅ Tool tests (reference)
├── api/                           # FastAPI layer (not modified in this story)
├── alembic/                       # Database migrations (not modified)
├── CLAUDE.md                      # ← UPDATE with Developer agent docs
├── Makefile                       # ✅ Development commands
├── pytest.ini                     # ✅ Test configuration
└── requirements.txt               # ✅ Dependencies
```

---

### Desired Codebase Structure with File Responsibilities

```bash
the0-ai/
├── the0/
│   ├── agents/
│   │   ├── __init__.py            # UPDATE: Add developer_agent export
│   │   │                          # Responsibility: Export developer_agent for imports
│   │   │
│   │   └── developer.py           # CREATE: Developer agent definition
│   │                              # Responsibilities:
│   │                              #   - Define DEVELOPER_DESCRIPTION constant (100-200 chars)
│   │                              #   - Define DEVELOPER_INSTRUCTION constant (200+ lines)
│   │                              #   - Create developer_agent = Agent(...) instance
│   │                              #   - Import tools: save_artifact, deploy_bot, docs tools
│   │                              #   - Import constants: DEFAULT_MODEL, STATE_KEY_*
│   │                              # Structure mirrors researcher.py:
│   │                              #   1. Module docstring with state schema
│   │                              #   2. Imports
│   │                              #   3. DEVELOPER_DESCRIPTION
│   │                              #   4. DEVELOPER_INSTRUCTION (comprehensive)
│   │                              #   5. developer_agent = Agent(...)
│
├── tests/
│   └── the0/
│       └── agents/
│           └── test_developer.py   # CREATE: Developer agent tests
│                                   # Responsibilities:
│                                   #   - Test agent configuration (name, model)
│                                   #   - Test description quality (length, key phrases)
│                                   #   - Test instruction completeness (>200 lines)
│                                   #   - Test tools assigned (4 tools)
│                                   #   - Test state key references
│                                   #   - Test agent export from __init__.py
│                                   #   - Test instruction examples present
│                                   # Structure mirrors test_researcher.py:
│                                   #   - TestDeveloperAgent class
│                                   #   - TestDeveloperConstants class
│
└── CLAUDE.md                       # UPDATE: Add Developer agent section
                                    # Responsibilities:
                                    #   - Add to Agent Hierarchy diagram
                                    #   - Add to Tool Distribution table
                                    #   - Update "Future Agents" section
                                    #   - Add Developer Agent Details subsection
```

---

### Known Gotchas & Library Quirks

```yaml
google_adk_agents:
  - "CRITICAL: Agent instance can only be added as sub-agent once - attempting second parent raises error"
  - "CRITICAL: Use ToolContext for session-aware tools - extract session_id from tool_context._invocation_context.session.id"
  - "CRITICAL: State should ALWAYS be updated as part of Event history - avoid direct session.state modification"
  - "Use output_key in Agent() for automatic state storage of final responses"
  - "For complex updates, use EventActions.state_delta or CallbackContext.state"
  - "Shared InvocationContext enables state sharing between agents"
  - "Clear agent descriptions critical for LLM-driven delegation (Story 4)"
  - "Model: gemini-2.5-flash (128K context, optimized for code generation)"

agent_instructions:
  - "CRITICAL: Quality depends on instruction comprehensiveness - aim for 250+ lines"
  - "CRITICAL: Emphasize engineering principles, not rigid code structure"
  - "CRITICAL: Teach WHAT makes good code, not HOW to structure it exactly"
  - "Static instructions should be invariant (policy, persona, tools, examples)"
  - "Turn instructions (per-request) not used yet - just static instruction for now"
  - "Include explicit output format instructions even with schemas"
  - "Show good/bad examples with ✅ and ❌ for clarity"
  - "Use clear section headers (## Core Responsibilities, ## Workflow, etc.)"
  - "Include when to escalate to Supervisor section"
  - "Add IMPORTANT REMINDERS section at end with critical points"
  - "Emphasize: Learn from docs (bot-config, backtesting), design yourself (classes, modules)"
  - "Emphasize: Use classes, keep entrypoints small (<50 lines), separate concerns"

state_management:
  - "CRITICAL: Read research_data from state BEFORE starting development"
  - "CRITICAL: Write bot_metadata to state AFTER completing bot"
  - "State keys: research_data (input), bot_metadata (output)"
  - "Check if research_data exists - escalate if missing"
  - "Use structured JSON format - see schemas below"
  - "Avoid storing complex objects - only serializable types"

tool_usage:
  - "CRITICAL: save_artifact creates ONE file at a time - call multiple times"
  - "CRITICAL: deploy_bot packages ALL artifacts - call once at end"
  - "CRITICAL: Always consult quick-start-guide.md and backtesting.md"
  - "list_documentation to discover available guides"
  - "get_documentation(path) to read specific docs"
  - "Tools return dicts with status, message, and result data"

the0_platform:
  - "CRITICAL: Follow bot-config.yaml structure from quick-start-guide"
  - "CRITICAL: All bots must have backtesting capability"
  - "CRITICAL: Bots MUST be executable - verify before completion"
  - "CRITICAL: Backtests MUST run and produce correct results"
  - "Required files: main.py, bot-config.yaml, requirements.txt, bot-schema.json, README.md, backtest.py, backtest-schema.json"
  - "Use environment variables for secrets (API keys, tokens)"
  - "Implement proper entrypoint function (def main())"
  - "Support both scheduled and real-time execution modes"
  - "Add comprehensive error handling and logging"

executable_validation:
  - "CRITICAL: Set up development environment BEFORE testing (venv/node_modules)"
  - "CRITICAL: Install dependencies from requirements.txt or package.json"
  - "CRITICAL: Execute main.py - must run without errors"
  - "CRITICAL: Execute backtest.py - must run and produce results"
  - "CRITICAL: Fix any execution errors before marking complete"
  - "Ask user for paper API credentials if needed for testing"
  - "Do NOT persist credentials - use only in memory"
  - "Document execution results in bot_metadata (execution_verified, backtest_verified)"
  - "Results can be unprofitable but MUST be correct"

credentials_handling:
  - "CRITICAL: Ask user if they want to provide paper API credentials"
  - "Use credentials ONLY in current session memory"
  - "Do NOT save credentials to files or artifacts"
  - "Set as temporary environment variables for testing"
  - "Document in README.md how to provide real credentials"
  - "Fall back to mock data if no credentials provided"
  - "Security first - never persist sensitive data"

library_preferences:
  - "CRITICAL: Prefer established libraries over custom implementations"
  - "Technical indicators: ta-lib, pandas-ta, tulipy (Python); tulind, technicalindicators (JS)"
  - "Exchange APIs: ccxt (universal Python/JS), python-binance, binance-api-node"
  - "Data manipulation: pandas (Python), lodash (JS)"
  - "Backtesting: backtrader, vectorbt (Python); ccxt built-in"
  - "Use library documentation URLs from research findings"
  - "Check version compatibility before implementation"
  - "Simpler code = fewer bugs = faster development"

code_quality:
  - "CRITICAL: Generate production-ready code, not examples"
  - "CRITICAL: Use classes to organize code (avoid procedural scripts)"
  - "CRITICAL: Keep entrypoints small (<50 lines) - main.py and backtest.py"
  - "CRITICAL: Business logic in classes, not in entrypoints"
  - "Follow SOLID principles (Single Responsibility, Open/Closed, etc.)"
  - "Follow Clean Architecture (separation of concerns - data, strategy, execution, risk)"
  - "Follow DRY (Don't Repeat Yourself)"
  - "Learn structure from documentation, don't impose rigid templates"
  - "Design architecture based on complexity (simple vs complex bots)"
  - "Use type hints for all functions and methods"
  - "Add docstrings for all classes and public methods"
  - "Implement try/except error handling"
  - "Add informative logging throughout"
  - "No hardcoded secrets - use os.getenv()"

testing:
  - "CRITICAL: Mirror test_researcher.py structure exactly"
  - "Test agent configuration (name='developer', model=DEFAULT_MODEL)"
  - "Test description quality (length 50-500 chars, key phrases)"
  - "Test instruction completeness (>1000 chars, 200+ lines, keywords, sections)"
  - "Test tools assigned (len=4, names match)"
  - "Test state key references (STATE_KEY_BOT_METADATA in instruction)"
  - "Test agent export (from the0.agents import developer_agent)"
  - "Test instruction examples (good/bad patterns with ✅/❌)"
  - "Aim for >80% code coverage"
```

---

## Implementation Blueprint

### State Management Schemas

**INPUT - research_data (from Researcher Agent):**
```python
# Read from session state
research = session.state.get('research_data', {})

# Schema:
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
```

**OUTPUT - bot_metadata (to Session State):**
```python
# Write to session state (Story 5 will implement automatic writing)
# For now, document in instruction that this is the expected output

# Schema:
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
    "execution_verified": true,        # NEW: Bot execution tested
    "backtest_verified": true,         # NEW: Backtest execution tested
    "libraries_used": [                # NEW: Libraries in requirements
        "ccxt==4.1.0",
        "pandas-ta==0.3.14b0",
        "pandas==2.1.4"
    ],
    "test_results": {                  # NEW: Execution test results
        "bot_execution": "success",
        "backtest_execution": "success",
        "backtest_trades": 42,
        "backtest_pnl": -2.3,          # Can be negative - that's OK
        "used_mock_data": false
    },
    "timestamp": "2025-11-12T15:00:00Z"
}

# State key: "bot_metadata"
```

---

### Task List (Implementation Order)

```yaml
Task 1: Create Developer Agent File Structure
CREATE the0/agents/developer.py:
  - ADD module docstring with Developer agent description
  - INCLUDE state schema documentation (bot_metadata)
  - ADD example usage in docstring
  - IMPORT Agent from google.adk.agents
  - IMPORT tools: save_artifact, deploy_bot, list_documentation, get_documentation
  - IMPORT constants: DEFAULT_MODEL, STATE_KEY_BOT_METADATA, STATE_KEY_RESEARCH from base
  - FOLLOW researcher.py structure exactly

Task 2: Write Agent Description Constant
DEFINE DEVELOPER_DESCRIPTION:
  - LENGTH: 100-200 characters (concise for LLM delegation)
  - CONTENT: "Trading bot development specialist that builds, tests, and deploys automated trading bots on the0 platform. Creates production-ready code following platform standards and best practices."
  - PURPOSE: Used by Supervisor in Story 4 for LLM-driven delegation
  - INCLUDE keywords: "development", "builds", "deploys", "production-ready", "platform standards"
  - CLEAR when to use: "Use this agent when..."

Task 3: Write Comprehensive Agent Instruction (CRITICAL)
DEFINE DEVELOPER_INSTRUCTION (200+ lines minimum):

  Section 1: Role Definition
  - "You are the Developer agent - a trading bot development specialist on the the0 team."
  - Core responsibilities list

  Section 2: Development Workflow (9 steps)
  Step 1: Receive Development Task
    - User requirements (strategy, platform, preferences)
    - Research findings (from session state)
    - Trading strategy specifications

  Step 2: Review Internal Documentation (CRITICAL)
    - "**CRITICAL - Always start here:**"
    - Use list_documentation to discover guides
    - "**MUST READ**: custom-bot-development/quick-start-guide.md"
    - "**MUST READ**: custom-bot-development/backtesting.md"
    - Read platform-specific guides as needed

  Step 3: Analyze Research Findings
    - Read research_data from session state
    - Code example: research = session.state.get('research_data', {})
    - Review: api_info, recommendations, sources
    - Identify recommended libraries from research

  Step 4: Design Bot Architecture
    - Simple Bot (single file): MA crossover, RSI, DCA
    - Complex Bot (modular): ML strategies, arbitrage, market making
    - Decision criteria
    - **Prefer established libraries** over custom implementations

  Step 5: Implement Bot Step-by-Step
    - "**Create files using save_artifact IMMEDIATELY - don't wait for perfection!**"
    - File creation order:
      1. bot-config.yaml
      2. requirements.txt (include all dependencies)
      3. main.py
      4. bot-schema.json
      5. README.md
      6. backtest.py
      7. backtest-schema.json
      8. Additional modules (if complex)

  Step 6: Create Backtesting Implementation
    - "**CRITICAL**: Every bot must have backtesting capability"
    - Implement backtest.py following backtesting.md guide
    - Create backtest-schema.json for parameters
    - Test strategy logic with historical data

  Step 7: Set Up Development Environment (CRITICAL)
    - "**CRITICAL**: Bot must be executable before completion"
    - For Python:
      - Create virtual environment: `python -m venv venv`
      - Activate: `source venv/bin/activate` (Linux/Mac) or `venv\\Scripts\\activate` (Windows)
      - Install dependencies: `pip install -r requirements.txt`
    - For JavaScript:
      - Install dependencies: `npm install` or `yarn install`
    - Verify all dependencies install successfully
    - Fix any dependency conflicts

  Step 8: Execute and Validate (CRITICAL)
    - "**CRITICAL**: Bot and backtest MUST run successfully"
    - Ask user: "Would you like to provide paper API credentials for testing? (optional)"
    - If yes: Collect credentials, set as temp environment variables
    - If no: Use mock data for testing
    - Execute bot: `python main.py` or `node main.js`
    - Verify: Bot runs without errors
    - Execute backtest: `python backtest.py` or `node backtest.js`
    - Verify: Backtest runs and produces results
    - Results can be unprofitable but MUST be mathematically correct
    - Fix any errors: Debug, update code, re-test
    - Document results in bot_metadata: execution_verified=true, backtest_verified=true

  Step 9: Package for Deployment
    - Verify all required files present
    - Verify execution and backtest passed
    - Use deploy_bot to create distribution ZIP
    - Store bot metadata in session state with verification flags

  Section 3: Required Artifacts Checklist
  Core Files (REQUIRED):
    - [ ] main.py - Entry point with clean main() function
    - [ ] bot-config.yaml - Bot configuration following the0 standard
    - [ ] requirements.txt - Dependencies with pinned versions
    - [ ] bot-schema.json - Input/output schema
    - [ ] README.md - Comprehensive documentation

  Testing Files (REQUIRED):
    - [ ] backtest.py - Backtesting implementation
    - [ ] backtest-schema.json - Backtest parameters schema

  Optional Library Files (for complex bots):
    - [ ] Data fetching modules
    - [ ] Indicator calculation modules
    - [ ] Strategy logic modules
    - [ ] Risk management modules

  Section 4: Engineering Principles & Code Quality (CRITICAL)
  **Focus on WHAT makes good code, not HOW to structure it**

  1. **Object-Oriented Design (CRITICAL):**
    - **Use classes** to encapsulate related functionality
    - Each class has single, clear responsibility
    - Classes make code testable, reusable, maintainable
    - Example classes: TradingBot, Strategy, DataFetcher, Indicator, PortfolioManager
    - Avoid procedural "script-style" code with many functions

  2. **SOLID Principles:**
    - **S**ingle Responsibility: Each class/method has one job
    - **O**pen/Closed: Extend via inheritance/composition, not modification
    - **L**iskov Substitution: Subtypes must be substitutable for base types
    - **I**nterface Segregation: Many specific interfaces > one general interface
    - **D**ependency Inversion: Depend on abstractions, not concrete implementations

  3. **Clean Architecture:**
    - **Separation of concerns**: Data, strategy, execution, risk in separate classes
    - **Small entrypoints**: main.py and backtest.py should be <50 lines
    - **Business logic in classes**: Import and orchestrate, don't implement inline
    - **Configuration-driven**: All parameters from bot-config.yaml or env vars
    - **No hardcoded values**: Symbols, timeframes, thresholds all configurable

  4. **Code Quality Essentials:**
    - **Type hints**: For all function/method signatures
    - **Docstrings**: For all classes and public methods
    - **Error handling**: Try/except with specific exceptions
    - **Logging**: Info for flow, warning for issues, error for failures
    - **Comments**: For complex algorithms or business logic
    - **No secrets in code**: Use os.getenv() for all credentials

  5. **the0 Platform Standards:**
    - **Learn from quick-start-guide.md**: Bot configuration, entrypoints, metadata
    - **Learn from backtesting.md**: Backtest structure, parameters, output format
    - **Environment variables**: API keys, secrets, configuration
    - **Both modes**: Support scheduled (periodic) and real-time (continuous) execution
    - **Comprehensive logging**: Track bot lifecycle, signals, trades, errors

  Section 5: Library Preferences (CRITICAL)
  **ALWAYS prefer established libraries over custom implementations:**

  Technical Indicators:
    - Python: ta-lib, pandas-ta, tulipy, vectorbt
    - JavaScript: tulind, technicalindicators, ta.js
    - Why: Battle-tested, optimized, well-documented

  Exchange APIs:
    - Universal: ccxt (Python & JavaScript) - supports 100+ exchanges
    - Python-specific: python-binance, python-kucoin
    - JavaScript-specific: binance-api-node, node-binance-api
    - Why: Handle authentication, rate limits, errors automatically

  Data Manipulation:
    - Python: pandas (DataFrames), numpy (arrays)
    - JavaScript: lodash (utilities), moment (dates)
    - Why: Clean, readable code with fewer bugs

  Backtesting Frameworks:
    - Python: backtrader, vectorbt, zipline
    - JavaScript: ccxt (built-in), backtesting.js
    - Why: Handle edge cases, proper position sizing, realistic fills

  **Benefits of using libraries:**
    - Faster development (no reinventing the wheel)
    - Fewer bugs (community-tested code)
    - Better performance (optimized implementations)
    - Easier maintenance (documentation, updates)

  **When to use libraries vs custom code:**
    - ✅ Use library: RSI, MACD, Bollinger Bands, EMA, SMA, etc.
    - ✅ Use library: Exchange API calls, WebSocket connections
    - ✅ Use library: Data manipulation, time series analysis
    - ❌ Custom code: Unique strategy logic, custom signals
    - ❌ Custom code: Platform-specific integrations

  Section 6: Credentials Handling (CRITICAL)
  **User Credentials for Testing:**
    - Ask user: "Would you like to provide paper trading API credentials for testing? This helps verify the bot works correctly with real exchange APIs."
    - If yes:
      - Collect: API key, API secret, (optional) passphrase
      - Validate: Non-empty strings, reasonable length
      - Set as temp env vars: EXCHANGE_API_KEY, EXCHANGE_API_SECRET, EXCHANGE_PASSPHRASE
      - Use ONLY for current execution - DO NOT persist
    - If no:
      - Use mock data for testing
      - Document: "Tested with mock data - user should test with paper trading before live"

  **Security Best Practices:**
    - NEVER save credentials to files
    - NEVER include credentials in artifacts
    - NEVER log credentials (even partially)
    - Use env vars for bot execution: os.getenv(), process.env
    - Document in README.md:
      ```
      # Setup
      export EXCHANGE_API_KEY="your_key_here"
      export EXCHANGE_API_SECRET="your_secret_here"
      python main.py
      ```

  **Mock Data Fallback:**
    - If no credentials: Generate realistic mock OHLCV data
    - Use historical data patterns (trends, volatility)
    - Document: "This backtest uses mock data - results are for demonstration only"

  Section 7: Language Choice
  Choose Python OR JavaScript based on:
    - User preference (if stated)
    - Platform compatibility
    - Library availability (check research findings)
    - Complexity of strategy
  **Stick to ONE language for entire bot**

  Section 8: Code Architecture & Engineering (CRITICAL)
  **Learn structure from documentation - do NOT impose rigid templates**

  **CRITICAL Engineering Principles:**

  1. **Use Classes for Organization:**
     - Create classes to encapsulate related functionality
     - Example: `TradingBot`, `Strategy`, `DataFetcher`, `Indicator`, `RiskManager`
     - Classes make code testable, maintainable, and extensible
     - Follow single responsibility principle - one class, one purpose

  2. **Keep Entrypoints Small and Purposeful:**
     - `main.py`: Should be <50 lines - just initialization and execution
     - `backtest.py`: Should be <50 lines - just setup and run backtest
     - Entrypoints should NOT contain business logic
     - Entrypoints should import and orchestrate classes

  3. **Separation of Concerns:**
     - Data layer: Fetch and normalize market data
     - Strategy layer: Calculate indicators and generate signals
     - Execution layer: Place orders and manage positions
     - Risk layer: Position sizing, stop losses, risk limits
     - Each layer in separate module/class

  4. **Module Organization:**
     - For simple bots: All classes in main.py is acceptable
     - For complex bots: Create separate modules (data.py, strategy.py, indicators.py, risk.py)
     - Import and use in entrypoints

  **Example Entrypoint Structure (NOT prescriptive - adapt as needed):**

  ```python
  # main.py - Small and purposeful entrypoint
  from bot import TradingBot  # Your implementation
  import logging

  logging.basicConfig(level=logging.INFO)

  def main():
      """Bot entrypoint - initialize and run."""
      bot = TradingBot()  # Initialize your bot class
      bot.run()           # Run your bot logic

  if __name__ == "__main__":
      main()
  ```

  ```python
  # backtest.py - Small and purposeful entrypoint
  from bot import TradingBot  # Your implementation
  from backtester import Backtester  # Your backtesting logic
  import logging

  logging.basicConfig(level=logging.INFO)

  def main():
      """Backtest entrypoint - initialize and run backtest."""
      bot = TradingBot()       # Initialize bot
      tester = Backtester(bot) # Initialize backtester
      results = tester.run()   # Run backtest
      print(results.summary()) # Display results

  if __name__ == "__main__":
      main()
  ```

  **What to Learn from Documentation:**
    - Bot configuration structure (bot-config.yaml)
    - Required metadata and fields
    - Entrypoint function signatures
    - Environment variable conventions
    - Platform integration patterns
    - Backtesting framework usage

  **What to Design Yourself:**
    - Class structure and organization
    - Module layout (single file vs. multi-module)
    - Method signatures and responsibilities
    - Error handling approach
    - Logging strategy
    - Testing approach

  Section 9: Session State Management
  Reading Research Data:
  ```python
  research = session.state.get('research_data', {})
  if research:
      summary = research.get('summary', '')
      findings = research.get('findings', [])
      recommendations = research.get('recommendations', [])
      sources = research.get('sources', [])
  ```

  Storing Bot Metadata:
  ```json
  {
      "bot_name": "momentum_btc_binance",
      "language": "python",
      "files_created": ["main.py", "bot-config.yaml", ...],
      "strategy_type": "momentum",
      "platform": "binance",
      "status": "ready_for_deploy",
      "timestamp": "2025-11-12T15:00:00Z"
  }
  ```
  Use state key: `bot_metadata`

  Section 9: When to Escalate to Supervisor
  Return to Supervisor when:
    - Bot is complete and all artifacts created
    - User requirements are unclear (ask Supervisor to clarify)
    - Research data is insufficient (request more research)
    - Critical platform documentation is missing
    - Deployment is ready for user review

  Section 10: Execution Validation Workflow (CRITICAL)
  After creating all artifacts:

  1. **Environment Setup:**
     ```bash
     # Python
     python -m venv venv
     source venv/bin/activate  # or venv\Scripts\activate on Windows
     pip install -r requirements.txt

     # JavaScript
     npm install  # or yarn install
     ```

  2. **Credentials Collection (Optional):**
     Ask user: "Would you like to provide paper API credentials for testing?"
     If yes: Collect and set as temp env vars (NEVER save to files)
     If no: Use mock data

  3. **Bot Execution Test:**
     ```bash
     python main.py  # or node main.js
     ```
     Expected: Runs without errors
     If errors: Debug, fix, re-test

  4. **Backtest Execution Test:**
     ```bash
     python backtest.py  # or node backtest.js
     ```
     Expected: Runs and produces results
     Results can be unprofitable but MUST be mathematically correct
     If errors: Debug, fix, re-test

  5. **Document Results:**
     Update bot_metadata with:
     - execution_verified: true/false
     - backtest_verified: true/false
     - test_results: summary of execution

  Section 11: IMPORTANT REMINDERS
    - **ALWAYS** consult quick-start-guide.md and backtesting.md first
    - **ALWAYS** prefer established libraries for indicators and APIs
    - **ALWAYS** use classes for organization (avoid procedural scripts)
    - **ALWAYS** keep entrypoints small (<50 lines) and purposeful
    - Create files **immediately** using save_artifact
    - **MUST** set up environment and test execution before completion
    - **MUST** ask user for credentials (don't assume)
    - **NEVER** persist credentials to files
    - **NEVER** put business logic in main.py or backtest.py (use classes)
    - **NEVER** impose rigid code templates (learn from docs, design yourself)
    - Bot and backtest **MUST** run successfully
    - Results must be **mathematically correct** (even if unprofitable)
    - **Iterate** based on user feedback and execution errors
    - Ensure **all required artifacts** are created
    - Follow the0 platform standards strictly
    - Document execution results in bot_metadata

  "You are an expert developer. Take pride in delivering production-ready, EXECUTABLE, well-engineered, properly documented trading bots that users can deploy with confidence.

  Focus on ENGINEERING PRINCIPLES:
  - Use classes to organize code
  - Keep entrypoints small and purposeful
  - Separate concerns into layers
  - Learn structure from documentation
  - Design architecture based on complexity

  A bot that doesn't run is worthless - always validate execution."

Task 4: Create Agent Instance
DEFINE developer_agent = Agent():
  - name="developer"
  - model=DEFAULT_MODEL (from base.py)
  - description=DEVELOPER_DESCRIPTION
  - instruction=DEVELOPER_INSTRUCTION
  - tools=[save_artifact, deploy_bot, list_documentation, get_documentation]
  - FOLLOW exact pattern from researcher.py

Task 5: Update Package __init__.py
UPDATE the0/agents/__init__.py:
  - IMPORT: from the0.agents.developer import developer_agent
  - ADD to __all__: "developer_agent"
  - MAINTAIN existing imports (base, researcher_agent)

Task 6: Create Comprehensive Test Suite
CREATE tests/the0/agents/test_developer.py:
  - MIRROR test_researcher.py structure EXACTLY
  - CREATE TestDeveloperAgent class
  - CREATE TestDeveloperConstants class

  TestDeveloperAgent tests:
    1. test_developer_configuration:
       - assert developer_agent.name == "developer"
       - assert developer_agent.model == DEFAULT_MODEL
       - assert developer_agent.description == DEVELOPER_DESCRIPTION
       - assert developer_agent.instruction == DEVELOPER_INSTRUCTION

    2. test_developer_model_uses_constant:
       - assert developer_agent.model == DEFAULT_MODEL
       - assert DEFAULT_MODEL == "gemini-2.5-flash"

    3. test_developer_description_quality:
       - description = developer_agent.description.lower()
       - assert "develop" in description or "build" in description
       - assert "specialist" in description or "expert" in description
       - assert "trading" in description or "bot" in description
       - assert 50 < len(developer_agent.description) < 500

    4. test_developer_instruction_completeness:
       - assert len(instruction) > 1000 (comprehensive)
       - lines = instruction.split("\n")
       - assert len(lines) >= 200 (200+ lines requirement)
       - Check required keywords: "develop", "build", "artifact", "backtest", "quality", "SOLID", "workflow", "documentation", "platform", "standards"
       - Check structured sections present

    5. test_developer_tools_assigned:
       - assert len(developer_agent.tools) == 4
       - Extract tool names from tools list
       - expected_tools = {"save_artifact", "deploy_bot", "list_documentation", "get_documentation"}
       - assert tool_names == expected_tools

    6. test_developer_state_key_reference:
       - instruction = developer_agent.instruction
       - assert STATE_KEY_BOT_METADATA in instruction or "bot_metadata" in instruction
       - assert STATE_KEY_RESEARCH in instruction or "research_data" in instruction

    7. test_developer_agent_export:
       - from the0.agents import developer_agent as exported_agent
       - assert exported_agent is not None
       - assert exported_agent.name == "developer"
       - assert exported_agent is developer_agent

    8. test_developer_instruction_examples:
       - instruction = developer_agent.instruction
       - assert "Example" in instruction or "example" in instruction.lower()
       - assert "✅" in instruction or "good" in instruction.lower()
       - assert "❌" in instruction or "bad" in instruction.lower() or "not" in instruction.lower()

  TestDeveloperConstants tests:
    1. test_default_model_constant:
       - assert DEFAULT_MODEL == "gemini-2.5-flash"

    2. test_state_key_constant:
       - assert STATE_KEY_BOT_METADATA == "bot_metadata"

Task 7: Update Documentation
UPDATE CLAUDE.md:
  - LOCATE "Multi-Agent System (MAS) Architecture" section
  - UPDATE Agent Hierarchy diagram to show Developer agent
  - UPDATE Tool Distribution table:
    | Tool | Supervisor | Researcher | Developer |
    |------|-----------|-----------|-----------|
    | tavily_search | ❌ | ✅ | ❌ |
    | browse_url | ❌ | ✅ | ❌ |
    | save_artifact | ❌ | ❌ | ✅ |
    | deploy_bot | ❌ | ❌ | ✅ |
    | list_documentation | ✅ | ✅ | ✅ |
    | get_documentation | ✅ | ✅ | ✅ |

  - UPDATE "Future Agents" to move Developer from future to complete
  - ADD "Developer Agent Details" subsection with:
    - File: the0/agents/developer.py
    - Purpose: Trading bot development specialist
    - Model: Gemini 2.5 Flash
    - Status: ✅ Complete (Story 3)
    - Capabilities list
    - Development workflow summary
    - Quality standards summary
    - State output: bot_metadata structure

Task 8: Run Validation
EXECUTE validation commands:
  1. make format (Black formatting)
  2. make lint (Flake8 linting)
  3. make test (Pytest suite, >80% coverage target)
  4. Verify all tests pass
  5. Review coverage report
```

---

## Validation Loop

### Level 1: Syntax & Style

```bash
# MUST RUN FIRST - Code formatting
make format
# OR: black the0/ tests/

# Verify formatting applied
git diff the0/agents/developer.py tests/the0/agents/test_developer.py

# Linting
make lint
# OR: flake8 the0/ tests/

# Python syntax validation
python -m py_compile the0/agents/developer.py
python -m py_compile tests/the0/agents/test_developer.py
```

### Level 2: Unit Tests

```bash
# Run all tests
make test
# OR: pytest

# Run developer agent tests specifically
pytest tests/the0/agents/test_developer.py -v

# Run with coverage
pytest tests/the0/agents/test_developer.py --cov=the0.agents.developer --cov-report=term

# Target: >80% coverage

# Check for test failures
# All tests MUST pass before proceeding
```

### Level 3: Integration Verification

```bash
# Verify agent can be imported
python -c "from the0.agents import developer_agent; print(developer_agent.name)"
# Expected output: developer

# Verify executable validation sections present
python -c "from the0.agents.developer import DEVELOPER_INSTRUCTION; assert 'environment' in DEVELOPER_INSTRUCTION.lower(); assert 'execute' in DEVELOPER_INSTRUCTION.lower(); assert 'credentials' in DEVELOPER_INSTRUCTION.lower(); print('✓ Executable validation sections present')"

# Verify library preferences documented
python -c "from the0.agents.developer import DEVELOPER_INSTRUCTION; assert 'library' in DEVELOPER_INSTRUCTION.lower() or 'libraries' in DEVELOPER_INSTRUCTION.lower(); assert 'ccxt' in DEVELOPER_INSTRUCTION.lower() or 'pandas' in DEVELOPER_INSTRUCTION.lower(); print('✓ Library preferences documented')"

# Verify agent configuration
python -c "from the0.agents.developer import developer_agent; print(f'Name: {developer_agent.name}, Model: {developer_agent.model}, Tools: {len(developer_agent.tools)}')"
# Expected output: Name: developer, Model: gemini-2.5-flash, Tools: 4

# Verify instruction length
python -c "from the0.agents.developer import developer_agent; lines = len(developer_agent.instruction.split('\n')); chars = len(developer_agent.instruction); print(f'Instruction: {lines} lines, {chars} chars')"
# Expected: >200 lines, >1000 chars

# Manual review of instruction quality
python -c "from the0.agents.developer import DEVELOPER_INSTRUCTION; print(DEVELOPER_INSTRUCTION)" | less
# Verify: sections, examples, quality standards, workflow
```

### Level 4: Documentation Verification

```bash
# Check CLAUDE.md updated
grep -A 10 "Developer Agent" CLAUDE.md
grep "developer_agent" CLAUDE.md

# Verify __init__.py export
grep "developer_agent" the0/agents/__init__.py

# Check git status
git status
# Should show:
# - the0/agents/developer.py (new)
# - the0/agents/__init__.py (modified)
# - tests/the0/agents/test_developer.py (new)
# - CLAUDE.md (modified)
```

---

## Final Validation Checklist

Before marking story complete, verify ALL items:

**Agent Implementation:**
- [ ] File created: `the0/agents/developer.py`
- [ ] Module docstring with state schema present
- [ ] DEVELOPER_DESCRIPTION defined (100-200 chars)
- [ ] DEVELOPER_INSTRUCTION defined (200+ lines)
- [ ] developer_agent instance created with Agent()
- [ ] Agent name: "developer"
- [ ] Agent model: "gemini-2.5-flash" (DEFAULT_MODEL)
- [ ] Four tools assigned: save_artifact, deploy_bot, list_documentation, get_documentation
- [ ] Instruction includes all required sections (workflow, standards, examples, state management)
- [ ] Instruction includes 9-step workflow with environment setup and validation
- [ ] Instruction includes good/bad examples with ✅/❌
- [ ] Instruction references STATE_KEY_BOT_METADATA and STATE_KEY_RESEARCH
- [ ] Instruction includes production-ready code example (main.py structure)
- [ ] **CRITICAL**: Instruction includes executable validation workflow (Step 7-8)
- [ ] **CRITICAL**: Instruction includes library preferences section
- [ ] **CRITICAL**: Instruction includes credentials handling section
- [ ] **CRITICAL**: Instruction emphasizes bots MUST run before completion

**Testing:**
- [ ] File created: `tests/the0/agents/test_developer.py`
- [ ] TestDeveloperAgent class with 8+ tests
- [ ] TestDeveloperConstants class with 2+ tests
- [ ] All tests pass: `pytest tests/the0/agents/test_developer.py -v`
- [ ] Code coverage >80%: `pytest --cov=the0.agents.developer --cov-report=term`
- [ ] Test structure mirrors test_researcher.py

**Package Integration:**
- [ ] Updated: `the0/agents/__init__.py` with developer_agent import
- [ ] Updated: `the0/agents/__init__.py` with __all__ export
- [ ] Agent can be imported: `from the0.agents import developer_agent`
- [ ] Import test passes in test_developer_agent_export

**Documentation:**
- [ ] Updated: CLAUDE.md with Developer agent section
- [ ] Agent Hierarchy diagram updated
- [ ] Tool Distribution table updated
- [ ] Developer Agent Details subsection added
- [ ] bot_metadata schema documented

**Code Quality:**
- [ ] Code formatted: `make format` passes
- [ ] Linting passes: `make lint` passes
- [ ] No Python syntax errors
- [ ] Type hints used appropriately
- [ ] Docstrings present and clear
- [ ] Follows existing code patterns from researcher.py

**Functional Verification:**
- [ ] Agent instance created successfully
- [ ] Agent properties accessible (name, model, description, instruction, tools)
- [ ] Tools list contains 4 tools
- [ ] Instruction length >200 lines, >1000 chars
- [ ] State key constants referenced in instruction
- [ ] All required instruction sections present
- [ ] Executable validation sections present (environment, execute, credentials keywords)
- [ ] Library preferences documented (ccxt, pandas-ta, etc.)
- [ ] Credentials handling documented (ask user, don't persist)
- [ ] bot_metadata schema includes execution_verified, backtest_verified fields

---

## Anti-Patterns to Avoid

**Agent Definition:**
- ❌ Don't copy/paste researcher instruction without adaptation - Developer has different workflow
- ❌ Don't skip engineering principles - OOP, SOLID, Clean Architecture are critical
- ❌ Don't prescribe rigid code templates - provide principles, not structure
- ❌ Don't forget bot-config.yaml structure - platform standards are paramount
- ❌ Don't skip backtesting requirements - every bot must have backtest capability
- ❌ Don't skip executable validation workflow - bots MUST run before completion
- ❌ Don't skip library preferences section - prefer established libraries
- ❌ Don't skip credentials handling - ask user, don't assume
- ❌ Don't forget to emphasize classes over procedural code
- ❌ Don't forget to emphasize small entrypoints (<50 lines)

**Instruction Writing:**
- ❌ Don't write vague instructions - be specific about principles, not rigid structure
- ❌ Don't prescribe exact code structure - teach principles, let agent design
- ❌ Don't omit good/bad examples - ✅/❌ patterns crucial for clarity
- ❌ Don't forget state management examples - show exactly how to read research_data and write bot_metadata
- ❌ Don't skip "CRITICAL" and "MUST" emphasis - highlight non-negotiable requirements
- ❌ Don't forget escalation conditions - agent needs to know when to return to Supervisor
- ❌ Don't forget to emphasize learning from documentation first

**State Management:**
- ❌ Don't forget to document reading research_data from state
- ❌ Don't forget to document writing bot_metadata to state
- ❌ Don't use wrong state keys - use constants from base.py
- ❌ Don't omit state schema documentation - include JSON examples

**Tool Usage:**
- ❌ Don't confuse tool order - save_artifact first (multiple times), deploy_bot last (once)
- ❌ Don't forget ToolContext parameter - tools need session awareness
- ❌ Don't skip documentation tools - list_documentation and get_documentation are critical
- ❌ Don't forget to emphasize quick-start-guide.md and backtesting.md

**Executable Validation:**
- ❌ Don't skip environment setup - venv/node_modules required
- ❌ Don't skip execution tests - bot and backtest MUST run
- ❌ Don't assume credentials - ask user first
- ❌ Don't persist credentials - security risk
- ❌ Don't accept execution errors - debug and fix
- ❌ Don't skip bot_metadata updates - document execution results
- ❌ Don't accept incorrect backtest results - math must be correct

**Library Usage:**
- ❌ Don't reinvent the wheel - use established libraries
- ❌ Don't write custom indicators - use ta-lib, pandas-ta, etc.
- ❌ Don't write custom exchange APIs - use ccxt
- ❌ Don't ignore research recommendations - libraries already researched
- ❌ Don't use outdated versions - check latest stable

**Code Architecture:**
- ❌ Don't write procedural "script-style" code - use classes
- ❌ Don't put business logic in entrypoints - keep main.py/backtest.py small
- ❌ Don't create monolithic functions - break into classes and methods
- ❌ Don't mix concerns - separate data, strategy, execution, risk
- ❌ Don't follow rigid templates - design based on complexity
- ❌ Don't ignore documentation patterns - learn from quick-start-guide.md

**Testing:**
- ❌ Don't skip tests - they ensure quality and catch regressions
- ❌ Don't copy tests without updating assertions - developer != researcher
- ❌ Don't ignore coverage - aim for >80%
- ❌ Don't forget to test agent export from __init__.py
- ❌ Don't skip instruction completeness tests - verify 200+ lines, keywords, sections

**Documentation:**
- ❌ Don't forget to update CLAUDE.md - it's the architecture source of truth
- ❌ Don't forget to update __init__.py exports - agent must be importable
- ❌ Don't skip Tool Distribution table - it shows agent capabilities at a glance
- ❌ Don't forget bot_metadata schema documentation - downstream agents need it

---

## PR Message Template

When implementation is complete, use this PR message template:

```markdown
## Feature: Developer Agent - Trading Bot Development Specialist (Story 3)

Implemented specialized Developer Agent for automated trading bot creation, backtesting, and deployment on the0 platform.

### Background

Part of Multi-Agent System (MAS) architecture enabling specialized agents for research, development, and future optimization/monitoring. Developer Agent focuses solely on bot creation following platform standards and code quality guidelines.

### Changes Made

#### Agent Implementation
- Created `the0/agents/developer.py` with comprehensive agent definition
- Agent name: "developer", model: "gemini-2.5-flash"
- 4 tools assigned: save_artifact, deploy_bot, list_documentation, get_documentation
- Comprehensive instructions (200+ lines) covering:
  - Development workflow (7 steps)
  - Required artifacts checklist (7 core files)
  - Code quality standards (SOLID, Clean Architecture, DRY)
  - the0 platform standards
  - Production-ready code examples
  - State management (read research_data, write bot_metadata)

#### Testing
- Created `tests/the0/agents/test_developer.py` with comprehensive test suite
- 10+ tests covering configuration, instruction quality, tool assignment, state keys
- Test structure mirrors test_researcher.py for consistency
- Code coverage: >80%

#### Package Integration
- Updated `the0/agents/__init__.py` to export developer_agent
- Agent importable: `from the0.agents import developer_agent`

#### Documentation
- Updated CLAUDE.md with Developer Agent Details section
- Updated Agent Hierarchy diagram
- Updated Tool Distribution table
- Documented bot_metadata state schema

### Validation Steps

#### Code Quality & Safety
- [x] Python syntax validation passed
- [x] Code formatting with black applied: `make format`
- [x] Linting with flake8 passed: `make lint`

#### Testing
- [x] Unit tests passing: `pytest tests/the0/agents/test_developer.py -v`
- [x] Code coverage >80%
- [x] All tests pass: `make test`

#### Functional Verification
- [x] Agent instance created successfully
- [x] Agent properties verified (name, model, tools)
- [x] Instruction length verified (>200 lines, >1000 chars)
- [x] Agent importable from package

### Known Issues

None

### Follow-up Tasks

- Story 4: Supervisor Transformation (integrate Developer as sub-agent)
- Story 5: State Management (implement automatic state writing for bot_metadata)
- Future: Add code review capabilities, automated testing validation

### Agent Performance

Developer agent ready for integration into Supervisor (Story 4). Instruction quality optimized for production-ready code generation following the0 platform standards.
```

Save as: `PR_MSG/developer-agent.md`

---

## PRP Quality Score

**Confidence Level for One-Pass Implementation Success: 9.5/10**

**Strengths:**
- ✅ Comprehensive context (ADK docs, codebase files, external research)
- ✅ Clear reference implementation (researcher.py) to mirror
- ✅ Complete instruction template (200+ lines with all required sections including NEW executable validation)
- ✅ **CRITICAL additions**: Executable validation workflow (environment setup, execution tests)
- ✅ **CRITICAL additions**: Library preferences (prefer established libraries over custom)
- ✅ **CRITICAL additions**: Credentials handling (ask user, don't persist)
- ✅ **CRITICAL additions**: Backtest execution verification (must run and produce correct results)
- ✅ Executable validation gates (make format, lint, test)
- ✅ State management schemas updated (execution_verified, backtest_verified, test_results)
- ✅ Testing patterns established (test_researcher.py as reference)
- ✅ Anti-patterns expanded (executable validation, library usage)
- ✅ Code quality examples (production-ready main.py structure)
- ✅ Tool usage patterns clear (save_artifact, deploy_bot, docs)
- ✅ Platform standards emphasized (quick-start-guide, backtesting)
- ✅ 9-step workflow with environment setup and validation clearly defined

**Minor Gaps (preventing 10/10):**
- Story 5 (State Management) not yet implemented - automatic state writing will come later
- Execution validation requires Bash tool access (agent must be able to run commands)
- User interaction for credentials requires coordination with frontend

**Mitigation:**
- Document that state writing is manual for now (Story 5 will automate)
- Bash tool access already available - agent can run python/node commands
- User questions supported via agent instruction - can ask for credentials
- Follow test_researcher.py patterns exactly - they work
- Developer agent designed to be ready for Story 4 integration

**Key Improvements from Original:**
1. **Executable validation**: Bots must run before completion (addresses "MUST be executable")
2. **Environment setup**: Clear instructions for venv/node_modules setup
3. **Credentials handling**: Ask user for paper API keys, don't persist (addresses "ask user if they want to provide credentials")
4. **Library preferences**: Prefer established libraries for calculations (addresses "opt for libraries to simplify calculations")
5. **Backtest verification**: Backtest must run and produce correct results (addresses "backtests NEED to be correct")
6. **Enhanced bot_metadata**: Added execution_verified, backtest_verified, libraries_used, test_results fields

**Overall:** This updated PRP provides comprehensive context, executable validation workflow, library preferences, and credentials handling. The agent will not only create bots but ENSURE they run correctly before completion. The 9-step workflow with environment setup and validation ensures production-ready, executable bots every time.
