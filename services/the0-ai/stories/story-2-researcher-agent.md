# Story 2: Researcher Agent

**Epic:** Specialized Agent Development
**Status:** Not Started
**Estimated Effort:** 8-10 hours
**Dependencies:** Story 1 (Tavily Integration)

---

## Description

Create a specialized Researcher Agent focused on quantitative research and information gathering. This agent uses Tavily search and web browsing to research trading strategies, APIs, libraries, and technical documentation, returning structured findings with citations.

---

## Why

- **Specialization**: Dedicated agent for research improves quality
- **Clear Responsibility**: Separate research from development concerns
- **Expertise**: Optimized prompts for research methodology
- **Scalability**: Foundation for adding more research capabilities
- **Quality**: Structured output with citations and sources

---

## What

Create a Researcher Agent that:
- Performs comprehensive web research using Tavily
- Analyzes and verifies information from multiple sources
- Researches trading strategies, APIs, and technical documentation
- Returns structured findings with citations
- Stores research in session state for other agents to use

---

## Tasks

### 1. Create Agent Definition
- [ ] Create `the0/agents/researcher.py`
- [ ] Define agent with `Agent()` constructor
- [ ] Set name: "researcher"
- [ ] Set model: "gemini-2.5-flash"
- [ ] Write clear description for LLM delegation
- [ ] Assign tools: tavily_search, browse_url, list_documentation, get_documentation

### 2. Write Agent Instructions
- [ ] Define agent personality and role
- [ ] Document research workflow (query formulation → search → verify → structure)
- [ ] Specify when to use Tavily vs browse_url vs documentation
- [ ] Define output format (Summary, Findings, Recommendations, Sources)
- [ ] Add citation requirements (all claims must have sources)
- [ ] Include research quality standards
- [ ] Add examples of good research output

### 3. Implement State Management
- [ ] Define how to store research findings in session state
- [ ] Use structured JSON format with state key "research_data"
- [ ] Include: query, summary, findings, recommendations, sources
- [ ] Add timestamp for research freshness

### 4. Testing
- [ ] Create `tests/the0/agents/test_researcher.py`
- [ ] Test agent configuration (name, model, tools)
- [ ] Test agent can execute standalone
- [ ] Test research workflow with mocked tools
- [ ] Test state storage functionality
- [ ] Achieve >80% code coverage

### 5. Documentation
- [ ] Add comprehensive docstrings
- [ ] Document research workflow
- [ ] Provide example research outputs
- [ ] Document state schema for research_data
- [ ] Update CLAUDE.md with Researcher agent details

---

## Acceptance Criteria

- [ ] Researcher agent defined in `the0/agents/researcher.py`
- [ ] Agent name: "researcher", model: "gemini-2.5-flash"
- [ ] Four tools assigned: tavily_search, browse_url, list_documentation, get_documentation
- [ ] Comprehensive instructions (150+ lines) covering:
  - Role and responsibilities
  - Research workflow
  - Tool usage guidelines
  - Output format requirements
  - Citation requirements
  - Quality standards
- [ ] Research findings stored in session state with key "research_data"
- [ ] Structured JSON format defined
- [ ] Unit tests achieve >80% coverage
- [ ] Integration test demonstrates research workflow
- [ ] Documentation complete
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### Agent Definition

```python
# the0/agents/researcher.py

from google.genai.agents import Agent
from the0.tools.tavily_search import tavily_search
from the0.tools.web_browser import browse_url
from the0.tools.documentation import list_documentation, get_documentation

researcher_agent = Agent(
    name="researcher",
    model="gemini-2.5-flash",
    description=(
        "Quantitative research specialist that performs web searches, "
        "analyzes market data, and researches trading strategies, "
        "APIs, and technical documentation. Returns structured research "
        "findings with citations."
    ),
    instruction="""
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
- Automatic citation URLs

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
- Finding 1: [Specific insight] [Source](URL)
- Finding 2: [Specific insight] [Source](URL)
- Finding 3: [Specific insight] [Source](URL)
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

## Sources
1. [Source Title](URL) - Brief description and relevance
2. [Source Title](URL) - Brief description and relevance
[Continue for all sources]

## Citation Requirements

**CRITICAL**: Every factual claim MUST have a citation:
- Use markdown format: [Description](URL)
- Cite immediately after claim: "Binance supports WebSockets [Source](URL)"
- Include source URLs in Sources section
- Prefer official documentation and authoritative sources
- Note version numbers: "as of Python 3.11 [Source](URL)"

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

### Completeness
- Answer all aspects of the research request
- If information is unavailable, explicitly state gaps
- Provide alternatives when preferred option unavailable
- Include pros/cons for important decisions

## Session State Storage

Store your research in structured JSON format:

```json
{
    "query": "Original research request from Supervisor",
    "summary": "Executive summary of findings",
    "findings": [
        {
            "point": "Key finding or insight",
            "source": "https://source-url.com",
            "confidence": "high|medium|low"
        }
    ],
    "recommendations": [
        "Actionable recommendation 1",
        "Actionable recommendation 2"
    ],
    "sources": [
        {
            "title": "Source Title",
            "url": "https://source-url.com",
            "relevance": "Why this source matters",
            "published": "2025-01-15" (if available)
        }
    ],
    "timestamp": "ISO 8601 timestamp"
}
```

Use the state key: `research_data`

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
- REST API endpoint: `/api/v3/ticker/price` for current prices [Binance Docs](https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker)
- WebSocket stream: `wss://stream.binance.com:9443/ws/<symbol>@trade` for real-time trades [Binance Docs](https://binance-docs.github.io/apidocs/spot/en/#websocket-market-streams)
- Python library: `python-binance` recommended, actively maintained [PyPI](https://pypi.org/project/python-binance/)
...

### Example 2: Strategy Research
**Request**: "Research momentum trading strategies suitable for crypto"

**Good Response**:
## Research Summary
Momentum trading strategies for crypto typically use RSI, MACD, and moving averages to identify trend strength. Studies show 65-70% win rates in trending markets but perform poorly in sideways markets [Research Paper](URL). Key risk: high volatility requires strict stop-losses.

## Key Findings
- RSI Momentum: Buy when RSI crosses above 30, sell above 70 [Investopedia](URL)
...

## Tools Available

1. **tavily_search**: Primary research tool
   - Use for: Broad information gathering, current data, comparisons
   - Best for: Technical documentation, API research, strategy overviews

2. **browse_url**: Deep dive tool
   - Use for: Reading full documentation pages, detailed tutorials
   - Best for: Implementation details, code examples, specifications

3. **list_documentation**: Internal docs discovery
   - Use for: Finding relevant the0 platform guides
   - Best for: Understanding platform capabilities and standards

4. **get_documentation**: Internal docs reading
   - Use for: Reading specific the0 platform documentation
   - Best for: Platform-specific implementation guidance

## Important Notes

- ALWAYS cite sources - no citation means no credibility
- Quality over quantity - 3 authoritative sources > 10 mediocre ones
- Be specific - vague research is not helpful
- Structure matters - use the prescribed output format
- Time-sensitive info - note dates and versions
- When in doubt, verify across multiple sources

You are an expert researcher. Take pride in delivering thorough, accurate, well-cited research that enables the team to build exceptional trading bots.
    """,
    tools=[
        tavily_search,
        browse_url,
        list_documentation,
        get_documentation,
    ],
)
```

### State Storage Format

```python
# Example of research_data stored in session state
{
    "query": "Research Binance Spot API for real-time price data",
    "summary": "Binance Spot API v3 provides comprehensive real-time market data via REST and WebSocket. Python library python-binance (v1.0.19) recommended.",
    "findings": [
        {
            "point": "REST endpoint /api/v3/ticker/price provides current prices for all symbols",
            "source": "https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker",
            "confidence": "high"
        },
        {
            "point": "WebSocket streams available for real-time trade updates with no rate limits",
            "source": "https://binance-docs.github.io/apidocs/spot/en/#websocket-market-streams",
            "confidence": "high"
        },
        {
            "point": "python-binance library actively maintained, version 1.0.19 latest",
            "source": "https://pypi.org/project/python-binance/",
            "confidence": "high"
        }
    ],
    "recommendations": [
        "Use WebSocket for real-time data to avoid rate limits",
        "Implement python-binance library for easier integration",
        "Add reconnection logic for WebSocket disconnections",
        "Store API credentials securely in environment variables"
    ],
    "sources": [
        {
            "title": "Binance API Documentation - Spot",
            "url": "https://binance-docs.github.io/apidocs/spot/en/",
            "relevance": "Official API documentation with complete specification",
            "published": "2025"
        },
        {
            "title": "python-binance PyPI",
            "url": "https://pypi.org/project/python-binance/",
            "relevance": "Python library package information and version history"
        }
    ],
    "timestamp": "2025-11-10T14:30:00Z"
}
```

---

## Technical Considerations

### Prompt Engineering
The instructions are critical - they:
- Define research methodology
- Set quality standards
- Provide examples
- Establish output format
- Guide tool selection

Test and iterate on instructions based on actual research quality.

### Tool Selection Logic
- **Tavily**: Broad research, comparisons, current information
- **browse_url**: Specific pages, detailed docs, code examples
- **Documentation tools**: Platform-specific guidance, standards

### State Management
- Use standardized JSON structure
- Keep state size reasonable (<50KB)
- Include timestamps for freshness
- Version research data if format changes

---

## Risks & Mitigations

### Medium Risk: Research Quality Depends on Prompt
**Risk:** Poor instructions lead to low-quality research
**Impact:** Downstream agents get incomplete/inaccurate info
**Mitigation:**
- Extensive instructions with examples
- Iterative testing and refinement
- Quality checks in testing

**Contingency:** Update instructions based on real usage patterns

### Low Risk: State Size Limits
**Risk:** Research data too large for session state
**Impact:** State storage fails, data loss
**Mitigation:**
- Summarize when needed
- Monitor state sizes in testing
- Set reasonable limits in instructions

**Contingency:** Implement truncation with priority (keep summary + top findings)

---

## Testing Strategy

```python
# tests/the0/agents/test_researcher.py

import pytest
from the0.agents.researcher import researcher_agent

def test_researcher_configuration():
    """Test researcher agent properly configured."""
    assert researcher_agent.name == "researcher"
    assert researcher_agent.model == "gemini-2.5-flash"
    assert len(researcher_agent.tools) == 4
    assert "research" in researcher_agent.description.lower()
    assert "citation" in researcher_agent.instruction.lower()

@pytest.mark.asyncio
async def test_researcher_standalone():
    """Test researcher can execute independently (mocked)."""
    # Mock tools
    # Provide research task
    # Verify structured output
    # Check session state written
    pass

@pytest.mark.asyncio
async def test_researcher_state_storage():
    """Verify research data stored in correct format."""
    # Execute research (mocked)
    # Check session.state['research_data']
    # Verify JSON structure matches schema
    pass

@pytest.mark.asyncio
async def test_researcher_citations():
    """Verify research includes citations."""
    # Execute research (mocked)
    # Parse output
    # Verify markdown links present
    # Verify Sources section populated
    pass
```

---

## Related Stories

**Depends On:**
- Story 0: Foundation & Setup
- Story 1: Tavily Integration

**Blocks:**
- Story 4: Supervisor Transformation (needs researcher as sub-agent)
- Story 5: State Management (defines research_data schema)

**Related:**
- Story 3: Developer Agent (will read researcher's state)
- Story 6: Testing & Validation

---

## Notes

- Focus on prompt quality - this determines research quality
- Test with real research scenarios during development
- Iterate on instructions based on output quality
- Consider adding research templates for common queries
- Future: Add specialized research agents (market data, backtesting analysis)

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Agent configuration tested
- [ ] Instructions comprehensive and examples included
- [ ] State storage working correctly
- [ ] Unit tests passing (>80% coverage)
- [ ] Integration test demonstrates full workflow
- [ ] Documentation complete
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
