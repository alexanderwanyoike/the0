# Story 7: Documentation & Knowledge Transfer

**Epic:** Documentation & Training
**Status:** Not Started
**Estimated Effort:** 8-10 hours
**Dependencies:** Stories 1-6 (all implementation complete)

---

## Description

Comprehensive documentation for the Multi-Agent System including architecture docs, developer guides, troubleshooting, examples, and migration guidance. Ensures the team and future developers can understand, maintain, and extend the MAS.

---

## Why

- **Knowledge Transfer**: Enable team to work with MAS
- **Maintenance**: Support future development and debugging
- **Onboarding**: Help new developers understand architecture
- **Troubleshooting**: Reduce time to resolve issues
- **Evolution**: Document patterns for adding new agents

---

## What

Create comprehensive documentation covering:
- MAS architecture and design decisions
- Developer guides for common tasks
- Troubleshooting guide
- Workflow examples
- Migration guide from single-agent to MAS
- API documentation updates

---

## Tasks

### 1. Update CLAUDE.md
- [ ] Add MAS Architecture section
- [ ] Create agent hierarchy diagram (ASCII art)
- [ ] Document communication patterns
- [ ] Add tool distribution table
- [ ] Include workflow examples
- [ ] Document state management patterns

### 2. Create Developer Guides
- [ ] Create `docs/mas-architecture.md`
- [ ] Create `docs/developer-guide/adding-agents.md`
- [ ] Create `docs/developer-guide/modifying-instructions.md`
- [ ] Create `docs/developer-guide/tool-development.md`
- [ ] Create `docs/developer-guide/state-management.md`

### 3. Create Troubleshooting Guide
- [ ] Create `docs/troubleshooting.md`
- [ ] Document common issues and solutions
- [ ] Add debugging techniques
- [ ] Include performance optimization tips

### 4. Create Examples & Tutorials
- [ ] Create `docs/examples/workflow-examples.md`
- [ ] Add simple bot creation example
- [ ] Add complex research task example
- [ ] Add iterative development example
- [ ] Include state structure examples

### 5. Create Migration Guide
- [ ] Create `docs/migration-guide.md`
- [ ] Document changes from single-agent to MAS
- [ ] Include backward compatibility notes
- [ ] Add rollback procedures

### 6. Update API Documentation
- [ ] Update endpoint documentation if changed
- [ ] Document new environment variables
- [ ] Update configuration guide
- [ ] Add Tavily API key setup instructions

---

## Acceptance Criteria

- [ ] CLAUDE.md updated with MAS architecture
- [ ] Architecture diagrams created (ASCII art or mermaid)
- [ ] Developer guides complete and tested
- [ ] Troubleshooting guide covers common scenarios
- [ ] Examples demonstrate key workflows
- [ ] Migration guide assists transition
- [ ] API documentation updated
- [ ] All documentation reviewed and proofread
- [ ] Documentation versioned with code
- [ ] Code examples in docs are tested
- [ ] Links between docs work correctly

---

## Implementation Details

### CLAUDE.md Updates

```markdown
# CLAUDE.md (add new section)

## Multi-Agent System (MAS) Architecture

### Overview

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
    │  Agent           │  │  Agent             │
    │  - Tavily search │  │  - Build bots      │
    │  - Quantitative  │  │  - Backtest        │
    │  - Analysis      │  │  - Deploy          │
    └──────────────────┘  └────────────────────┘
```

### Communication Patterns

Agents communicate via two mechanisms:

1. **LLM-Driven Delegation**: Supervisor uses `transfer_to_agent()` function calls
2. **Shared Session State**: Agents read/write structured data to session state

**Flow Example**:
```
User → Supervisor → Researcher → [writes research_data to state]
                  → Supervisor → [reads research_data]
                  → Developer → [reads research_data, creates artifacts]
                  → Supervisor → [presents results to user]
```

### Tool Distribution

| Tool | Supervisor | Researcher | Developer |
|------|-----------|-----------|-----------|
| tavily_search | ❌ | ✅ | ❌ |
| browse_url | ❌ | ✅ | ❌ |
| save_artifact | ❌ | ❌ | ✅ |
| deploy_bot | ❌ | ❌ | ✅ |
| list_documentation | ✅ | ✅ | ✅ |
| get_documentation | ✅ | ✅ | ✅ |

### State Management

Agents use session state for data sharing:

**research_data** (written by Researcher):
```json
{
    "query": "original research request",
    "summary": "executive summary",
    "findings": [{"point": "...", "source": "...", "confidence": "high"}],
    "recommendations": ["...", "..."],
    "sources": [{"title": "...", "url": "...", "relevance": "..."}]
}
```

**bot_metadata** (written by Developer):
```json
{
    "bot_name": "momentum_btc",
    "language": "python",
    "files_created": ["main.py", "bot-config.yaml", ...],
    "strategy_type": "momentum",
    "platform": "binance",
    "status": "ready_for_deploy"
}
```

See `the0/agents/state_schema.py` for complete schema definitions.
```

### Developer Guide: Adding Agents

```markdown
# docs/developer-guide/adding-agents.md

## Adding New Agents to the MAS

### When to Add a New Agent

Consider adding a new agent when:
- The task requires specialized expertise
- The task is complex enough to warrant focused instructions
- The task would benefit from dedicated tools
- The task is logically separate from existing agents

### Step-by-Step Guide

#### 1. Create Agent Definition

Create a new file in `the0/agents/`:

```python
# the0/agents/new_agent.py

from google.genai.agents import Agent
from the0.tools.some_tool import some_tool

new_agent = Agent(
    name="new_agent",  # Unique name for delegation
    model="gemini-2.5-flash",
    description=(
        "Clear, concise description of what this agent does. "
        "This helps the Supervisor decide when to delegate."
    ),
    instruction="""
    You are the New Agent - [role description].

    ## Core Responsibilities
    1. [Responsibility 1]
    2. [Responsibility 2]

    ## Workflow
    [Step-by-step workflow]

    ## Output Format
    [Expected output structure]

    ## When to Escalate
    [When to return to Supervisor]
    """,
    tools=[
        some_tool,
        # ... other tools
    ],
)
```

#### 2. Assign Appropriate Tools

Determine which tools the agent needs:
- **Specialized tools**: Unique to this agent
- **Shared tools**: Used by multiple agents (e.g., documentation)
- **Avoid duplication**: Don't give all tools to all agents

#### 3. Write Comprehensive Instructions

Agent quality depends on instructions. Include:
- Role and responsibilities
- Step-by-step workflow
- Tool usage guidelines
- Output format requirements
- Quality standards
- When to escalate to Supervisor

#### 4. Add to Supervisor Sub-Agents

Update `the0/agent.py`:

```python
from the0.agents.new_agent import new_agent

supervisor_agent = Agent(
    name="the0",
    # ... existing config
    sub_agents=[
        researcher_agent,
        developer_agent,
        new_agent,  # Add here
    ],
)
```

#### 5. Update Supervisor Instructions

Add delegation logic:

```python
instruction="""
...
### Use New Agent When:
- ✅ "Task that requires new agent"
- ✅ User requests specific capability
...
"""
```

#### 6. Update State Schema (if needed)

If agent needs to store/read state, update `the0/agents/state_schema.py`:

```python
class NewAgentData(BaseModel):
    version: str = Field(default="1.0")
    # ... fields

class SessionStateManager:
    NEW_AGENT_KEY = "new_agent_data"

    @staticmethod
    def store_new_agent_data(state: dict, data: NewAgentData):
        # ...
```

#### 7. Write Tests

Create `tests/the0/agents/test_new_agent.py`:

```python
def test_new_agent_configuration():
    assert new_agent.name == "new_agent"
    assert len(new_agent.tools) > 0
    # ...

@pytest.mark.asyncio
async def test_new_agent_workflow():
    # Test agent execution
    pass
```

#### 8. Update Documentation

- Add to CLAUDE.md agent hierarchy
- Update tool distribution table
- Document state schema changes
- Add examples of agent usage

### Example: Adding a "Backtester" Agent

[Complete example with code]

### Best Practices

- **Single Responsibility**: Each agent has one clear purpose
- **Clear Descriptions**: Help Supervisor delegate correctly
- **Comprehensive Instructions**: Quality depends on prompt
- **Minimal Tools**: Only assign necessary tools
- **Test Thoroughly**: Unit + integration tests
- **Document Well**: Future developers will thank you
```

### Troubleshooting Guide

```markdown
# docs/troubleshooting.md

## Troubleshooting the Multi-Agent System

### Agent Not Delegating

**Symptom**: Supervisor tries to do everything itself instead of delegating

**Diagnosis**:
- Check agent descriptions in agent definitions
- Review Supervisor instructions for delegation logic
- Look at logs for transfer_to_agent calls

**Solutions**:
1. Make agent descriptions more specific and action-oriented
2. Add explicit examples in Supervisor instructions
3. Ensure agent names match in delegation logic

**Example Fix**:
```python
# Bad description
description="Research agent"

# Good description
description=(
    "Quantitative research specialist that performs web searches, "
    "analyzes market data, and researches trading strategies"
)
```

### State Not Shared Between Agents

**Symptom**: Developer can't read Researcher's data

**Diagnosis**:
- Check session state keys match
- Verify SessionStateManager usage
- Look for serialization errors in logs

**Solutions**:
1. Verify state keys: `research_data`, `bot_metadata`
2. Check Pydantic validation errors
3. Ensure using SessionStateManager methods

**Example Fix**:
```python
# Bad: Direct state access
state["research"] = data  # Wrong key

# Good: Using SessionStateManager
from the0.agents.state_schema import SessionStateManager
SessionStateManager.store_research(state, research_data)
```

### Tavily API Errors

**Symptom**: Search tool returns errors or "API key not configured"

**Diagnosis**:
- Check TAVILY_API_KEY environment variable
- Verify API key in database settings
- Check Tavily API quota/limits

**Solutions**:
1. Set environment variable: `export TAVILY_API_KEY=...`
2. Configure via API: `POST /settings/tavily-api-key`
3. Check quota at https://tavily.com dashboard

### Performance Issues

**Symptom**: Slow response times, timeouts

**Diagnosis**:
- Check agent delegation overhead
- Review tool execution times
- Monitor state size

**Solutions**:
1. Use basic search depth for quick queries
2. Implement caching for frequent searches
3. Reduce state payload size
4. Optimize agent instructions

[More scenarios...]
```

---

## Technical Considerations

### Diagram Tools
- Use ASCII art for simple diagrams (GitHub-compatible)
- Use mermaid.js for complex diagrams (renders on GitHub)
- Keep diagrams simple and focused

### Code Examples
- All code examples must be tested
- Include complete, runnable examples
- Add comments explaining key points

### Versioning
- Clearly mark version applicability
- Update docs with code changes
- Use git tags for documentation versions

### Links
- Use relative links between docs
- Verify all links work
- Keep internal links up to date

---

## Risks & Mitigations

### Low Risk: Documentation Drift
**Risk:** Docs become outdated as code evolves
**Impact:** Confusion, incorrect implementations
**Mitigation:**
- Include docs in PR review process
- Schedule quarterly documentation audits
- Test code examples as part of CI

**Contingency:** Documentation sprint to catch up

---

## Documentation Structure

```
docs/
├── mas-architecture.md
├── developer-guide/
│   ├── adding-agents.md
│   ├── modifying-instructions.md
│   ├── tool-development.md
│   └── state-management.md
├── examples/
│   └── workflow-examples.md
├── troubleshooting.md
└── migration-guide.md

CLAUDE.md (updated with MAS section)
README.md (if exists, update with MAS info)
```

---

## Related Stories

**Depends On:**
- All previous stories (0-6)

**Blocks:**
- None (final story)

---

## Notes

- Documentation is as important as code
- Write for future developers, not just current team
- Examples are worth a thousand words
- Keep docs close to code (same repo)
- Future: Consider automated doc generation from code

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] CLAUDE.md updated comprehensively
- [ ] All developer guides complete
- [ ] Troubleshooting guide covers common scenarios
- [ ] Examples tested and working
- [ ] Migration guide written
- [ ] API docs updated
- [ ] All documentation reviewed and proofread
- [ ] Links verified and working
- [ ] Code examples tested
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
