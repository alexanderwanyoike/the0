# Story 0: Foundation & Setup

**Epic:** Infrastructure Preparation
**Status:** Not Started
**Estimated Effort:** 2-4 hours
**Dependencies:** None

---

## Description

Prepare the codebase for multi-agent architecture by refactoring existing structure and setting up development infrastructure. This foundational work ensures smooth implementation of the Multi-Agent System without breaking existing functionality.

---

## Why

- Organize codebase to support multiple specialized agents
- Set up configuration for new integrations (Tavily)
- Document architectural decisions early
- Enable team to work on agents independently
- Maintain backward compatibility during transition

---

## What

Transform the current single-agent structure to support a multi-agent architecture:
- Create new directory structure for agent organization
- Add environment configuration for Tavily API
- Update project documentation with MAS architecture overview
- Ensure no breaking changes to existing functionality

---

## Tasks

### 1. Create Directory Structure
- [ ] Create `the0/agents/` directory for agent definitions
- [ ] Create `the0/agents/__init__.py`
- [ ] Create `the0/agents/base.py` for shared agent configurations and utilities
- [ ] Verify imports work correctly

### 2. Environment Configuration
- [ ] Add `TAVILY_API_KEY` to environment variables
- [ ] Update `.env.example` with new Tavily configuration
- [ ] Add configuration comments explaining Tavily setup
- [ ] Document API key acquisition process

### 3. Update Documentation
- [ ] Update `CLAUDE.md` with MAS architecture section:
  - Agent hierarchy diagram
  - Communication patterns
  - Tool distribution overview
- [ ] Add "Future Agents" section for extensibility
- [ ] Document design principles for agents

### 4. Verify Backward Compatibility
- [ ] Run existing tests to ensure nothing breaks
- [ ] Verify `api/agent_service.py` still works
- [ ] Check that current agent functionality unchanged
- [ ] Test chat endpoints

---

## Acceptance Criteria

- [ ] `the0/agents/` directory exists with proper structure
- [ ] `base.py` created with placeholder for shared utilities
- [ ] Environment variables updated and documented
- [ ] `.env.example` includes Tavily configuration
- [ ] `CLAUDE.md` updated with MAS architecture overview
- [ ] All existing tests pass
- [ ] No breaking changes to current functionality
- [ ] Code formatted with `make format`
- [ ] Linting passes with `make lint`

---

## Technical Considerations

### Directory Organization
```
the0/
├── agent.py           # Will become supervisor_agent
├── tools/             # Existing tools
│   ├── save_artifact.py
│   ├── web_browser.py
│   ├── documentation.py
│   └── deploy_bot.py
└── agents/            # NEW - Agent definitions
    ├── __init__.py
    ├── base.py        # Shared utilities
    ├── researcher.py  # Story 2
    └── developer.py   # Story 3
```

### Environment Variables
Add to `.env` and `.env.example`:
```bash
# Tavily API Configuration (Story 1)
TAVILY_API_KEY=your_tavily_api_key_here
TAVILY_SEARCH_DEPTH=advanced  # Options: basic, advanced
TAVILY_MAX_RESULTS=5
```

### base.py Template
```python
# the0/agents/base.py
"""
Shared utilities and base configurations for the0 agents.
"""

from typing import Optional, Dict, Any

# Agent configuration constants
DEFAULT_MODEL = "gemini-2.5-flash"
DEFAULT_TEMPERATURE = 0.7

# State management keys (used in Story 5)
STATE_KEY_RESEARCH = "research_data"
STATE_KEY_BOT_METADATA = "bot_metadata"

# Shared utility functions
def format_citations(sources: list) -> str:
    """Format sources as markdown citations."""
    citations = []
    for i, source in enumerate(sources, 1):
        citations.append(f"{i}. [{source.get('title', 'Source')}]({source.get('url')})")
    return "\n".join(citations)
```

---

## Risks & Mitigations

### Low Risk: Import Path Changes
**Risk:** New directory structure might affect imports
**Impact:** Build errors, test failures
**Mitigation:**
- Test imports after creating structure
- Update `__init__.py` files properly
- Run full test suite to verify

**Contingency:** Revert directory changes if issues arise

### Low Risk: Documentation Drift
**Risk:** Docs become outdated as we implement
**Impact:** Confusion for future developers
**Mitigation:**
- Update docs incrementally with each story
- Include docs in code review
- Keep architecture diagrams simple (ASCII art)

**Contingency:** Schedule documentation review after Story 7

---

## Testing Strategy

### Validation Steps
```bash
# 1. Verify directory structure
ls -la the0/agents/
cat the0/agents/__init__.py
cat the0/agents/base.py

# 2. Check environment configuration
cat .env.example | grep TAVILY

# 3. Run existing tests (should all pass)
make test

# 4. Check imports
python -c "from the0.agents import base; print('Import successful')"

# 5. Verify formatting and linting
make format
make lint
```

### Success Indicators
- All commands execute without errors
- Test suite passes (100% existing tests)
- New files properly formatted
- No linting errors introduced

---

## Related Stories

**Blocks:**
- Story 1: Tavily Integration (needs environment config)
- Story 2: Researcher Agent (needs agents/ directory)
- Story 3: Developer Agent (needs agents/ directory)

**Related:**
- Story 7: Documentation (will expand on MAS architecture)

---

## Notes

- This is foundational work - take time to get structure right
- Keep changes minimal and focused
- Ensure backward compatibility at all times
- Use feature flags if needed for gradual rollout
- Document all architectural decisions in CLAUDE.md

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Tests passing
- [ ] Code formatted and linted
- [ ] Documentation updated
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
