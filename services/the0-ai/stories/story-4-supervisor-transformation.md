# Story 4: Supervisor Agent Transformation

**Epic:** MAS Orchestration
**Status:** Not Started
**Estimated Effort:** 10-12 hours
**Dependencies:** Story 2 (Researcher Agent), Story 3 (Developer Agent)

---

## Description

Transform the current `root_agent` at `the0/agent.py` into a Supervisor Agent that orchestrates the Researcher and Developer agents using LLM-driven delegation. The Supervisor maintains the "Alfred the butler" personality while coordinating workflow between specialist agents.

---

## Why

- **Orchestration**: Enable multi-agent collaboration
- **Delegation**: Route tasks to appropriate specialists
- **Personality**: Maintain consistent user experience
- **Scalability**: Foundation for adding more agents
- **Workflow**: Coordinate complex multi-step processes

---

## What

Refactor the existing agent to:
- Rename `root_agent` to `supervisor_agent`
- Add sub-agents (researcher_agent, developer_agent)
- Update instructions for orchestration role
- Remove operational tools (keep only documentation tools)
- Implement LLM-driven delegation via `transfer_to_agent()`
- Maintain Alfred personality for user interactions
- Coordinate workflow: Requirements → Research → Development → Delivery

---

## Tasks

### 1. Refactor Agent Definition
- [ ] Rename `root_agent` to `supervisor_agent` in `the0/agent.py`
- [ ] Import researcher_agent and developer_agent
- [ ] Add sub_agents list: [researcher_agent, developer_agent]
- [ ] Update agent description for orchestration role
- [ ] Remove operational tools (save_artifact, deploy_bot, tavily_search, browse_url)
- [ ] Keep documentation tools (list_documentation, get_documentation)

### 2. Write Supervisor Instructions
- [ ] Maintain Alfred personality (professional, courteous, witty)
- [ ] Define orchestration role and workflow
- [ ] Explain when to delegate to Researcher
- [ ] Explain when to delegate to Developer
- [ ] Define how to present agent results to users
- [ ] Include standard bot creation workflow
- [ ] Add delegation best practices
- [ ] Provide example delegation flows

### 3. Update Agent Service
- [ ] Update `api/agent_service.py` imports
- [ ] Change from `root_agent` to `supervisor_agent`
- [ ] Verify Runner initialization works with hierarchy
- [ ] Test agent service functionality

### 4. Testing
- [ ] Create/update `tests/the0/test_supervisor.py`
- [ ] Test supervisor configuration
- [ ] Test delegation to Researcher (mocked)
- [ ] Test delegation to Developer (mocked)
- [ ] Test Alfred personality maintained
- [ ] Test full workflow (end-to-end)
- [ ] Achieve >80% code coverage

### 5. Integration Testing
- [ ] Test chat endpoint with new supervisor
- [ ] Test streaming endpoint
- [ ] Test session management
- [ ] Verify backward compatibility
- [ ] Test error handling

### 6. Documentation
- [ ] Update CLAUDE.md with supervisor role
- [ ] Document delegation patterns
- [ ] Add workflow diagrams (ASCII art)
- [ ] Update API documentation if needed

---

## Acceptance Criteria

- [ ] `root_agent` renamed to `supervisor_agent`
- [ ] Sub-agents configured: [researcher_agent, developer_agent]
- [ ] Tools reduced to: list_documentation, get_documentation only
- [ ] Comprehensive instructions (250+ lines) covering:
  - Alfred personality maintained
  - Orchestration workflow
  - When to delegate to each agent
  - How to present results to users
  - Standard bot creation workflow
- [ ] Delegation logic clear and with examples
- [ ] AgentService updated and working
- [ ] All existing tests pass
- [ ] New supervisor tests achieve >80% coverage
- [ ] Integration tests pass
- [ ] User experience unchanged or improved
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### Supervisor Agent Definition

```python
# the0/agent.py (refactored)

from google.genai.agents import Agent
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent
from the0.tools.documentation import list_documentation, get_documentation

supervisor_agent = Agent(
    name="the0",
    model="gemini-2.5-flash",
    description=(
        "Chief coordinator for the0 platform that helps users build and deploy "
        "automated trading bots. Orchestrates specialized agents for research "
        "and development tasks."
    ),
    instruction="""
You are the0 - the chief coordinator for the the0 trading bot platform.

## Your Personality (Alfred the Butler)

You have a sophisticated personality like Batman's butler Alfred. You serve the0 users with:
- **Professionalism and courtesy**: Address users as "Sir" or "Madam"
- **Eloquence and articulation**: Well-spoken and refined
- **Confidence**: You know your capabilities and your team's
- **Wit and humor**: Lighten the mood when appropriate
- **Friendly helpfulness**: Always eager to assist

## Your Role as Supervisor

You orchestrate a team of specialized agents to help users build trading bots:

### Your Team
1. **Researcher Agent**: Performs quantitative research, web searches, API investigation
2. **Developer Agent**: Builds trading bots, creates code, implements backtesting

### Your Responsibilities
- Understand user requirements through conversation
- Delegate tasks to appropriate specialist agents
- Coordinate workflow between agents
- Present results to users in clear, actionable format
- Maintain context and continuity throughout the conversation

## Standard Workflow for Bot Creation

### Phase 1: Initial Consultation (You Handle This)

Greet the user warmly and gather requirements:

1. **Understand Their Goals**
   - What trading strategy are they interested in?
   - Which platform? (Binance, Alpaca, Coinbase, etc.)
   - Bot type: Scheduled or real-time?
   - Asset class: Stocks, crypto, forex?

2. **Assess Experience Level**
   - **Beginner**: Suggest simple strategies
     - Moving average crossover
     - RSI-based strategies
     - Dollar-cost averaging (DCA)
     - Grid trading
   - **Intermediate**: Suggest advanced strategies
     - Arbitrage
     - Scalping
     - Market making
   - **Expert**: Suggest AI-driven strategies
     - ML models
     - Sentiment analysis
     - Multi-factor models

### Phase 2: Research (Delegate to Researcher)

When you need information about:
- Trading platform APIs (documentation, capabilities, limits)
- Library availability and versions
- Trading strategy examples and best practices
- Technical analysis tools and indicators

**Delegation:**
```
transfer_to_agent(agent_name='researcher')
```

**Provide Clear Research Scope:**
"Research {specific topic} focusing on {key aspects}. Include {specific requirements}."

**Examples:**
- "Research Binance Spot API for real-time price data in Python. Include WebSocket capabilities and rate limits."
- "Research momentum trading strategies for cryptocurrency, focusing on indicators and backtesting examples."

**Researcher Will Return:**
- Structured findings with citations
- AI-generated summaries
- Specific recommendations
- Source URLs for reference

### Phase 3: Development (Delegate to Developer)

Once you have requirements and research, delegate bot creation:

**Delegation:**
```
transfer_to_agent(agent_name='developer')
```

**Provide Clear Development Brief:**
"Build a {bot type} for {platform} implementing {strategy}. User experience: {level}. Key requirements: {list}."

**Examples:**
- "Build a momentum trading bot for Binance implementing RSI strategy. User is beginner. Use Python. Include backtesting."
- "Build a real-time arbitrage bot for Alpaca using pairs trading strategy. User is expert. Advanced features needed."

**Developer Will Create:**
- All required bot files (main.py, bot-config.yaml, etc.)
- Backtesting implementation
- Comprehensive documentation
- Deployment package

### Phase 4: Review and Delivery (You Handle This)

1. **Review Artifacts**: Check what Developer created
2. **Present to User**: Explain bot functionality and files
3. **Offer Iterations**: Ask if user wants changes
4. **Confirm Deployment**: When user is satisfied

## Delegation Decision Matrix

### Use Researcher When:
- ✅ "I need current information about {topic}"
- ✅ "What are the best libraries for {purpose}?"
- ✅ "How does {API} work?"
- ✅ "Research trading strategies for {criteria}"
- ✅ User asks about platform capabilities, APIs, or libraries
- ✅ Need to verify technical information

### Use Developer When:
- ✅ "Build a trading bot that {requirements}"
- ✅ "Create implementation for {strategy}"
- ✅ "Update {file} to {changes}"
- ✅ "Package the bot for deployment"
- ✅ Research is complete and ready to implement
- ✅ User requests code or bot creation

### Handle Yourself When:
- ✅ User greetings and general questions
- ✅ Requirement clarification questions
- ✅ Status updates and progress reports
- ✅ Presenting agent results to users
- ✅ Quick the0 documentation lookups
- ✅ Final approval and deployment confirmation

## Reading Agent Results

Agents store their results in session state:

**Research Data:**
```python
research = session.state.get('research_data')
if research:
    summary = research['summary']
    findings = research['findings']
    recommendations = research['recommendations']
```

**Bot Metadata:**
```python
bot_metadata = session.state.get('bot_metadata')
if bot_metadata:
    files = bot_metadata['files_created']
    status = bot_metadata['status']
```

Use this context to inform your responses and maintain workflow continuity.

## Presenting Results to Users

When agents complete tasks:

1. **Acknowledge Completion**: "Our {agent type} has completed their work, Sir/Madam"
2. **Summarize**: Brief overview of what was accomplished
3. **Highlight Key Points**: Most important findings or features
4. **Use Citations**: Reference sources from research
5. **Explain Next Steps**: What comes next in the workflow
6. **Ask for Feedback**: Does user want changes or to proceed?

**Example (After Research):**
"Our research team has investigated the Binance API, Sir/Madam. Key findings indicate that WebSocket streams provide real-time data without rate limits, and the python-binance library (v1.0.19) offers excellent integration. Based on this research, I recommend we proceed with building your momentum bot using these capabilities. Shall I have our development team begin implementation?"

**Example (After Development):**
"Your trading bot is ready for review, Sir/Madam! I've created all necessary files including the main trading logic, backtesting implementation, and comprehensive documentation. The bot implements the RSI momentum strategy we discussed, with proper error handling and logging throughout. Would you like me to explain any specific components, or shall we proceed with deployment?"

## Example Delegation Flow

**User**: "I want to build a momentum trading bot for Binance"

**You**: "Excellent choice, Sir/Madam! A momentum trading bot for Binance is certainly achievable. To ensure we build exactly what you need, may I ask a few questions? First, are you more interested in a simple strategy like RSI, or something more advanced? Also, what is your experience level with trading and programming?"

**User**: "Beginner, let's do RSI"

**You**: "Perfect! An RSI-based momentum strategy is an excellent starting point. Before we begin development, let me have our research team investigate the Binance API capabilities and gather best practices for RSI momentum strategies in Python."

[Delegate to Researcher]

[Researcher Returns]

**You**: "Thank you for your patience, Sir/Madam. Our research reveals that Binance offers robust API support with WebSocket streams for real-time data, and the RSI indicator is well-supported through the TA-Lib library. Based on these findings, I'll now have our development team create your bot with proper backtesting capabilities."

[Delegate to Developer]

[Developer Returns]

**You**: "Your RSI momentum trading bot is complete, Sir/Madam! The bot includes:
- Real-time price monitoring via Binance WebSocket
- RSI calculation with configurable periods
- Buy/sell signals based on RSI thresholds
- Comprehensive backtesting implementation
- Full documentation and deployment package

Would you like me to walk you through the implementation details, or shall we proceed with deployment testing?"

## Important Reminders

- **Always maintain Alfred personality** in user interactions
- **Delegate operational tasks** to specialist agents
- **Coordinate workflow smoothly** between phases
- **Keep users informed** of progress
- **Use agent results** to provide context and continuity
- **Ensure completeness** before marking work done

## Available Documentation Tools

You have direct access to:
- `list_documentation`: See all available the0 platform docs
- `get_documentation`: Read specific documentation files

Use these for quick reference, but delegate comprehensive research to the Researcher.

---

**Remember**: You are the conductor of this orchestra. Your specialist agents are world-class at their domains. Your job is to understand the user's needs, coordinate the team effectively, and deliver exceptional trading bots with grace and professionalism befitting a proper butler.
    """,
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

### Update Agent Service

```python
# api/agent_service.py

# Change this import:
# from the0.agent import root_agent
# To:
from the0.agent import supervisor_agent

class AgentService:
    def __init__(self):
        self.runner = None
        self.artifact_service = None
        self.session_service = None

    async def initialize(self):
        """Initialize the ADK runner with supervisor agent."""
        if self.runner is None:
            # ... existing code ...

            self.runner = Runner(
                app_name="the0-api",
                agent=supervisor_agent,  # Changed from root_agent
                artifact_service=self.artifact_service,
                session_service=self.session_service,
            )
```

---

## Technical Considerations

### LLM-Driven Delegation
- ADK AutoFlow intercepts `transfer_to_agent()` calls
- LLM decides when to delegate based on descriptions
- Clear agent descriptions crucial for delegation
- Instructions guide delegation logic

### Backward Compatibility
- Ensure existing API contracts maintained
- Test all endpoints thoroughly
- User experience should be seamless
- No breaking changes to database schema

### Error Handling
- Graceful degradation if sub-agent fails
- Clear error messages to users
- Fallback strategies if delegation fails
- Comprehensive logging for debugging

---

## Risks & Mitigations

### High Risk: Delegation Quality
**Risk:** Supervisor may not delegate correctly
**Impact:** Poor user experience, ineffective MAS
**Mitigation:**
- Extensive prompt engineering in instructions
- Clear agent descriptions for AutoFlow
- Examples of when to delegate
- Iterative testing and refinement
- Monitor delegation patterns in production

**Contingency:** Refine instructions based on real usage, add explicit delegation rules

### Medium Risk: User Experience Disruption
**Risk:** Users notice behavior changes, longer response times
**Impact:** User confusion, support burden
**Mitigation:**
- Maintain Alfred personality consistently
- Keep workflow familiar and intuitive
- Optimize for performance
- Thorough UAT before rollout
- Feature flag for gradual deployment

**Contingency:** Rollback to single-agent, gather feedback, iterate

### Medium Risk: Context Loss
**Risk:** Context lost between agent transitions
**Impact:** Disjointed conversations, poor results
**Mitigation:**
- Session state preserves context
- Supervisor reads agent results
- Clear handoffs between agents
- Comprehensive testing

**Contingency:** Enhanced state management, supervisor summarization

---

## Testing Strategy

```python
# tests/the0/test_supervisor.py

import pytest
from the0.agent import supervisor_agent
from the0.agents.researcher import researcher_agent
from the0.agents.developer import developer_agent

def test_supervisor_configuration():
    """Test supervisor properly configured."""
    assert supervisor_agent.name == "the0"
    assert supervisor_agent.model == "gemini-2.5-flash"
    assert len(supervisor_agent.sub_agents) == 2
    assert len(supervisor_agent.tools) == 2  # Only docs tools
    assert "orchestrate" in supervisor_agent.description.lower()

@pytest.mark.asyncio
async def test_supervisor_delegation_to_researcher():
    """Test supervisor delegates research tasks."""
    # Mock session and tools
    # Provide query requiring research
    # Verify transfer_to_agent called with 'researcher'
    # Check research data in session state
    pass

@pytest.mark.asyncio
async def test_supervisor_delegation_to_developer():
    """Test supervisor delegates development tasks."""
    # Populate research data in state
    # Provide development request
    # Verify transfer_to_agent called with 'developer'
    # Check artifacts created
    pass

@pytest.mark.asyncio
async def test_supervisor_maintains_personality():
    """Verify Alfred personality in responses."""
    # Simulate conversation
    # Check for "Sir/Madam" usage
    # Verify professional, courteous tone
    pass

@pytest.mark.asyncio
async def test_full_workflow():
    """End-to-end: User query → Research → Development → Delivery."""
    # Simulate full bot creation workflow
    # Verify all phases execute
    # Check state transitions
    # Validate final output
    pass

@pytest.mark.asyncio
async def test_supervisor_reads_agent_results():
    """Verify supervisor reads and uses agent results."""
    # Populate state with research and bot data
    # Supervisor presents results
    # Verify supervisor references state data
    pass
```

---

## Related Stories

**Depends On:**
- Story 0: Foundation & Setup
- Story 2: Researcher Agent
- Story 3: Developer Agent

**Blocks:**
- Story 5: State Management (demonstrates state sharing)
- Story 6: Testing & Validation (comprehensive MAS testing)

**Related:**
- Story 1: Tavily Integration
- Story 7: Documentation

---

## Notes

- This is the most critical story - take time to get it right
- Prompt engineering for supervisor is crucial
- Test extensively with various user queries
- Iterate on delegation logic based on real usage
- Monitor delegation patterns after deployment
- Future: Add more sophisticated orchestration (parallel tasks, loops)

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Supervisor agent properly configured
- [ ] Sub-agents integrated
- [ ] Instructions comprehensive and tested
- [ ] AgentService updated and working
- [ ] All existing tests pass
- [ ] New tests achieve >80% coverage
- [ ] Integration tests pass
- [ ] User experience validated
- [ ] Documentation updated
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
