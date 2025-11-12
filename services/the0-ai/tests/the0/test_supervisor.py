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
        assert tool_names.isdisjoint(
            forbidden_tools
        ), f"Supervisor should not have operational tools: {forbidden_tools & tool_names}"

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
            "research" in instruction.lower()
            and ("phase 2" in instruction.lower() or "delegate to researcher" in instruction.lower()),
            "development" in instruction.lower()
            and ("phase 3" in instruction.lower() or "delegate to developer" in instruction.lower()),
            "review" in instruction.lower() or "delivery" in instruction.lower() or "phase 4" in instruction.lower(),
        ]

        assert (
            sum(phases) >= 3
        ), "Instruction should cover workflow phases (consultation, research, development, delivery)"

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
