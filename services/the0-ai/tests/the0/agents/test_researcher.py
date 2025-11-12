"""
Unit tests for the Researcher Agent.

Tests agent configuration, instruction completeness, tool assignment,
and integration with the agents package.
"""

from the0.agents.researcher import (
    researcher_agent,
    RESEARCHER_DESCRIPTION,
    RESEARCHER_INSTRUCTION,
)
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
        lines = instruction.split("\n")
        assert len(lines) >= 150, "Instruction should have 150+ lines"

        # Check for required keywords and sections
        instruction_lower = instruction.lower()
        required_keywords = [
            "research",
            "tavily",
            "search",
            "citation",
            "sources",
            "findings",
            "recommendations",
            "quality",
            "workflow",
            "browse_url",
            "documentation",
            "verify",
            "authority",
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
        assert hasattr(researcher_agent, "tools")
        assert researcher_agent.tools is not None
        assert len(researcher_agent.tools) == 4, "Researcher should have exactly 4 tools"

        # Verify tool names (tools may be wrapped, check by name or function)
        tool_names = set()
        for tool in researcher_agent.tools:
            # ADK may wrap tools, extract function name
            if hasattr(tool, "__name__"):
                tool_names.add(tool.__name__)
            elif hasattr(tool, "func_declarations"):
                # Tool wrapper object
                for decl in tool.func_declarations:
                    if hasattr(decl, "__name__"):
                        tool_names.add(decl.__name__)

        expected_tools = {
            "tavily_search",
            "browse_url",
            "list_documentation",
            "get_documentation",
        }
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
