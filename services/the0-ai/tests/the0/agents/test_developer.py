"""
Unit tests for the Developer Agent.

Tests agent configuration, instruction completeness, tool assignment,
and integration with the agents package.
"""

from the0.agents.developer import (
    developer_agent,
    DEVELOPER_DESCRIPTION,
    DEVELOPER_INSTRUCTION,
)
from the0.agents.base import DEFAULT_MODEL, STATE_KEY_BOT_METADATA, STATE_KEY_RESEARCH


class TestDeveloperAgent:
    """Test suite for Developer Agent configuration and setup."""

    def test_developer_configuration(self):
        """Test developer agent basic configuration."""
        # Verify agent exists and has correct properties
        assert developer_agent is not None
        assert developer_agent.name == "developer"
        assert developer_agent.model == DEFAULT_MODEL
        assert developer_agent.description == DEVELOPER_DESCRIPTION
        assert developer_agent.instruction == DEVELOPER_INSTRUCTION

    def test_developer_model_uses_constant(self):
        """Test that agent uses DEFAULT_MODEL constant."""
        assert developer_agent.model == DEFAULT_MODEL
        assert DEFAULT_MODEL == "gemini-2.5-flash"

    def test_developer_description_quality(self):
        """Test that description contains key phrases for LLM delegation."""
        description = developer_agent.description.lower()

        # Check for key delegation phrases
        assert "develop" in description or "build" in description
        assert "specialist" in description or "expert" in description
        assert "trading" in description or "bot" in description

        # Check length is reasonable (not too short, not too long)
        assert 50 < len(developer_agent.description) < 500

    def test_developer_instruction_completeness(self):
        """Test that instruction is comprehensive and includes required sections."""
        instruction = developer_agent.instruction

        # Check instruction length (minimum 200 lines, ~1000+ characters)
        assert len(instruction) > 1000, "Instruction should be comprehensive (>1000 chars)"
        lines = instruction.split("\n")
        assert len(lines) >= 200, "Instruction should have 200+ lines"

        # Check for required keywords and sections
        instruction_lower = instruction.lower()
        required_keywords = [
            "develop",
            "build",
            "artifact",
            "backtest",
            "quality",
            "solid",
            "workflow",
            "documentation",
            "platform",
            "standards",
            "executable",
            "validation",
            "library",
            "class",
        ]
        for keyword in required_keywords:
            assert keyword in instruction_lower, f"Instruction missing keyword: {keyword}"

        # Check for structured sections
        assert "## Core Responsibilities" in instruction or "core responsibilit" in instruction_lower
        assert "## Development Workflow" in instruction or "workflow" in instruction_lower
        assert "## Engineering Principles" in instruction or "engineering principle" in instruction_lower
        assert "## Library Preferences" in instruction or "library preference" in instruction_lower
        assert "## Credentials Handling" in instruction or "credential" in instruction_lower

    def test_developer_tools_assigned(self):
        """Test that all required tools are assigned to the agent."""
        # Verify tools list exists and has correct count
        assert hasattr(developer_agent, "tools")
        assert developer_agent.tools is not None
        assert len(developer_agent.tools) == 9, "Developer should have exactly 9 tools"

        # Verify tool names (tools may be wrapped, check by name or function)
        tool_names = set()
        for tool in developer_agent.tools:
            # ADK may wrap tools, extract function name
            if hasattr(tool, "__name__"):
                tool_names.add(tool.__name__)
            elif hasattr(tool, "func_declarations"):
                # Tool wrapper object
                for decl in tool.func_declarations:
                    if hasattr(decl, "__name__"):
                        tool_names.add(decl.__name__)

        expected_tools = {
            "save_artifact",
            "deploy_bot",
            "list_documentation",
            "get_documentation",
            "execute_command",
            "read_file",
            "list_directory",
            "tavily_search",
            "browse_url",
        }
        assert tool_names == expected_tools, f"Expected tools {expected_tools}, got {tool_names}"

    def test_developer_state_key_reference(self):
        """Test instruction references state key constants."""
        # Check that state key constants are used in instruction or documentation
        instruction = developer_agent.instruction

        # Should reference both state keys
        assert STATE_KEY_BOT_METADATA in instruction or "bot_metadata" in instruction
        assert STATE_KEY_RESEARCH in instruction or "research_data" in instruction

    def test_developer_agent_export(self):
        """Test that developer_agent can be imported from package."""
        # This test verifies the __init__.py export
        from the0.agents import developer_agent as exported_agent

        assert exported_agent is not None
        assert exported_agent.name == "developer"
        assert exported_agent is developer_agent  # Should be same instance

    def test_developer_instruction_examples(self):
        """Test that instruction includes development examples."""
        instruction = developer_agent.instruction

        # Should include example code and patterns
        assert "Example" in instruction or "example" in instruction.lower()

        # Should show good vs bad patterns
        assert "✅" in instruction or "good" in instruction.lower()
        assert "❌" in instruction or "bad" in instruction.lower() or "don't" in instruction.lower()

    def test_developer_instruction_executable_validation(self):
        """Test that instruction includes executable validation sections."""
        instruction = developer_agent.instruction
        instruction_lower = instruction.lower()

        # Must include executable validation keywords
        assert "environment" in instruction_lower, "Missing environment setup section"
        assert "execute" in instruction_lower or "execution" in instruction_lower, "Missing execution section"
        assert "validate" in instruction_lower or "validation" in instruction_lower, "Missing validation section"
        assert "credentials" in instruction_lower, "Missing credentials handling section"

    def test_developer_instruction_library_preferences(self):
        """Test that instruction includes library preference guidelines."""
        instruction = developer_agent.instruction
        instruction_lower = instruction.lower()

        # Must include library preference keywords
        assert "library" in instruction_lower or "libraries" in instruction_lower
        # Should mention specific libraries
        assert any(lib in instruction_lower for lib in ["ccxt", "pandas", "ta-lib", "talib"])


class TestDeveloperConstants:
    """Test agent constants and configuration."""

    def test_default_model_constant(self):
        """Test DEFAULT_MODEL constant value."""
        assert DEFAULT_MODEL == "gemini-2.5-flash"

    def test_state_key_bot_metadata_constant(self):
        """Test STATE_KEY_BOT_METADATA constant value."""
        assert STATE_KEY_BOT_METADATA == "bot_metadata"

    def test_state_key_research_constant(self):
        """Test STATE_KEY_RESEARCH constant value."""
        assert STATE_KEY_RESEARCH == "research_data"
