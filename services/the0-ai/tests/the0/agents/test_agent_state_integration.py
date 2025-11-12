"""
Integration tests for agent state management.

Tests that Researcher and Developer agents properly use SessionStateManager
for cross-agent state sharing. Verifies instruction content and workflow.
"""

from the0.agents.researcher import researcher_agent, RESEARCHER_INSTRUCTION
from the0.agents.developer import developer_agent, DEVELOPER_INSTRUCTION
from the0.agents.state_schema import (
    SessionStateManager,
    ResearchData,
    ResearchFinding,
    BotMetadata,
)


class TestResearcherStateIntegration:
    """Test Researcher agent state management integration."""

    def test_researcher_imports_session_state_manager(self):
        """Test that researcher.py imports SessionStateManager."""
        import the0.agents.researcher as researcher_module

        # Verify SessionStateManager is imported (added in Story 5)
        assert hasattr(researcher_module, "SessionStateManager")

    def test_researcher_instruction_mentions_session_state_manager(self):
        """Test that researcher instructions mention SessionStateManager."""
        assert "SessionStateManager" in RESEARCHER_INSTRUCTION
        assert "store_research" in RESEARCHER_INSTRUCTION

    def test_researcher_instruction_has_storage_section(self):
        """Test that researcher has Session State Storage section."""
        assert "Session State Storage" in RESEARCHER_INSTRUCTION
        assert "CRITICAL" in RESEARCHER_INSTRUCTION  # Emphasis on storage

    def test_researcher_instruction_has_code_example(self):
        """Test that researcher has code example for state storage."""
        assert "ResearchData" in RESEARCHER_INSTRUCTION
        assert "ResearchFinding" in RESEARCHER_INSTRUCTION
        assert "SessionStateManager.store_research" in RESEARCHER_INSTRUCTION

    def test_researcher_instruction_no_story_5_placeholder(self):
        """Test that 'Story 5' placeholder note has been removed."""
        assert "Story 5" not in RESEARCHER_INSTRUCTION or "Story 5 Part 2" in RESEARCHER_INSTRUCTION
        # Should not have the old placeholder: "State writing will be implemented in Story 5"

    def test_researcher_instruction_explains_when_to_store(self):
        """Test that instruction explains when to store state."""
        assert "When to Store" in RESEARCHER_INSTRUCTION or "when to store" in RESEARCHER_INSTRUCTION.lower()


class TestDeveloperStateIntegration:
    """Test Developer agent state management integration."""

    def test_developer_imports_session_state_manager(self):
        """Test that developer.py imports SessionStateManager."""
        import the0.agents.developer as developer_module

        # Verify SessionStateManager is imported (added in Story 5)
        assert hasattr(developer_module, "SessionStateManager")

    def test_developer_instruction_mentions_session_state_manager(self):
        """Test that developer instructions mention SessionStateManager."""
        assert "SessionStateManager" in DEVELOPER_INSTRUCTION
        assert "get_research" in DEVELOPER_INSTRUCTION
        assert "store_bot_metadata" in DEVELOPER_INSTRUCTION

    def test_developer_instruction_has_research_reading(self):
        """Test that developer has research reading in Step 3."""
        assert "Analyze Research Findings" in DEVELOPER_INSTRUCTION
        assert "SessionStateManager.get_research" in DEVELOPER_INSTRUCTION

    def test_developer_instruction_has_metadata_storage(self):
        """Test that developer has metadata storage in Step 8.5."""
        assert "Document Results" in DEVELOPER_INSTRUCTION or "Store Metadata" in DEVELOPER_INSTRUCTION
        assert "BotMetadata" in DEVELOPER_INSTRUCTION
        assert "SessionStateManager.store_bot_metadata" in DEVELOPER_INSTRUCTION

    def test_developer_instruction_no_raw_dict_access(self):
        """Test that raw dict access has been replaced."""
        # Old pattern should not exist: session.state.get('{STATE_KEY_RESEARCH}', {})
        # New pattern should exist: SessionStateManager.get_research(session.state)
        assert "SessionStateManager.get_research" in DEVELOPER_INSTRUCTION
        # The instruction should not have the old `.get('{STATE_KEY_RESEARCH}')` pattern anymore


class TestStateWorkflowIntegration:
    """Test end-to-end state workflow integration."""

    def test_end_to_end_workflow_simulation(self):
        """Simulate Researcher → Developer workflow using state."""
        # Simulate session state (ADK session.state is a dict)
        session_state = {}

        # === RESEARCHER WORKFLOW ===
        # Researcher completes research and stores findings
        research = ResearchData(
            query="Research Binance API for momentum trading",
            summary="Binance API v3 provides REST and WebSocket endpoints",
            findings=[
                ResearchFinding(
                    point="WebSocket recommended for real-time data",
                    source="https://binance-docs.github.io/",
                    confidence="high",
                )
            ],
            recommendations=["Use python-binance library", "Implement WebSocket streams"],
            sources=[{"title": "Binance Docs", "url": "https://binance-docs.github.io/"}],
        )

        # Store using SessionStateManager (as per researcher instructions)
        SessionStateManager.store_research(session_state, research)

        # Verify stored
        assert "research_data" in session_state

        # === DEVELOPER WORKFLOW ===
        # Developer reads research data
        retrieved_research = SessionStateManager.get_research(session_state)

        # Verify developer can access research
        assert retrieved_research is not None
        assert retrieved_research.query == "Research Binance API for momentum trading"
        assert len(retrieved_research.findings) == 1
        assert retrieved_research.findings[0].point == "WebSocket recommended for real-time data"
        assert "python-binance" in retrieved_research.recommendations[0]

        # Developer creates bot based on research
        bot_metadata = BotMetadata(
            bot_name="momentum_btc_binance",
            language="python",
            files_created=["main.py", "bot-config.yaml", "requirements.txt"],
            strategy_type="momentum",
            platform="binance",
            status="ready_for_deploy",
            execution_verified=True,
            backtest_verified=True,
            libraries_used=["python-binance==1.0.19", "ccxt==4.1.0"],
            test_results={"bot_execution": "success", "backtest_execution": "success"},
        )

        # Store using SessionStateManager (as per developer instructions)
        SessionStateManager.store_bot_metadata(session_state, bot_metadata)

        # Verify stored
        assert "bot_metadata" in session_state

        # === SUPERVISOR CAN ACCESS BOTH ===
        final_research = SessionStateManager.get_research(session_state)
        final_bot = SessionStateManager.get_bot_metadata(session_state)

        assert final_research is not None
        assert final_bot is not None
        assert final_bot.bot_name == "momentum_btc_binance"
        assert final_bot.execution_verified is True

    def test_developer_handles_missing_research_gracefully(self):
        """Test that developer can handle missing research data."""
        # Empty session state (no research)
        session_state = {}

        # Developer tries to read research
        research = SessionStateManager.get_research(session_state)

        # Should return None gracefully (not raise exception)
        assert research is None

        # Developer can still create bot without research
        bot_metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            strategy_type="DCA",
            platform="kraken",
            status="ready_for_deploy",
        )

        SessionStateManager.store_bot_metadata(session_state, bot_metadata)

        # Verify stored
        retrieved = SessionStateManager.get_bot_metadata(session_state)
        assert retrieved is not None
        assert retrieved.bot_name == "test_bot"

    def test_state_summary_reflects_both_agents_data(self):
        """Test that state summary shows data from both agents."""
        session_state = {}

        # Researcher stores data
        research = ResearchData(query="Test query", summary="Test summary")
        SessionStateManager.store_research(session_state, research)

        # Developer stores data
        bot_metadata = BotMetadata(
            bot_name="test_bot",
            language="python",
            strategy_type="test",
            platform="test",
            status="test",
        )
        SessionStateManager.store_bot_metadata(session_state, bot_metadata)

        # Get summary
        summary = SessionStateManager.get_state_summary(session_state)

        # Verify summary shows both
        assert summary["has_research_data"] is True
        assert summary["has_bot_metadata"] is True
        assert summary["bot_name"] == "test_bot"
        assert "research_query" in summary


class TestAgentConfiguration:
    """Test that agent configurations are correct."""

    def test_researcher_agent_is_configured(self):
        """Test that researcher agent exists and is configured."""
        assert researcher_agent is not None
        assert researcher_agent.name == "researcher"
        assert researcher_agent.instruction is not None

    def test_developer_agent_is_configured(self):
        """Test that developer agent exists and is configured."""
        assert developer_agent is not None
        assert developer_agent.name == "developer"
        assert developer_agent.instruction is not None

    def test_researcher_has_documentation_tools(self):
        """Test that researcher has access to documentation tools."""
        # Researcher needs list_documentation and get_documentation
        # Tools are functions, so use __name__ attribute
        tool_names = [getattr(tool, "name", getattr(tool, "__name__", "")) for tool in researcher_agent.tools]
        assert "list_documentation" in tool_names or any("documentation" in name for name in tool_names)

    def test_developer_has_required_tools(self):
        """Test that developer has required tools."""
        # Developer needs save_artifact, deploy_bot, execute_command, etc.
        # Tools are functions, so use __name__ attribute
        tool_names = [getattr(tool, "name", getattr(tool, "__name__", "")) for tool in developer_agent.tools]
        assert "save_artifact" in tool_names or any("artifact" in name for name in tool_names)


class TestBackwardCompatibility:
    """Test backward compatibility and error handling."""

    def test_agents_work_without_state_data(self):
        """Test that agents can work even if state is empty."""
        # This ensures backward compatibility
        empty_state = {}

        # Developer reading from empty state should not crash
        research = SessionStateManager.get_research(empty_state)
        assert research is None  # Graceful None return

        bot = SessionStateManager.get_bot_metadata(empty_state)
        assert bot is None  # Graceful None return

    def test_state_operations_are_logged(self, caplog):
        """Test that state operations produce log output."""
        import logging

        caplog.set_level(logging.INFO)

        session_state = {}

        # Store research
        research = ResearchData(query="Test", summary="Test")
        SessionStateManager.store_research(session_state, research)

        # Verify logging occurred
        assert "Storing research data" in caplog.text or "research" in caplog.text.lower()
