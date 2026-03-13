"""Tests for the interactive session detection guard."""

import subprocess
from unittest.mock import patch

import pytest

from tech_debt_agents.session_guard import check_no_interactive_session


class TestSessionGuard:
    def test_no_session_running(self):
        """Should return normally when no claude processes are found."""
        mock_result = subprocess.CompletedProcess(args=[], returncode=1, stdout="", stderr="")
        with patch("tech_debt_agents.session_guard.subprocess.run", return_value=mock_result):
            check_no_interactive_session()  # Should not raise or exit

    def test_headless_session_ignored(self):
        """Should not exit when only headless (claude -p) sessions are running."""
        output = "12345 claude -p --model sonnet some prompt here\n"
        mock_result = subprocess.CompletedProcess(args=[], returncode=0, stdout=output, stderr="")
        with patch("tech_debt_agents.session_guard.subprocess.run", return_value=mock_result):
            check_no_interactive_session()  # Should not raise or exit

    def test_interactive_session_detected(self):
        """Should exit when an interactive claude session is detected."""
        output = "99999 claude\n"
        mock_result = subprocess.CompletedProcess(args=[], returncode=0, stdout=output, stderr="")
        with patch("tech_debt_agents.session_guard.subprocess.run", return_value=mock_result):
            with pytest.raises(SystemExit) as exc_info:
                check_no_interactive_session()
            assert exc_info.value.code == 0

    def test_pgrep_not_found(self):
        """Should proceed if pgrep is not available."""
        with patch(
            "tech_debt_agents.session_guard.subprocess.run",
            side_effect=FileNotFoundError,
        ):
            check_no_interactive_session()  # Should not raise or exit

    def test_non_claude_processes_ignored(self):
        """Should ignore processes that aren't the claude binary."""
        output = "12345 tee -a /some/claude/path/log\n54321 bash /some/claude/script.sh\n"
        mock_result = subprocess.CompletedProcess(args=[], returncode=0, stdout=output, stderr="")
        with patch("tech_debt_agents.session_guard.subprocess.run", return_value=mock_result):
            check_no_interactive_session()  # Should not raise or exit
