import os
import subprocess
import sys
import logging

logger = logging.getLogger(__name__)


def check_no_interactive_session() -> None:
    """Exit if an interactive claude session is running. Headless (-p) sessions are ignored."""
    try:
        result = subprocess.run(
            ["pgrep", "-af", "claude"],
            capture_output=True, text=True, timeout=5,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return  # Can't check, assume safe

    current_pid = str(os.getpid())
    for line in result.stdout.strip().splitlines():
        # Skip headless sessions and our own process tree
        if "claude -p" in line or current_pid in line.split()[0:1]:
            continue
        # Skip non-claude-binary matches (tee, bash, zsh wrappers)
        parts = line.split(None, 1)
        if len(parts) < 2:
            continue
        cmd = parts[1]
        if cmd.startswith("claude") or cmd.endswith("/claude"):
            logger.info("Interactive claude session detected — skipping this run")
            logger.info(f"  {line}")
            sys.exit(0)
