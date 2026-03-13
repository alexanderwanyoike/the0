"""Auditor agent — scans codebase and updates the tech debt audit file."""

import asyncio
import logging
import shutil
from datetime import date

from claude_agent_sdk import query, ClaudeAgentOptions, ResultMessage

from .config import AUDIT_PATH, AUDIT_FILE, AUDITOR_LOCK, AUDITOR_MODEL, PROMPTS_DIR
from .lock import FileLock
from .logging_setup import setup_logging
from .session_guard import check_no_interactive_session
from .worktree import fetch_base_branch, auditor_worktree

logger = logging.getLogger(__name__)


async def run_auditor(force: bool = False) -> None:
    setup_logging("auditor")
    logger.info("=== Auditor run ===")

    if not force:
        check_no_interactive_session()

    with FileLock(AUDITOR_LOCK):
        fetch_base_branch()

        with auditor_worktree() as wt_path:
            # Copy existing audit file into worktree
            wt_audit = wt_path / AUDIT_FILE
            wt_audit.parent.mkdir(parents=True, exist_ok=True)
            if AUDIT_PATH.exists():
                shutil.copy2(AUDIT_PATH, wt_audit)

            # Build prompt
            system_prompt = PROMPTS_DIR / "auditor.md"
            prompt = system_prompt.read_text() + (
                f"\n\nScan this codebase and update the audit file at "
                f"{AUDIT_FILE}. Today's date is {date.today().isoformat()}."
            )

            # Run agent
            logger.info("Running auditor agent...")
            async for message in query(
                prompt=prompt,
                options=ClaudeAgentOptions(
                    cwd=str(wt_path),
                    allowed_tools=["Read", "Glob", "Grep", "Write", "Edit"],
                    permission_mode="acceptEdits",
                    model=AUDITOR_MODEL,
                ),
            ):
                if isinstance(message, ResultMessage):
                    logger.info(f"Auditor result:\n{message.result}")

            # Copy updated audit file back
            if wt_audit.exists():
                logger.info("Copying updated audit file back...")
                AUDIT_PATH.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(wt_audit, AUDIT_PATH)

    logger.info("=== Auditor complete ===")


def main(force: bool = False) -> None:
    asyncio.run(run_auditor(force))
