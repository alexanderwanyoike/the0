"""Fixer agent — picks up the highest priority issue and opens a PR."""

import asyncio
import logging
import re
from datetime import date

from claude_agent_sdk import query, ClaudeAgentOptions, ResultMessage

from .audit_file import AuditFile, slugify
from .config import AUDIT_PATH, FIXER_LOCK, FIXER_MODEL, PROMPTS_DIR, LOG_DIR, BASE_BRANCH
from .lock import FileLock
from .logging_setup import setup_logging
from .session_guard import check_no_interactive_session
from .worktree import fetch_base_branch, fixer_worktree, cleanup_failed_branch

logger = logging.getLogger(__name__)

PR_URL_PATTERNS = [
    re.compile(r"PR_URL:\s*(\S+)"),
    re.compile(r"(https://github\.com/[^\s]+/pull/\d+)"),
]


def find_pr_url(text: str) -> str | None:
    for pattern in PR_URL_PATTERNS:
        m = pattern.search(text)
        if m:
            return m.group(1)
    return None


async def run_fixer(force: bool = False) -> None:
    setup_logging("fixer")
    logger.info("=== Fixer run ===")

    if not force:
        check_no_interactive_session()

    with FileLock(FIXER_LOCK):
        fetch_base_branch()

        # Parse audit file and pick an issue
        audit = AuditFile.parse(AUDIT_PATH)
        issue = audit.pick_eligible_issue()
        if issue is None:
            logger.info("No eligible issues found. Exiting.")
            return

        slug = slugify(issue.title)
        branch_name = f"fix/audit-{issue.number}-{slug}"
        logger.info(f"Selected issue #{issue.number}: {issue.title}")
        logger.info(f"Branch: {branch_name}")

        # Mark in-progress
        audit.mark_in_progress(issue, branch_name)
        audit.write(AUDIT_PATH)

        try:
            with fixer_worktree(branch_name) as wt_path:
                # Build prompt
                system_prompt = (PROMPTS_DIR / "fixer.md").read_text()
                body_text = "".join(issue.body).strip()
                prompt = (
                    f"{system_prompt}\n\n"
                    f"Fix the following issue from the tech debt audit:\n\n"
                    f"## Issue #{issue.number}: {issue.title}\n"
                    f"{body_text}\n\n"
                    f"The codebase is at: {wt_path}\n"
                    f"The target branch for PRs is: {BASE_BRANCH}"
                )

                # Run agent
                logger.info("Running fixer agent...")
                output_parts: list[str] = []
                async for message in query(
                    prompt=prompt,
                    options=ClaudeAgentOptions(
                        cwd=str(wt_path),
                        allowed_tools=["Read", "Glob", "Grep", "Write", "Edit", "Bash"],
                        permission_mode="bypassPermissions",
                        model=FIXER_MODEL,
                    ),
                ):
                    if isinstance(message, ResultMessage):
                        output_parts.append(message.result)
                        logger.info(f"Fixer result:\n{message.result}")

                output = "\n".join(output_parts)

        except Exception as e:
            logger.error(f"Fixer failed with exception: {e}")
            output = ""

        # Check for PR URL
        pr_url = find_pr_url(output)

        if pr_url:
            logger.info(f"PR created: {pr_url}")
            pr_log = LOG_DIR / "pr_urls.log"
            pr_log.parent.mkdir(parents=True, exist_ok=True)
            with open(pr_log, "a") as f:
                f.write(f"{pr_url}\n")
        else:
            logger.warning("No PR URL found — marking as attempted")
            today = date.today().isoformat()
            audit.mark_attempted(issue, f"build-failure {today}")
            audit.write(AUDIT_PATH)
            cleanup_failed_branch(branch_name)

    logger.info("=== Fixer complete ===")


def main(force: bool = False) -> None:
    asyncio.run(run_fixer(force))
