"""Git worktree lifecycle management."""

from __future__ import annotations

import logging
import subprocess
from contextlib import contextmanager
from pathlib import Path

from .config import REPO_ROOT, WORKTREE_DIR, BASE_BRANCH

logger = logging.getLogger(__name__)


def _run(cmd: list[str], **kwargs) -> subprocess.CompletedProcess:
    logger.debug(f"Running: {' '.join(cmd)}")
    return subprocess.run(cmd, cwd=str(REPO_ROOT), capture_output=True, text=True, **kwargs)


def fetch_base_branch() -> None:
    logger.info(f"Fetching origin/{BASE_BRANCH}...")
    _run(["git", "fetch", "origin", BASE_BRANCH])


def _remove_worktree(path: Path) -> None:
    if path.exists():
        logger.info(f"Removing worktree at {path}")
        result = _run(["git", "worktree", "remove", str(path), "--force"])
        if result.returncode != 0:
            import shutil
            shutil.rmtree(path, ignore_errors=True)


@contextmanager
def auditor_worktree():
    """Create a detached worktree for the auditor at origin/{BASE_BRANCH}."""
    path = WORKTREE_DIR / "auditor"
    _remove_worktree(path)

    WORKTREE_DIR.mkdir(parents=True, exist_ok=True)
    result = _run(["git", "worktree", "add", str(path), f"origin/{BASE_BRANCH}", "--detach"])
    if result.returncode != 0:
        raise RuntimeError(f"Failed to create worktree: {result.stderr}")
    logger.info(f"Created auditor worktree at {path}")

    try:
        yield path
    finally:
        _remove_worktree(path)


@contextmanager
def fixer_worktree(branch_name: str):
    """Create a worktree with a new branch for the fixer."""
    path = WORKTREE_DIR / "fixer"
    _remove_worktree(path)

    # Clean up existing branch
    _run(["git", "branch", "-D", branch_name])
    _run(["git", "push", "origin", "--delete", branch_name])

    WORKTREE_DIR.mkdir(parents=True, exist_ok=True)
    result = _run(["git", "worktree", "add", "-b", branch_name, str(path), f"origin/{BASE_BRANCH}"])
    if result.returncode != 0:
        raise RuntimeError(f"Failed to create worktree: {result.stderr}")
    logger.info(f"Created fixer worktree at {path} on branch {branch_name}")

    try:
        yield path
    finally:
        _remove_worktree(path)


def cleanup_failed_branch(branch_name: str) -> None:
    """Delete a local branch after a failed fix attempt."""
    _run(["git", "branch", "-D", branch_name])
    logger.info(f"Cleaned up branch {branch_name}")
