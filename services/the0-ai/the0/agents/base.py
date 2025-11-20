"""
Shared utilities and base configurations for the0 agents.
"""

# Agent configuration constants
import logging
import os
import shutil

DEFAULT_MODEL = "gemini-2.5-flash"
DEFAULT_TEMPERATURE = 0.7

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Shared utility functions
def format_citations(sources: list) -> str:
    """
    Format sources as markdown citations.

    Args:
        sources: List of source dictionaries with 'title' and 'url' keys

    Returns:
        Markdown-formatted citations string
    """
    citations = []
    for i, source in enumerate(sources, 1):
        title = source.get("title", "Source")
        url = source.get("url", "")
        citations.append(f"{i}. [{title}]({url})")
    return "\n".join(citations)

WORKSPACE_DIR = os.path.abspath("workspace")

def setup_workspace():
  """Clears and creates a fresh workspace for the agent."""
  shutil.rmtree(WORKSPACE_DIR, ignore_errors=True)
  os.makedirs(WORKSPACE_DIR, exist_ok=True)
  logger.info(f"Workspace set up at {WORKSPACE_DIR}")