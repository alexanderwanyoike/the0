"""
Shared utilities and base configurations for the0 agents.
"""

# Agent configuration constants
import logging
import os
import shutil
import contextvars
from typing import Optional
from google.genai import types

DEFAULT_MODEL = "gemini-2.5-flash"
DEFAULT_TEMPERATURE = 0.7

# Configure robust retry options for rate limits
DEFAULT_RETRY_OPTIONS = types.HttpRetryOptions(
    attempts=10,  # High retry count for rate limits
    initial_delay=5.0,
    max_delay=60.0,
    exp_base=2.0,
    jitter=1.0,
    http_status_codes=[429, 503]
)

DEFAULT_HTTP_OPTIONS = types.HttpOptions(
    retry_options=DEFAULT_RETRY_OPTIONS
)

DEFAULT_GENERATE_CONTENT_CONFIG = types.GenerateContentConfig(
    http_options=DEFAULT_HTTP_OPTIONS,
    temperature=DEFAULT_TEMPERATURE
)

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

# ContextVar to store the current workspace ID (session ID)
workspace_id_var: contextvars.ContextVar[str] = contextvars.ContextVar("workspace_id", default="default")

def get_workspace_path(session_id: Optional[str] = None) -> str:
  """
  Returns the path to the current workspace.
  If session_id is provided, uses it. Otherwise falls back to context or default.
  """
  if session_id is None:
    # Fallback to context var if available, else default
    try:
      session_id = workspace_id_var.get()
    except LookupError:
      session_id = "default"
      
  return os.path.abspath(os.path.join("workspace", session_id))

def setup_workspace(session_id: Optional[str] = None):
  """
  Clears and creates a fresh workspace for the agent.
  If session_id is provided, it sets the context var and creates a unique workspace.
  """
  if session_id:
    workspace_id_var.set(session_id)
  
  workspace_path = get_workspace_path(session_id)
  
  # Only clear if it exists? Or always clear?
  # Usually for a new session we want a clean slate.
  shutil.rmtree(workspace_path, ignore_errors=True)
  os.makedirs(workspace_path, exist_ok=True)
  logger.info(f"Workspace set up at {workspace_path}")
