"""
Shared utilities and base configurations for the0 agents.
"""

# Agent configuration constants
DEFAULT_MODEL = "gemini-2.5-flash"
DEFAULT_TEMPERATURE = 0.7

# State management keys (used in Story 5)
STATE_KEY_RESEARCH = "research_data"
STATE_KEY_BOT_METADATA = "bot_metadata"


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
