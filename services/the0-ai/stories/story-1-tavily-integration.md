# Story 1: Tavily Integration

**Epic:** Search Infrastructure
**Status:** Not Started
**Estimated Effort:** 6-8 hours
**Dependencies:** Story 0 (Foundation & Setup)

---

## Description

Replace the hack implementation of `search_web` using `googlesearch-python` with production-grade Tavily API integration. Tavily is optimized for AI agents and provides citation-rich, structured search results with AI-generated answers.

---

## Why

- **Production Quality**: Current search is a hack using googlesearch-python
- **AI-Optimized**: Tavily designed specifically for LLM/agent use cases
- **Rich Results**: Citations, relevance scores, AI-generated summaries
- **Reliability**: Stable API with proper rate limiting and error handling
- **Cost Effective**: Free tier (1K searches/month), reasonable paid pricing

---

## What

Create a robust Tavily search tool that:
- Supports basic and advanced search modes
- Returns structured results with citations
- Includes AI-generated answers
- Handles errors gracefully
- Manages API keys securely
- Integrates seamlessly with existing agent architecture

---

## Tasks

### 1. Install Tavily SDK
- [ ] Add `tavily-python` to `requirements.txt`
- [ ] Run `pip install tavily-python` in development environment
- [ ] Verify installation: `python -c "import tavily; print(tavily.__version__)"`

### 2. Create Tavily Search Tool
- [ ] Create `the0/tools/tavily_search.py`
- [ ] Implement `tavily_search()` async function
- [ ] Add ToolContext parameter for session awareness
- [ ] Support search_depth: "basic" or "advanced"
- [ ] Support max_results configuration (1-20)
- [ ] Support include_answer option
- [ ] Format results with citations
- [ ] Export as google.genai.types.Tool

### 3. API Key Management
- [ ] Add `TAVILY_API_KEY` handling in tool
- [ ] Support environment variable
- [ ] Add database settings support (like Google API key)
- [ ] Create API endpoint: `POST /settings/tavily-api-key`
- [ ] Create API endpoint: `GET /settings/tavily-api-key/status`
- [ ] Create API endpoint: `DELETE /settings/tavily-api-key`
- [ ] Update `SettingsRepository` with Tavily key methods

### 4. Error Handling & Rate Limiting
- [ ] Implement try/except for API errors
- [ ] Add exponential backoff for rate limits
- [ ] Set timeout (30s recommended)
- [ ] Return user-friendly error messages
- [ ] Log errors for debugging

### 5. Testing
- [ ] Create `tests/the0/tools/test_tavily_search.py`
- [ ] Test basic search functionality (mocked)
- [ ] Test advanced search functionality (mocked)
- [ ] Test error handling (no API key, API failure)
- [ ] Test result formatting
- [ ] Test with real API (manual/integration test)
- [ ] Achieve >80% code coverage

### 6. Documentation
- [ ] Add docstrings to tavily_search function
- [ ] Document API key setup process
- [ ] Document search_depth options and cost implications
- [ ] Add example usage
- [ ] Update CLAUDE.md with Tavily integration details

---

## Acceptance Criteria

- [ ] `tavily-python` added to requirements.txt
- [ ] `tavily_search()` tool implemented in `the0/tools/tavily_search.py`
- [ ] Tool accepts ToolContext parameter
- [ ] Search depth configurable (basic/advanced)
- [ ] Results include: titles, URLs, content, relevance scores
- [ ] AI-generated answers included when requested
- [ ] Citations formatted as markdown links
- [ ] API key managed via database and environment variable
- [ ] API endpoints for key management working
- [ ] Error handling returns user-friendly messages
- [ ] Rate limiting implemented
- [ ] Unit tests achieve >80% coverage
- [ ] Integration test with real API passes (manual)
- [ ] Documentation complete
- [ ] Code formatted (`make format`)
- [ ] Linting passes (`make lint`)

---

## Implementation Details

### Tool Implementation

```python
# the0/tools/tavily_search.py

from tavily import AsyncTavilyClient
from google.genai.types import Tool, ToolContext
import os
from typing import Optional

async def tavily_search(
    query: str,
    search_depth: str = "advanced",
    max_results: int = 5,
    include_answer: bool = True,
    tool_context: ToolContext = None
) -> str:
    """
    Search the web using Tavily API optimized for AI agents.

    Tavily provides high-quality search results with citations, relevance scores,
    and AI-generated answers. Ideal for research and information gathering.

    Args:
        query: Search query string
        search_depth: "basic" for quick results (cheaper), "advanced" for
                     comprehensive research (more expensive but better quality)
        max_results: Number of results to return (1-20)
        include_answer: If True, includes AI-generated answer synthesizing results
        tool_context: ADK tool context (provides session information)

    Returns:
        Formatted search results with citations, or error message

    Example:
        results = await tavily_search(
            query="momentum trading strategies for crypto",
            search_depth="advanced",
            max_results=5,
            include_answer=True
        )
    """
    try:
        # Get API key from environment or database
        api_key = os.getenv("TAVILY_API_KEY")
        if not api_key:
            return (
                "Error: Tavily API key not configured. "
                "Please set TAVILY_API_KEY environment variable or "
                "configure via API: POST /settings/tavily-api-key"
            )

        # Initialize async client
        client = AsyncTavilyClient(api_key)

        # Perform search with timeout
        response = await client.search(
            query=query,
            search_depth=search_depth,
            max_results=max_results,
            include_answer=include_answer,
            include_raw_content=False,  # We use browse_url for full content
            timeout=30.0
        )

        # Format results with citations
        result_parts = [f"# Search Results: {query}\n"]

        # Add AI-generated answer if available
        if include_answer and response.get("answer"):
            result_parts.append("## AI-Generated Summary")
            result_parts.append(f"{response['answer']}\n")

        # Add individual search results
        result_parts.append("## Search Results\n")
        for i, result in enumerate(response.get("results", []), 1):
            result_parts.append(f"### {i}. {result['title']}")
            result_parts.append(f"**URL**: {result['url']}")
            result_parts.append(f"**Relevance**: {result.get('score', 'N/A')}")
            result_parts.append(f"\n{result['content']}\n")

        # Add sources section
        result_parts.append("## Sources")
        for i, result in enumerate(response.get("results", []), 1):
            result_parts.append(f"{i}. [{result['title']}]({result['url']})")

        return "\n".join(result_parts)

    except TimeoutError:
        return "Error: Tavily search timed out (>30s). Please try again."
    except Exception as e:
        return f"Error performing Tavily search: {str(e)}"

# Export as ADK Tool
tavily_search_tool = Tool(func_declarations=[tavily_search])
```

### API Endpoints (in `api/main.py`)

```python
# Add to api/main.py

@app.post("/settings/tavily-api-key")
async def set_tavily_api_key(
    request: dict,
    repository: SettingsRepository = Depends(get_settings_repository)
):
    """Set Tavily API key in database."""
    api_key = request.get("api_key")
    if not api_key:
        raise HTTPException(status_code=400, detail="API key required")

    await repository.set_setting("tavily_api_key", api_key)
    return {"message": "Tavily API key configured successfully"}

@app.get("/settings/tavily-api-key/status")
async def get_tavily_api_key_status(
    repository: SettingsRepository = Depends(get_settings_repository)
):
    """Check if Tavily API key is configured."""
    key = await repository.get_setting("tavily_api_key")
    env_key = os.getenv("TAVILY_API_KEY")

    return {
        "configured_in_database": key is not None,
        "configured_in_environment": env_key is not None,
        "active_source": "environment" if env_key else ("database" if key else "none")
    }

@app.delete("/settings/tavily-api-key")
async def delete_tavily_api_key(
    repository: SettingsRepository = Depends(get_settings_repository)
):
    """Remove Tavily API key from database."""
    await repository.delete_setting("tavily_api_key")
    return {"message": "Tavily API key removed"}
```

---

## Technical Considerations

### Search Depth Options
- **basic**: Quick results, ~0.5s response time, lower cost (~$0.005/search)
- **advanced**: Comprehensive research, ~2s response time, higher cost (~$0.01/search)
- Use "basic" for quick lookups, "advanced" for in-depth research

### Cost Management
- Free tier: 1,000 API calls/month
- Estimated usage: 100-500 searches/day = ~$15-150/month
- Monitor usage with Tavily dashboard
- Implement rate limiting to control costs
- Consider caching frequent queries (15-min TTL)

### Rate Limiting Strategy
```python
import asyncio
from functools import wraps

async def with_rate_limit(func):
    """Decorator for rate limiting with exponential backoff."""
    @wraps(func)
    async def wrapper(*args, **kwargs):
        max_retries = 3
        base_delay = 1

        for attempt in range(max_retries):
            try:
                return await func(*args, **kwargs)
            except Exception as e:
                if "rate limit" in str(e).lower() and attempt < max_retries - 1:
                    delay = base_delay * (2 ** attempt)
                    await asyncio.sleep(delay)
                else:
                    raise

    return wrapper
```

### Session Context Usage
While this tool doesn't currently use session state, the ToolContext parameter is included for future enhancements:
```python
# Future: Store search history
if tool_context:
    session_id = tool_context._invocation_context.session.id
    # Could log searches for analytics or caching
```

---

## Risks & Mitigations

### Medium Risk: API Key Security
**Risk:** API keys exposed in logs, errors, or database
**Impact:** Unauthorized usage, security breach
**Mitigation:**
- Store encrypted in database
- Use environment variables for production
- Never log API keys
- Sanitize error messages
- Implement key rotation

**Contingency:** Revoke compromised key immediately, generate new key

### Medium Risk: API Quota Exhaustion
**Risk:** Hitting Tavily rate limits or monthly quota
**Impact:** Search functionality unavailable
**Mitigation:**
- Implement rate limiting
- Monitor usage via dashboard
- Set up quota alerts
- Cache common queries
- Use "basic" depth when possible

**Contingency:** Fallback to old search method, increase quota, or throttle usage

### Low Risk: API Downtime
**Risk:** Tavily service unavailable
**Impact:** Search functionality broken
**Mitigation:**
- Comprehensive error handling
- User-friendly error messages
- Retry logic with exponential backoff

**Contingency:** Manual research or fallback search method

---

## Testing Strategy

### Unit Tests

```python
# tests/the0/tools/test_tavily_search.py

import pytest
from unittest.mock import AsyncMock, patch
from the0.tools.tavily_search import tavily_search

@pytest.mark.asyncio
async def test_tavily_search_basic():
    """Test basic search functionality."""
    with patch('the0.tools.tavily_search.AsyncTavilyClient') as mock_client:
        mock_instance = AsyncMock()
        mock_instance.search = AsyncMock(return_value={
            "answer": "Test answer",
            "results": [
                {
                    "title": "Test Result",
                    "url": "https://example.com",
                    "content": "Test content",
                    "score": 0.95
                }
            ]
        })
        mock_client.return_value = mock_instance

        result = await tavily_search("test query", search_depth="basic")

        assert "Test Result" in result
        assert "https://example.com" in result
        assert "Test content" in result

@pytest.mark.asyncio
async def test_tavily_search_no_api_key():
    """Test error when API key not configured."""
    with patch.dict('os.environ', {}, clear=True):
        result = await tavily_search("test query")
        assert "Error: Tavily API key not configured" in result

@pytest.mark.asyncio
async def test_tavily_search_timeout():
    """Test timeout handling."""
    with patch('the0.tools.tavily_search.AsyncTavilyClient') as mock_client:
        mock_instance = AsyncMock()
        mock_instance.search = AsyncMock(side_effect=TimeoutError())
        mock_client.return_value = mock_instance

        result = await tavily_search("test query")
        assert "timed out" in result.lower()

@pytest.mark.asyncio
async def test_tavily_search_advanced_with_answer():
    """Test advanced search with AI answer."""
    with patch('the0.tools.tavily_search.AsyncTavilyClient') as mock_client:
        mock_instance = AsyncMock()
        mock_instance.search = AsyncMock(return_value={
            "answer": "AI generated answer",
            "results": [{"title": "Result 1", "url": "https://test.com", "content": "Content", "score": 0.9}]
        })
        mock_client.return_value = mock_instance

        result = await tavily_search("test", search_depth="advanced", include_answer=True)

        assert "AI-Generated Summary" in result
        assert "AI generated answer" in result
```

### Integration Test (Manual)

```python
# Manual test with real API (requires TAVILY_API_KEY)
@pytest.mark.integration
@pytest.mark.asyncio
async def test_tavily_real_api():
    """Integration test with real Tavily API."""
    result = await tavily_search(
        query="Python asyncio tutorial",
        search_depth="basic",
        max_results=3
    )

    print(result)  # Manual verification
    assert "http" in result  # Should have URLs
    assert "Source" in result or "Result" in result
```

---

## Related Stories

**Depends On:**
- Story 0: Foundation & Setup (environment configuration)

**Blocks:**
- Story 2: Researcher Agent (needs Tavily tool)

**Related:**
- Story 6: Testing & Validation (comprehensive testing)
- Story 7: Documentation (usage documentation)

---

## Notes

- Get Tavily API key from https://tavily.com
- Free tier sufficient for development and testing
- Consider cost implications of "advanced" vs "basic" search
- Tavily results are already optimized for LLMs - minimal post-processing needed
- Future enhancement: Cache search results for common queries

---

## Definition of Done

- [ ] All tasks completed
- [ ] All acceptance criteria met
- [ ] Unit tests passing with >80% coverage
- [ ] Integration test with real API successful
- [ ] API key management endpoints working
- [ ] Error handling tested
- [ ] Documentation complete
- [ ] Code formatted and linted
- [ ] Peer review completed (if applicable)
- [ ] Changes committed with clear message
