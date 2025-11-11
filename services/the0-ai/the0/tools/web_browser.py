"""
Web Browser Tool for the0 Agent
Fetches web pages and converts them to readable markdown format.
"""

import requests
from urllib.parse import urljoin, urlparse, quote_plus
import time


def browse_url(url: str) -> str:
    """
    Fetch a web page and convert it to readable markdown format.

    Args:
        url (str): The URL to fetch and convert. Should be a valid HTTP/HTTPS URL.

    Returns:
        str: The webpage content converted to markdown format with source citation
    """
    try:
        # Validate URL
        parsed_url = urlparse(url)
        if not parsed_url.scheme or not parsed_url.netloc:
            return f"Error: Invalid URL format: {url}"

        # Set headers to mimic a real browser
        headers = {
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
            "Accept-Language": "en-US,en;q=0.5",
            "Accept-Encoding": "gzip, deflate",
            "Connection": "keep-alive",
            "Upgrade-Insecure-Requests": "1",
        }

        # Fetch the webpage
        response = requests.get(url, headers=headers, timeout=30, allow_redirects=True)
        response.raise_for_status()

        # Check if content is HTML
        content_type = response.headers.get("content-type", "").lower()
        if "text/html" not in content_type and "application/xhtml" not in content_type:
            return f"Error: URL does not contain HTML content. Content-Type: {content_type}"

        # Convert HTML to Markdown using markitdown
        try:
            from markitdown import MarkItDown
            import tempfile
            import os

            md_converter = MarkItDown(enable_plugins=False)

            # Create temporary file for markitdown
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".html", delete=False, encoding="utf-8"
            ) as temp_file:
                temp_file.write(response.text)
                temp_file_path = temp_file.name

            try:
                result = md_converter.convert(temp_file_path)
                markdown_content = result.text_content

                if not markdown_content or len(markdown_content.strip()) < 50:
                    return f"Error: No meaningful content extracted from {url}"

                # Format the response with citation information
                citation_info = f"**Source**: [{urlparse(url).netloc}]({url})\n\n"
                final_content = citation_info + markdown_content

                # Limit content length to prevent overwhelming responses
                if len(final_content) > 15000:
                    final_content = (
                        final_content[:15000]
                        + f"\n\n... (Content truncated. Original URL: {url})"
                    )

                return final_content

            finally:
                os.unlink(temp_file_path)

        except ImportError:
            return f"Error: markitdown library not available"
        except Exception as e:
            return f"Error: Markdown conversion failed for {url}. Details: {str(e)}"

    except requests.exceptions.Timeout:
        return f"Error: Request timeout while accessing {url}. The page took too long to load."

    except requests.exceptions.ConnectionError:
        return f"Error: Could not connect to {url}. Please check the URL and try again."

    except requests.exceptions.HTTPError as e:
        return f"Error: HTTP {e.response.status_code} when accessing {url}. The page may not exist or be unavailable."

    except requests.exceptions.RequestException as e:
        return f"Error: Request failed for {url}. Details: {str(e)}"

    except Exception as e:
        return f"Error: Failed to process {url}. Details: {str(e)}"


def browse_multiple_urls(urls: list) -> str:
    """
    Browse multiple URLs and return their combined content.

    Useful for reading multiple related documentation pages or
    following a series of links from search results.

    Args:
        urls (list): List of URLs to browse

    Returns:
        str: Combined markdown content from all URLs
    """
    if not urls:
        return "Error: No URLs provided"

    if len(urls) > 5:
        return "Error: Too many URLs. Please limit to 5 URLs at a time to avoid overwhelming responses."

    results = []
    for i, url in enumerate(urls, 1):
        results.append(f"## Document {i}: {url}\n")
        content = browse_url(url)
        results.append(content)
        results.append("\n" + "=" * 80 + "\n")

        # Add small delay between requests to be respectful
        if i < len(urls):
            time.sleep(1)

    return "\n".join(results)


async def tavily_search(
    query: str,
    search_depth: str = "basic",
    max_results: int = 5,
    include_answer: bool = True,
    tool_context=None,
) -> str:
    """
    Search the web using Tavily API optimized for AI agents.

    Tavily provides high-quality, AI-optimized search results with citations,
    relevance scores, and AI-generated answers. Designed specifically for LLMs.

    Args:
        query (str): The search query
        search_depth (str): "basic" for quick results (cheaper, ~0.5s) or
                           "advanced" for comprehensive research (expensive, ~2s)
                           Default: "basic"
        max_results (int): Number of results to return (1-20). Default: 5
        include_answer (bool): Include AI-generated answer synthesizing results.
                              Default: True
        tool_context: ADK ToolContext (optional, for session tracking)

    Returns:
        str: Formatted search results with citations and optional AI answer

    Cost:
        - Basic search: ~$0.005 per search
        - Advanced search: ~$0.01 per search
        - Free tier: 1,000 searches/month

    Example:
        results = await tavily_search(
            query="momentum trading strategies",
            search_depth="advanced",
            max_results=5,
            include_answer=True
        )
    """
    try:
        from tavily import AsyncTavilyClient
        import os

        # Get API key from environment or database
        api_key = os.getenv("TAVILY_API_KEY")

        if not api_key:
            # Try database settings as fallback
            try:
                from api.database import get_db_session
                from api.repositories import get_settings_repository

                async with get_db_session() as db_session:
                    settings_repo = get_settings_repository(db_session)
                    api_key = await settings_repo.get_setting("tavily_api_key")
            except Exception:
                pass  # Database not available or key not set

        if not api_key:
            return (
                "Error: Tavily API key not configured.\n\n"
                "Please set the API key using one of these methods:\n"
                "1. Environment variable: TAVILY_API_KEY=tvly-your-key\n"
                "2. API endpoint: POST /settings/tavily-api-key\n\n"
                "Get your free API key at: https://tavily.com"
            )

        # Initialize async Tavily client
        client = AsyncTavilyClient(api_key=api_key)

        # Perform search with timeout
        response = await client.search(
            query=query,
            search_depth=search_depth,
            max_results=max_results,
            include_answer=include_answer,
            include_raw_content=False,  # We use browse_url for full content
            timeout=30.0,
        )

        # Format results with markdown and footnote-style citations
        result_parts = [f"# Search Results: {query}\n"]

        # Add search metadata
        result_parts.append(
            f"**Search Type**: {search_depth.capitalize()} "
            f"| **Results**: {len(response.get('results', []))} "
            f"| **Response Time**: {response.get('response_time', 0):.2f}s\n"
        )

        # Add AI-generated answer if available
        if include_answer and response.get("answer"):
            result_parts.append("## AI-Generated Summary\n")
            result_parts.append(f"{response['answer']}\n")

        # Add individual search results with inline footnote references
        results = response.get("results", [])
        if results:
            result_parts.append("## Top Results\n")
            for i, result in enumerate(results, 1):
                title = result.get("title", "No title")
                url = result.get("url", "")
                content = result.get("content", "No content available")
                score = result.get("score", 0.0)

                # Add result with footnote reference [i]
                result_parts.append(f"### {i}. {title} [^{i}]")
                result_parts.append(f"**Relevance**: {score:.2f}/1.00")
                result_parts.append(f"\n{content}\n")
        else:
            result_parts.append("No results found for this query.\n")

        # Add footnotes section at the end with all citations
        if results:
            result_parts.append("---\n")
            result_parts.append("## References\n")
            for i, result in enumerate(results, 1):
                title = result.get("title", "No title")
                url = result.get("url", "")
                result_parts.append(f"[^{i}]: [{title}]({url})")

        result_parts.append(
            "\n**Tip**: Use `browse_url(URL)` to read full content of any result."
        )

        return "\n".join(result_parts)

    except TimeoutError:
        return (
            f"Error: Tavily search timed out (>30s) for query: {query}\n"
            "Please try again or use a more specific query."
        )

    except ImportError:
        return (
            "Error: Tavily SDK not installed.\n" "Please run: pip install tavily-python"
        )

    except Exception as e:
        error_msg = str(e)

        # Handle rate limiting
        if "rate limit" in error_msg.lower() or "429" in error_msg:
            return (
                f"Error: Tavily API rate limit exceeded.\n"
                "Please wait a moment before searching again.\n\n"
                "Free tier: 1,000 searches/month\n"
                "Upgrade at: https://tavily.com/pricing"
            )

        # Handle authentication errors
        if (
            "authentication" in error_msg.lower()
            or "401" in error_msg
            or "403" in error_msg
        ):
            return (
                "Error: Invalid Tavily API key.\n"
                "Please check your API key configuration.\n\n"
                "Set via: POST /settings/tavily-api-key\n"
                "Get a new key at: https://tavily.com"
            )

        # Generic error
        return (
            f"Error: Tavily search failed for query: {query}\n"
            f"Details: {error_msg}\n\n"
            "If this persists, please check your API key and network connection."
        )
