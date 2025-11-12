"""
Unit tests for the web_browser tool.
"""

import os
import pytest
from unittest.mock import patch, MagicMock, AsyncMock

from the0.tools.web_browser import (
    browse_url,
    browse_multiple_urls,
    tavily_search,
)


class TestBrowseUrl:
    """Tests for the browse_url function."""

    @patch("the0.tools.web_browser.requests.get")
    @patch("markitdown.MarkItDown")
    def test_browse_url_success(self, mock_markitdown, mock_requests):
        """Test successful URL browsing and conversion to markdown."""
        url = "https://example.com/docs"

        # Mock HTTP response
        mock_response = MagicMock()
        mock_response.text = "<html><body><h1>Test Page</h1><p>Content</p></body></html>"
        mock_response.headers = {"content-type": "text/html; charset=utf-8"}
        mock_response.raise_for_status.return_value = None
        mock_requests.return_value = mock_response

        # Mock MarkItDown conversion
        mock_converter = MagicMock()
        mock_result = MagicMock()
        mock_result.text_content = "# Test Page\n\nThis is a comprehensive test page with enough content to pass the minimum length requirement. It contains multiple paragraphs and detailed information about the test subject. This should be sufficient for the content length validation."
        mock_converter.convert.return_value = mock_result
        mock_markitdown.return_value = mock_converter

        # Mock tempfile operations
        with patch("tempfile.NamedTemporaryFile") as mock_tempfile, patch("os.unlink") as mock_unlink:

            mock_file = MagicMock()
            mock_file.name = "/tmp/test.html"
            mock_tempfile.return_value.__enter__.return_value = mock_file

            result = browse_url(url)

            assert "**Source**: [example.com](https://example.com/docs)" in result
            assert "# Test Page" in result
            assert "comprehensive test page" in result

            # Verify calls
            mock_requests.assert_called_once()
            mock_converter.convert.assert_called_once_with("/tmp/test.html")
            mock_unlink.assert_called_once_with("/tmp/test.html")

    def test_browse_url_invalid_url(self):
        """Test browsing with invalid URL format."""
        result = browse_url("not-a-url")
        assert result.startswith("Error: Invalid URL format")

    def test_browse_url_missing_scheme(self):
        """Test browsing with URL missing scheme."""
        result = browse_url("example.com")
        assert result.startswith("Error: Invalid URL format")

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_timeout(self, mock_requests):
        """Test browsing with request timeout."""
        url = "https://slow-site.com"

        import requests

        mock_requests.side_effect = requests.exceptions.Timeout()

        result = browse_url(url)
        assert result.startswith("Error: Request timeout")
        assert url in result

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_connection_error(self, mock_requests):
        """Test browsing with connection error."""
        url = "https://nonexistent-site.com"

        import requests

        mock_requests.side_effect = requests.exceptions.ConnectionError()

        result = browse_url(url)
        assert result.startswith("Error: Could not connect")
        assert url in result

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_http_error(self, mock_requests):
        """Test browsing with HTTP error (404, 500, etc.)."""
        url = "https://example.com/not-found"

        import requests

        mock_response = MagicMock()
        mock_response.status_code = 404
        http_error = requests.exceptions.HTTPError()
        http_error.response = mock_response
        mock_requests.side_effect = http_error

        result = browse_url(url)
        assert result.startswith("Error: HTTP 404")
        assert url in result

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_non_html_content(self, mock_requests):
        """Test browsing non-HTML content."""
        url = "https://example.com/data.json"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "application/json"}
        mock_requests.return_value = mock_response

        result = browse_url(url)
        assert result.startswith("Error: URL does not contain HTML content")
        assert "application/json" in result

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_markitdown_import_error(self, mock_requests):
        """Test browsing when MarkItDown library is not available."""
        url = "https://example.com"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "text/html"}
        mock_requests.return_value = mock_response

        # Patch the import inside the function by patching builtins.__import__
        def mock_import(name, *args, **kwargs):
            if name == "markitdown":
                raise ImportError("No module named 'markitdown'")
            return __import__(name, *args, **kwargs)

        with patch("builtins.__import__", side_effect=mock_import):
            result = browse_url(url)
            assert result == "Error: markitdown library not available"

    @patch("the0.tools.web_browser.requests.get")
    @patch("markitdown.MarkItDown")
    def test_browse_url_markdown_conversion_error(self, mock_markitdown, mock_requests):
        """Test browsing when markdown conversion fails."""
        url = "https://example.com"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "text/html"}
        mock_requests.return_value = mock_response

        mock_converter = MagicMock()
        mock_converter.convert.side_effect = Exception("Conversion failed")
        mock_markitdown.return_value = mock_converter

        with patch("tempfile.NamedTemporaryFile") as mock_tempfile, patch("os.unlink"):
            mock_file = MagicMock()
            mock_file.name = "/tmp/test.html"
            mock_tempfile.return_value.__enter__.return_value = mock_file

            result = browse_url(url)
            assert result.startswith("Error: Markdown conversion failed")
            assert "Conversion failed" in result

    @patch("the0.tools.web_browser.requests.get")
    @patch("markitdown.MarkItDown")
    def test_browse_url_empty_content(self, mock_markitdown, mock_requests):
        """Test browsing when extracted content is empty or too short."""
        url = "https://example.com"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "text/html"}
        mock_requests.return_value = mock_response

        mock_converter = MagicMock()
        mock_result = MagicMock()
        mock_result.text_content = "   "  # Empty content
        mock_converter.convert.return_value = mock_result
        mock_markitdown.return_value = mock_converter

        with patch("tempfile.NamedTemporaryFile") as mock_tempfile, patch("os.unlink"):
            mock_file = MagicMock()
            mock_file.name = "/tmp/test.html"
            mock_tempfile.return_value.__enter__.return_value = mock_file

            result = browse_url(url)
            assert result.startswith("Error: No meaningful content extracted")

    @patch("the0.tools.web_browser.requests.get")
    @patch("markitdown.MarkItDown")
    def test_browse_url_content_truncation(self, mock_markitdown, mock_requests):
        """Test content truncation for very long pages."""
        url = "https://example.com/long-page"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "text/html"}
        mock_requests.return_value = mock_response

        # Create very long content
        long_content = "A" * 20000
        mock_converter = MagicMock()
        mock_result = MagicMock()
        mock_result.text_content = long_content
        mock_converter.convert.return_value = mock_result
        mock_markitdown.return_value = mock_converter

        with patch("tempfile.NamedTemporaryFile") as mock_tempfile, patch("os.unlink"):
            mock_file = MagicMock()
            mock_file.name = "/tmp/test.html"
            mock_tempfile.return_value.__enter__.return_value = mock_file

            result = browse_url(url)

            assert len(result) <= 15100  # Content + citation should be truncated
            assert "Content truncated" in result
            assert url in result

    @patch("the0.tools.web_browser.requests.get")
    def test_browse_url_request_headers(self, mock_requests):
        """Test that proper headers are sent with requests."""
        url = "https://example.com"

        mock_response = MagicMock()
        mock_response.headers = {"content-type": "application/pdf"}  # Will cause early return
        mock_requests.return_value = mock_response

        browse_url(url)

        # Verify proper headers were sent
        call_args = mock_requests.call_args
        headers = call_args[1]["headers"]
        assert "Mozilla/5.0" in headers["User-Agent"]
        assert headers["Accept"].startswith("text/html")
        assert call_args[1]["timeout"] == 30
        assert call_args[1]["allow_redirects"] is True


class TestBrowseMultipleUrls:
    """Tests for the browse_multiple_urls function."""

    def test_browse_multiple_urls_empty_list(self):
        """Test browsing with empty URL list."""
        result = browse_multiple_urls([])
        assert result == "Error: No URLs provided"

    def test_browse_multiple_urls_too_many(self):
        """Test browsing with too many URLs."""
        urls = [f"https://example{i}.com" for i in range(6)]
        result = browse_multiple_urls(urls)
        assert result.startswith("Error: Too many URLs")

    @patch("the0.tools.web_browser.browse_url")
    @patch("time.sleep")
    def test_browse_multiple_urls_success(self, mock_sleep, mock_browse):
        """Test successful browsing of multiple URLs."""
        urls = ["https://site1.com", "https://site2.com"]

        mock_browse.side_effect = [
            "**Source**: [site1.com](https://site1.com)\n\n# Site 1",
            "**Source**: [site2.com](https://site2.com)\n\n# Site 2",
        ]

        result = browse_multiple_urls(urls)

        assert "## Document 1: https://site1.com" in result
        assert "## Document 2: https://site2.com" in result
        assert "# Site 1" in result
        assert "# Site 2" in result
        assert "=" * 80 in result  # Separator

        # Verify sleep was called between requests
        mock_sleep.assert_called_once_with(1)

    @patch("the0.tools.web_browser.browse_url")
    def test_browse_multiple_urls_single_url(self, mock_browse):
        """Test browsing single URL (no sleep needed)."""
        urls = ["https://example.com"]
        mock_browse.return_value = "Content"

        with patch("time.sleep") as mock_sleep:
            result = browse_multiple_urls(urls)

            assert "## Document 1: https://example.com" in result
            assert "Content" in result
            mock_sleep.assert_not_called()  # No sleep for single URL


class TestTavilySearch:
    """Tests for the tavily_search function."""

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_basic_success(self, mock_client_class):
        """Test basic Tavily search with AI answer."""
        query = "python asyncio tutorial"

        # Mock Tavily API response
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(
            return_value={
                "query": query,
                "answer": ("Python asyncio is a library for concurrent programming " "using async/await syntax."),
                "results": [
                    {
                        "title": "Python Asyncio Tutorial",
                        "url": "https://docs.python.org/3/library/asyncio.html",
                        "content": ("asyncio is a library to write concurrent code " "using async/await syntax."),
                        "score": 0.98,
                    },
                    {
                        "title": "Real Python Asyncio Guide",
                        "url": "https://realpython.com/async-io-python/",
                        "content": ("Complete guide to asynchronous programming in " "Python."),
                        "score": 0.95,
                    },
                ],
                "response_time": 1.23,
            }
        )
        mock_client_class.return_value = mock_client

        # Set API key in environment
        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-test-key"}):
            result = await tavily_search(query, search_depth="basic", max_results=5)

        # Verify result formatting
        assert f"# Search Results: {query}" in result
        assert "AI-Generated Summary" in result
        assert "Python asyncio is a library" in result
        assert "Python Asyncio Tutorial" in result
        assert "https://docs.python.org/3/library/asyncio.html" in result
        assert "0.98" in result  # Check relevance score
        assert "## References" in result  # Footnotes section
        assert "[^1]:" in result  # Footnote format
        assert "[^2]:" in result  # Multiple footnotes

        # Verify API was called correctly
        mock_client.search.assert_called_once_with(
            query=query,
            search_depth="basic",
            max_results=5,
            include_answer=True,
            include_raw_content=False,
            timeout=30.0,
        )

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_advanced_no_answer(self, mock_client_class):
        """Test advanced search without AI answer."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(
            return_value={
                "query": "test",
                "results": [
                    {
                        "title": "Test",
                        "url": "https://test.com",
                        "content": "Content",
                        "score": 0.9,
                    }
                ],
                "response_time": 2.1,
            }
        )
        mock_client_class.return_value = mock_client

        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-test-key"}):
            result = await tavily_search("test", search_depth="advanced", include_answer=False)

        assert "# Search Results: test" in result
        assert "AI-Generated Summary" not in result  # No answer requested
        assert "Advanced" in result  # Search type should be capitalized
        assert "2.10s" in result  # Response time

    @pytest.mark.asyncio
    async def test_tavily_search_no_api_key(self):
        """Test error when API key not configured."""
        # Clear environment and mock database failure
        with patch.dict("os.environ", {}, clear=True):
            with patch("api.database.get_db_session") as mock_db:
                mock_db.side_effect = Exception("DB not available")

                result = await tavily_search("test query")

        assert "Error: Tavily API key not configured" in result
        assert "TAVILY_API_KEY" in result
        assert "POST /settings/tavily-api-key" in result
        assert "https://tavily.com" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_timeout(self, mock_client_class):
        """Test timeout handling."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(side_effect=TimeoutError())
        mock_client_class.return_value = mock_client

        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-test-key"}):
            result = await tavily_search("test query")

        assert "Error: Tavily search timed out" in result
        assert "test query" in result
        assert "30s" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_rate_limit(self, mock_client_class):
        """Test rate limit error handling."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(side_effect=Exception("429 rate limit exceeded"))
        mock_client_class.return_value = mock_client

        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-test-key"}):
            result = await tavily_search("test query")

        assert "Error: Tavily API rate limit exceeded" in result
        assert "1,000 searches/month" in result
        assert "https://tavily.com/pricing" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_auth_error(self, mock_client_class):
        """Test authentication error handling."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(side_effect=Exception("401 authentication failed"))
        mock_client_class.return_value = mock_client

        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-invalid"}):
            result = await tavily_search("test query")

        assert "Error: Invalid Tavily API key" in result
        assert "POST /settings/tavily-api-key" in result
        assert "https://tavily.com" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_no_results(self, mock_client_class):
        """Test handling of empty results."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(
            return_value={
                "query": "extremely rare query",
                "results": [],
                "response_time": 0.5,
            }
        )
        mock_client_class.return_value = mock_client

        with patch.dict("os.environ", {"TAVILY_API_KEY": "tvly-test-key"}):
            result = await tavily_search("extremely rare query")

        assert "# Search Results: extremely rare query" in result
        assert "No results found" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_import_error(self, mock_client_class):
        """Test error when Tavily SDK not installed."""
        # Simulate import error
        with patch("builtins.__import__", side_effect=ImportError("No module named 'tavily'")):
            result = await tavily_search("test")

        assert "Error: Tavily SDK not installed" in result
        assert "pip install tavily-python" in result

    @pytest.mark.asyncio
    @patch("tavily.AsyncTavilyClient")
    async def test_tavily_search_database_fallback(self, mock_client_class):
        """Test API key retrieval from database when env var not set."""
        mock_client = AsyncMock()
        mock_client.search = AsyncMock(
            return_value={
                "query": "test",
                "results": [
                    {
                        "title": "T",
                        "url": "https://t.com",
                        "content": "C",
                        "score": 0.9,
                    }
                ],
                "response_time": 1.0,
            }
        )
        mock_client_class.return_value = mock_client

        # Mock database session and repository
        mock_settings_repo = AsyncMock()
        mock_settings_repo.get_setting = AsyncMock(return_value="tvly-db-key")

        with patch.dict("os.environ", {}, clear=True):
            with patch("api.database.get_db_session"):
                with patch(
                    "api.repositories.get_settings_repository",
                    return_value=mock_settings_repo,
                ):
                    result = await tavily_search("test")

        # Should succeed using database key
        assert "# Search Results: test" in result
        mock_settings_repo.get_setting.assert_called_once_with("tavily_api_key")
