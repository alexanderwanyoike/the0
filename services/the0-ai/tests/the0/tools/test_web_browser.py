"""
Unit tests for the web_browser tool.
"""

import pytest
import tempfile
import os
from unittest.mock import patch, MagicMock, mock_open
from urllib.parse import urlparse

from the0.tools.web_browser import browse_url, browse_multiple_urls, search_web


class TestBrowseUrl:
    """Tests for the browse_url function."""

    @patch("the0.tools.web_browser.requests.get")
    @patch("markitdown.MarkItDown")
    def test_browse_url_success(self, mock_markitdown, mock_requests):
        """Test successful URL browsing and conversion to markdown."""
        url = "https://example.com/docs"

        # Mock HTTP response
        mock_response = MagicMock()
        mock_response.text = (
            "<html><body><h1>Test Page</h1><p>Content</p></body></html>"
        )
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
        with patch("tempfile.NamedTemporaryFile") as mock_tempfile, patch(
            "os.unlink"
        ) as mock_unlink:

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
        mock_response.headers = {
            "content-type": "application/pdf"
        }  # Will cause early return
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


class TestSearchWeb:
    """Tests for the search_web function."""

    @patch("googlesearch.search")
    def test_search_web_success(self, mock_search):
        """Test successful web search."""
        query = "python tutorial"
        search_results = [
            "https://docs.python.org/3/tutorial/",
            "https://realpython.com/python-basics/",
            "https://w3schools.com/python/",
        ]

        mock_search.return_value = iter(search_results)

        result = search_web(query)

        assert f"# Search Results for: {query}" in result
        assert "## 1. docs.python.org" in result
        assert "## 2. realpython.com" in result
        assert "## 3. w3schools.com" in result

        for url in search_results:
            assert f"**URL**: {url}" in result

        assert "browse_url(URL)" in result  # Usage instruction

        # Verify search was called with correct parameters
        mock_search.assert_called_once_with(query, num_results=8, sleep_interval=1)

    @patch("googlesearch.search")
    def test_search_web_no_results(self, mock_search):
        """Test web search with no results."""
        query = "extremely rare query that returns nothing"
        mock_search.return_value = iter([])

        result = search_web(query)

        assert f"# Search Results for: {query}" in result
        assert "No search results found." in result

    @patch("googlesearch.search")
    def test_search_web_import_error(self, mock_search):
        """Test web search when googlesearch library is not available."""
        query = "test query"

        with patch(
            "googlesearch.search",
            side_effect=ImportError("No module named 'googlesearch'"),
        ):
            # Need to patch the import itself
            with patch.dict("sys.modules", {"googlesearch": None}):
                result = search_web(query)
                assert result.startswith("Error: Search failed")
                assert query in result

    @patch("googlesearch.search")
    def test_search_web_search_exception(self, mock_search):
        """Test web search when search function raises exception."""
        query = "test query"
        mock_search.side_effect = Exception("Search service unavailable")

        result = search_web(query)
        assert result.startswith("Error: Search failed")
        assert query in result
        assert "Search service unavailable" in result

    @patch("googlesearch.search")
    def test_search_web_url_parsing(self, mock_search):
        """Test that domains are correctly parsed from URLs."""
        query = "test"
        search_results = [
            "https://www.example.com/path/to/page",
            "http://subdomain.test.org/article",
            "https://simple.net",
        ]

        mock_search.return_value = iter(search_results)

        result = search_web(query)

        assert "## 1. www.example.com" in result
        assert "## 2. subdomain.test.org" in result
        assert "## 3. simple.net" in result

    def test_search_web_empty_query(self):
        """Test web search with empty query."""
        with patch("googlesearch.search") as mock_search:
            result = search_web("")

            # Should still attempt search
            assert "# Search Results for:" in result
            mock_search.assert_called_once()

    @patch("googlesearch.search")
    def test_search_web_special_characters_query(self, mock_search):
        """Test web search with special characters in query."""
        query = "C++ programming & algorithms (advanced)"
        mock_search.return_value = iter(["https://example.com"])

        result = search_web(query)

        assert f"# Search Results for: {query}" in result
        mock_search.assert_called_once_with(query, num_results=8, sleep_interval=1)
