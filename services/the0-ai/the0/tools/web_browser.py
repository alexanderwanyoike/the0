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
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate',
            'Connection': 'keep-alive',
            'Upgrade-Insecure-Requests': '1'
        }
        
        # Fetch the webpage
        response = requests.get(url, headers=headers, timeout=30, allow_redirects=True)
        response.raise_for_status()
        
        # Check if content is HTML
        content_type = response.headers.get('content-type', '').lower()
        if 'text/html' not in content_type and 'application/xhtml' not in content_type:
            return f"Error: URL does not contain HTML content. Content-Type: {content_type}"
        
        # Convert HTML to Markdown using markitdown
        try:
            from markitdown import MarkItDown
            import tempfile
            import os
            
            md_converter = MarkItDown(enable_plugins=False)
            
            # Create temporary file for markitdown
            with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False, encoding='utf-8') as temp_file:
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
                    final_content = final_content[:15000] + f"\n\n... (Content truncated. Original URL: {url})"
                
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
        results.append("\n" + "="*80 + "\n")
        
        # Add small delay between requests to be respectful
        if i < len(urls):
            time.sleep(1)
    
    return "\n".join(results)


def search_web(query: str) -> str:
    """
    Search the web using Google search and return URLs for further browsing.
    
    Args:
        query (str): The search query
                    
    Returns:
        str: Search results with URLs that can be used with browse_url
    """
    try:
        from googlesearch import search
        
        results = [f"# Search Results for: {query}\n"]
        
        # Use googlesearch library to get clean URLs
        search_results = list(search(query, num_results=8, sleep_interval=1))
        
        for i, url in enumerate(search_results, 1):
            domain = urlparse(url).netloc
            results.append(f"## {i}. {domain}")
            results.append(f"**URL**: {url}\n")
        
        if len(search_results) == 0:
            results.append("No search results found.")
        
        results.append("**Note**: Use `browse_url(URL)` to read the full content of any result.")
        return "\n".join(results)
        
    except Exception as e:
        return f"Error: Search failed for query: {query}. Details: {str(e)}"