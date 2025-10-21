"""
Documentation Viewer Tools for the0 Agent
Provides access to internal documentation via the documentation API.
"""

import os
import logging
import httpx
from typing import List, Dict, Any
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)

# Get docs endpoint from environment variable
DOCS_ENDPOINT = os.getenv("DOCS_ENDPOINT", "http://localhost:3002")


async def list_documentation(tool_context: ToolContext) -> str:
    """
    List all available documentation files from the documentation API.

    Returns:
        str: Formatted list of all documentation files with their paths and titles
    """
    try:
        async with httpx.AsyncClient(timeout=30.0) as client:
            response = await client.get(f"{DOCS_ENDPOINT}/api/docs/raw/")
            response.raise_for_status()

            data = response.json()

            if not data.get("success"):
                error_msg = data.get("error", "Unknown error from documentation API")
                logger.error(f"API returned error: {error_msg}")
                return f"Error fetching documentation list: {error_msg}"

            docs = data.get("docs", [])

            if not docs:
                logger.warning("No documentation files found from API.")
                return "No documentation files available."

            # Format the output
            result = [f"# Available Documentation ({len(docs)} documents)\n"]

            for doc in docs:
                path = doc.get("path", "")
                title = doc.get("title", path)
                description = doc.get("description", "")

                if description:
                    result.append(f"- **{title}** (`{path}`)")
                    result.append(f"  {description}")
                else:
                    result.append(f"- **{title}** (`{path}`)")

            result.append(f"\nTotal: {len(docs)} document(s)")
            result.append(
                "\n**Usage**: Use `get_documentation(path)` with the path shown above to read a specific document."
            )
            result.append(f"\n**Example**: `get_documentation('welcome-to-the0')` or `get_documentation('terminology/bots')`")

            return "\n".join(result)

    except httpx.RequestError as e:
        error_msg = f"Network error connecting to documentation API at {DOCS_ENDPOINT}: {str(e)}"
        logger.error(error_msg)
        return f"{error_msg}\n\nPlease ensure the documentation service is running and the DOCS_ENDPOINT environment variable is correctly configured."
    except httpx.HTTPStatusError as e:
        error_msg = f"HTTP error from documentation API: {e.response.status_code}"
        logger.error(error_msg)
        return f"{error_msg}\n\nThe documentation API returned an error. Please check the documentation service."
    except Exception as e:
        error_msg = f"Error listing documentation: {str(e)}"
        logger.error(error_msg)
        return error_msg


async def get_documentation(path: str, tool_context: ToolContext) -> str:
    """
    Read and return the contents of a specific documentation file from the API.

    Args:
        path (str): The path to the documentation file
                   (e.g., "welcome-to-the0" or "terminology/bots")

    Returns:
        str: The contents of the documentation file with metadata
    """
    try:
        if not path:
            return "Error: No file path provided"

        # Remove leading/trailing slashes and clean the path
        path = path.strip("/")

        # Construct the API URL (with trailing slash as required by Next.js)
        api_url = f"{DOCS_ENDPOINT}/api/docs/raw/{path}/"

        logger.info(f"Fetching documentation from: {api_url}")

        async with httpx.AsyncClient(timeout=30.0) as client:
            response = await client.get(api_url)
            response.raise_for_status()

            # Check if we got markdown content
            content_type = response.headers.get("content-type", "")
            if "markdown" not in content_type and "text" not in content_type:
                return f"Error: Expected markdown content but got {content_type}"

            content = response.text

            if not content or content.strip() == "":
                return f"Error: Documentation file '{path}' is empty."

            # Format the response with metadata
            result = [
                f"# Documentation: {path}",
                f"**Source**: {DOCS_ENDPOINT}/api/docs/raw/{path}/",
                "",
                "---",
                "",
                content,
            ]

            return "\n".join(result)

    except httpx.HTTPStatusError as e:
        if e.response.status_code == 404:
            error_msg = f"Documentation file '{path}' not found."
            logger.warning(error_msg)
            return f"{error_msg}\n\nUse `list_documentation()` to see all available documentation files."
        else:
            error_msg = f"HTTP error {e.response.status_code} fetching documentation '{path}'"
            logger.error(error_msg)
            return f"{error_msg}\n\nThe documentation API returned an error."
    except httpx.RequestError as e:
        error_msg = f"Network error connecting to documentation API at {DOCS_ENDPOINT}: {str(e)}"
        logger.error(error_msg)
        return f"{error_msg}\n\nPlease ensure the documentation service is running and the DOCS_ENDPOINT environment variable is correctly configured."
    except Exception as e:
        error_msg = f"Error reading documentation file '{path}': {str(e)}"
        logger.error(error_msg)
        return error_msg
