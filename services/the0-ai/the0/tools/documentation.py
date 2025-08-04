"""
Documentation Viewer Tools for the0 Agent
Provides access to internal documentation stored in the docs folder.
"""

import os
import logging
from pathlib import Path
from typing import List, Dict, Any
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)

async def list_documentation(tool_context: ToolContext) -> str:
    """
    List all available documentation files in the docs folder.
    Creates the docs folder if it doesn't exist and logs a warning.
    
    Returns:
        str: Formatted list of all documentation files with their paths
    """
    try:
        docs_dir = Path(__file__).parent / "docs"
        
        # Create docs directory if it doesn't exist
        if not docs_dir.exists():
            docs_dir.mkdir(exist_ok=True)
            logger.warning("Documentation folder 'docs' was created. No documents are currently available.")
            return "Documentation folder 'docs' was created but is empty. No documents are currently available."
        
        # Find all documentation files (markdown, txt, rst)
        doc_extensions = {'.md', '.txt', '.rst', '.markdown'}
        doc_files = []
        
        for ext in doc_extensions:
            doc_files.extend(docs_dir.rglob(f"*{ext}"))
        
        if not doc_files:
            logger.warning("No documentation files found in the docs folder.")
            return "No documentation files found in the docs folder. The folder exists but contains no readable documents."
        
        # Sort files by name for consistent output
        doc_files.sort()
        
        # Format the output
        result = ["# Available Documentation\n"]
        
        for doc_file in doc_files:
            # Get relative path from docs folder
            relative_path = doc_file.relative_to(docs_dir)
            file_size = doc_file.stat().st_size
            
            # Format size in human readable format
            if file_size < 1024:
                size_str = f"{file_size}B"
            elif file_size < 1024 * 1024:
                size_str = f"{file_size/1024:.1f}KB"
            else:
                size_str = f"{file_size/(1024*1024):.1f}MB"
            
            result.append(f"- **{relative_path}** ({size_str})")
        
        result.append(f"\nTotal: {len(doc_files)} document(s)")
        result.append("\n**Usage**: Use `get_documentation(path)` to read a specific document.")
        
        return "\n".join(result)
        
    except Exception as e:
        error_msg = f"Error listing documentation: {str(e)}"
        logger.error(error_msg)
        return error_msg


async def get_documentation(path: str, tool_context: ToolContext) -> str:
    """
    Read and return the contents of a specific documentation file.
    
    Args:
        path (str): The relative path to the documentation file from the docs folder
                   (e.g., "getting-started.md" or "api/endpoints.md")
    
    Returns:
        str: The contents of the documentation file with metadata
    """
    try:
        if not path:
            return "Error: No file path provided"
        
        # Get the base directory (project root)
        docs_dir = Path(__file__).parent / "docs"

        
        # Check if docs directory exists
        if not docs_dir.exists():
            docs_dir.mkdir(exist_ok=True)
            logger.warning("Documentation folder 'docs' was created. No documents are currently available.")
            return "Documentation folder 'docs' was created but is empty. The requested document does not exist."
        
        # Construct the full file path
        doc_file = docs_dir / path
        
        # Security check: ensure the path is within the docs directory
        try:
            doc_file.resolve().relative_to(docs_dir.resolve())
        except ValueError:
            return f"Error: Invalid path '{path}'. Path must be within the docs folder."
        
        # Check if file exists
        if not doc_file.exists():
            return f"Error: Documentation file '{path}' not found in docs folder."
        
        # Check if it's a file (not a directory)
        if not doc_file.is_file():
            return f"Error: '{path}' is not a file."
        
        # Read the file content
        try:
            with open(doc_file, 'r', encoding='utf-8') as f:
                content = f.read()
        except UnicodeDecodeError:
            # Try with different encoding if UTF-8 fails
            with open(doc_file, 'r', encoding='latin-1') as f:
                content = f.read()
        
        # Get file metadata
        file_stat = doc_file.stat()
        file_size = file_stat.st_size
        
        # Format size in human readable format
        if file_size < 1024:
            size_str = f"{file_size}B"
        elif file_size < 1024 * 1024:
            size_str = f"{file_size/1024:.1f}KB"
        else:
            size_str = f"{file_size/(1024*1024):.1f}MB"
        
        # Format the response with metadata
        result = [
            f"# Documentation: {path}",
            f"**Size**: {size_str}",
            f"**Path**: docs/{path}",
            "",
            "---",
            "",
            content
        ]
        
        return "\n".join(result)
        
    except Exception as e:
        error_msg = f"Error reading documentation file '{path}': {str(e)}"
        logger.error(error_msg)
        return error_msg