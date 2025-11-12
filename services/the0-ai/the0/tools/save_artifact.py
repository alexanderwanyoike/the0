import google.genai.types as types
from google.adk.tools.tool_context import ToolContext
import mimetypes
import os
import json
from pathlib import Path


def get_mime_type_from_filename(filename: str) -> str:
    """Get MIME type based on file extension."""
    # First check our custom extension mapping for programming files
    ext = os.path.splitext(filename)[1].lower()
    extension_mapping = {
        ".py": "text/x-python",
        ".js": "text/javascript",
        ".ts": "text/typescript",
        ".json": "application/json",
        ".yaml": "text/yaml",
        ".yml": "text/yaml",
        ".md": "text/markdown",
        ".txt": "text/plain",
        ".csv": "text/csv",
        ".xml": "text/xml",
        ".html": "text/html",
        ".css": "text/css",
    }

    # Return custom mapping if available
    if ext in extension_mapping:
        return extension_mapping[ext]

    # Fall back to system MIME type detection for other files
    mime_type, _ = mimetypes.guess_type(filename)
    return mime_type or "text/plain"


async def save_artifact(
    code: str,
    filename: str,
    tool_context: ToolContext,
) -> dict:
    """Save code as an artifact with proper MIME type detection and persistent file storage.

    Args:
        code: The code content to save
        filename: The filename for the artifact
        tool_context: The tool context for artifact operations

    Returns:
        Dictionary with status and result information
    """
    try:
        # Get session ID from tool context using the proper Google ADK method
        try:
            session_id = tool_context._invocation_context.session.id
            print(f"Saving artifact {filename} for session {session_id}")
        except AttributeError:
            # Fallback to state if session ID was stored there
            if hasattr(tool_context, "state") and "current_session_id" in tool_context.state:
                session_id = tool_context.state["current_session_id"]
            elif hasattr(tool_context, "state") and "session_id" in tool_context.state:
                session_id = tool_context.state["session_id"]
            else:
                session_id = "default"
                print(f"Could not find session ID in tool context, using default")
            print(f"Saving artifact {filename} for session {session_id} (via fallback)")

        # Save file using storage service (MinIO or filesystem fallback)
        from api.storage import storage_service
        from api.database import get_db_session
        from api.repositories import get_chat_repository

        # Check if this is an update by looking for existing artifact
        async with get_db_session() as db_session:
            chat_repo = get_chat_repository(db_session)
            existing_artifact = await chat_repo.get_artifact(session_id, filename)
            is_update = existing_artifact is not None
            action = "Updated" if is_update else "Created"

        # Save file to storage
        storage_path = await storage_service.save_artifact(session_id, filename, code)
        print(f"{action} artifact file: {storage_path}")

        # Also save to ADK artifact service for backward compatibility
        code_artifact = types.Part.from_text(text=code)
        version = await tool_context.save_artifact(filename=filename, artifact=code_artifact)

        # Save artifact metadata to database
        try:
            mime_type = get_mime_type_from_filename(filename)
            async with get_db_session() as db_session:
                chat_repo = get_chat_repository(db_session)
                await chat_repo.save_artifact(
                    session_id=session_id,
                    filename=filename,
                    file_path=storage_path,
                    mime_type=mime_type,
                    version=version,
                )
            print(f"Saved artifact metadata to database: {filename}")
        except Exception as e:
            print(f"Error saving artifact metadata to database: {e}")

        return {
            "status": "success",
            "message": f"Artifact '{filename}' {action.lower()} successfully to {storage_path}",
            "filename": filename,
            "version": version,
            "file_path": storage_path,
            "action": action.lower(),
        }

    except ValueError as e:
        return {
            "status": "error",
            "error_type": "configuration_error",
            "message": f"Artifact service configuration error: {str(e)}. Is ArtifactService configured in Runner?",
            "filename": filename,
        }
    except Exception as e:
        return {
            "status": "error",
            "error_type": "unexpected_error",
            "message": f"Unexpected error saving artifact: {str(e)}",
            "filename": filename,
        }
