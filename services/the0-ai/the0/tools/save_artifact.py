import google.genai.types as types
from google.adk.tools.tool_context import ToolContext
import mimetypes
import os
import json
from pathlib import Path


def get_mime_type_from_filename(filename: str) -> str:
    """Get MIME type based on file extension."""
    mime_type, _ = mimetypes.guess_type(filename)
    if mime_type:
        return mime_type
    
    # Fallback for common code file extensions
    ext = os.path.splitext(filename)[1].lower()
    extension_mapping = {
        '.py': 'text/x-python',
        '.js': 'text/javascript', 
        '.ts': 'text/typescript',
        '.json': 'application/json',
        '.yaml': 'text/yaml',
        '.yml': 'text/yaml',
        '.md': 'text/markdown',
        '.txt': 'text/plain',
        '.csv': 'text/csv',
        '.xml': 'text/xml',
        '.html': 'text/html',
        '.css': 'text/css'
    }
    return extension_mapping.get(ext, 'text/plain')


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
            if hasattr(tool_context, 'state') and 'current_session_id' in tool_context.state:
                session_id = tool_context.state['current_session_id']
            elif hasattr(tool_context, 'state') and 'session_id' in tool_context.state:
                session_id = tool_context.state['session_id']
            else:
                session_id = 'default'
                print(f"Could not find session ID in tool context, using default")
            print(f"Saving artifact {filename} for session {session_id} (via fallback)")
        
        # Create session-specific artifacts directory in user data dir
        from api.config import get_data_dir
        artifacts_dir = get_data_dir() / session_id / "artifacts"
        artifacts_dir.mkdir(parents=True, exist_ok=True)
        
        # Save file to disk
        file_path = artifacts_dir / filename
        is_update = file_path.exists()
        action = "Updated" if is_update else "Created"
        
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(code)
        
        print(f"{action} artifact file: {file_path}")
        
        # Also save to ADK artifact service for backward compatibility
        code_artifact = types.Part.from_text(text=code)
        version = await tool_context.save_artifact(
            filename=filename,
            artifact=code_artifact
        )
        
        # Save artifact metadata to database
        try:
            from api.repositories import chat_repository
            mime_type = get_mime_type_from_filename(filename)
            chat_repository.save_artifact(
                session_id=session_id,
                filename=filename,
                file_path=str(file_path),
                mime_type=mime_type,
                version=version
            )
            print(f"Saved artifact metadata to database: {filename}")
        except Exception as e:
            print(f"Error saving artifact metadata to database: {e}")
        
        # Save artifact metadata as JSON file for backup
        metadata = {
            "filename": filename,
            "session_id": session_id,
            "file_path": str(file_path),
            "version": version,
            "mime_type": get_mime_type_from_filename(filename)
        }
        
        metadata_path = artifacts_dir / f"{filename}.meta.json"
        with open(metadata_path, 'w', encoding='utf-8') as f:
            json.dump(metadata, f, indent=2)
        
        return {
            "status": "success",
            "message": f"Artifact '{filename}' {action.lower()} successfully to {file_path}",
            "filename": filename,
            "version": version,
            "file_path": str(file_path),
            "action": action.lower()
        }
        
    except ValueError as e:
        return {
            "status": "error",
            "error_type": "configuration_error",
            "message": f"Artifact service configuration error: {str(e)}. Is ArtifactService configured in Runner?",
            "filename": filename
        }
    except Exception as e:
        return {
            "status": "error",
            "error_type": "unexpected_error", 
            "message": f"Unexpected error saving artifact: {str(e)}",
            "filename": filename
        }