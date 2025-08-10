import zipfile
import os
from datetime import datetime
from google.adk.tools.tool_context import ToolContext


async def deploy_bot(
    bot_name: str,
    tool_context: ToolContext,
) -> dict:
    """Deploy bot by creating a zip file with all artifacts in the bots folder.

    Args:
        bot_name: Name of the bot to deploy
        tool_context: The tool context for artifact operations

    Returns:
        Dictionary with status and deployment information
    """
    try:
        # Create bots directory if it doesn't exist
        bots_dir = "bots"
        os.makedirs(bots_dir, exist_ok=True)

        # Generate zip filename with timestamp
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        zip_filename = f"{bot_name}_{timestamp}.zip"
        zip_path = os.path.join(bots_dir, zip_filename)

        # Get list of all artifacts
        artifact_keys = await tool_context.list_artifacts()

        if not artifact_keys:
            return {
                "status": "error",
                "error_type": "no_artifacts",
                "message": "No artifacts found to deploy. Please create bot files first.",
                "bot_name": bot_name,
            }

        # Create zip file with all artifacts
        with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zipf:
            for artifact_key in artifact_keys:
                try:
                    # Load the latest version of each artifact
                    artifact = await tool_context.load_artifact(filename=artifact_key)

                    # Extract text content from the artifact
                    if hasattr(artifact, "text") and artifact.text:
                        content = artifact.text
                    elif hasattr(artifact, "inline_data") and artifact.inline_data:
                        # Handle binary data if needed
                        content = artifact.inline_data.data.decode("utf-8")
                    else:
                        content = str(artifact)

                    # Add file to zip
                    zipf.writestr(artifact_key, content)

                except Exception as e:
                    return {
                        "status": "error",
                        "error_type": "artifact_load_error",
                        "message": f"Failed to load artifact '{artifact_key}': {str(e)}",
                        "bot_name": bot_name,
                    }

        return {
            "status": "success",
            "message": f"Bot '{bot_name}' deployed successfully",
            "bot_name": bot_name,
            "zip_file": zip_path,
            "artifacts_count": len(artifact_keys),
            "artifacts": artifact_keys,
        }

    except Exception as e:
        return {
            "status": "error",
            "error_type": "deployment_error",
            "message": f"Failed to deploy bot: {str(e)}",
            "bot_name": bot_name,
        }
