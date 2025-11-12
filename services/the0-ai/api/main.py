from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, StreamingResponse
from typing import List
import json
import os
from api.schemas import (
    ChatRequest,
    ChatResponse,
    ArtifactResponse,
    HealthResponse,
    StreamChunk,
)
from api.agent_service import agent_service
from api.database import init_database, get_db_session
from api.repositories import get_chat_repository, get_settings_repository

app = FastAPI(
    title="the0 AI Agent API",
    description="FastAPI service for interacting with the0 trading bot AI agent",
    version="1.0.0",
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.on_event("startup")
async def startup_event():
    """Initialize database on startup."""
    await init_database()


@app.get("/health", response_model=HealthResponse)
async def health_check():
    """Health check endpoint."""
    return HealthResponse(status="healthy", message="the0 AI Agent API is running")


@app.post("/chat", response_model=ChatResponse)
async def chat(request: ChatRequest):
    """Send a message to the agent and get a response."""
    try:
        response = await agent_service.chat(message=request.message, session_id=request.session_id)
        return response
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/chat/stream")
async def chat_stream(request: ChatRequest):
    """Send a message to the agent and stream the response."""

    async def generate_stream():
        try:
            async for chunk in agent_service.chat_stream(message=request.message, session_id=request.session_id):
                # Format as Server-Sent Events
                chunk_json = chunk.model_dump_json()
                yield f"data: {chunk_json}\n\n"
        except Exception as e:
            # Send error as final chunk
            error_chunk = StreamChunk(type="error", error=str(e), session_id=request.session_id)
            yield f"data: {error_chunk.model_dump_json()}\n\n"

        # Send final done signal
        yield "data: [DONE]\n\n"

    return StreamingResponse(
        generate_stream(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",  # Disable nginx buffering
        },
    )


@app.get("/artifacts", response_model=List[str])
async def list_artifacts():
    """List all available artifacts."""
    try:
        artifacts = await agent_service.list_artifact_keys()
        return artifacts
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/artifacts/session/{session_id}", response_model=List[str])
async def list_session_artifacts(session_id: str):
    """List artifacts for a specific session."""
    try:
        artifacts = await agent_service.list_session_artifact_keys(session_id)
        return artifacts
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/artifacts/download")
async def download_artifacts():
    """Download all artifacts as a ZIP file."""
    try:
        zip_path = await agent_service.create_artifacts_zip()
        if zip_path is None:
            raise HTTPException(status_code=404, detail="No artifacts found")
        return FileResponse(zip_path, media_type="application/zip", filename="trading-bot.zip")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/artifacts/{filename}", response_model=ArtifactResponse)
async def get_artifact(filename: str, session_id: str = None):
    """Get a specific artifact by filename."""
    try:
        artifact = await agent_service.get_artifact(filename, session_id)
        if artifact is None:
            raise HTTPException(status_code=404, detail="Artifact not found")
        return artifact
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/artifacts/session/{session_id}/{filename}", response_model=ArtifactResponse)
async def get_session_artifact(session_id: str, filename: str):
    """Get a specific artifact by session and filename."""
    try:
        artifact = await agent_service.get_artifact(filename, session_id)
        if artifact is None:
            raise HTTPException(status_code=404, detail="Artifact not found")
        return artifact
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/chat/sessions")
async def list_chat_sessions(user_id: str = "default-user"):
    """List all chat sessions for a user."""
    try:
        async with get_db_session() as session:
            chat_repo = get_chat_repository(session)
            sessions = await chat_repo.list_sessions(user_id)
            return [
                {
                    "id": session.id,
                    "title": session.title or f"Chat {session.id[:8]}",
                    "created_at": session.created_at.isoformat(),
                    "updated_at": session.updated_at.isoformat(),
                    "is_active": session.is_active,
                }
                for session in sessions
            ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/chat/sessions/{session_id}")
async def get_chat_session(session_id: str):
    """Get a chat session with all its messages."""
    try:
        async with get_db_session() as db_session:
            chat_repo = get_chat_repository(db_session)
            session_data = await chat_repo.get_session_with_messages(session_id)
            if session_data is None:
                raise HTTPException(status_code=404, detail="Session not found")

            session = session_data["session"]
            messages = session_data["messages"]

            return {
                "session": {
                    "id": session.id,
                    "title": session.title or f"Chat {session.id[:8]}",
                    "created_at": session.created_at.isoformat(),
                    "updated_at": session.updated_at.isoformat(),
                    "is_active": session.is_active,
                },
                "messages": [
                    {
                        "id": msg.id,
                        "role": msg.role,
                        "content": msg.content,
                        "artifacts_created": msg.artifacts_created,
                        "timestamp": msg.timestamp.isoformat(),
                    }
                    for msg in messages
                ],
            }
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.put("/chat/sessions/{session_id}/title")
async def update_session_title(session_id: str, title: str = None):
    """Update the title of a chat session."""
    try:
        if not title:
            raise HTTPException(status_code=400, detail="Title is required")
        async with get_db_session() as session:
            chat_repo = get_chat_repository(session)
            success = await chat_repo.update_session_title(session_id, title)
            if not success:
                raise HTTPException(status_code=404, detail="Session not found")
            return {"message": "Title updated successfully"}
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.delete("/chat/sessions/{session_id}")
async def delete_chat_session(session_id: str):
    """Delete (soft delete) a chat session."""
    try:
        async with get_db_session() as session:
            chat_repo = get_chat_repository(session)
            success = await chat_repo.delete_session(session_id)
            if not success:
                raise HTTPException(status_code=404, detail="Session not found")
            return {"message": "Session deleted successfully"}
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/settings/api-key/status")
async def check_api_key_status():
    """Check if API key is configured."""
    try:
        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            has_key = await settings_repo.has_api_key()
            return {"has_api_key": has_key}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/settings/api-key")
async def set_api_key(request: dict):
    """Set the Google AI API key."""
    try:
        api_key = request.get("api_key")
        if not api_key:
            raise HTTPException(status_code=400, detail="API key is required")

        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            await settings_repo.set_api_key(api_key)
            # Refresh the API key in the agent service
            await agent_service.refresh_api_key()
            return {"message": "API key saved successfully"}
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.delete("/settings/api-key")
async def reset_api_key():
    """Reset/delete the Google AI API key."""
    try:
        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            success = await settings_repo.delete_setting("google_ai_api_key")
            if success:
                return {"message": "API key reset successfully"}
            else:
                raise HTTPException(status_code=404, detail="No API key found to reset")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# Tavily API Key Management
@app.post("/settings/tavily-api-key")
async def set_tavily_api_key(request: dict):
    """Set Tavily API key in database."""
    try:
        api_key = request.get("api_key")
        if not api_key:
            raise HTTPException(status_code=400, detail="API key is required")

        # Validate key format (should start with tvly-)
        if not api_key.startswith("tvly-"):
            raise HTTPException(
                status_code=400,
                detail="Invalid Tavily API key format. Key should start with 'tvly-'",
            )

        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            await settings_repo.set_setting("tavily_api_key", api_key)
            return {"message": "Tavily API key configured successfully"}
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/settings/tavily-api-key/status")
async def get_tavily_api_key_status():
    """Check if Tavily API key is configured."""
    try:
        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            db_key = await settings_repo.get_setting("tavily_api_key")

        env_key = os.getenv("TAVILY_API_KEY")

        return {
            "configured_in_database": db_key is not None,
            "configured_in_environment": env_key is not None,
            "active_source": ("environment" if env_key else ("database" if db_key else "none")),
            "has_api_key": bool(env_key or db_key),
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.delete("/settings/tavily-api-key")
async def delete_tavily_api_key():
    """Remove Tavily API key from database."""
    try:
        async with get_db_session() as session:
            settings_repo = get_settings_repository(session)
            success = await settings_repo.delete_setting("tavily_api_key")

            if success:
                return {"message": "Tavily API key removed successfully"}
            else:
                raise HTTPException(status_code=404, detail="No Tavily API key found to remove")
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/version")
async def get_version():
    """Get current application version."""
    try:
        from version import get_build_info

        return get_build_info()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)
