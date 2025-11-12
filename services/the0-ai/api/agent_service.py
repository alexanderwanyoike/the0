import uuid
import zipfile
import tempfile
import os
from pathlib import Path
from typing import Dict, List, Optional
from dotenv import load_dotenv
from google.adk.sessions import DatabaseSessionService, InMemorySessionService
from google.adk.artifacts import InMemoryArtifactService
from google.adk.runners import Runner
from google.genai import types
from the0.agent import supervisor_agent
from api.schemas import ChatResponse, ArtifactResponse, StreamChunk
from api.database import get_database_url, init_database, get_db_session
from api.repositories import get_chat_repository, get_settings_repository
from api.storage import storage_service

# Load environment variables from .env file
load_dotenv()


class AgentService:
    def __init__(self):
        self.artifact_service = InMemoryArtifactService()
        self.session_service = None
        self.runner = None
        self.sessions: Dict[str, str] = {}
        self._initialized = False

    async def initialize(self):
        """Initialize the service asynchronously."""
        if self._initialized:
            return

        # Initialize database on startup
        await init_database()

        # Set up Google AI API key from database or environment
        await self._setup_api_key()

        # Try DatabaseSessionService, fallback to InMemorySessionService
        try:
            self.session_service = DatabaseSessionService(db_url=get_database_url())
            print("Using DatabaseSessionService")
        except Exception as e:
            print(f"DatabaseSessionService failed: {e}")
            print("Falling back to InMemorySessionService")
            self.session_service = InMemorySessionService()

        self.runner = Runner(
            app_name="the0-api",
            agent=supervisor_agent,
            artifact_service=self.artifact_service,
            session_service=self.session_service,
        )

        self._initialized = True

    async def _setup_api_key(self):
        """Set up Google AI API key from database or environment."""
        try:
            # Try to get API key from database first
            async with get_db_session() as session:
                settings_repo = get_settings_repository(session)
                api_key = await settings_repo.get_api_key()
                if api_key:
                    os.environ["GOOGLE_API_KEY"] = api_key
                    print("Using Google AI API key from database")
                elif os.getenv("GOOGLE_API_KEY"):
                    print("Using Google AI API key from environment")
                else:
                    print("Warning: No Google AI API key found. Please configure one via the settings.")
        except Exception as e:
            print(f"Error setting up API key: {e}")

    async def refresh_api_key(self):
        """Refresh the API key from database (call this when API key is updated)."""
        await self._setup_api_key()

    async def chat(self, message: str, session_id: Optional[str] = None) -> ChatResponse:
        """Send a message to the agent and get a response."""
        await self.initialize()

        if session_id is None:
            session_id = str(uuid.uuid4())

        try:
            # Get or create session in ADK
            if session_id not in self.sessions:
                try:
                    await self.session_service.create_session(
                        session_id=session_id,
                        app_name="the0-api",
                        user_id="default-user",
                    )
                    self.sessions[session_id] = session_id
                except Exception as e:
                    print(f"Error creating ADK session: {e}")
                    # Continue without ADK session if it fails
                    self.sessions[session_id] = session_id

                # Create session in our database
                try:
                    async with get_db_session() as db_session:
                        chat_repo = get_chat_repository(db_session)
                        await chat_repo.create_session(session_id, "default-user")
                except Exception as e:
                    print(f"Error creating database session: {e}")
                    # Continue without database session if it fails

            # Store user message in database
            try:
                async with get_db_session() as db_session:
                    chat_repo = get_chat_repository(db_session)
                    await chat_repo.add_message(session_id, "user", message)
            except Exception as e:
                print(f"Error storing user message: {e}")

            # Create content object
            content = types.Content(role="user", parts=[types.Part.from_text(text=message)])

            # Send message to agent
            events = self.runner.run_async(user_id="default-user", session_id=session_id, new_message=content)

            # Process events to get final response
            response = ""
            event_count = 0

            async for event in events:
                event_count += 1
                print(f"Processing event {event_count}: {type(event)}")

                if event.is_final_response():
                    print(f"Found final response event: {event}")

                    # Check if event has content and content has parts
                    if hasattr(event, "content") and event.content is not None:
                        print(f"Event has content: {type(event.content)}")

                        if hasattr(event.content, "parts") and event.content.parts is not None:
                            print(f"Content has {len(event.content.parts)} parts")
                            # Extract all text parts from the response
                            text_parts = []
                            for i, part in enumerate(event.content.parts):
                                print(f"Part {i}: {type(part)}, has text: {hasattr(part, 'text')}")
                                if hasattr(part, "text") and part.text:
                                    text_parts.append(part.text)
                                    print(f"Added text: {part.text[:100]}...")
                            response = " ".join(text_parts) if text_parts else "Agent completed the task."
                        else:
                            print(f"Warning: Event content has no parts or parts is None. Content: {event.content}")
                            response = "Agent completed the task."
                    else:
                        print(f"Warning: Event has no content or content is None. Event: {event}")
                        response = "Agent completed the task."
                    break

            # Fallback if no response was generated
            if not response:
                response = "I'm here to help you build trading bots. What would you like to create?"
                print("Warning: No response generated, using fallback")

            # Store assistant message in database
            try:
                artifacts_created = await self.list_session_artifact_keys(session_id)
                async with get_db_session() as db_session:
                    chat_repo = get_chat_repository(db_session)
                    await chat_repo.add_message(
                        session_id,
                        "assistant",
                        response,
                        artifacts_created=(artifacts_created if artifacts_created else None),
                    )
            except Exception as e:
                print(f"Error storing assistant message: {e}")

            return ChatResponse(
                response=response,
                session_id=session_id,
                artifacts=await self.list_session_artifact_keys(session_id),
            )

        except Exception as e:
            print(f"Error in chat: {e}")
            return ChatResponse(
                response=f"I encountered an error: {str(e)}",
                session_id=session_id,
                artifacts=[],
            )

    async def chat_stream(self, message: str, session_id: Optional[str] = None):
        """Send a message to the agent and stream the response."""
        await self.initialize()

        if session_id is None:
            session_id = str(uuid.uuid4())

        try:
            # Initialize session same as chat method
            if session_id not in self.sessions:
                try:
                    await self.session_service.create_session(
                        session_id=session_id,
                        app_name="the0-api",
                        user_id="default-user",
                    )
                    self.sessions[session_id] = session_id
                except Exception as e:
                    print(f"Error creating ADK session: {e}")
                    self.sessions[session_id] = session_id

                try:
                    async with get_db_session() as db_session:
                        chat_repo = get_chat_repository(db_session)
                        await chat_repo.create_session(session_id, "default-user")
                except Exception as e:
                    print(f"Error creating database session: {e}")

            # Store user message in database
            try:
                async with get_db_session() as db_session:
                    chat_repo = get_chat_repository(db_session)
                    await chat_repo.add_message(session_id, "user", message)
            except Exception as e:
                print(f"Error storing user message: {e}")

            # Create content object
            content = types.Content(role="user", parts=[types.Part.from_text(text=message)])

            # Send message to agent and stream events
            events = self.runner.run_async(user_id="default-user", session_id=session_id, new_message=content)

            accumulated_response = ""

            async for event in events:
                print(f"Streaming event: {type(event)}")

                if hasattr(event, "content") and event.content is not None:
                    if hasattr(event.content, "parts") and event.content.parts:
                        for part in event.content.parts:
                            if hasattr(part, "text") and part.text:
                                accumulated_response += part.text
                                yield StreamChunk(
                                    type="content",
                                    content=part.text,
                                    session_id=session_id,
                                )

                if event.is_final_response():
                    # Get artifacts that were created
                    artifacts = await self.list_session_artifact_keys(session_id)
                    if artifacts:
                        yield StreamChunk(type="artifacts", artifacts=artifacts, session_id=session_id)

                    # Store assistant message in database
                    try:
                        async with get_db_session() as db_session:
                            chat_repo = get_chat_repository(db_session)
                            await chat_repo.add_message(
                                session_id,
                                "assistant",
                                accumulated_response,
                                artifacts_created=artifacts if artifacts else None,
                            )
                    except Exception as e:
                        print(f"Error storing assistant message: {e}")

                    yield StreamChunk(type="complete", session_id=session_id)
                    break

        except Exception as e:
            print(f"Error in chat stream: {e}")
            yield StreamChunk(type="error", error=str(e), session_id=session_id)

    async def list_artifact_keys(self) -> List[str]:
        """List all available artifacts (for backwards compatibility)."""
        # This would need to aggregate across all sessions or use a different approach
        return []

    async def list_session_artifact_keys(self, session_id: str) -> List[str]:
        """List artifacts for a specific session."""
        try:
            # Get artifacts from storage service
            return await storage_service.list_artifacts(session_id)
        except Exception as e:
            print(f"Error listing artifacts: {e}")
            return []

    async def get_artifact(self, filename: str, session_id: Optional[str] = None) -> Optional[ArtifactResponse]:
        """Get a specific artifact by filename."""
        try:
            if not session_id:
                # Try to find the artifact in any session from database
                # This is less efficient but maintains backwards compatibility
                return None

            # Get artifact content from storage
            content = await storage_service.get_artifact(session_id, filename)
            if content is None:
                return None

            # Get artifact metadata from database
            async with get_db_session() as db_session:
                chat_repo = get_chat_repository(db_session)
                artifact_meta = await chat_repo.get_artifact(session_id, filename)

            return ArtifactResponse(
                filename=filename,
                content=content,
                version=artifact_meta.version if artifact_meta else 1,
            )

        except Exception as e:
            print(f"Error getting artifact: {e}")
            return None

    async def create_artifacts_zip(self) -> Optional[str]:
        """Create a ZIP file with all artifacts."""
        try:
            temp_zip = tempfile.NamedTemporaryFile(delete=False, suffix=".zip")
            temp_zip.close()

            with zipfile.ZipFile(temp_zip.name, "w") as zip_file:
                # This would need to be updated to work with the new storage system
                # For now, return empty zip
                pass

            return temp_zip.name

        except Exception as e:
            print(f"Error creating artifacts zip: {e}")
            return None


# Global service instance
agent_service = AgentService()
