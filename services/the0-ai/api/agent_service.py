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
from the0.agent import root_agent
from api.models import ChatResponse, ArtifactResponse, StreamChunk
from api.database import get_database_url, init_database
from api.repositories import chat_repository, settings_repository

# Load environment variables from .env file
load_dotenv()


class AgentService:
    def __init__(self):
        # Initialize database on startup
        init_database()
        
        # Set up Google AI API key from database or environment
        self._setup_api_key()
        
        self.artifact_service = InMemoryArtifactService()
        
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
            agent=root_agent,
            artifact_service=self.artifact_service,
            session_service=self.session_service
        )
        self.sessions: Dict[str, str] = {}
    
    def _setup_api_key(self):
        """Set up Google AI API key from database or environment."""
        try:
            # Try to get API key from database first
            api_key = settings_repository.get_api_key()
            if api_key:
                os.environ['GOOGLE_API_KEY'] = api_key
                print("Using Google AI API key from database")
            elif os.getenv('GOOGLE_API_KEY'):
                print("Using Google AI API key from environment")
            else:
                print("Warning: No Google AI API key found. Please configure one via the settings.")
        except Exception as e:
            print(f"Error setting up API key: {e}")
    
    def refresh_api_key(self):
        """Refresh the API key from database (call this when API key is updated)."""
        self._setup_api_key()
    
    async def chat(self, message: str, session_id: Optional[str] = None) -> ChatResponse:
        """Send a message to the agent and get a response."""
        if session_id is None:
            session_id = str(uuid.uuid4())
        
        try:
            # Get or create session in ADK
            if session_id not in self.sessions:
                try:
                    await self.session_service.create_session(
                        session_id=session_id,
                        app_name="the0-api",
                        user_id="default-user"
                    )
                    self.sessions[session_id] = session_id
                except Exception as e:
                    print(f"Error creating ADK session: {e}")
                    # Continue without ADK session if it fails
                    self.sessions[session_id] = session_id
                
                # Create session in our database
                try:
                    chat_repository.create_session(session_id, "default-user")
                except Exception as e:
                    print(f"Error creating database session: {e}")
                    # Continue without database session if it fails
            
            # Store user message in database
            try:
                chat_repository.add_message(session_id, "user", message)
            except Exception as e:
                print(f"Error storing user message: {e}")
            
            # Create content object
            content = types.Content(
                role='user',
                parts=[types.Part.from_text(text=message)]
            )
            
            # Send message to agent
            events = self.runner.run_async(
                user_id="default-user",
                session_id=session_id,
                new_message=content
            )
            
            # Process events to get final response
            response = ""
            event_count = 0
            
            async for event in events:
                event_count += 1
                print(f"Processing event {event_count}: {type(event)}")
                
                if event.is_final_response():
                    print(f"Found final response event: {event}")
                    
                    # Check if event has content and content has parts
                    if hasattr(event, 'content') and event.content is not None:
                        print(f"Event has content: {type(event.content)}")
                        
                        if hasattr(event.content, 'parts') and event.content.parts is not None:
                            print(f"Content has {len(event.content.parts)} parts")
                            # Extract all text parts from the response
                            text_parts = []
                            for i, part in enumerate(event.content.parts):
                                print(f"Part {i}: {type(part)}, has text: {hasattr(part, 'text')}")
                                if hasattr(part, 'text') and part.text:
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
                print(f"No response generated after processing {event_count} events")
                response = "Agent completed the task but provided no response."
            
            print(f"Final response: {response[:200]}...")
            
            # Get list of current artifacts
            artifact_names = await self.list_artifact_keys()
            
            # Store assistant response in database
            try:
                chat_repository.add_message(
                    session_id, 
                    "assistant", 
                    response, 
                    artifacts_created=artifact_names if artifact_names else None
                )
            except Exception as e:
                print(f"Error storing assistant response: {e}")
            
            return ChatResponse(
                response=response,
                session_id=session_id,
                artifacts=artifact_names
            )
            
        except Exception as e:
            error_message = f"Error: {str(e)}"
            # Still try to store the error in the database
            try:
                chat_repository.add_message(session_id, "assistant", error_message)
            except:
                pass  # Don't fail if we can't store the error
                
            return ChatResponse(
                response=error_message,
                session_id=session_id,
                artifacts=[]
            )

    async def chat_stream(self, message: str, session_id: Optional[str] = None):
        """Send a message to the agent and stream the response."""
        if session_id is None:
            session_id = str(uuid.uuid4())
        
        try:
            # Get or create session in ADK
            if session_id not in self.sessions:
                try:
                    await self.session_service.create_session(
                        session_id=session_id,
                        app_name="the0-api",
                        user_id="default-user"
                    )
                    self.sessions[session_id] = session_id
                except Exception as e:
                    print(f"Error creating ADK session: {e}")
                    self.sessions[session_id] = session_id
                
                # Create session in our database
                try:
                    chat_repository.create_session(session_id, "default-user")
                except Exception as e:
                    print(f"Error creating database session: {e}")
            
            # Store user message in database
            try:
                chat_repository.add_message(session_id, "user", message)
            except Exception as e:
                print(f"Error storing user message: {e}")
            
            # Create content object
            content = types.Content(
                role='user',
                parts=[types.Part.from_text(text=message)]
            )
            
            # Send message to agent
            events = self.runner.run_async(
                user_id="default-user",
                session_id=session_id,
                new_message=content
            )
            
            # Process events and stream response
            full_response = ""
            event_count = 0
            
            async for event in events:
                event_count += 1
                print(f"Processing event {event_count}: {type(event)}")
                
                # Check for content chunks to stream
                if hasattr(event, 'content') and event.content is not None:
                    if hasattr(event.content, 'parts') and event.content.parts is not None:
                        for part in event.content.parts:
                            if hasattr(part, 'text') and part.text:
                                # Break large text into smaller streaming chunks
                                chunk_text = part.text
                                full_response += chunk_text
                                
                                # Stream in smaller pieces for better UX
                                words = chunk_text.split(' ')
                                current_chunk = ''
                                
                                for word in words:
                                    current_chunk += word + ' '
                                    # Send chunk every 3-5 words or when it gets long enough
                                    if len(current_chunk.split()) >= 3 or len(current_chunk) > 30:
                                        yield StreamChunk(
                                            type='content',
                                            content=current_chunk
                                        )
                                        current_chunk = ''
                                
                                # Send any remaining chunk
                                if current_chunk.strip():
                                    yield StreamChunk(
                                        type='content',
                                        content=current_chunk
                                    )
                
                # Check if this is the final response
                if event.is_final_response():
                    print(f"Found final response event: {event}")
                    
                    # Send artifacts if any
                    artifact_names = await self.list_artifact_keys()
                    if artifact_names:
                        yield StreamChunk(
                            type='artifacts',
                            artifacts=artifact_names
                        )
                    
                    # Send completion signal
                    yield StreamChunk(
                        type='complete',
                        session_id=session_id
                    )
                    break
            
            # Fallback if no response was generated
            if not full_response:
                print(f"No response generated after processing {event_count} events")
                full_response = "Agent completed the task but provided no response."
                yield StreamChunk(
                    type='content',
                    content=full_response
                )
                yield StreamChunk(
                    type='complete',
                    session_id=session_id
                )
            
            print(f"Final response: {full_response[:200]}...")
            
            # Store assistant response in database
            try:
                artifact_names = await self.list_artifact_keys()
                chat_repository.add_message(
                    session_id, 
                    "assistant", 
                    full_response, 
                    artifacts_created=artifact_names if artifact_names else None
                )
            except Exception as e:
                print(f"Error storing assistant response: {e}")
            
        except Exception as e:
            error_message = f"Error: {str(e)}"
            print(f"Error in chat_stream: {error_message}")
            
            # Send error chunk
            yield StreamChunk(
                type='error',
                error=error_message,
                session_id=session_id
            )
            
            # Still try to store the error in the database
            try:
                chat_repository.add_message(session_id, "assistant", error_message)
            except:
                pass  # Don't fail if we can't store the error
    
    async def list_artifact_keys(self) -> List[str]:
        """List all artifact keys."""
        try:
            # Access the internal artifacts storage to find all saved artifacts
            if hasattr(self.artifact_service, 'artifacts'):
                storage = self.artifact_service.artifacts
                print(f"Artifact storage has {len(storage)} keys: {list(storage.keys())}")
                
                all_filenames = []
                
                # Extract filenames from the storage keys (format: app_name/user_id/session_id/filename)
                for key in storage.keys():
                    print(f"Processing key: {key}")
                    parts = key.split('/')
                    if len(parts) >= 4:
                        filename = '/'.join(parts[3:])  # Everything after session_id is the filename
                        all_filenames.append(filename)
                        print(f"Added filename: {filename}")
                
                # Remove duplicates and return
                unique_filenames = list(set(all_filenames))
                print(f"Returning {len(unique_filenames)} unique artifacts: {unique_filenames}")
                return unique_filenames
            else:
                print("Artifact service has no 'artifacts' attribute")
            
            return []
            
        except Exception as e:
            print(f"Error listing artifacts: {e}")
            return []
    
    async def list_session_artifact_keys(self, session_id: str) -> List[str]:
        """List artifact keys for a specific session from persistent storage."""
        try:
            # Get artifacts from database
            artifacts = chat_repository.get_session_artifacts(session_id)
            filenames = [artifact.filename for artifact in artifacts]
            
            print(f"Found {len(filenames)} persistent artifacts for session {session_id}: {filenames}")
            return filenames
            
        except Exception as e:
            print(f"Error listing session artifacts: {e}")
            return []
    
    async def get_artifact(self, filename: str, session_id: Optional[str] = None) -> Optional[ArtifactResponse]:
        """Get a specific artifact by filename from persistent storage."""
        try:
            if not session_id:
                # If no session_id provided, try to find the artifact in any session
                # This is for backward compatibility with existing API calls
                print(f"No session_id provided for artifact {filename}, searching all sessions")
                return None
            
            # Get artifact metadata from database
            artifact = chat_repository.get_artifact(session_id, filename)
            if not artifact:
                print(f"Artifact {filename} not found in database for session {session_id}")
                return None
            
            # Read content from file system
            file_path = Path(artifact.file_path)
            if not file_path.exists():
                print(f"Artifact file {file_path} does not exist on disk")
                return None
            
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            return ArtifactResponse(
                filename=artifact.filename,
                content=content,
                version=artifact.version
            )
            
        except Exception as e:
            print(f"Error getting artifact {filename}: {e}")
            return None
    
    async def create_artifacts_zip(self) -> Optional[str]:
        """Create a ZIP file containing all artifacts."""
        try:
            # Get all artifact filenames
            artifact_names = await self.list_artifact_keys()
            if not artifact_names:
                return None
            
            # Create temporary ZIP file
            temp_dir = tempfile.mkdtemp()
            zip_path = os.path.join(temp_dir, 'trading-bot.zip')
            
            with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
                for filename in artifact_names:
                    try:
                        artifact = await self.get_artifact(filename)
                        if artifact and artifact.content:
                            zipf.writestr(filename, artifact.content)
                    except Exception as e:
                        print(f"Error adding {filename} to ZIP: {e}")
                        continue
            
            return zip_path
            
        except Exception as e:
            print(f"Error creating artifacts ZIP: {e}")
            return None