name: "Base PRP Template v2 - Context-Rich with Validation Loops (Python ADK + FastAPI Stack)"
description: |

## Purpose

Template optimized for AI agents to implement features in a Python application with Google Agent Development Kit (ADK), FastAPI backend, and sufficient context and self-validation capabilities to achieve working code through iterative refinement.

## Core Principles

1. **Context is King**: Include ALL necessary documentation, examples, and caveats for the stack
2. **Validation Loops**: Provide executable tests/lints the AI can run and fix
3. **Information Dense**: Use keywords and patterns from Python, FastAPI, Google ADK, and Pydantic
4. **Progressive Success**: Start simple, validate, then enhance
5. **Global rules**: Be sure to follow all rules in CLAUDE.md
6. **Consistency**: Maintain patterns and conventions across the application

---

## Goal

[What needs to be built - be specific about the end state and desires, including which parts of the stack are involved (ADK agents, FastAPI endpoints, data models, external integrations, etc.)]

## Why

- [Business value and user impact]
- [Integration with existing agents and workflows]
- [Problems this solves and for whom]
- [Performance and scalability implications]

## What

[User-visible behavior and technical requirements across agents, API endpoints, and data processing]

### Success Criteria

- [ ] [Specific measurable outcomes]
- [ ] [Agent functionality works as expected]
- [ ] [FastAPI integration functions correctly]
- [ ] [All tests pass]

## All Needed Context

### Documentation & References (list all context needed to implement the feature)

```yaml
# MUST READ - Include these in your context window
# Google ADK documentation
- url: https://cloud.google.com/agent-development-kit/docs
  why: Agent patterns, configuration, and deployment
- url: https://cloud.google.com/agent-development-kit/docs/quickstart
  why: Setup and initialization patterns

# FastAPI documentation
- url: https://fastapi.tiangolo.com/
  why: API patterns, dependency injection, and async operations
- url: https://fastapi.tiangolo.com/tutorial/
  why: Request/response patterns and middleware

# Pydantic documentation
- url: https://docs.pydantic.dev/
  why: Data validation and serialization patterns

# SQLAlchemy documentation
- url: https://docs.sqlalchemy.org/en/20/
  why: Async ORM patterns
- url: https://docs.sqlalchemy.org/en/20/orm/extensions/asyncio.html
  why: AsyncIO extension usage

# Testing documentation
- url: https://docs.pytest.org/
  why: Testing patterns and fixtures
- url: https://docs.pytest.org/en/stable/how-to/asyncio.html
  why: Async testing patterns

# Project-specific files (CRITICAL - Read these first!)
- file: CLAUDE.md
  why: Architecture overview, patterns, and development guidelines
- file: api/main.py
  why: FastAPI app initialization and endpoint definitions
- file: api/agent_service.py
  why: Core agent business logic and session management
- file: api/schemas.py
  why: Pydantic request/response models
- file: api/database.py
  why: Database connection management and session handling
- file: api/repositories.py
  why: Repository pattern for data access
- file: api/storage.py
  why: MinIO storage service for artifacts
- file: api/models/database.py
  why: SQLAlchemy ORM models
- file: the0/agent.py
  why: Google ADK agent definition and instructions
- file: the0/tools/
  why: Custom agent tools (save_artifact, web_browser, etc.)
- file: requirements.txt
  why: Python dependencies
- file: Dockerfile
  why: Container configuration
- file: Makefile
  why: Development commands and workflows
```

### Current Codebase tree (run `tree -I '__pycache__|.pytest_cache|.git|venv|env' -L 3` in the root)

```bash
# Include the current structure showing:
# - api/ directory with FastAPI application layer
# - the0/ directory with ADK agent and tools
# - tests/ directory with test modules
# - alembic/ directory with database migrations
# - Configuration files (Dockerfile, Makefile, requirements.txt)
```

### Desired Codebase tree with files to be added and responsibility of file

```bash
# Show the desired structure with:
# - New API endpoints in api/main.py
# - New agent tools in the0/tools/
# - New database models in api/models/database.py
# - New repository methods in api/repositories.py
# - New storage operations in api/storage.py
# - Updated database migrations in alembic/versions/
```

### Known Gotchas of our codebase & Library Quirks

```yaml
python:
  - "CRITICAL: Use async/await for ALL I/O operations"
  - "Use type hints everywhere (Python 3.11)"
  - "Follow Black formatting (88 character line length)"
  - "Use Pydantic BaseModel for all data structures"

fastapi:
  - "CRITICAL: Use async context manager pattern for database sessions"
  - "Always validate request/response with Pydantic models"
  - "Use StreamingResponse with SSE for real-time updates"
  - "Implement proper error handling with HTTPException"
  - "Follow existing CORS configuration patterns"
  - "Use dependency injection with Depends()"

google_adk:
  - "CRITICAL: Use ToolContext for session-aware tools"
  - "Initialize Runner with InMemoryArtifactService and DatabaseSessionService"
  - "Use Gemini 2.5 Flash model (gemini-2.5-flash)"
  - "Handle streaming responses with async for loops"
  - "Store agent instructions as detailed personality prompts"
  - "Use google.genai.types.Tool for tool definitions"

database:
  - "CRITICAL: Use asyncpg for PostgreSQL connections"
  - "Convert URLs between asyncpg and psycopg2 for ADK compatibility"
  - "Use SQLAlchemy async ORM with AsyncSession"
  - "Follow repository pattern for data access"
  - "Use Alembic for database migrations"
  - "Include func.now() for timestamp defaults"

storage:
  - "CRITICAL: Use dual storage strategy (MinIO + ADK in-memory)"
  - "Store artifacts in session-scoped paths: {session_id}/artifacts/{filename}"
  - "Use async executor pattern for blocking I/O"
  - "Detect MIME types from file extensions"
  - "Track versions in database metadata"

agent_tools:
  - "CRITICAL: Accept ToolContext as parameter in all custom tools"
  - "Extract session_id from tool_context._invocation_context.session.id"
  - "Return dict with success status and result data"
  - "Handle errors gracefully and return error messages"
  - "Use async def for all tool functions"

testing:
  - "CRITICAL: Use pytest with pytest-asyncio"
  - "Use testcontainers for integration tests"
  - "Follow pytest.ini configuration (asyncio_mode = auto)"
  - "Mock external services in unit tests"
  - "Test both success and error scenarios"

deployment:
  - "CRITICAL: Use environment variables for all configuration"
  - "Run Alembic migrations on startup (entrypoint.sh)"
  - "Include health checks (GET /health every 30s)"
  - "Run as non-root user (appuser)"
  - "Use Uvicorn with proper port configuration"
```

## Implementation Blueprint

### Data models and structure

Create the core data models and types following the project's patterns.

```python
# api/models/database.py
from datetime import datetime
from sqlalchemy import Column, Integer, String, Text, DateTime, Boolean, ForeignKey, ARRAY
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func

Base = declarative_base()

class FeatureSession(Base):
    """Example session model following ChatSession pattern."""
    __tablename__ = "feature_sessions"

    id = Column(String, primary_key=True)
    user_id = Column(String, nullable=False)
    title = Column(Text)
    is_active = Column(Boolean, default=True)
    created_at = Column(DateTime, server_default=func.now())
    updated_at = Column(DateTime, server_default=func.now(), onupdate=func.now())

    # Relationships
    messages = relationship("FeatureMessage", back_populates="session", cascade="all, delete-orphan")
    artifacts = relationship("FeatureArtifact", back_populates="session", cascade="all, delete-orphan")

class FeatureMessage(Base):
    """Example message model following ChatMessage pattern."""
    __tablename__ = "feature_messages"

    id = Column(Integer, primary_key=True, autoincrement=True)
    session_id = Column(String, ForeignKey("feature_sessions.id", ondelete="CASCADE"))
    role = Column(String, nullable=False)  # 'user' or 'assistant'
    content = Column(Text, nullable=False)
    artifacts_created = Column(ARRAY(String), default=list)
    timestamp = Column(DateTime, server_default=func.now())

    # Relationships
    session = relationship("FeatureSession", back_populates="messages")

class FeatureArtifact(Base):
    """Example artifact model following Artifact pattern."""
    __tablename__ = "feature_artifacts"

    id = Column(Integer, primary_key=True, autoincrement=True)
    session_id = Column(String, ForeignKey("feature_sessions.id", ondelete="CASCADE"))
    filename = Column(String, nullable=False)
    file_path = Column(Text, nullable=False)  # MinIO object key
    mime_type = Column(String)
    version = Column(Integer, default=1)
    created_at = Column(DateTime, server_default=func.now())
    updated_at = Column(DateTime, server_default=func.now(), onupdate=func.now())

    # Relationships
    session = relationship("FeatureSession", back_populates="artifacts")
```

```python
# api/schemas.py
from typing import Optional, List, Literal
from pydantic import BaseModel, Field

class FeatureRequest(BaseModel):
    """Request model following ChatRequest pattern."""
    message: str
    session_id: Optional[str] = None
    metadata: Optional[dict] = None

class FeatureResponse(BaseModel):
    """Response model following ChatResponse pattern."""
    response: str
    session_id: str
    artifacts: Optional[List[str]] = None

class StreamChunk(BaseModel):
    """Streaming response model for SSE."""
    type: Literal["content", "artifacts", "complete", "error"]
    content: Optional[str] = None
    artifacts: Optional[List[str]] = None
    session_id: Optional[str] = None
    error: Optional[str] = None
```

### List of tasks to be completed to fulfill the PRP in the order they should be completed

```yaml
Task 1: Update Database Models and Schema
UPDATE api/models/database.py:
  - ADD new SQLAlchemy models
  - INCLUDE relationships with cascade deletes
  - USE func.now() for timestamps
  - FOLLOW existing patterns (ChatSession, ChatMessage, Artifact)

CREATE alembic/versions/{timestamp}_add_{feature}.py:
  - RUN: alembic revision --autogenerate -m "add {feature}"
  - VERIFY migration script
  - TEST with: alembic upgrade head
  - PRESERVE existing data integrity

Task 2: Create Repository Methods
UPDATE api/repositories.py:
  - ADD new repository class or extend existing
  - IMPLEMENT async CRUD operations
  - USE AsyncSession throughout
  - FOLLOW existing repository patterns
  - ADD factory functions for dependency injection

Task 3: Update API Endpoints
UPDATE api/main.py:
  - ADD new route handlers
  - USE Pydantic models from api/schemas.py
  - IMPLEMENT error handling with HTTPException
  - USE Depends() for dependency injection
  - FOLLOW existing endpoint patterns (chat, sessions, artifacts)

UPDATE api/schemas.py:
  - ADD request/response Pydantic models
  - USE Optional[] for nullable fields
  - FOLLOW existing patterns (ChatRequest, ChatResponse, StreamChunk)

Task 4: Update Agent Service (if needed)
UPDATE api/agent_service.py:
  - ADD new methods to AgentService class
  - USE async/await throughout
  - HANDLE session management (ADK + database dual storage)
  - IMPLEMENT streaming with async generators
  - FOLLOW lazy initialization pattern

Task 5: Create Agent Tools (if needed)
CREATE the0/tools/{tool_name}.py:
  - ACCEPT ToolContext parameter
  - EXTRACT session_id from tool_context._invocation_context.session.id
  - USE async def for tool function
  - RETURN dict with success/result
  - EXPORT as google.genai.types.Tool

UPDATE the0/agent.py:
  - IMPORT new tool
  - ADD to tools list in Agent definition
  - UPDATE instruction if needed

Task 6: Update Storage (if file handling needed)
UPDATE api/storage.py:
  - ADD new storage methods to StorageService
  - USE executor pattern for blocking I/O
  - STORE in session-scoped paths
  - DETECT MIME types
  - FOLLOW dual storage strategy (MinIO + database metadata)

Task 7: Create Tests
CREATE tests/api/test_{feature}.py:
  - IMPLEMENT async tests with pytest-asyncio
  - USE fixtures from conftest.py
  - MOCK external services
  - TEST success and error scenarios
  - FOLLOW existing test patterns

CREATE tests/the0/tools/test_{tool}.py (if tool added):
  - TEST tool functionality
  - MOCK ToolContext
  - TEST session awareness
  - VERIFY error handling

Task 8: Run Validation
RUN validation commands:
  - make format (Black formatting)
  - make lint (Flake8 linting)
  - make test (Pytest suite)
  - make dev (Test locally with auto-reload)
```

### Per task pseudocode as needed added to each task

```python
# Task 3 - API Endpoint Implementation
# api/main.py (add new endpoints here)

from fastapi import APIRouter, Depends, HTTPException, status
from fastapi.responses import StreamingResponse
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List

from api.database import get_db_session
from api.repositories import get_feature_repository, FeatureRepository
from api.schemas import FeatureRequest, FeatureResponse, StreamChunk
from api.agent_service import agent_service

# PATTERN: Add new endpoints to existing router or create new router
@app.post("/features", response_model=FeatureResponse)
async def create_feature(
    request: FeatureRequest,
    repository: FeatureRepository = Depends(get_feature_repository)
):
    """Create a new feature."""
    try:
        # PATTERN: Use repository for data access
        feature = await repository.create_feature(
            message=request.message,
            session_id=request.session_id
        )
        return FeatureResponse(
            response="Feature created",
            session_id=feature.session_id,
            artifacts=[]
        )
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Failed to create feature: {str(e)}"
        )

@app.post("/features/stream")
async def feature_stream(request: FeatureRequest):
    """Stream feature processing with Server-Sent Events."""
    async def generate_stream():
        try:
            # PATTERN: Use agent_service for streaming responses
            async for chunk in agent_service.process_feature(request.message, request.session_id):
                # PATTERN: Yield SSE formatted data
                yield f"data: {chunk.model_dump_json()}\n\n"
        except Exception as e:
            error_chunk = StreamChunk(
                type="error",
                error=str(e),
                session_id=request.session_id
            )
            yield f"data: {error_chunk.model_dump_json()}\n\n"

    # CRITICAL: Return StreamingResponse with proper media type
    return StreamingResponse(
        generate_stream(),
        media_type="text/event-stream"
    )

@app.get("/features/sessions/{session_id}")
async def get_feature_session(
    session_id: str,
    repository: FeatureRepository = Depends(get_feature_repository)
):
    """Get feature session with messages."""
    try:
        session = await repository.get_session_with_messages(session_id)
        if not session:
            raise HTTPException(status_code=404, detail="Session not found")
        return session
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Failed to retrieve session: {str(e)}"
        )
```

```python
# Task 4 - ADK Agent Tool Implementation
# the0/tools/feature_tool.py

from typing import Optional
from google.genai.types import Tool, ToolContext
from api.storage import storage_service
from api.database import db_manager
from api.repositories import ChatRepository

async def process_feature(
    input_text: str,
    metadata: Optional[str] = None,
    tool_context: ToolContext = None
) -> dict:
    """
    Process a feature request and return analysis.

    Args:
        input_text: Feature description or request
        metadata: Optional metadata as JSON string
        tool_context: ADK tool context (provides session information)

    Returns:
        dict with success status and result data
    """
    try:
        # CRITICAL: Extract session ID from tool context
        session_id = tool_context._invocation_context.session.id

        # PATTERN: Use async context manager for database session
        async with db_manager.get_session() as db_session:
            repository = ChatRepository(db_session)

            # PATTERN: Perform business logic
            # Example: Save metadata or process feature
            result = {
                "status": "processed",
                "session_id": session_id,
                "input": input_text[:100],  # Truncate for display
                "message": "Feature processed successfully"
            }

            # Optional: Store result as artifact
            if metadata:
                artifact_content = f"# Feature Analysis\n\n{input_text}\n\nMetadata: {metadata}"
                await storage_service.save_object(
                    bucket_name="ai-artifacts",
                    object_name=f"{session_id}/artifacts/feature_analysis.md",
                    data=artifact_content.encode()
                )

                # Track in database
                await repository.create_artifact(
                    session_id=session_id,
                    filename="feature_analysis.md",
                    file_path=f"{session_id}/artifacts/feature_analysis.md",
                    mime_type="text/markdown"
                )

            return {
                "success": True,
                "result": result
            }

    except Exception as e:
        # PATTERN: Return error in structured format
        return {
            "success": False,
            "error": f"Feature processing failed: {str(e)}"
        }

# CRITICAL: Export as ADK Tool
process_feature_tool = Tool(func_declarations=[process_feature])
```

```python
# UPDATE the0/agent.py to include new tool

from the0.tools.save_artifact import save_artifact
from the0.tools.web_browser import search_web, browse_url
from the0.tools.documentation import list_documentation, get_documentation
from the0.tools.deploy_bot import deploy_bot
from the0.tools.feature_tool import process_feature  # NEW IMPORT

# ... existing code ...

root_agent = Agent(
    name="the0",
    model="gemini-2.5-flash",
    description="Agent that helps build and deploy automated trading bots",
    instruction="""
    [Existing instruction...]

    # NEW TOOL: Feature Processing
    When the user requests feature analysis or processing, use the process_feature tool.
    """,
    tools=[
        search_web,
        browse_url,
        list_documentation,
        get_documentation,
        save_artifact,
        deploy_bot,
        process_feature,  # NEW TOOL
    ],
)
```

## Validation Loop

### Level 1: Syntax & Style

```bash
# Code formatting (MUST RUN FIRST)
make format
# OR: black api/ the0/ tests/

# Linting
make lint
# OR: flake8 api/ the0/ tests/

# Python syntax validation
python -m py_compile api/
python -m py_compile the0/

# Type checking (optional, if mypy is configured)
mypy api/ the0/
```

### Level 2: Unit Tests

```bash
# Run all tests
make test
# OR: pytest

# Run tests with verbose output
pytest -v

# Run specific test files
pytest tests/api/test_main.py
pytest tests/the0/tools/test_save_artifact.py

# Run tests with coverage
pytest --cov=api --cov=the0 --cov-report=html

# Run async tests specifically
pytest tests/api/ -k "async"
```

### Level 3: Integration Tests

```bash
# Start development server
make dev
# OR: uvicorn api.main:app --reload --host 0.0.0.0 --port 8000

# Test manually with curl
curl http://localhost:8000/health
curl -X POST http://localhost:8000/chat -H "Content-Type: application/json" -d '{"message": "test"}'

# Run integration tests with testcontainers
pytest tests/api/ -v

# Test streaming endpoint
curl -N http://localhost:8000/chat/stream -H "Content-Type: application/json" -d '{"message": "test"}'
```

### Level 4: Build & Deployment Validation

```bash
# Build Docker image
docker build -t the0-ai:latest .

# Run container locally with environment variables
docker run -p 8000:8000 --env-file .env the0-ai:latest

# Test container health
curl http://localhost:8000/health

# Check logs
docker logs <container_id>

# Validate database migrations
alembic current
alembic upgrade head
alembic history

# Clean up build artifacts
make clean
```

## Final validation Checklist

- [ ] Code formatting applied: `make format`
- [ ] Linting passes: `make lint`
- [ ] Python syntax is valid: `python -m py_compile api/ the0/`
- [ ] All tests pass: `make test`
- [ ] Database migrations created and applied: `alembic upgrade head`
- [ ] Docker build succeeds: `docker build -t the0-ai .`
- [ ] Development server runs: `make dev`
- [ ] Health endpoint responds: `curl http://localhost:8000/health`
- [ ] API endpoints return expected responses
- [ ] Agent tools work correctly with ToolContext
- [ ] Streaming responses work (SSE format)
- [ ] Storage operations succeed (MinIO + database)
- [ ] Repository methods function correctly
- [ ] CLAUDE.md patterns followed

---

## Anti-Patterns to Avoid

- ❌ Don't use synchronous I/O - always use async/await
- ❌ Don't hardcode secrets - use environment variables
- ❌ Don't skip Pydantic validation on API endpoints
- ❌ Don't forget ToolContext parameter in agent tools
- ❌ Don't ignore dual storage (MinIO + database metadata)
- ❌ Don't skip database migrations with Alembic
- ❌ Don't use blocking operations in async handlers
- ❌ Don't forget to convert database URLs (asyncpg ↔ psycopg2) for ADK
- ❌ Don't ignore the repository pattern - use it for data access
- ❌ Don't forget Server-Sent Events format for streaming
- ❌ Don't skip session management (ADK + database dual storage)
- ❌ Don't ignore existing patterns in CLAUDE.md

## PR Message

When done write a PR message summarizing the feature, the changes made, the validation steps taken, and any known issues or follow-up tasks.

```markdown
## Feature: [Feature Name] (Python ADK + FastAPI)

[Brief description of the feature and its purpose]

## Background

[Context on why this feature is needed and how it fits into the application]

## Changes Made

### API Endpoints

[List of new/modified FastAPI endpoints]

### ADK Agents

[List of new/modified Google ADK agents]

### Database Schema

[List of new/modified database tables and migrations]

### Services

[List of new/modified service layer components]

### Configuration

[List of configuration changes]

## Validation Steps

### Code Quality & Safety

- [ ] Python syntax validation passed
- [ ] Type checking with mypy passed
- [ ] Code formatting with black applied
- [ ] Linting with flake8 passed
- [ ] Security scan with bandit passed

### Testing

- [ ] Unit tests passing
- [ ] Integration tests passing
- [ ] Agent functionality tested
- [ ] Manual testing completed

### Build & Deployment

- [ ] Docker build successful
- [ ] Local container testing passed
- [ ] Performance benchmarks met

## Known Issues

[List any known issues or limitations]

## Follow-up Tasks

[List any tasks that need to be completed in subsequent PRs]

## Agent Performance

[Include metrics on agent response times and accuracy]
```

Save as: `PR_MSG/{feature-name}-python-adk-fastapi.md`
