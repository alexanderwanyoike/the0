from pydantic import BaseModel
from typing import List, Optional, Dict, Any, Literal


class ChatRequest(BaseModel):
    message: str
    session_id: Optional[str] = None


class ChatResponse(BaseModel):
    response: str
    session_id: str
    artifacts: Optional[List[str]] = None


class ArtifactResponse(BaseModel):
    filename: str
    content: str
    version: int
    

class HealthResponse(BaseModel):
    status: str
    message: str


class ApiKeyRequest(BaseModel):
    api_key: str


class StreamChunk(BaseModel):
    type: Literal['content', 'artifacts', 'complete', 'error']
    content: Optional[str] = None
    artifacts: Optional[List[str]] = None
    session_id: Optional[str] = None
    error: Optional[str] = None