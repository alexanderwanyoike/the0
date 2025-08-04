import json
import sqlite3
from datetime import datetime
from typing import List, Optional, Dict, Any
from dataclasses import dataclass
from api.database import db_manager

@dataclass
class ChatSession:
    """Represents a chat session."""
    id: str
    user_id: str
    title: Optional[str]
    created_at: datetime
    updated_at: datetime
    is_active: bool

@dataclass
class ChatMessage:
    """Represents a chat message."""
    id: Optional[int]
    session_id: str
    role: str  # 'user', 'assistant', 'system'
    content: str
    artifacts_created: Optional[List[str]]
    timestamp: datetime

@dataclass
class Artifact:
    """Represents an artifact file."""
    id: Optional[int]
    session_id: str
    filename: str
    file_path: str
    mime_type: Optional[str]
    version: int
    created_at: datetime
    updated_at: datetime

class ChatHistoryRepository:
    """Repository for managing chat sessions and messages."""
    
    def create_session(self, session_id: str, user_id: str = "default-user", title: Optional[str] = None) -> ChatSession:
        """Create a new chat session."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            conn.execute(
                """
                INSERT INTO chat_sessions (id, user_id, title)
                VALUES (?, ?, ?)
                """,
                (session_id, user_id, title)
            )
            conn.commit()
            
            # Fetch the created session
            cursor = conn.execute(
                "SELECT id, user_id, title, created_at, updated_at, is_active FROM chat_sessions WHERE id = ?",
                (session_id,)
            )
            row = cursor.fetchone()
            
            return ChatSession(
                id=row[0],
                user_id=row[1],
                title=row[2],
                created_at=datetime.fromisoformat(row[3]),
                updated_at=datetime.fromisoformat(row[4]),
                is_active=bool(row[5])
            )
    
    def get_session(self, session_id: str) -> Optional[ChatSession]:
        """Get a chat session by ID and update its access time."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            # Update the access time to mark this session as recently accessed
            conn.execute(
                "UPDATE chat_sessions SET updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                (session_id,)
            )
            conn.commit()
            
            cursor = conn.execute(
                "SELECT id, user_id, title, created_at, updated_at, is_active FROM chat_sessions WHERE id = ?",
                (session_id,)
            )
            row = cursor.fetchone()
            
            if row:
                return ChatSession(
                    id=row[0],
                    user_id=row[1],
                    title=row[2],
                    created_at=datetime.fromisoformat(row[3]),
                    updated_at=datetime.fromisoformat(row[4]),
                    is_active=bool(row[5])
                )
            return None
    
    def list_sessions(self, user_id: str = "default-user", limit: int = 50) -> List[ChatSession]:
        """List all chat sessions for a user, ordered by most recent."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                """
                SELECT id, user_id, title, created_at, updated_at, is_active 
                FROM chat_sessions 
                WHERE user_id = ? AND is_active = 1
                ORDER BY updated_at DESC
                LIMIT ?
                """,
                (user_id, limit)
            )
            rows = cursor.fetchall()
            
            return [
                ChatSession(
                    id=row[0],
                    user_id=row[1],
                    title=row[2],
                    created_at=datetime.fromisoformat(row[3]),
                    updated_at=datetime.fromisoformat(row[4]),
                    is_active=bool(row[5])
                )
                for row in rows
            ]
    
    def update_session_title(self, session_id: str, title: str) -> bool:
        """Update the title of a chat session."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                "UPDATE chat_sessions SET title = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                (title, session_id)
            )
            conn.commit()
            return cursor.rowcount > 0
    
    def delete_session(self, session_id: str) -> bool:
        """Soft delete a chat session (mark as inactive)."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                "UPDATE chat_sessions SET is_active = 0, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                (session_id,)
            )
            conn.commit()
            return cursor.rowcount > 0
    
    def add_message(
        self, 
        session_id: str, 
        role: str, 
        content: str, 
        artifacts_created: Optional[List[str]] = None
    ) -> ChatMessage:
        """Add a message to a chat session."""
        artifacts_json = json.dumps(artifacts_created) if artifacts_created else None
        
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                """
                INSERT INTO chat_messages (session_id, role, content, artifacts_created)
                VALUES (?, ?, ?, ?)
                """,
                (session_id, role, content, artifacts_json)
            )
            conn.commit()
            
            message_id = cursor.lastrowid
            
            # Fetch the created message
            cursor = conn.execute(
                """
                SELECT id, session_id, role, content, artifacts_created, timestamp 
                FROM chat_messages WHERE id = ?
                """,
                (message_id,)
            )
            row = cursor.fetchone()
            
            artifacts = json.loads(row[4]) if row[4] else None
            
            return ChatMessage(
                id=row[0],
                session_id=row[1],
                role=row[2],
                content=row[3],
                artifacts_created=artifacts,
                timestamp=datetime.fromisoformat(row[5])
            )
    
    def get_session_messages(self, session_id: str, limit: Optional[int] = None) -> List[ChatMessage]:
        """Get all messages for a chat session."""
        query = """
            SELECT id, session_id, role, content, artifacts_created, timestamp 
            FROM chat_messages 
            WHERE session_id = ? 
            ORDER BY timestamp ASC
        """
        params = [session_id]
        
        if limit:
            query += " LIMIT ?"
            params.append(limit)
        
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(query, params)
            rows = cursor.fetchall()
            
            messages = []
            for row in rows:
                artifacts = json.loads(row[4]) if row[4] else None
                messages.append(ChatMessage(
                    id=row[0],
                    session_id=row[1],
                    role=row[2],
                    content=row[3],
                    artifacts_created=artifacts,
                    timestamp=datetime.fromisoformat(row[5])
                ))
            
            return messages
    
    def get_session_with_messages(self, session_id: str) -> Optional[Dict[str, Any]]:
        """Get a session with all its messages."""
        session = self.get_session(session_id)
        if not session:
            return None
        
        messages = self.get_session_messages(session_id)
        
        return {
            "session": session,
            "messages": messages
        }
    
    def save_artifact(
        self, 
        session_id: str, 
        filename: str, 
        file_path: str, 
        mime_type: Optional[str] = None,
        version: int = 1
    ) -> Artifact:
        """Save artifact metadata to the database."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                """
                INSERT OR REPLACE INTO artifacts (session_id, filename, file_path, mime_type, version)
                VALUES (?, ?, ?, ?, ?)
                """,
                (session_id, filename, file_path, mime_type, version)
            )
            conn.commit()
            
            # Fetch the created/updated artifact
            cursor = conn.execute(
                """
                SELECT id, session_id, filename, file_path, mime_type, version, created_at, updated_at
                FROM artifacts WHERE session_id = ? AND filename = ?
                """,
                (session_id, filename)
            )
            row = cursor.fetchone()
            
            return Artifact(
                id=row[0],
                session_id=row[1],
                filename=row[2],
                file_path=row[3],
                mime_type=row[4],
                version=row[5],
                created_at=datetime.fromisoformat(row[6]),
                updated_at=datetime.fromisoformat(row[7])
            )
    
    def get_session_artifacts(self, session_id: str) -> List[Artifact]:
        """Get all artifacts for a session."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                """
                SELECT id, session_id, filename, file_path, mime_type, version, created_at, updated_at
                FROM artifacts WHERE session_id = ?
                ORDER BY created_at ASC
                """,
                (session_id,)
            )
            rows = cursor.fetchall()
            
            return [
                Artifact(
                    id=row[0],
                    session_id=row[1],
                    filename=row[2],
                    file_path=row[3],
                    mime_type=row[4],
                    version=row[5],
                    created_at=datetime.fromisoformat(row[6]),
                    updated_at=datetime.fromisoformat(row[7])
                )
                for row in rows
            ]
    
    def get_artifact(self, session_id: str, filename: str) -> Optional[Artifact]:
        """Get a specific artifact by session and filename."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                """
                SELECT id, session_id, filename, file_path, mime_type, version, created_at, updated_at
                FROM artifacts WHERE session_id = ? AND filename = ?
                """,
                (session_id, filename)
            )
            row = cursor.fetchone()
            
            if row:
                return Artifact(
                    id=row[0],
                    session_id=row[1],
                    filename=row[2],
                    file_path=row[3],
                    mime_type=row[4],
                    version=row[5],
                    created_at=datetime.fromisoformat(row[6]),
                    updated_at=datetime.fromisoformat(row[7])
                )
            return None

class SettingsRepository:
    """Repository for managing application settings."""
    
    def get_setting(self, key: str) -> Optional[str]:
        """Get a setting value by key."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                "SELECT value FROM settings WHERE key = ?",
                (key,)
            )
            row = cursor.fetchone()
            return row[0] if row else None
    
    def set_setting(self, key: str, value: str, encrypted: bool = False) -> None:
        """Set a setting value."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            conn.execute(
                """
                INSERT OR REPLACE INTO settings (key, value, encrypted, updated_at)
                VALUES (?, ?, ?, CURRENT_TIMESTAMP)
                """,
                (key, value, encrypted)
            )
            conn.commit()
    
    def delete_setting(self, key: str) -> bool:
        """Delete a setting."""
        with sqlite3.connect(db_manager.db_path, check_same_thread=False) as conn:
            cursor = conn.execute(
                "DELETE FROM settings WHERE key = ?",
                (key,)
            )
            conn.commit()
            return cursor.rowcount > 0
    
    def get_api_key(self) -> Optional[str]:
        """Get the Google AI API key."""
        return self.get_setting("google_ai_api_key")
    
    def set_api_key(self, api_key: str) -> None:
        """Set the Google AI API key."""
        self.set_setting("google_ai_api_key", api_key, encrypted=False)
    
    def has_api_key(self) -> bool:
        """Check if API key is configured."""
        return self.get_api_key() is not None

# Global repository instances
chat_repository = ChatHistoryRepository()
settings_repository = SettingsRepository()