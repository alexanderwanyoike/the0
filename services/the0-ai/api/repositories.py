from typing import List, Optional, Dict, Any
from datetime import datetime
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload
from sqlalchemy import select, update, delete
from sqlalchemy.exc import NoResultFound, IntegrityError

from api.models.database import ChatSession, ChatMessage, Artifact, Setting


class ChatRepository:
    """Repository for chat-related database operations using SQLAlchemy."""

    def __init__(self, session: AsyncSession):
        self.session = session

    async def create_session(
        self,
        session_id: str,
        user_id: str = "default-user",
        title: Optional[str] = None,
    ) -> ChatSession:
        """Create a new chat session."""
        chat_session = ChatSession(
            id=session_id, user_id=user_id, title=title, is_active=True
        )

        self.session.add(chat_session)
        try:
            await self.session.commit()
            return chat_session
        except IntegrityError:
            await self.session.rollback()
            # Session might already exist, try to get it
            existing = await self.get_session(session_id)
            if existing:
                return existing
            raise
        except Exception:
            await self.session.rollback()
            raise

    async def get_session(self, session_id: str) -> Optional[ChatSession]:
        """Get a chat session by ID."""
        result = await self.session.execute(
            select(ChatSession).where(ChatSession.id == session_id)
        )
        return result.scalar_one_or_none()

    async def list_sessions(self, user_id: str = "default-user") -> List[ChatSession]:
        """List all chat sessions for a user."""
        result = await self.session.execute(
            select(ChatSession)
            .where(ChatSession.user_id == user_id)
            .where(ChatSession.is_active == True)
            .order_by(ChatSession.updated_at.desc())
        )
        return list(result.scalars().all())

    async def get_session_with_messages(
        self, session_id: str
    ) -> Optional[Dict[str, Any]]:
        """Get a session with all its messages."""
        result = await self.session.execute(
            select(ChatSession)
            .options(selectinload(ChatSession.messages))
            .where(ChatSession.id == session_id)
        )
        session = result.scalar_one_or_none()

        if not session:
            return None

        return {
            "session": session,
            "messages": sorted(
                session.messages, key=lambda m: m.timestamp or datetime.min
            ),
        }

    async def update_session_title(self, session_id: str, title: str) -> bool:
        """Update the title of a chat session."""
        result = await self.session.execute(
            update(ChatSession)
            .where(ChatSession.id == session_id)
            .values(title=title, updated_at=datetime.utcnow())
        )
        await self.session.commit()
        return result.rowcount > 0

    async def delete_session(self, session_id: str) -> bool:
        """Soft delete a chat session."""
        result = await self.session.execute(
            update(ChatSession)
            .where(ChatSession.id == session_id)
            .values(is_active=False, updated_at=datetime.utcnow())
        )
        await self.session.commit()
        return result.rowcount > 0

    async def add_message(
        self,
        session_id: str,
        role: str,
        content: str,
        artifacts_created: Optional[List[str]] = None,
    ) -> ChatMessage:
        """Add a message to a chat session."""
        message = ChatMessage(
            session_id=session_id,
            role=role,
            content=content,
            artifacts_created=artifacts_created or [],
            timestamp=datetime.utcnow(),
        )

        self.session.add(message)
        await self.session.commit()

        # Update session's updated_at timestamp
        await self.session.execute(
            update(ChatSession)
            .where(ChatSession.id == session_id)
            .values(updated_at=datetime.utcnow())
        )
        await self.session.commit()

        return message

    async def save_artifact(
        self,
        session_id: str,
        filename: str,
        file_path: str,
        mime_type: str = "text/plain",
        version: int = 1,
    ) -> Artifact:
        """Save artifact metadata to the database."""
        # Check if artifact already exists
        existing_result = await self.session.execute(
            select(Artifact)
            .where(Artifact.session_id == session_id)
            .where(Artifact.filename == filename)
        )
        existing = existing_result.scalar_one_or_none()

        if existing:
            # Update existing artifact
            existing.file_path = file_path
            existing.mime_type = mime_type
            existing.version = version
            existing.updated_at = datetime.utcnow()
            artifact = existing
        else:
            # Create new artifact
            artifact = Artifact(
                session_id=session_id,
                filename=filename,
                file_path=file_path,
                mime_type=mime_type,
                version=version,
            )
            self.session.add(artifact)

        await self.session.commit()
        return artifact

    async def get_artifact(self, session_id: str, filename: str) -> Optional[Artifact]:
        """Get artifact metadata from the database."""
        result = await self.session.execute(
            select(Artifact)
            .where(Artifact.session_id == session_id)
            .where(Artifact.filename == filename)
        )
        return result.scalar_one_or_none()

    async def get_session_artifacts(self, session_id: str) -> List[Artifact]:
        """Get all artifacts for a session."""
        result = await self.session.execute(
            select(Artifact)
            .where(Artifact.session_id == session_id)
            .order_by(Artifact.created_at.desc())
        )
        return list(result.scalars().all())


class SettingsRepository:
    """Repository for settings database operations using SQLAlchemy."""

    def __init__(self, session: AsyncSession):
        self.session = session

    async def get_setting(self, key: str) -> Optional[str]:
        """Get a setting value by key."""
        result = await self.session.execute(select(Setting).where(Setting.key == key))
        setting = result.scalar_one_or_none()
        return setting.value if setting else None

    async def set_setting(self, key: str, value: str) -> None:
        """Set a setting value."""
        # Check if setting exists
        existing_result = await self.session.execute(
            select(Setting).where(Setting.key == key)
        )
        existing = existing_result.scalar_one_or_none()

        if existing:
            existing.value = value
            existing.updated_at = datetime.utcnow()
        else:
            setting = Setting(key=key, value=value)
            self.session.add(setting)

        await self.session.commit()

    async def delete_setting(self, key: str) -> bool:
        """Delete a setting."""
        result = await self.session.execute(delete(Setting).where(Setting.key == key))
        await self.session.commit()
        return result.rowcount > 0

    async def get_api_key(self) -> Optional[str]:
        """Get the Google AI API key."""
        return await self.get_setting("google_ai_api_key")

    async def set_api_key(self, api_key: str) -> None:
        """Set the Google AI API key."""
        await self.set_setting("google_ai_api_key", api_key)

    async def has_api_key(self) -> bool:
        """Check if API key is configured."""
        api_key = await self.get_api_key()
        return api_key is not None and api_key.strip() != ""


# Factory functions for dependency injection
def get_chat_repository(session: AsyncSession) -> ChatRepository:
    """Get a chat repository instance."""
    return ChatRepository(session)


def get_settings_repository(session: AsyncSession) -> SettingsRepository:
    """Get a settings repository instance."""
    return SettingsRepository(session)
