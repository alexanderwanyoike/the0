import os
from contextlib import asynccontextmanager
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine, async_sessionmaker
from sqlalchemy.pool import StaticPool
from api.models.database import Base


class DatabaseManager:
    """Manages PostgreSQL database connections using SQLAlchemy."""

    def __init__(self):
        self.database_url = self._get_database_url()
        self.engine = None
        self.async_session_maker = None

    def _get_database_url(self) -> str:
        """Get PostgreSQL database URL from environment."""
        database_url = os.environ.get("DATABASE_URL")
        if not database_url:
            raise RuntimeError(
                "DATABASE_URL environment variable is required. "
                "Please provide a PostgreSQL connection string."
            )

        # Convert to async URL if needed
        if database_url.startswith("postgresql://"):
            database_url = database_url.replace(
                "postgresql://", "postgresql+asyncpg://", 1
            )
        elif not database_url.startswith("postgresql+asyncpg://"):
            raise RuntimeError(
                "DATABASE_URL must be a PostgreSQL connection string. "
                "Expected format: postgresql://user:password@host:port/database"
            )

        return database_url

    async def initialize(self):
        """Initialize the database engine and session maker."""
        if self.engine is None:
            self.engine = create_async_engine(
                self.database_url,
                echo=False,  # Set to True for SQL logging in development
                pool_pre_ping=True,
                pool_recycle=3600,
            )

            self.async_session_maker = async_sessionmaker(
                bind=self.engine, class_=AsyncSession, expire_on_commit=False
            )

    async def create_all_tables(self):
        """Create all tables (for development only - use Alembic in production)."""
        await self.initialize()
        async with self.engine.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)

    async def get_session(self) -> AsyncSession:
        """Get an async database session."""
        if self.async_session_maker is None:
            await self.initialize()
        return self.async_session_maker()

    async def close(self):
        """Close the database engine."""
        if self.engine:
            await self.engine.dispose()
            self.engine = None
            self.async_session_maker = None

    @property
    def database_url_for_adk(self) -> str:
        """Get the database URL for ADK DatabaseSessionService."""
        # ADK expects sync URL format
        sync_url = self.database_url.replace(
            "postgresql+asyncpg://", "postgresql://", 1
        )
        return sync_url


# Global database manager instance
db_manager = DatabaseManager()


async def init_database():
    """Initialize the database - call this on startup."""
    await db_manager.initialize()
    # Note: In production, use Alembic migrations instead of create_all_tables
    # For development, you can uncomment the line below:
    # await db_manager.create_all_tables()


@asynccontextmanager
async def get_db_session():
    """Get an async context manager for database sessions."""
    session = await db_manager.get_session()
    try:
        yield session
    finally:
        await session.close()


def get_database_url() -> str:
    """Get the database URL for ADK session service."""
    return db_manager.database_url_for_adk
