import sqlite3
import aiosqlite
from api.config import get_database_path, get_schema_sql, ensure_user_data_dir

class DatabaseManager:
    """Manages SQLite database initialization and connections."""
    
    def __init__(self):
        # Ensure user data directory exists
        ensure_user_data_dir()
        self.db_path = get_database_path()
    
    def initialize_database(self) -> None:
        """Initialize the database with the schema if it doesn't exist."""
        if not self.db_path.exists():
            print(f"Creating new database at {self.db_path}")
        else:
            print(f"Using existing database at {self.db_path}")
        
        # Get schema SQL from config (no longer reading from file)
        schema_sql = get_schema_sql()
        
        # Execute schema - simple and straightforward
        with sqlite3.connect(self.db_path, check_same_thread=False) as conn:
            conn.executescript(schema_sql)
            conn.commit()
        
        print("Database initialized successfully")
    
    async def get_connection(self) -> aiosqlite.Connection:
        """Get an async database connection."""
        return await aiosqlite.connect(self.db_path)
    
    @property
    def database_url(self) -> str:
        """Get the database URL for ADK DatabaseSessionService."""
        return f"sqlite:///{self.db_path.absolute()}?check_same_thread=False"

# Global database manager instance
db_manager = DatabaseManager()

def init_database():
    """Initialize the database - call this on startup."""
    db_manager.initialize_database()

async def get_db_connection() -> aiosqlite.Connection:
    """Get a database connection for use in repositories."""
    return await db_manager.get_connection()

def get_database_url() -> str:
    """Get the database URL for ADK session service."""
    return db_manager.database_url