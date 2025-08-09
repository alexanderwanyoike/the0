import os
import sys
from pathlib import Path


def get_user_data_dir():
    """Get the user data directory for the0-ai."""
    if sys.platform == "win32":
        # Windows: %APPDATA%\the0-ai
        base_dir = Path(os.environ.get("APPDATA", Path.home() / "AppData" / "Roaming"))
        return base_dir / "the0-ai"
    else:
        # Linux/macOS: ~/.the0
        return Path.home() / ".the0"


def get_database_path():
    """Get the path to the SQLite database."""
    return get_user_data_dir() / "sessions.sqlite"


def get_data_dir():
    """Get the data directory for sessions and artifacts."""
    return get_user_data_dir() / "data"


def get_schema_sql():
    """Get the schema SQL content from bundled file."""
    if getattr(sys, "frozen", False):
        # Running as PyInstaller executable
        bundle_dir = Path(sys._MEIPASS)
        schema_path = bundle_dir / "schemas" / "schema.sql"
    else:
        # Running as script
        schema_path = Path(__file__).parent.parent / "schemas" / "schema.sql"

    if not schema_path.exists():
        raise FileNotFoundError(f"Schema file not found: {schema_path}")

    with open(schema_path, "r", encoding="utf-8") as f:
        return f.read()


def ensure_user_data_dir():
    """Ensure the user data directory and subdirectories exist."""
    user_data_dir = get_user_data_dir()
    data_dir = get_data_dir()

    # Create directories if they don't exist
    user_data_dir.mkdir(parents=True, exist_ok=True)
    data_dir.mkdir(parents=True, exist_ok=True)

    print(f"User data directory: {user_data_dir}")
    print(f"Database path: {get_database_path()}")
    print(f"Sessions data directory: {data_dir}")

    return user_data_dir
