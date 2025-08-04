from typing import Protocol, Dict, Any


class StorageInterface(Protocol):
    """Interface for storage operations"""

    def download_file(self, gcs_path: str) -> bytes:
        """Download file from storage and return bytes"""
        ...


class DatabaseInterface(Protocol):
    """Interface for database operations"""

    def update_bot_status(
        self, bot_id: str, status: str, review_data: Dict[str, Any]
    ) -> None:
        """Update bot status in database"""
        ...
