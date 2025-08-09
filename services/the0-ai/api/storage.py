import os
import io
import asyncio
from typing import Optional, List
from minio import Minio
from minio.error import S3Error


class StorageService:
    """Service for handling file storage using MinIO as the single source of truth."""

    def __init__(self):
        self.bucket_name = os.environ.get("AI_ARTIFACTS_BUCKET", "ai-artifacts")
        self.minio_client = self._init_minio()

    def _init_minio(self) -> Minio:
        """Initialize MinIO client from environment variables."""
        minio_endpoint = os.environ.get("MINIO_ENDPOINT")
        minio_access_key = os.environ.get("MINIO_ACCESS_KEY")
        minio_secret_key = os.environ.get("MINIO_SECRET_KEY")
        minio_use_ssl = os.environ.get("MINIO_USE_SSL", "false").lower() == "true"

        if not all([minio_endpoint, minio_access_key, minio_secret_key]):
            raise RuntimeError(
                "MinIO configuration is incomplete. Required environment variables: "
                "MINIO_ENDPOINT, MINIO_ACCESS_KEY, MINIO_SECRET_KEY"
            )

        try:
            client = Minio(
                minio_endpoint,
                access_key=minio_access_key,
                secret_key=minio_secret_key,
                secure=minio_use_ssl,
            )

            # Create bucket if it doesn't exist
            if not client.bucket_exists(self.bucket_name):
                client.make_bucket(self.bucket_name)
                print(f"Created MinIO bucket: {self.bucket_name}")

            print(f"MinIO initialized successfully at {minio_endpoint}")
            return client

        except Exception as e:
            raise RuntimeError(f"Failed to initialize MinIO: {e}")

    async def save_artifact(self, session_id: str, filename: str, content: str) -> str:
        """Save artifact content and return the storage path."""
        loop = asyncio.get_event_loop()

        def _save():
            object_key = self._get_object_key(session_id, filename)
            content_bytes = content.encode("utf-8")
            content_stream = io.BytesIO(content_bytes)

            self.minio_client.put_object(
                self.bucket_name,
                object_key,
                content_stream,
                len(content_bytes),
                content_type="text/plain",
            )
            return object_key

        return await loop.run_in_executor(None, _save)

    async def get_artifact(self, session_id: str, filename: str) -> Optional[str]:
        """Get artifact content by session and filename."""
        loop = asyncio.get_event_loop()

        def _get():
            try:
                object_key = self._get_object_key(session_id, filename)
                response = self.minio_client.get_object(self.bucket_name, object_key)
                content = response.read().decode("utf-8")
                response.close()
                response.release_conn()
                return content
            except S3Error as e:
                if e.code == "NoSuchKey":
                    return None
                raise

        return await loop.run_in_executor(None, _get)

    async def list_artifacts(self, session_id: str) -> List[str]:
        """List all artifact filenames for a session."""
        loop = asyncio.get_event_loop()

        def _list():
            prefix = f"{session_id}/artifacts/"
            objects = self.minio_client.list_objects(self.bucket_name, prefix=prefix)
            filenames = []
            for obj in objects:
                # Extract filename from object key
                filename = obj.object_name.replace(prefix, "")
                if filename:  # Skip directory entries
                    filenames.append(filename)
            return filenames

        return await loop.run_in_executor(None, _list)

    async def delete_artifact(self, session_id: str, filename: str) -> bool:
        """Delete an artifact."""
        loop = asyncio.get_event_loop()

        def _delete():
            try:
                object_key = self._get_object_key(session_id, filename)
                self.minio_client.remove_object(self.bucket_name, object_key)
                return True
            except S3Error as e:
                if e.code == "NoSuchKey":
                    return False
                raise

        return await loop.run_in_executor(None, _delete)

    def _get_object_key(self, session_id: str, filename: str) -> str:
        """Generate MinIO object key from session and filename."""
        return f"{session_id}/artifacts/{filename}"


# Global storage service instance
storage_service = StorageService()
