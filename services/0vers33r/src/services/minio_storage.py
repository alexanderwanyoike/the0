from minio import Minio
from minio.error import S3Error
import os
from typing import Optional


class MinIOStorageClient:
    """MinIO storage implementation for OSS the0 platform"""

    def __init__(self):
        self.endpoint = os.getenv('MINIO_ENDPOINT', 'localhost:9000')
        self.access_key = os.getenv('MINIO_ACCESS_KEY', 'minioadmin')
        self.secret_key = os.getenv('MINIO_SECRET_KEY', 'minioadmin')
        self.secure = os.getenv('MINIO_USE_SSL', 'false').lower() == 'true'
        
        self.client = Minio(
            endpoint=self.endpoint,
            access_key=self.access_key,
            secret_key=self.secret_key,
            secure=self.secure
        )
        
        # Default bucket for custom bots
        self.bucket_name = os.getenv('CUSTOM_BOTS_BUCKET', 'custom-bots')

    def download_file(self, file_path: str) -> bytes:
        """
        Download file from MinIO
        
        Args:
            file_path: Path to file in MinIO (e.g., "user123/bot-v1.0.0.zip")
            
        Returns:
            File content as bytes
            
        Raises:
            FileNotFoundError: If file doesn't exist
            ValueError: If file is too large or other validation fails
        """
        try:
            # Get object info to check size
            obj_info = self.client.stat_object(self.bucket_name, file_path)
            
            # Check file size (prevent ZIP bombs) - 1GB limit
            if obj_info.size > 1 * 1024 * 1024 * 1024:
                raise ValueError(f"File too large: {obj_info.size} bytes")
            
            # Download the file
            response = self.client.get_object(self.bucket_name, file_path)
            content = response.read()
            response.close()
            response.release_conn()
            
            return content
            
        except S3Error as e:
            if e.code == 'NoSuchKey':
                raise FileNotFoundError(f"File not found: {file_path}")
            elif e.code == 'NoSuchBucket':
                raise FileNotFoundError(f"Bucket not found: {self.bucket_name}")
            else:
                raise ValueError(f"MinIO error: {e}")
        except Exception as e:
            raise ValueError(f"Failed to download file: {e}")

    def file_exists(self, file_path: str) -> bool:
        """Check if file exists in MinIO"""
        try:
            self.client.stat_object(self.bucket_name, file_path)
            return True
        except S3Error as e:
            if e.code == 'NoSuchKey':
                return False
            raise ValueError(f"MinIO error checking file existence: {e}")

    def get_file_info(self, file_path: str) -> Optional[dict]:
        """Get file metadata from MinIO"""
        try:
            obj_info = self.client.stat_object(self.bucket_name, file_path)
            return {
                'size': obj_info.size,
                'last_modified': obj_info.last_modified,
                'etag': obj_info.etag,
                'content_type': obj_info.content_type,
                'metadata': obj_info.metadata
            }
        except S3Error as e:
            if e.code == 'NoSuchKey':
                return None
            raise ValueError(f"MinIO error getting file info: {e}")

    def list_files(self, prefix: str = "") -> list:
        """List files in bucket with optional prefix"""
        try:
            objects = self.client.list_objects(
                self.bucket_name, 
                prefix=prefix, 
                recursive=True
            )
            return [obj.object_name for obj in objects]
        except S3Error as e:
            raise ValueError(f"MinIO error listing files: {e}")