from google.cloud import storage as gcs


class GCSStorageClient:
    """Google Cloud Storage implementation"""

    def __init__(self):
        self.storage_client = gcs.Client()

    def download_file(self, gcs_path: str) -> bytes:
        """Download file from GCS"""
        if not gcs_path.startswith("gs://"):
            raise ValueError("Invalid GCS path format")

        # Parse GCS path
        path_parts = gcs_path[5:].split("/", 1)  # Remove 'gs://' prefix
        if len(path_parts) != 2:
            raise ValueError("Malformed GCS path")

        bucket_name, blob_name = path_parts

        # Download the file
        bucket = self.storage_client.bucket(bucket_name)
        blob = bucket.blob(blob_name)

        if not blob.exists():
            raise FileNotFoundError(f"File not found: {gcs_path}")

        # Check file size (prevent ZIP bombs)
        blob.reload()
        if blob.size > 1 * 1024 * 1024 * 1024:  # 1GB limit
            raise ValueError("File too large")

        return blob.download_as_bytes()
