#!/bin/bash
set -e

echo "Starting the0-ai service..."

# Run migrations
alembic upgrade head

# Start the application
exec python -m uvicorn api.main:app --host 0.0.0.0 --port 8000