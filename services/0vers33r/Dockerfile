# 0vers33r Security Analysis Service Dockerfile
FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    libyara-dev \
    pkg-config \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements first for better caching
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy application code
COPY . .

# Create log directory
RUN mkdir -p /var/log && chmod 755 /var/log

# Create non-root user for security
RUN groupadd -r analyzer && useradd -r -g analyzer analyzer

# Set ownership of the application
RUN chown -R analyzer:analyzer /app /var/log

# Switch to non-root user
USER analyzer

# Set environment variables
ENV PYTHONPATH=/app
ENV PYTHONUNBUFFERED=1

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD python -c "import asyncio; from src.services.nats_client import NATSClient; asyncio.run(NATSClient().connect())" || exit 1

# Expose port (if needed for health checks)
EXPOSE 8080

# Run the service
CMD ["python", "main.py"]