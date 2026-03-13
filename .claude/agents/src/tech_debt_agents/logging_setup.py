import logging
from datetime import datetime
from pathlib import Path

from .config import LOG_DIR


def setup_logging(agent_name: str) -> logging.Logger:
    """Configure logging to both stdout and a timestamped log file."""
    LOG_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y-%m-%d_%H%M%S")
    log_file = LOG_DIR / f"{agent_name}_{timestamp}.log"

    root = logging.getLogger()
    root.setLevel(logging.INFO)

    # Clear any existing handlers
    root.handlers.clear()

    fmt = logging.Formatter("%(asctime)s %(levelname)s %(message)s", datefmt="%H:%M:%S")

    stdout_handler = logging.StreamHandler()
    stdout_handler.setFormatter(fmt)
    root.addHandler(stdout_handler)

    file_handler = logging.FileHandler(log_file)
    file_handler.setFormatter(fmt)
    root.addHandler(file_handler)

    logger = logging.getLogger(agent_name)
    logger.info(f"Logging to {log_file}")
    return logger
