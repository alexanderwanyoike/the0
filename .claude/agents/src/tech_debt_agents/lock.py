import fcntl
import sys
import logging
from pathlib import Path

logger = logging.getLogger(__name__)


class FileLock:
    """flock-based file lock. Exits cleanly if lock is already held."""

    def __init__(self, path: Path):
        self.path = path
        self._fd = None

    def __enter__(self):
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._fd = open(self.path, "w")
        try:
            fcntl.flock(self._fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            logger.info(f"Lock acquired: {self.path}")
        except BlockingIOError:
            logger.info(f"Lock already held: {self.path} — exiting cleanly")
            sys.exit(0)
        return self

    def __exit__(self, *_):
        if self._fd:
            fcntl.flock(self._fd, fcntl.LOCK_UN)
            self._fd.close()
            logger.info("Lock released")
