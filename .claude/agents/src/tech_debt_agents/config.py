from pathlib import Path

# Repository
REPO_ROOT = Path(__file__).resolve().parents[4]  # src/tech_debt_agents -> agents -> .claude -> repo
BASE_BRANCH = "dev"
AUDIT_FILE = ".notes/issues-audit.md"
AUDIT_PATH = REPO_ROOT / AUDIT_FILE

# Worktrees
WORKTREE_DIR = REPO_ROOT / ".claude" / "worktrees"

# Locks
LOCK_DIR = REPO_ROOT / ".claude" / "agents"
AUDITOR_LOCK = LOCK_DIR / "auditor.lock"
FIXER_LOCK = LOCK_DIR / "fixer.lock"

# Logs
LOG_DIR = REPO_ROOT / ".claude" / "agents" / "logs"

# Models
AUDITOR_MODEL = "sonnet"
FIXER_MODEL = "opus"

# Prompts
PROMPTS_DIR = REPO_ROOT / ".claude" / "agents" / "prompts"
