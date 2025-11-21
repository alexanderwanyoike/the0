import os
import subprocess 
import logging
from google.adk.agents import LlmAgent, LoopAgent
from google.adk.tools import FunctionTool
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner
from google.adk.tools.tool_context import ToolContext
from google.genai import types
from the0.agents.base import get_workspace_path, DEFAULT_MODEL, DEFAULT_GENERATE_CONTENT_CONFIG
from the0.tools.documentation import list_documentation, get_documentation
from the0.tools.web_browser import browse_url, tavily_search
from the0.tools.control import task_complete


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def get_session_id(tool_context: ToolContext) -> str:
  """Helper to safely extract session ID from tool context."""
  try:
    return tool_context._invocation_context.session.id
  except AttributeError:
    # Fallback logic similar to save_artifact
    if hasattr(tool_context, "state"):
      if "current_session_id" in tool_context.state:
        return tool_context.state["current_session_id"]
      if "session_id" in tool_context.state:
        return tool_context.state["session_id"]
  return "default"


def get_safe_path(file_path: str, session_id: str = None) -> str:
  """Ensure file paths are within the current workspace."""
  workspace_path = get_workspace_path(session_id)
  abs_path = os.path.abspath(
    os.path.join(workspace_path, file_path)
  )
  # Security check ensure the result path is within the workspace
  if not abs_path.startswith(workspace_path):
    raise ValueError(f"Attempted to access a path outside the workspace: {abs_path}")
  return abs_path

# Engineering Agent Tools
def write_file(path: str, content: str, tool_context: ToolContext) -> str:
  """
  Creates or overwrites a file with the given content.
  `path` is relative to the workspace.
  path: str - Relative file path within the workspace.
  content: str - Content to write into the file.
  """
  try:
    session_id = get_session_id(tool_context)
    safe_path = get_safe_path(path, session_id)
    # Ensure parent directories exist
    os.makedirs(os.path.dirname(safe_path), exist_ok=True)
    with open(safe_path, "w") as f:
      f.write(content)
    logger.info(f"File written: {safe_path}")
    return f"Successfully wrote to {len(content)} bytes to {path}."
  except Exception as e:
    return f"Error writing file {path}: {str(e)}"

def read_file(path: str, tool_context: ToolContext) -> str:
  """
  Reads and returns the content of a file.
  `path` is relative to the workspace.
  path: str - Relative file path within the workspace.
  """
  try:
    session_id = get_session_id(tool_context)
    safe_path = get_safe_path(path, session_id)
    with open(safe_path, "r") as f:
      content = f.read()
    
    # Truncate if too long to prevent context window issues and rate limits
    if len(content) > 5000:
      content = content[:5000] + "\n...[Content truncated due to size limit]..."
      
    logger.info(f"File read: {safe_path}")
    return content
  except Exception as e:
    return f"Error reading file {path}: {str(e)}"
  
def list_files(path: str, tool_context: ToolContext) -> str:
  """
  Lists files and directories at the given path.
  `path` is relative to the workspace.
  path: str - Relative directory path within the workspace.
  """
  try:
    session_id = get_session_id(tool_context)
    workspace_path = get_workspace_path(session_id)
    safe_path = get_safe_path(path, session_id)
    tree = []
    for root, dirs, files in os.walk(safe_path):
      rel_root = os.path.relpath(root, workspace_path)
      if rel_root == ".":
        rel_root = ""
      for d in dirs:
        tree.append(f"{rel_root}/{d}/")
      for f in files:
        tree.append(f"{rel_root}/{f}")
    logger.info(f"Listed files in: {safe_path}")
    return "File tree:\n" + "\n".join(tree)
  except Exception as e:
    return f"Error listing files in {path}: {str(e)}"


def run_shell_command(command: str, tool_context: ToolContext) -> str:
  """
  Runs a shell command *inside the project workspace* and returns the output.
  Returns the combined stdout and stderr.
  
  WARNING: this tool can be dangerous if misused. Use with caution.
  """
  logger.info(f"EXECUTING: {command}")
  try:
    session_id = get_session_id(tool_context)
    workspace_path = get_workspace_path(session_id)
    # Ensure directory exists before running command
    os.makedirs(workspace_path, exist_ok=True)
    
    result = subprocess.run(
      command,
      shell=True,
      cwd=workspace_path,
      capture_output=True,
      text=True,
      timeout=30,
    )
    output = f"STDOUT:\n{result.stdout}\nSTDERR:\n{result.stderr}"
    
    # Truncate output to prevent context window issues and rate limits
    if len(output) > 5000:
      output = output[:5000] + "\n...[Output truncated due to size limit]..."

    logger.info(f"Command executed with return code {result.returncode}")
    logger.info(f"Command output: \n{output}")
    if result.returncode != 0:
      output = f"Command exited with code {result.returncode}.\n" + output
    return output
  except subprocess.TimeoutExpired:
    return f"Error: Command '{command}' timed out (30s limit)."
  except Exception as e:
    return f"Error executing command '{command}': {str(e)}"


AGENT_INSTRUCTIONS = """
You are a software development agent. Your goal is to complete the users software development tasks.
You must use you available tools to engineer a solution.

**Environment:**
- You have dedicated workspace.
- **File Structure**: All files must be created in the **ROOT** of your workspace. Do NOT create a project subfolder (e.g., do not create `bot/main.py`, create `main.py`).
**Available Tools:**
- write_file(path: str, content: str) -> str: Creates or overwrites a file with the given content.
- read_file(path: str) -> str: Reads and returns the content of a file.
- list_files(path: str = ".") -> str: Lists files and directories at the given path.
- run_shell_command(command: str) -> str: Runs a shell command inside the project workspace and returns the output.
- list_documentation() -> str: Lists available the0 bot development documentation topics.
- get_documentation(topic: str) -> str: Retrieves the0 bot development documentation content for a specific topic.
- tavily_search(query: str, search_depth: str = "basic") -> str: Searches the web for information about libraries, APIs, and programming concepts.
- browse_url(url: str) -> str: Browses a specific URL and returns the content related to library documentation or programming concepts.
- task_complete(result: str) -> str: Signals that the assigned task is fully complete. You MUST call this to finish.


**Your task:**
1. **Document (CRITICAL - MUST DO FIRST):**
   - You **MUST** start by calling `list_documentation()` to see what guides are available.
   - You **MUST** read relevant guides using `get_documentation(topic)` (e.g., quick-start, backtesting).
   - **FAILURE to consult internal documentation will result in incompatible code.**
   - **DO NOT ask the Orchestrator for platform details.** You have the documentation tools; use them to find the answers yourself.

2. **Plan & Design:**
   - Think step-by-step about how to accomplish the user's request based on the internal documentation.
   - **Technical Ownership**: YOU are responsible for determining the implementation details (libraries, specific code patterns) by consulting the internal documentation.
   - **Code Quality (OOP):** You MUST write clean, object-oriented code.
     - Create a `Strategy` class for the core trading logic.
     - Create a `Backtest` class for backtesting.
     - The `Backtest` class MUST utilize the `Strategy` class logic (no code duplication).
     - Ensure entry points (`main.py`, `backtest.py`) use these classes cleanly.

3. **Execute:**
   - Create a virtual environment (venv).
   - Write code files (in ROOT).
   - Install dependencies.

4. **Test (SAFE EXECUTION REQUIRED):**
   - **DO NOT RUN INFINITE LOOPS** (like `while True`) directly in the shell. This will hang the agent.
   - Implement a `--dry-run` or `--test` flag in your bot to run a single iteration and exit.
   - **Unit Tests:** Prefer creating and running unit tests (`python -m unittest`) to verify logic.
   - If you must run the bot, ensure it has a mechanism to exit quickly.
   - Verify API key handling (use env vars).

5. **Finish:**
   - When all files are created and tested, you **MUST** call the `task_complete` tool to signal completion.
   - Do not just say "I am done". Call the tool.

**Python venv project setup (CRITICAL)**
- To create a venv run: `python3 -m venv venv` or `virtualenv venv`
- **DO NOT** try to run `source venv/bin/activate`. It will not work.
- To install packages, run: `venv/bin/pip install <package>`
- To run python scripts, run: `venv/bin/python <script.py>`
- To run tests, run: `venv/bin/python -m unittest <test_file.py>`

**Workflow Summary:**
1. **READ DOCS** -> 2. Plan -> 3. venv -> 4. Code (OOP, ROOT) -> 5. **TEST SAFELY** -> 6. **CALL TASK_COMPLETE**.
"""

# Base LLM Agent
_engineering_agent_llm = LlmAgent(
  name="engineering_agent_llm",
  model=DEFAULT_MODEL,
  generate_content_config=DEFAULT_GENERATE_CONTENT_CONFIG,
  instruction=AGENT_INSTRUCTIONS,
  tools=[
    FunctionTool(write_file),
    FunctionTool(read_file),
    FunctionTool(list_files),
    FunctionTool(run_shell_command),
    list_documentation,
    get_documentation,
    tavily_search,
    browse_url,
    FunctionTool(task_complete),
  ],
)

# Wrapped in LoopAgent for autonomous execution until task_complete (escalate)
engineering_agent = LoopAgent(
    name="engineering_agent",
    sub_agents=[_engineering_agent_llm],
    max_iterations=30 
)
