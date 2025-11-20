import os
import asyncio
import subprocess 
import logging
import shutil
import traceback
from google.adk.agents import LlmAgent
from google.adk.tools import FunctionTool
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner
from google.adk.tools.tool_context import ToolContext
from google.genai import types
from the0.agents.base import get_workspace_path
from the0.tools.documentation import list_documentation, get_documentation



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
    logger.info(f"Command executed with return code {result.returncode}")
    logger.info(f"Command output: \n{output}")
    if result.returncode != 0:
      output = f"Command exited with code {result.returncode}.\n" + output
    return output
  except subprocess.TimeoutExpired:
    return f"Error: Command '{command}' timed out."
  except Exception as e:
    return f"Error executing command '{command}': {str(e)}"


AGENT_INSTRUCTIONS = """
You are a software development agent. Your goal is to complete the users software development tasks.
You must use you available tools to engineer a solution.

**Environment:**
- You have dedicated workspace.
**Available Tools:**
- write_file(path: str, content: str) -> str: Creates or overwrites a file with the given content.
- read_file(path: str) -> str: Reads and returns the content of a file.
- list_files(path: str = ".") -> str: Lists files and directories at the given path.
- run_shell_command(command: str) -> str: Runs a shell command inside the project workspace and returns the output.
- list_documentation() -> str: Lists available the0 bot development documentation topics.
- get_documentation(topic: str) -> str: Retrieves the0 bot development documentation content for a specific topic.


**Your task:**
1. **Document (MANDATORY):**
   - FIRST, call `list_documentation()` to see what guides are available.
   - SECOND, call `get_documentation(topic)` to read relevant guides (e.g., quick-start, backtesting, best practices).
   - You MUST follow the patterns and standards found in the internal documentation.

2. **Plan & Design:**
   - Think step-by-step about how to accomplish the user's request based on the internal documentation.
   - **Code Quality (OOP):** You MUST write clean, object-oriented code.
     - Create a `Strategy` class for the core trading logic.
     - Create a `Backtest` class for backtesting.
     - The `Backtest` class MUST utilize the `Strategy` class logic (no code duplication).
     - Ensure entry points (`main.py`, `backtest.py`) use these classes cleanly.

3. **Execute:**
   - Create a virtual environment (venv).
   - Write code files.
   - Install dependencies.

4. **Test (MANDATORY):**
   - You MUST verify your code runs.
   - Run `python -m unittest` or execute the script directly.
   - Check for errors and fix them.
   - If API keys are required, use environment variables (do not hardcode).

**Python venv project setup (CRITICAL)**
- To create a venv run: `python3 -m venv venv` or `virtualenv venv`
- **DO NOT** try to run `source venv/bin/activate`. It will not work.
- To install packages, run: `venv/bin/pip install <package>`
- To run python scripts, run: `venv/bin/python <script.py>`
- To run tests, run: `venv/bin/python -m unittest <test_file.py>`

**Workflow Summary:**
1. Read Docs -> 2. Create venv -> 3. Write Code -> 4. Install Deps -> 5. Run/Test Code.
"""

engineering_agent = LlmAgent(
  name="engineering_agent",
  model="gemini-2.5-flash",
  instruction=AGENT_INSTRUCTIONS,
  tools=[
    FunctionTool(write_file),
    FunctionTool(read_file),
    FunctionTool(list_files),
    FunctionTool(run_shell_command),
    list_documentation,
    get_documentation
  ],
)
