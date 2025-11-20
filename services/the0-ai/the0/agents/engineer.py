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
from google.genai import types
from the0.agents.base import get_workspace_path


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)




def get_safe_path(file_path: str) -> str:
  """Ensure file paths are within the current workspace."""
  workspace_path = get_workspace_path()
  abs_path = os.path.abspath(
    os.path.join(workspace_path, file_path)
  )
  # Security check ensure the result path is within the workspace
  if not abs_path.startswith(workspace_path):
    raise ValueError(f"Attempted to access a path outside the workspace: {abs_path}")
  return abs_path

# Engineering Agent Tools
def write_file(path: str, content: str) -> str:
  """
  Creates or overwrites a file with the given content.
  `path` is relative to the workspace.
  path: str - Relative file path within the workspace.
  content: str - Content to write into the file.
  """
  try:
    safe_path = get_safe_path(path)
    # Ensure parent directories exist
    os.makedirs(os.path.dirname(safe_path), exist_ok=True)
    with open(safe_path, "w") as f:
      f.write(content)
    logger.info(f"File written: {safe_path}")
    return f"Successfully wrote to {len(content)} bytes to {path}."
  except Exception as e:
    return f"Error writing file {path}: {str(e)}"

def read_file(path: str) -> str:
  """
  Reads and returns the content of a file.
  `path` is relative to the workspace.
  path: str - Relative file path within the workspace.
  """
  try:
    safe_path = get_safe_path(path)
    with open(safe_path, "r") as f:
      content = f.read()
    logger.info(f"File read: {safe_path}")
    return content
  except Exception as e:
    return f"Error reading file {path}: {str(e)}"
  
def list_files(path: str = ".") -> str:
  """
  Lists files and directories at the given path.
  `path` is relative to the workspace.
  path: str - Relative directory path within the workspace.
  """
  try:
    workspace_path = get_workspace_path()
    safe_path = get_safe_path(path)
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


def run_shell_command(command: str) -> str:
  """
  Runs a shell command *inside the project workspace* and returns the output.
  Returns the combined stdout and stderr.
  
  WARNING: this tool can be dangerous if misused. Use with caution.
  """
  logger.info(f"EXECUTING: {command}")
  try:
    workspace_path = get_workspace_path()
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


**Your task:**
1. **Plan:** Think step-by-step about how to accomplish the user's request.
2. **Execute:** Use the available tools to implement your plan.
3. **Observe:** After each action, review the results and adjust your plan as necessary.
4. **Iterate:** Repeat the Execute and Observe steps until the task is complete.

**Python venv project setup (CRITICAL)**
- To create a venv run: `python3 -m venv venv` or `virtualenv venv`
- **DO NOT** try to run `source venv/bin/activate`. It will not work.
- To install packages, run: `venv/bin/pip install <package>`
- To run python scripts, run: `venv/bin/python <script.py>`
- To run tests, run: `venv/bin/python -m unittest <test_file.py>`

Firstly, create the venv. Then, write your code files. Then install any dependencies. Finally, run and test your code.
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
  ],
)
