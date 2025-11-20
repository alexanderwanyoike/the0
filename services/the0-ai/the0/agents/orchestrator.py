
import os
import asyncio
import logging
import traceback
from google.adk.agents import LlmAgent
from google.adk.tools import FunctionTool
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner
from google.genai import types

import os
from the0.agents.base import DEFAULT_MODEL, setup_workspace, workspace_id_var, get_workspace_path
from the0.agents.engineer import engineering_agent
from the0.agents.researcher import researcher_agent
from the0.agents.agent_delegator import AgentDelegator
from the0.tools.save_artifact import save_artifact

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

session_service = InMemorySessionService()


research_delegator = AgentDelegator(
  agent=researcher_agent,
  app_name="researcher_app",
  session_service=session_service,
)

engineering_delegator = AgentDelegator(
  agent=engineering_agent,
  app_name="engineering_app",
  session_service=session_service,
)

async def ask_researcher(question: str) -> str:
  """Helper to ask the researcher agent for information."""
  return await research_delegator.run_task(task_description=question)

async def instruct_engineer(instruction: str) -> str:
  """Helper to instruct the engineering agent to perform tasks."""
  return await engineering_delegator.run_task(task_description=instruction)

def list_engineer_files(path: str = ".") -> str:
  """
  Lists files in the engineering agent's workspace.
  Use this to inspect what the engineer has built.
  """
  if not engineering_delegator.session_id:
    return "Error: Engineer has not started a session yet."
  
  workspace_path = get_workspace_path(engineering_delegator.session_id)
  abs_path = os.path.abspath(os.path.join(workspace_path, path))
  
  if not abs_path.startswith(workspace_path):
    return "Error: Invalid path"

  tree = []
  for root, dirs, files in os.walk(abs_path):
    rel_root = os.path.relpath(root, workspace_path)
    if rel_root == ".":
      rel_root = ""
    for d in dirs:
      tree.append(f"{rel_root}/{d}/")
    for f in files:
      tree.append(f"{rel_root}/{f}")
  return "Engineer's Workspace Files:\n" + "\n".join(tree)

def read_engineer_file(path: str) -> str:
  """
  Reads a file from the engineering agent's workspace.
  Use this to get the content of files to save as artifacts.
  """
  if not engineering_delegator.session_id:
    return "Error: Engineer has not started a session yet."
  
  workspace_path = get_workspace_path(engineering_delegator.session_id)
  abs_path = os.path.abspath(os.path.join(workspace_path, path))
  
  if not abs_path.startswith(workspace_path):
    return "Error: Invalid path"
    
  try:
    with open(abs_path, "r") as f:
      return f.read()
  except Exception as e:
    return f"Error reading file: {str(e)}"

ORCHESTRATOR_DESCRIPTION = """
You are the Orchestrator and 'Product Owner' of the0 AI system.
Your goal is to build and deploy automated trading bots on the0 platform using a research-driven engineering approach.

**Your Team:**
1. `ask_researcher`: Your researcher who can find library docs, facts, and platform documentation.
2. `instruct_engineer`: Your software engineer who builds and executes code in a dedicated workspace.

**Your Process:**
1. **Analyze & Research**: Understand the user's request. If you need documentation, library info, or "the0 way" of doing things, use `ask_researcher`.
2. **Implement**: Create a plan and instruct the engineer to build the bot using `instruct_engineer`. Keep it simple and aligned with the0 platform standards.
3. **Save Artifacts (CRITICAL)**:
   - Once the engineer has completed the implementation and testing, YOU must inspect their work.
   - Use `list_engineer_files` to see what was created in the engineer's workspace.
   - Use `read_engineer_file` to get the content of relevant source files (e.g., .py, .json, .yaml, .md).
   - Use `save_artifact` to save these files as system artifacts.
   - **IMPORTANT**: DO NOT save the `venv` folder, `node_modules` folder, or any files within them. Only save the source code and configuration files.

**Core Principles:**
- **Delegate**: Don't write code yourself. Tell the engineer what to do.
- **The0 Way**: Follow the platform's best practices (ask researcher if unsure).
- **Simple**: Avoid over-engineering. Build a working bot.
"""

orchestrator_agent = LlmAgent(
  name="orchestrator_agent",
  model=DEFAULT_MODEL,
  instruction=ORCHESTRATOR_DESCRIPTION,
  tools=[
    FunctionTool(ask_researcher),
    FunctionTool(instruct_engineer),
    FunctionTool(save_artifact),
    FunctionTool(list_engineer_files),
    FunctionTool(read_engineer_file),
  ]
)


async def main():
  try:
    if os.getenv("GOOGLE_API_KEY") is None:
      logger.error("GOOGLE_API_KEY environment variable not set.")
      return
    
    APP_NAME = "orchestrator_app"
    USER_ID = "test_user"

    session = await session_service.create_session(
      app_name=APP_NAME,
      user_id=USER_ID,
    )

    # Set the workspace context to the current session ID
    workspace_id_var.set(session.id)
    setup_workspace(session_id=session.id)

    runner = Runner(
      app_name=APP_NAME,
      agent=orchestrator_agent,
      session_service=session_service,
    )

    PROJECT_GOAL = """
    I want to build a python script that scrapes the current stock price of Google 
    and saves it to a CSV file. 
    
    1. Research which library is best for getting stock prices (yfinance vs alpha vantage).
    2. Build the script using the best library found.
    """
    logger.info("Starting orchestrator agent...")
    logger.info(f"---- AGENT IS WORKING---")


    async for event in runner.run_async(
      session_id=session.id,
      user_id=USER_ID,
      new_message=types.Content(role="user", parts=[types.Part.from_text(text=PROJECT_GOAL)])
    ):
      if not event.content or not event.content.parts:
        continue
      for part in event.content.parts:
        if part.text:
          logger.info(f"Though/Plan: \n{part.text}")

        
        if part.function_call:
          cmd = part.function_call.args.get("command", "Unknown")
          path = part.function_call.args.get("path", "N/A")

          logger.info("-"*20 + " FUNCTION CALL " + "-"*20)
          logger.info(f"Action: Calling function `{part.function_call.name}`")
          if cmd != "Unknown":
            logger.info(f" > Command: {cmd}")
          if path:
            logger.info(f" > Path: {path}")
          logger.info("-"*50)
        
        if part.function_response:
          logger.info("-"*20 + " FUNCTION RESPONSE " + "-"*20)
          output = str(part.function_response.response)
          if len(output) > 500:
            output = output[:500] + "...[truncated]"
          else:
            output = output
          logger.info(f"Action: Function `{part.function_response.name}` returned:\n{output}")
          logger.info("-"*50)
  except Exception as e:
    logger.error("Runner error {}".format(e))
    logger.error(traceback.format_exc())
  logger.info("Engineering agent run complete.")

if __name__ == "__main__":
  asyncio.run(main())
