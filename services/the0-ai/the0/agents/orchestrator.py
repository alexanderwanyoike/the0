
import os
import asyncio
import logging
import traceback
from google.adk.agents import LlmAgent
from google.adk.tools import FunctionTool
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner
from google.genai import types

from the0.agents.base import DEFAULT_MODEL, setup_workspace
from the0.agents.engineer import engineering_agent
from the0.agents.researcher import researcher_agent
from the0.agents.agent_delegator import AgentDelegator

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


ORCHESTRATOR_DESCRIPTION = """
Your are the 'Product Owner' and Main Orchestrator.
You have two specialised teams (tools) at your disposal:

1. `ask_researcher`: Use this to find libraries docs or facts.
2. `instruct_engineer`: Use this to build software full on software applications.


**Your process:**
1. Analyze the user's request.
2. If you need more information, call `ask_researcher` (docs, versions, how-tos, facts, documentation).
3. Once you have the information create a plan and tell `instruct_engineer` to execute it.
4. Dont write code yourself, always delegate to the engineering agent.
"""

orchestrator_agent = LlmAgent(
  name="orchestrator_agent",
  model=DEFAULT_MODEL,
  instruction=ORCHESTRATOR_DESCRIPTION,
  tools=[
    FunctionTool(ask_researcher),
    FunctionTool(instruct_engineer),
  ]
)


async def main():
  try:
    if os.getenv("GOOGLE_API_KEY") is None:
      logger.error("GOOGLE_API_KEY environment variable not set.")
      return
    
    setup_workspace()
    APP_NAME = "orchestrator_app"
    USER_ID = "test_user"

    session = await session_service.create_session(
      app_name=APP_NAME,
      user_id=USER_ID,
    )

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
