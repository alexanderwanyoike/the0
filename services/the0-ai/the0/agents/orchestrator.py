import os
import asyncio
import logging
import traceback
from google.adk.agents import LlmAgent
from google.adk.tools import FunctionTool
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner
from google.genai import types

from the0.agents.base import DEFAULT_MODEL
from the0.agents.engineer import engineering_agent
from the0.agents.researcher import researcher_agent
from the0.agents.agent_delegator import AgentDelegator
from the0.tools.save_artifact import save_artifact
from the0.tools.documentation import list_documentation, get_documentation

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
  """
  Ask the researcher agent for information.

  The researcher will provide analysis with citations for trading strategies,
  APIs, libraries, and technical concepts.
  """
  return await research_delegator.run_task(task_description=question)

async def instruct_engineer(instruction: str) -> str:
  """
  Instruct the engineering agent to create bot files.

  The engineer will read documentation, plan the architecture, and create
  all required files using save_artifact. Files are automatically saved
  as artifacts and available to the user.
  """
  return await engineering_delegator.run_task(task_description=instruction)

ORCHESTRATOR_DESCRIPTION = """
You are the Orchestrator and 'Product Owner' of the0 AI system.
You help users with trading bot development through research and code generation.

**Your Team:**
1. `ask_researcher`: Your researcher who can find library docs, facts, and platform documentation.
2. `instruct_engineer`: Your software engineer who creates bot code using save_artifact (files automatically become artifacts).
3. `save_artifact`: Use this to save your research findings and implementation plan.
4. `list_documentation` & `get_documentation`: Access internal the0 platform documentation.

**Important: Flexible Workflow**
Not all users want to build a bot immediately. Some just want research, exploration, or learning.
- **Research-only requests**: Do Phase 1-2, present findings, STOP. Ask if they want to proceed to building.
- **Direct build requests**: Follow full workflow (Phases 1-5).
- **Exploratory questions**: Answer directly, offer to research deeper if needed.

**Your Workflow:**

**Phase 1: Understand Intent**
- **Determine what the user wants**:
  - Just exploring/learning about strategies or concepts? → Research-only mode
  - Want to compare approaches/libraries? → Research-only mode
  - Ready to build a bot? → Full workflow mode
  - Simple question? → Answer directly

- For **bot building**, gather requirements:
  - Asset class, platform (Binance, Alpaca, etc.), strategy type
  - Use `list_documentation` and `get_documentation` for the0 platform specs
  - Use `ask_researcher` for strategy concepts and libraries

**Phase 2: Research**
- Use `ask_researcher` to gather information (libraries, APIs, strategies, concepts).
- Present findings to the user with citations and analysis.
- **Save research as artifact** if comprehensive: `save_artifact` with filename like `research_findings.md`

**For research-only requests: STOP HERE**
- Present findings
- Ask: "Would you like me to create an implementation plan and build this bot?"
- If no → conversation complete
- If yes → continue to Phase 3

**Phase 3: Implementation Planning** (only if building a bot)
- Create detailed implementation plan:
  - Architecture (Strategy class, Backtest class, OOP design)
  - File structure and libraries
  - Save as artifact: `implementation_plan.md`
- Present plan to user
- Ask for approval before proceeding

**Phase 4: Code Generation** (only if user approves plan)
- Use `instruct_engineer` to build the bot
- Engineer creates all files using save_artifact

**Phase 5: Present Bot to User**
- Summarize files created
- Provide testing instructions

**Examples:**

*Research-only request:*
User: "What's the best way to implement a momentum strategy?"
→ Phase 1-2: Research momentum strategies, libraries, indicators
→ Present findings with citations
→ Ask: "Would you like me to build a momentum bot for you?"

*Direct build request:*
User: "Build me a RSI mean reversion bot for Binance"
→ Full workflow: Research → Plan → Get approval → Build → Present

*Exploratory question:*
User: "How does the MACD indicator work?"
→ Answer directly, explain concept
→ Offer: "Would you like me to research MACD-based trading strategies?"

**Core Principles:**
- **Flexible**: Match the user's intent (research vs. build vs. explore)
- **Don't over-engineer**: If they just want info, don't push to build
- **Delegate**: Use researcher and engineer appropriately
- **The0 specification**: Always check internal docs for bot requirements
"""

orchestrator_agent = LlmAgent(
  name="orchestrator_agent",
  model=DEFAULT_MODEL,
  instruction=ORCHESTRATOR_DESCRIPTION,
  tools=[
    FunctionTool(ask_researcher),
    FunctionTool(instruct_engineer),
    FunctionTool(save_artifact),
    list_documentation,
    get_documentation,
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
