import logging
from google.adk.runners import Runner
from google.genai import types

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class AgentDelegator:
  """
  Helper class to manage running sub agents as a tool.
  It keeps track of the sub agents session id so conversation context is not lost
  between multiple calls (e.g. Engineer needs to remember what it just wrote)
  """
  def __init__(self, agent, app_name, session_service):
    self.agent = agent
    self.app_name = app_name
    self.session_service = session_service
    self.session_id = None
    self.user_id = "orchestrator_delegator"

  async def run_task(self, task_description: str) -> str:
    """
    Runs the agent and returns the final text response.
    Errors are returned to the orchestrator for handling.
    """
    logger.info(f"Delegating task to {self.agent.name}")

    # 1. Create session if it doesn't exist (persist context!)
    if not self.session_id:
      session = await self.session_service.create_session(
        app_name=self.app_name,
        user_id=self.user_id,
      )
      self.session_id = session.id

    # 2. Setup runner (ADK handles retries with defaults)
    runner = Runner(
      app_name=self.app_name,
      agent=self.agent,
      session_service=self.session_service,
    )

    final_response = "No response generated."
    message = types.Content(role="user", parts=[types.Part.from_text(text=task_description)])

    # 3. Run the agent (single attempt, ADK handles retries internally)
    try:
      async for event in runner.run_async(
        session_id=self.session_id,
        user_id=self.user_id,
        new_message=message
      ):
        if event.content and event.content.parts:
          for part in event.content.parts:
            if part.function_call:
              logger.info(f"    [{self.agent.name} Tool]: {part.function_call.name}")
            if part.text:
              final_response = part.text

      logger.info(f"{self.agent.name} completed task.")
      return final_response

    except Exception as e:
      logger.error(f"Error executing {self.agent.name}: {str(e)}")
      return f"Error: {self.agent.name} failed - {str(e)}"
