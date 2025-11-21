import logging
from google.adk.tools.tool_context import ToolContext

logger = logging.getLogger(__name__)

def task_complete(result: str, tool_context: ToolContext) -> str:
    """
    Signals that the assigned task is fully complete.
    
    Args:
        result (str): A summary of the completed task and any artifacts created.
        tool_context (ToolContext): The context for the tool execution.
        
    Returns:
        str: Confirmation message.
    """
    logger.info(f"Task completed with result: {result}")
    
    # Set the escalate flag to signal the LoopAgent to stop
    if hasattr(tool_context, 'actions'):
        tool_context.actions.escalate = True
    else:
        logger.warning("ToolContext does not have 'actions' attribute. Loop might not terminate automatically.")
        
    return f"Task marked as complete. Result: {result}"
