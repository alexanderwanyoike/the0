import operator
import logging
from typing import Literal
from langchain.messages import AnyMessage, SystemMessage, ToolMessage, HumanMessage
from langgraph.graph import StateGraph, START, END
from typing_extensions import TypedDict, Annotated
from IPython.display import Image, display
from langchain_google_genai import ChatGoogleGenerativeAI
from .tools import tools
import os

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Load system prompt from file

def get_prompt(filename: str) -> str:
    path = os.path.join(os.path.dirname(__file__), "prompts", filename)
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


system_prompt = get_prompt("system_prompt.md")
clarification_prompt = get_prompt("clarification_prompt.md")
plan_prompt = get_prompt("plan_prompt.md")
research_prompt = get_prompt("research_prompt.md")
synthesis_prompt = get_prompt("synthesis_prompt.md")

model = ChatGoogleGenerativeAI(model="gemini-2.5-flash", temperature=0, max_tokens=20192)

class MessagesState(TypedDict):
    messages: Annotated[list[AnyMessage], operator.add]
    plan_created: bool
    research_done: bool
    clarification_asked: bool

tools_by_name = {tool.name: tool for tool in tools}
model_with_tools = model.bind_tools(tools)

def llm_call(state: MessagesState) -> MessagesState:
    """LLM decision whether to use a tool or respond directly."""
    # Build context aware system message
    plan_created = state.get("plan_created", False)
    research_done = state.get("research_done", False)
    clarification_asked = state.get("clarification_asked", False)
    updates = {}

    if not clarification_asked:
        logger.debug("Entering Clarification Phase")
        prompt = (system_prompt + clarification_prompt)
        updates["clarification_asked"] = True
        response = model.invoke(
            [SystemMessage(content=prompt)] + state["messages"]
        )

        return {"messages": [response], **updates}
   
    elif clarification_asked and not plan_created:
        prompt = (system_prompt + plan_prompt)
        response = model_with_tools.invoke(
            [SystemMessage(content=prompt)] + state["messages"]
        )

        return {"messages": [response], **updates}
    elif plan_created and not research_done:
        prompt = (system_prompt + research_prompt)
        response = model_with_tools.invoke(
            [SystemMessage(content=prompt)] + state["messages"]
        )

        return {"messages": [response], **updates}
   
    else:
        prompt = (system_prompt + synthesis_prompt)

        response = model.invoke(
            [SystemMessage(content=prompt)] + state["messages"]
        )

        return {"messages": [response], **updates}
   


def tool_node(state: MessagesState) -> MessagesState:
    """Perform a tool call"""
    result = []
    updates = {}
    for tool_call in state["messages"][-1].tool_calls:
        tool = tools_by_name[tool_call['name']]
        observation = tool.invoke(tool_call['args'])
        result.append(
            ToolMessage(
                tool_call_id=tool_call['id'],
                content=str(observation)
            )
        )

        if tool_call['name'] == "create_plan":
            updates["plan_created"] = True
        if tool_call['name'] == "research":
            updates["research_done"] = True
    return {
        "messages": result,
        **updates
    }


def should_continue(state: MessagesState) -> Literal["tool_node", "wait_for_human", END]:
    """Decide if we should continue the loop or stop based upon whether the LLM made a tool call"""
    last_message = state["messages"][-1] 

    if getattr(last_message, "tool_calls", None):
        return "tool_node"

    return END


agent_builder = StateGraph(MessagesState)
agent_builder.add_node(
    "llm_call",
    llm_call,
)
agent_builder.add_node(
    "tool_node",
    tool_node,
)


agent_builder.add_edge(
    START,
    "llm_call",
)
agent_builder.add_conditional_edges(
    "llm_call",
    should_continue,
    ["tool_node", END],
)

agent_builder.add_edge(
    "tool_node",
    "llm_call",
)

agent = agent_builder.compile()

display(Image(agent.get_graph(xray=True).draw_mermaid_png()))
