import operator
from typing import Literal
from langchain.messages import AnyMessage, SystemMessage, ToolMessage
from langgraph.graph import StateGraph, START, END
from typing_extensions import TypedDict, Annotated
from IPython.display import Image, display
from langchain_google_genai import ChatGoogleGenerativeAI
from .tools import tools

model = ChatGoogleGenerativeAI(model="gemini-2.5-flash", temperature=0)

class MessagesState(TypedDict):
    messages: Annotated[list[AnyMessage], operator.add]
    llm_calls: int 

tools_by_name = {tool.name: tool for tool in tools}
model_with_tools = model.bind_tools(tools)

def llm_call(state: MessagesState) -> MessagesState:
    """LLM decision whether to use a tool or respond directly."""
    return {
        "messages": [
            model_with_tools.invoke(
                [
                    SystemMessage(content="You are an intelligent agent that can use tools to help answer user questions."),
                ] + state["messages"]
            )
        ],
        "llm_calls": state.get("llm_calls", 0) + 1,
    }


def tool_node(state: MessagesState) -> MessagesState:
    """Perform a tool call"""
    result = []
    for tool_call in state["messages"][-1].tool_calls:
        tool = tools_by_name[tool_call['name']]
        observation = tool.invoke(tool_call['args'])
        result.append(
            ToolMessage(
                tool_call_id=tool_call['id'],
                content=observation
            )
        )
    return {
        "messages": result,
    }


def should_continue(state: MessagesState) -> Literal["tool_node", END]:
    """Decide if we should continue the loop or stop based upon whether the LLM made a tool call"""

    messages = state["messages"]
    last_message = messages[-1] 

    if last_message.tool_calls:
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
