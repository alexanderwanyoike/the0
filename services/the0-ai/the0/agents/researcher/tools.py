import json
import logging
import os
from langchain.tools import tool
from tavily import TavilyClient
from dotenv import load_dotenv
from langchain_google_genai import ChatGoogleGenerativeAI

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

from langchain.messages import SystemMessage

load_dotenv()

tavily_client = TavilyClient(api_key=os.getenv("TAVILY_API_KEY"))
planner_model = ChatGoogleGenerativeAI(model="gemini-2.5-flash", temperature=0)

@tool
def create_plan(research_topic: str) -> str:
    """Create a research plan with 3 specific subtopics to investigate for a given research topic."""
    prompt = (f"""
    You are an expert research planner. Given the research topic "{research_topic}", create a detailed research plan that includes 5 specific subtopics to investigate. Each subtopic should be relevant and contribute to a comprehensive understanding of the main topic.

    Format the output as follows:
    1. Subtopic 1: [Description]
    2. Subtopic 2: [Description]
    3. Subtopic 3: [Description]
    """)
    response = planner_model.invoke(prompt)
    return response.content.strip()

@tool
def research(query: str) -> str:
    """
    Performs a comprehensive web search to gather contextual information on a given research query using Tavily.
    Reads and summarizes multiple sources HTML pdf etc.
    Args:
        query (str): The research query to search for.
    Returns:
        str: A summary of the findings from the research. 
    """
    try:
        MAX_QUERY_LENGTH = 400
        if len(query) <= MAX_QUERY_LENGTH:
            results = tavily_client.get_search_context(
                query=query,
                max_results=20,
                search_depth="advanced"
            )
            return results
        logger.info(f"Query length {len(query)} exceeds maximum of {MAX_QUERY_LENGTH}. Extracting key points and re querying each.")
        extraction_prompt = (f"""
        You are an expert research assistant. Given the following research query, extract the most important key points (20 words each) that need further investigation: "{query}".
        Format the response ONLY as json array with each key point as an item and NO additional text.
        Example:
        [
            "Key Point 1",
            "Key Point 2",
            "Key Point 3"
        ]
        Only return the raw json array do not include ```json ...``` blocks.
        """)

        extraction_response = planner_model.invoke(extraction_prompt)
        key_points = json.loads(extraction_response.content)
        logger.info(f"Extracted {len(key_points)} key points for further research.")
        all_results = []
        for search_point in key_points:
            try:
                results = tavily_client.get_search_context(
                    query=search_point,
                    max_results=10,
                    search_depth="advanced"
                )
                all_results.append(f"Results for key point '{search_point}':\n{results}\n")
            except Exception as e:
                logger.error(f"Error researching key point '{search_point}': {str(e)}")

        if not all_results:
            return "No results found for any of the key points."
        return "\n".join(all_results)

    except Exception as e:
        return f"An error occurred while performing the research: {str(e)}"


tools = [
    create_plan,
    research,
]