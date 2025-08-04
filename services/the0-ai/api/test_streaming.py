#!/usr/bin/env python3
"""
Quick test script to verify the streaming endpoint works.
Run this with: python test_streaming.py
"""

import asyncio
import json
from api.models import ChatRequest, StreamChunk
from api.agent_service import AgentService

async def test_streaming():
    """Test the streaming functionality locally."""
    print("Testing streaming functionality...")
    
    # Create agent service
    agent_service = AgentService()
    
    # Test message
    test_message = "Hello, can you help me create a simple trading bot?"
    
    print(f"Sending message: {test_message}")
    print("Streaming response:")
    print("-" * 50)
    
    try:
        async for chunk in agent_service.chat_stream(test_message):
            print(f"Chunk type: {chunk.type}")
            if chunk.content:
                print(f"Content: {chunk.content}")
            if chunk.artifacts:
                print(f"Artifacts: {chunk.artifacts}")
            if chunk.session_id:
                print(f"Session ID: {chunk.session_id}")
            if chunk.error:
                print(f"Error: {chunk.error}")
            print("-" * 30)
            
    except Exception as e:
        print(f"Error during streaming: {e}")

if __name__ == "__main__":
    asyncio.run(test_streaming())