#!/usr/bin/env python3
"""
Script to check artifacts in the InMemoryArtifactService
"""
import asyncio
import sys
import os

# Add the project root to the path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from google.adk.artifacts import InMemoryArtifactService
from api.agent_service import AgentService

async def check_artifacts():
    print("🔍 Checking artifacts in the system...")
    
    # Create agent service instance
    agent_service = AgentService()
    
    print(f"\n📋 Agent service artifact storage type: {type(agent_service.artifact_service)}")
    
    # Check if the artifact service has the artifacts attribute
    if hasattr(agent_service.artifact_service, 'artifacts'):
        storage = agent_service.artifact_service.artifacts
        print(f"📦 Internal storage type: {type(storage)}")
        print(f"📊 Number of storage keys: {len(storage)}")
        
        if storage:
            print("\n🗂️  All storage keys:")
            for i, key in enumerate(storage.keys(), 1):
                print(f"  {i}. {key}")
                
                # Get the artifact data
                artifact_data = storage[key]
                print(f"     Type: {type(artifact_data)}")
                print(f"     Length: {len(artifact_data) if hasattr(artifact_data, '__len__') else 'N/A'}")
                
                # If it's a list of Parts, show details
                if isinstance(artifact_data, list) and artifact_data:
                    for j, part in enumerate(artifact_data):
                        print(f"     Version {j}: {type(part)}")
                        if hasattr(part, 'text'):
                            preview = part.text[:100] + "..." if len(part.text) > 100 else part.text
                            print(f"       Content preview: {repr(preview)}")
        else:
            print("❌ No artifacts found in storage")
    else:
        print("❌ Artifact service doesn't have 'artifacts' attribute")
        print("Available attributes:")
        for attr in dir(agent_service.artifact_service):
            if not attr.startswith('_'):
                print(f"  - {attr}")
    
    # Test the API methods
    print("\n🔧 Testing API methods...")
    
    try:
        artifact_keys = await agent_service.list_artifact_keys()
        print(f"✅ list_artifact_keys() returned: {artifact_keys}")
    except Exception as e:
        print(f"❌ list_artifact_keys() failed: {e}")
    
    # Test with a sample filename if any artifacts exist
    if hasattr(agent_service.artifact_service, 'artifacts') and agent_service.artifact_service.artifacts:
        sample_key = list(agent_service.artifact_service.artifacts.keys())[0]
        # Extract filename from the key
        filename = sample_key.split('/')[-1] if '/' in sample_key else sample_key
        
        try:
            artifact = await agent_service.get_artifact(filename)
            if artifact:
                print(f"✅ get_artifact('{filename}') returned: {artifact.filename}")
                print(f"   Content length: {len(artifact.content)}")
            else:
                print(f"❌ get_artifact('{filename}') returned None")
        except Exception as e:
            print(f"❌ get_artifact('{filename}') failed: {e}")

if __name__ == "__main__":
    asyncio.run(check_artifacts())