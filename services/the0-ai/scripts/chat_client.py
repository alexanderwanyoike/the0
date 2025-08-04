#!/usr/bin/env python3

import requests
import json
import uuid
from typing import Optional


class ChatClient:
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
        self.session_id = str(uuid.uuid4())
    
    def chat(self, message: str) -> dict:
        """Send a chat message to the agent."""
        try:
            response = requests.post(
                f"{self.base_url}/chat",
                json={
                    "message": message,
                    "session_id": self.session_id
                }
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return {"error": f"Request failed: {str(e)}"}
    
    def list_artifacts(self) -> list:
        """List all available artifacts."""
        try:
            response = requests.get(f"{self.base_url}/artifacts")
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return []
    
    def health_check(self) -> dict:
        """Check if the API is healthy."""
        try:
            response = requests.get(f"{self.base_url}/health")
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return {"error": f"Health check failed: {str(e)}"}


def main():
    client = ChatClient()
    
    print("🤖 the0 AI Agent Chat Client")
    print("Commands: 'quit' to exit, 'artifacts' to list artifacts, 'health' to check status")
    print(f"Session ID: {client.session_id}")
    print("-" * 60)
    
    # Check health first
    health = client.health_check()
    if "error" in health:
        print(f"❌ {health['error']}")
        return
    else:
        print(f"✅ {health.get('message', 'API is healthy')}")
        print("-" * 60)
    
    while True:
        try:
            user_input = input("\n💬 You: ").strip()
            
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("👋 Goodbye!")
                break
            
            if user_input.lower() == 'artifacts':
                artifacts = client.list_artifacts()
                if artifacts:
                    print("📁 Available artifacts:")
                    for artifact in artifacts:
                        print(f"  - {artifact}")
                else:
                    print("📁 No artifacts found")
                continue
            
            if user_input.lower() == 'health':
                health = client.health_check()
                print(f"🏥 Health: {health}")
                continue
            
            if not user_input:
                continue
            
            # Send chat message
            response = client.chat(user_input)
            
            if "error" in response:
                print(f"❌ Error: {response['error']}")
            else:
                print(f"\n🤖 Agent: {response.get('response', 'No response')}")
                
                artifacts = response.get('artifacts', [])
                if artifacts:
                    print(f"📁 Artifacts: {', '.join(artifacts)}")
        
        except KeyboardInterrupt:
            print("\n👋 Goodbye!")
            break
        except Exception as e:
            print(f"❌ Unexpected error: {str(e)}")


if __name__ == "__main__":
    main()