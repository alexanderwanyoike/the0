import asyncio
import json
import os
from typing import Dict, Any, Callable, Optional
from nats.aio.client import Client as NATS
from nats.aio.errors import ErrConnectionClosed, ErrTimeout, ErrNoServers


class NATSClient:
    """NATS messaging client for OSS the0 platform"""

    def __init__(self):
        self.nats_url = os.getenv('NATS_URL', 'nats://localhost:4222')
        self.nc = NATS()
        self.connected = False

    async def connect(self):
        """Connect to NATS server"""
        try:
            await self.nc.connect(self.nats_url)
            self.connected = True
            print(f"✅ Connected to NATS server at {self.nats_url}")
        except Exception as e:
            print(f"❌ Failed to connect to NATS: {e}")
            raise e

    async def disconnect(self):
        """Disconnect from NATS server"""
        if self.connected:
            await self.nc.close()
            self.connected = False
            print("🔌 Disconnected from NATS server")

    async def publish_event(self, subject: str, event: Dict[str, Any]) -> None:
        """
        Publish an event to NATS
        
        Args:
            subject: NATS subject (e.g., 'custom-bot.approved')
            event: Event data dictionary
        """
        if not self.connected:
            raise RuntimeError("Not connected to NATS server")

        try:
            event_json = json.dumps(event).encode()
            await self.nc.publish(subject, event_json)
            print(f"📤 Published event to {subject}")
        except Exception as e:
            print(f"❌ Failed to publish event to {subject}: {e}")
            raise e

    async def subscribe(
        self, 
        subject: str, 
        callback: Callable[[Dict[str, Any]], None],
        queue_group: Optional[str] = None
    ) -> None:
        """
        Subscribe to NATS events
        
        Args:
            subject: NATS subject pattern (e.g., 'custom-bot.*')
            callback: Function to handle received messages
            queue_group: Optional queue group for load balancing
        """
        if not self.connected:
            raise RuntimeError("Not connected to NATS server")

        async def message_handler(msg):
            try:
                # Decode and parse the message
                data = json.loads(msg.data.decode())
                
                # Call the callback function
                if asyncio.iscoroutinefunction(callback):
                    await callback(data)
                else:
                    callback(data)
                    
                print(f"📥 Processed message from {msg.subject}")
                
            except Exception as e:
                print(f"❌ Error processing message from {msg.subject}: {e}")

        try:
            if queue_group:
                await self.nc.subscribe(subject, queue=queue_group, cb=message_handler)
                print(f"📨 Subscribed to {subject} with queue group {queue_group}")
            else:
                await self.nc.subscribe(subject, cb=message_handler)
                print(f"📨 Subscribed to {subject}")
        except Exception as e:
            print(f"❌ Failed to subscribe to {subject}: {e}")
            raise e

    async def publish_analysis_result(
        self, 
        bot_id: str, 
        status: str, 
        analysis_data: Dict[str, Any]
    ) -> None:
        """
        Publish analysis result event
        
        Args:
            bot_id: Custom bot ID
            status: Analysis result ('approved', 'declined', 'awaiting_human_review')
            analysis_data: Analysis results and metadata
        """
        event_type = f"custom-bot.{status.replace('_', '-')}"
        
        event = {
            "type": event_type,
            "botId": bot_id,
            "status": status,
            "analysis": analysis_data,
            "timestamp": analysis_data.get("reviewedAt"),
            "service": "0vers33r"
        }

        # Add specific fields based on status
        if status == "declined":
            event["reasons"] = analysis_data.get("issues", [])
        elif status == "approved":
            event["score"] = analysis_data.get("score", 0)

        await self.publish_event(event_type, event)

    async def publish_analysis_error(
        self, 
        bot_id: str, 
        error_message: str, 
        error_details: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Publish analysis error event
        
        Args:
            bot_id: Custom bot ID
            error_message: Error description
            error_details: Additional error information
        """
        event = {
            "type": "custom-bot.analysis-failed",
            "botId": bot_id,
            "error": error_message,
            "details": error_details or {},
            "timestamp": asyncio.get_event_loop().time(),
            "service": "0vers33r"
        }

        await self.publish_event("custom-bot.analysis-failed", event)

    async def health_check(self) -> bool:
        """Check if NATS connection is healthy"""
        try:
            if not self.connected:
                return False
            
            # Try to publish a test message
            await self.nc.publish("health.check", b"ping")
            return True
        except Exception:
            return False

    def is_connected(self) -> bool:
        """Check if connected to NATS"""
        return self.connected and not self.nc.is_closed

    async def __aenter__(self):
        """Async context manager entry"""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit"""
        await self.disconnect()