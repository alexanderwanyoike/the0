#!/usr/bin/env python3
"""
0vers33r OSS Security Analysis Service

A standalone microservice that listens for custom bot events via NATS,
analyzes code for security issues using YARA rules and AI, and publishes
analysis results back to the event stream.

This replaces the Firebase Cloud Function implementation with a scalable
cloud service architecture.
"""

import asyncio
import logging
import os
import signal
import sys
from typing import Dict, Any

from src.analyzer import The0vers33r
from src.services.minio_storage import MinIOStorageClient
from src.services.postgres_database import PostgreSQLDatabase
from src.services.nats_client import NATSClient

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler('/var/log/0vers33r.log', mode='a') if os.path.exists('/var/log') else logging.NullHandler()
    ]
)

logger = logging.getLogger(__name__)


class The0vers33rService:
    """Main service class for 0vers33r security analysis"""

    def __init__(self):
        self.running = False
        self.nats_client = None
        self.analyzer = None
        
        # Initialize dependencies
        self.storage_client = MinIOStorageClient()
        self.database_client = PostgreSQLDatabase()

    async def initialize(self):
        """Initialize the service components"""
        try:
            logger.info("🚀 Initializing 0vers33r Security Analysis Service...")
            
            # Initialize NATS client
            self.nats_client = NATSClient()
            await self.nats_client.connect()
            
            # Initialize analyzer with OSS dependencies
            self.analyzer = The0vers33r(
                rules_directory="./yara_rules",
                storage_client=self.storage_client,
                database_client=self.database_client,
                enable_ai_analysis=True
            )
            
            logger.info("✅ 0vers33r service initialized successfully")
            
        except Exception as e:
            logger.error(f"❌ Failed to initialize 0vers33r service: {e}")
            raise e

    async def handle_custom_bot_event(self, event_data: Dict[str, Any]):
        """
        Handle custom bot analysis events
        
        Args:
            event_data: Event data containing bot information
        """
        try:
            event_type = event_data.get("type")
            bot_id = event_data.get("botId")
            
            logger.info(f"🕵️ Processing {event_type} event for bot {bot_id}")
            
            if event_type == "custom-bot.submitted":
                await self.analyze_bot(event_data)
            else:
                logger.warning(f"⚠️ Unknown event type: {event_type}")
                
        except Exception as e:
            logger.error(f"❌ Error handling event: {e}")
            
            # Publish error event if bot_id is available
            bot_id = event_data.get("botId")
            if bot_id and self.nats_client:
                await self.nats_client.publish_analysis_error(
                    bot_id, 
                    str(e),
                    {"event_type": event_data.get("type"), "error_details": str(e)}
                )

    async def analyze_bot(self, event_data: Dict[str, Any]):
        """
        Analyze a custom bot for security issues
        
        Args:
            event_data: Event data containing bot analysis request
        """
        bot_id = event_data.get("botId")
        user_id = event_data.get("userId")
        file_path = event_data.get("filePath")
        config = event_data.get("config", {})
        
        try:
            logger.info(f"🔍 Starting analysis for bot {bot_id}")
            
            # Prepare bot data in expected format
            bot_data = {
                "id": bot_id,
                "name": event_data.get("name"),
                "userId": user_id,
                "filePath": file_path,
                "config": config,
                "status": "pending_review"
            }
            
            # Run the analysis
            status, analysis_results = self.analyzer.analyze_bot(bot_data)

            # Auto-approve all bots (override analysis result)
            if os.getenv("AUTO_APPROVE_BOTS", "false").lower() == "true":
                status = "approved"
                logger.info(f"🟢 Auto-approving bot {bot_id} (AUTO_APPROVE_BOTS enabled)")

            # Update database with results
            self.database_client.update_bot_status(bot_id, status, analysis_results)
            
            # Publish analysis result event
            await self.nats_client.publish_analysis_result(bot_id, status, analysis_results)
            
            # Log analysis summary
            score = analysis_results.get("score", 0)
            threat_level = analysis_results.get("threatSummary", {}).get("threatLevel", "unknown")
            files_scanned = len(analysis_results.get("filesScanned", []))
            
            status_emoji = {
                "approved": "🟢",
                "awaiting_human_review": "🟡", 
                "declined": "🔴"
            }
            
            logger.info(
                f"{status_emoji.get(status, '❓')} Bot {bot_id} analysis complete: "
                f"Status={status}, Score={score}/5, Threat={threat_level}, Files={files_scanned}"
            )
            
            # Record analysis metrics (optional, won't fail analysis if it fails)
            try:
                metrics = {
                    "analysis_duration": analysis_results.get("analysisDuration", 0),
                    "files_scanned": files_scanned,
                    "yara_matches": len(analysis_results.get("yaraMatches", [])),
                    "ai_score": analysis_results.get("aiAnalysis", {}).get("score", 0),
                    "total_score": score
                }
                self.database_client.record_analysis_metrics(bot_id, metrics)
            except Exception as metrics_error:
                logger.warning(f"⚠️ Failed to record metrics for {bot_id}: {metrics_error}")
                
        except Exception as e:
            logger.error(f"❌ Analysis failed for bot {bot_id}: {e}")
            
            # Update database with error status
            error_analysis = {
                "reviewedAt": "SERVER_TIMESTAMP",
                "reviewedBy": "0vers33r_oss",
                "version": "v3.0-oss",
                "score": 5,  # Max score for errors (triggers human review)
                "issues": ["analysis_error"],
                "error": str(e),
                "filesScanned": []
            }
            
            try:
                self.database_client.update_bot_status(
                    bot_id, 
                    "awaiting_human_review", 
                    error_analysis
                )
            except Exception as db_error:
                logger.error(f"💥 Failed to update error status for {bot_id}: {db_error}")
            
            # Publish error event
            await self.nats_client.publish_analysis_error(bot_id, str(e))

    async def start(self):
        """Start the service and begin listening for events"""
        self.running = True
        
        try:
            # Subscribe to custom bot events
            await self.nats_client.subscribe(
                subject="custom-bot.submitted",
                callback=self.handle_custom_bot_event,
                queue_group="0vers33r-workers"  # Load balancing across multiple instances
            )
            
            logger.info("📡 0vers33r service started, listening for events...")
            
            # Keep the service running
            while self.running:
                await asyncio.sleep(1)
                
                # Health check
                if not self.nats_client.is_connected():
                    logger.error("❌ Lost connection to NATS, attempting reconnect...")
                    await self.nats_client.connect()
                    
        except Exception as e:
            logger.error(f"❌ Service error: {e}")
            raise e

    async def stop(self):
        """Stop the service gracefully"""
        logger.info("🛑 Stopping 0vers33r service...")
        self.running = False
        
        if self.nats_client:
            await self.nats_client.disconnect()
            
        if self.database_client:
            self.database_client.close()
            
        logger.info("✅ 0vers33r service stopped")


# Global service instance
service = The0vers33rService()


async def main():
    """Main entry point"""
    try:
        # Initialize and start the service
        await service.initialize()
        await service.start()
        
    except KeyboardInterrupt:
        logger.info("🔄 Received interrupt signal")
    except Exception as e:
        logger.error(f"💥 Service crashed: {e}")
        sys.exit(1)
    finally:
        await service.stop()


def signal_handler(signum, frame):
    """Handle shutdown signals"""
    logger.info(f"🔄 Received signal {signum}")
    asyncio.create_task(service.stop())


if __name__ == "__main__":
    # Register signal handlers for graceful shutdown
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Run the service
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("👋 Service terminated by user")
    except Exception as e:
        logger.error(f"💥 Unhandled exception: {e}")
        sys.exit(1)