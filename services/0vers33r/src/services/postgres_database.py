import psycopg2
import psycopg2.extras
import json
import os
import time
from typing import Dict, Any, Optional
from datetime import datetime


class PostgreSQLDatabase:
    """PostgreSQL database implementation for OSS the0 platform"""

    def __init__(self):
        self.connection_string = os.getenv(
            'DATABASE_URL', 
            'postgresql://the0:password@localhost:5432/the0'
        )
        self.connection = None
        self._connect()

    def _connect(self):
        """Establish database connection with retry logic"""
        max_retries = 10
        retry_delay = 5
        
        for attempt in range(max_retries):
            try:
                print(f"🔄 Attempting database connection (attempt {attempt + 1}/{max_retries})...")
                print(f"🔗 Connection string: {self.connection_string}")
                
                self.connection = psycopg2.connect(
                    self.connection_string,
                    cursor_factory=psycopg2.extras.RealDictCursor
                )
                self.connection.autocommit = False
                print("✅ Connected to PostgreSQL database")
                return
                
            except Exception as e:
                print(f"❌ Database connection attempt {attempt + 1} failed: {e}")
                if attempt < max_retries - 1:
                    print(f"⏳ Retrying in {retry_delay} seconds...")
                    time.sleep(retry_delay)
                else:
                    print(f"💥 Failed to connect to database after {max_retries} attempts")
                    raise e

    def _ensure_connection(self):
        """Ensure database connection is active"""
        if self.connection is None or self.connection.closed:
            self._connect()

    def update_bot_status(
        self, bot_id: str, status: str, review_data: Dict[str, Any]
    ) -> None:
        """
        Update custom bot status and analysis results in PostgreSQL
        
        Args:
            bot_id: The custom bot ID
            status: New status ('approved', 'declined', 'awaiting_human_review')
            review_data: Analysis results and metadata
        """
        self._ensure_connection()
        
        try:
            with self.connection.cursor() as cursor:
                # Replace SERVER_TIMESTAMP placeholder with actual timestamp
                if review_data.get("reviewedAt") == "SERVER_TIMESTAMP":
                    review_data["reviewedAt"] = datetime.utcnow().isoformat()

                # Update the custom bot record
                update_query = """
                    UPDATE custom_bots 
                    SET status = %s, 
                        review = %s,
                        updated_at = NOW()
                    WHERE id = %s
                """
                
                cursor.execute(update_query, (
                    status,
                    json.dumps(review_data),
                    bot_id
                ))
                
                if cursor.rowcount == 0:
                    raise ValueError(f"Custom bot not found: {bot_id}")
                
                self.connection.commit()
                print(f"✅ Updated bot {bot_id} status to {status}")
                
        except Exception as e:
            self.connection.rollback()
            print(f"❌ Failed to update bot status: {e}")
            raise e

    def get_bot_info(self, bot_id: str) -> Optional[Dict[str, Any]]:
        """Get custom bot information from database"""
        self._ensure_connection()
        
        try:
            with self.connection.cursor() as cursor:
                query = """
                    SELECT id, user_id, name, version, config, file_path, status, 
                           marketplace, created_at, updated_at, review
                    FROM custom_bots 
                    WHERE id = %s
                """
                
                cursor.execute(query, (bot_id,))
                result = cursor.fetchone()
                
                if result:
                    return dict(result)
                return None
                
        except Exception as e:
            print(f"❌ Failed to get bot info: {e}")
            raise e

    def get_bots_pending_review(self) -> list:
        """Get list of bots pending security review"""
        self._ensure_connection()
        
        try:
            with self.connection.cursor() as cursor:
                query = """
                    SELECT id, user_id, name, version, config, file_path, 
                           created_at, updated_at
                    FROM custom_bots 
                    WHERE status = 'pending_review'
                    ORDER BY created_at ASC
                """
                
                cursor.execute(query)
                results = cursor.fetchall()
                
                return [dict(row) for row in results]
                
        except Exception as e:
            print(f"❌ Failed to get pending bots: {e}")
            raise e

    def get_analysis_stats(self) -> Dict[str, int]:
        """Get analysis statistics"""
        self._ensure_connection()
        
        try:
            with self.connection.cursor() as cursor:
                query = """
                    SELECT status, COUNT(*) as count
                    FROM custom_bots 
                    WHERE status IN ('approved', 'declined', 'awaiting_human_review', 'pending_review')
                    GROUP BY status
                """
                
                cursor.execute(query)
                results = cursor.fetchall()
                
                stats = {
                    'approved': 0,
                    'declined': 0,
                    'awaiting_human_review': 0,
                    'pending_review': 0
                }
                
                for row in results:
                    stats[row['status']] = row['count']
                
                return stats
                
        except Exception as e:
            print(f"❌ Failed to get analysis stats: {e}")
            raise e

    def record_analysis_metrics(self, bot_id: str, metrics: Dict[str, Any]) -> None:
        """Record analysis performance metrics"""
        self._ensure_connection()
        
        try:
            with self.connection.cursor() as cursor:
                insert_query = """
                    INSERT INTO analysis_metrics 
                    (bot_id, analysis_duration, files_scanned, yara_matches, 
                     ai_score, total_score, created_at)
                    VALUES (%s, %s, %s, %s, %s, %s, NOW())
                    ON CONFLICT (bot_id) DO UPDATE SET
                        analysis_duration = EXCLUDED.analysis_duration,
                        files_scanned = EXCLUDED.files_scanned,
                        yara_matches = EXCLUDED.yara_matches,
                        ai_score = EXCLUDED.ai_score,
                        total_score = EXCLUDED.total_score,
                        created_at = EXCLUDED.created_at
                """
                
                cursor.execute(insert_query, (
                    bot_id,
                    metrics.get('analysis_duration', 0),
                    metrics.get('files_scanned', 0),
                    metrics.get('yara_matches', 0),
                    metrics.get('ai_score', 0),
                    metrics.get('total_score', 0)
                ))
                
                self.connection.commit()
                
        except Exception as e:
            # Don't fail analysis if metrics recording fails
            self.connection.rollback()
            print(f"⚠️ Failed to record analysis metrics: {e}")

    def close(self):
        """Close database connection"""
        if self.connection and not self.connection.closed:
            self.connection.close()
            print("🔌 Disconnected from PostgreSQL database")

    def __del__(self):
        """Cleanup database connection"""
        self.close()