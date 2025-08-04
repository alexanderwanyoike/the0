import io
import tempfile
import unittest
import zipfile
from typing import Dict
from unittest.mock import Mock

from src.analyzer import The0vers33r
from src.rule_manager import RuleManager
from src.tests.mocks import MockStorageClient, MockDatabaseClient


class Test0VERS33RRefactored(unittest.TestCase):
    """
    Test suite for refactored 0VERS33R with dependency injection
    """

    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.mock_storage = MockStorageClient()
        self.mock_database = MockDatabaseClient()

        # Create a minimal rule manager for testing
        self.rule_manager = RuleManager("yara_rules")

        # Mock the rule manager to avoid dependency on YARA files
        self.rule_manager.load_and_compile_rules = Mock(return_value=True)
        self.rule_manager.scan = Mock(return_value=(0, []))

    def tearDown(self):
        """Clean up test environment"""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_analyzer_initialization_with_dependencies(self):
        """Test analyzer initialization with injected dependencies"""
        analyzer = The0vers33r(
            rules_directory="test_rules",
            storage_client=self.mock_storage,
            database_client=self.mock_database,
        )

        self.assertEqual(analyzer.storage_client, self.mock_storage)
        self.assertEqual(analyzer.database_client, self.mock_database)

    def test_analyzer_initialization_without_dependencies(self):
        """Test analyzer initialization without dependencies (for local use)"""
        analyzer = The0vers33r(rules_directory="test_rules")

        self.assertIsNone(analyzer.storage_client)
        self.assertIsNone(analyzer.database_client)

    def test_basic_validation_success(self):
        """Test basic validation with valid bot data"""
        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        valid_bot_data = {
            "name": "Test Bot",
            "config": {
                "name": "Test",
                "description": "Test bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/bot.zip",
            "userId": "user123",
        }

        result = analyzer._basic_validation(valid_bot_data)
        self.assertTrue(result)

    def test_basic_validation_failure(self):
        """Test basic validation with invalid bot data"""
        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        invalid_bot_data = {
            "name": "Test Bot",
            "config": {},
            # Missing gcsPath and userId
        }

        result = analyzer._basic_validation(invalid_bot_data)
        self.assertFalse(result)

    def test_analyze_bot_with_invalid_structure(self):
        """Test analysis of bot with invalid structure"""
        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        invalid_bot_data = {"name": "Incomplete Bot"}

        status, review_data = analyzer.analyze_bot(invalid_bot_data)

        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)
        self.assertIn("invalid_bot_structure", review_data["issues"])

    def test_analyze_bot_with_safe_code(self):
        """Test analysis of bot with safe code"""
        # Create a safe zip file
        safe_code = b"""
import ccxt
exchange = ccxt.binance()
balance = exchange.fetch_balance()
print(f"Balance: {balance}")
"""
        zip_content = self._create_test_zip({"bot.py": safe_code})
        self.mock_storage.add_file("gs://test/safe-bot.zip", zip_content)

        # Mock rule manager to return safe results
        self.rule_manager.scan = Mock(return_value=(0, []))

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )
        analyzer.rule_manager = self.rule_manager

        bot_data = {
            "name": "Safe Bot",
            "config": {
                "name": "Safe",
                "description": "Safe bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/safe-bot.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        self.assertEqual(status, "approved")
        self.assertEqual(review_data["score"], 0)

    def test_analyze_bot_with_malicious_code(self):
        """Test analysis of bot with malicious code"""
        # Create a malicious zip file
        malicious_code = b"""
import os
user_input = input("Enter command: ")
os.system(user_input)  # DANGEROUS!
"""
        zip_content = self._create_test_zip({"malicious.py": malicious_code})
        self.mock_storage.add_file("gs://test/malicious-bot.zip", zip_content)

        # Mock rule manager to return dangerous results
        mock_matches = [
            {"rule": "os_system_command", "severity": "critical"},
            {"rule": "user_input_execution", "severity": "high"},
        ]
        self.rule_manager.scan = Mock(return_value=(5, mock_matches))

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )
        analyzer.rule_manager = self.rule_manager

        bot_data = {
            "name": "Malicious Bot",
            "config": {
                "name": "Evil",
                "description": "Evil bot",
                "type": "trading",
                "author": "Hacker",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/malicious-bot.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)
        self.assertEqual(len(review_data["yaraMatches"]), 2)
        self.assertEqual(review_data["threatSummary"]["threatLevel"], "critical")

    def test_analyze_and_update_bot_success(self):
        """Test successful analysis and database update"""
        # Setup safe code
        safe_code = b"import ccxt\nprint('Safe trading bot')"
        zip_content = self._create_test_zip({"bot.py": safe_code})
        self.mock_storage.add_file("gs://test/bot.zip", zip_content)

        self.rule_manager.scan = Mock(return_value=(1, []))

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )
        analyzer.rule_manager = self.rule_manager

        bot_data = {
            "name": "Test Bot",
            "config": {
                "name": "Test",
                "description": "Test bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/bot.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_and_update_bot("bot123", bot_data)

        # Check analysis results
        self.assertEqual(status, "approved")
        self.assertEqual(review_data["score"], 1)

        # Check database was updated
        last_update = self.mock_database.get_last_update()
        self.assertIsNotNone(last_update)
        self.assertEqual(last_update["bot_id"], "bot123")
        self.assertEqual(last_update["status"], "approved")

    def test_storage_error_handling(self):
        """Test handling of storage errors"""
        # Setup storage to raise an exception
        self.mock_storage.set_exception(FileNotFoundError("File not found"))

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        bot_data = {
            "name": "Test Bot",
            "config": {
                "name": "Test",
                "description": "Test bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/missing.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)
        self.assertTrue(
            any("storage_error" in issue for issue in review_data["issues"])
        )

    def test_database_error_handling(self):
        """Test handling of database errors"""
        # Setup safe analysis
        safe_code = b"print('Hello world')"
        zip_content = self._create_test_zip({"bot.py": safe_code})
        self.mock_storage.add_file("gs://test/bot.zip", zip_content)
        self.rule_manager.scan = Mock(return_value=(0, []))

        # Setup database to raise an exception
        self.mock_database.set_exception(Exception("Database connection failed"))

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )
        analyzer.rule_manager = self.rule_manager

        bot_data = {
            "name": "Test Bot",
            "config": {
                "name": "Test",
                "description": "Test bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/bot.zip",
            "userId": "user123",
        }

        # Analysis should succeed even if database update fails
        with self.assertRaises(Exception):
            analyzer.analyze_and_update_bot("bot123", bot_data)

        # But analyze_bot should still work
        status, review_data = analyzer.analyze_bot(bot_data)
        self.assertEqual(status, "approved")

    def test_threat_summary_generation(self):
        """Test threat summary generation with various matches"""
        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        # Test with no matches
        summary = analyzer._generate_threat_summary([])
        self.assertEqual(summary["threatLevel"], "none")
        self.assertEqual(summary["totalMatches"], 0)

        # Test with critical matches
        critical_matches = [
            {"rule": "code_injection_eval", "severity": "critical"},
            {"rule": "os_system_call", "severity": "critical"},
            {"rule": "subprocess_dangerous", "severity": "high"},
        ]
        summary = analyzer._generate_threat_summary(critical_matches)
        self.assertEqual(summary["threatLevel"], "critical")
        self.assertEqual(summary["severityCounts"]["critical"], 2)
        self.assertEqual(summary["severityCounts"]["high"], 1)
        self.assertEqual(len(summary["criticalRules"]), 2)

    def test_file_scanning_logic(self):
        """Test file scanning logic with different file types"""
        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        # Test which files should be scanned
        self.assertTrue(analyzer._should_scan_file("bot.py"))
        self.assertTrue(analyzer._should_scan_file("config.json"))
        self.assertTrue(analyzer._should_scan_file("script.js"))
        self.assertTrue(analyzer._should_scan_file("readme.md"))
        self.assertTrue(analyzer._should_scan_file("setup.sh"))

        # Test which files should NOT be scanned
        self.assertFalse(analyzer._should_scan_file("image.png"))
        self.assertFalse(analyzer._should_scan_file("data.csv"))
        self.assertFalse(analyzer._should_scan_file("binary.exe"))
        self.assertFalse(analyzer._should_scan_file("archive.tar.gz"))

    def test_corrupted_zip_handling(self):
        """Test handling of corrupted ZIP files"""
        # Add corrupted ZIP content
        corrupted_zip = b"Not a valid ZIP file content"
        self.mock_storage.add_file("gs://test/corrupted.zip", corrupted_zip)

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )

        bot_data = {
            "name": "Corrupted Bot",
            "config": {
                "name": "Corrupted",
                "description": "Corrupted bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/corrupted.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)
        self.assertIn("corrupted_zip", review_data["issues"])

    def test_no_storage_client_handling(self):
        """Test behavior when no storage client is provided"""
        analyzer = The0vers33r(
            storage_client=None, database_client=self.mock_database  # No storage client
        )

        bot_data = {
            "name": "Test Bot",
            "config": {
                "name": "Test",
                "description": "Test bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/bot.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        # Should still validate basic structure but skip file scanning
        self.assertEqual(status, "approved")  # No files scanned, so no issues found
        self.assertEqual(review_data["score"], 0)

    def test_early_exit_on_critical_findings(self):
        """Test that scanning stops early when critical threats are found"""
        # Create ZIP with multiple files
        files = {
            "safe1.py": b'print("safe")',
            "dangerous.py": b"eval(user_input)  # Critical threat",
            "safe2.py": b'print("also safe")',
            "safe3.py": b'print("never scanned")',
        }
        zip_content = self._create_test_zip(files)
        self.mock_storage.add_file("gs://test/mixed.zip", zip_content)

        # Mock rule manager to return critical score for dangerous.py
        def mock_scan(content, filename, runtime=None):
            if b"eval" in content:
                return 5, [{"rule": "critical_eval", "severity": "critical"}]
            return 0, []

        self.rule_manager.scan = Mock(side_effect=mock_scan)

        analyzer = The0vers33r(
            storage_client=self.mock_storage, database_client=self.mock_database
        )
        analyzer.rule_manager = self.rule_manager

        bot_data = {
            "name": "Mixed Bot",
            "config": {
                "name": "Mixed",
                "description": "Mixed bot",
                "type": "trading",
                "author": "Test",
                "entrypoints": {},
            },
            "gcsPath": "gs://test/mixed.zip",
            "userId": "user123",
        }

        status, review_data = analyzer.analyze_bot(bot_data)

        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)

        # Should have scanned files until critical threat found
        # The exact number depends on ZIP file ordering, but should be less than all files
        self.assertLessEqual(len(review_data["filesScanned"]), 4)

    def _create_test_zip(self, files: Dict[str, bytes]) -> bytes:
        """Helper method to create test ZIP files"""
        zip_buffer = io.BytesIO()
        with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zip_file:
            for filename, content in files.items():
                zip_file.writestr(filename, content)
        return zip_buffer.getvalue()
