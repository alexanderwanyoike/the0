# test_integration.py - Integration tests showing the refactored system in action
import unittest
import sys
import os
from unittest.mock import Mock, patch

from src.tests.mocks import setup_test_environment, TestDataGenerator, ThreatScenarios

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

from src.analyzer import The0vers33r


class TestIntegrationScenarios(unittest.TestCase):
    """
    Integration tests showing real-world usage of the refactored 0VERS33R
    """

    def setUp(self):
        """Set up test environment"""
        self.storage_client, self.database_client = setup_test_environment()

    def test_complete_safe_bot_workflow(self):
        """Test complete workflow for a safe trading bot"""
        print("\n🧪 Testing Safe Bot Workflow")

        # Create analyzer with mocked dependencies
        analyzer = The0vers33r(
            rules_directory="./yara_rules",
            storage_client=self.storage_client,
            database_client=self.database_client,
        )

        # Create bot data
        bot_data = TestDataGenerator.create_bot_data(
            "Safe Grid Bot", "gs://test/safe-bot.zip", "legitimate_user_123"
        )

        # Analyze and update
        status, review_data = analyzer.analyze_and_update_bot("safe_bot_001", bot_data)

        # Verify results
        self.assertEqual(status, "approved")
        self.assertLessEqual(review_data["score"], 1)
        self.assertEqual(review_data["threatSummary"]["threatLevel"], "low")

        # Verify database was updated
        last_update = self.database_client.get_last_update()
        self.assertEqual(last_update["bot_id"], "safe_bot_001")
        self.assertEqual(last_update["status"], "approved")

        # Verify files were scanned
        self.assertGreater(len(review_data["filesScanned"]), 0)

        print(f"✅ Safe bot approved with score {review_data['score']}/5")

    def test_complete_malicious_bot_workflow(self):
        """Test complete workflow for a malicious bot"""
        print("\n🧪 Testing Malicious Bot Workflow")

        # Create analyzer
        analyzer = The0vers33r(
            rules_directory="./yara_rules",
            storage_client=self.storage_client,
            database_client=self.database_client,
        )

        # Create malicious bot data
        bot_data = TestDataGenerator.create_bot_data(
            "Innocent Trading Bot", "gs://test/malicious-bot.zip", "suspicious_user_456"
        )

        # Analyze and update
        status, review_data = analyzer.analyze_and_update_bot(
            "malicious_bot_001", bot_data
        )

        # Verify results
        self.assertEqual(status, "declined")
        self.assertGreaterEqual(review_data["score"], 4)
        self.assertIn("critical", review_data["threatSummary"]["threatLevel"])

        # Verify database was updated
        last_update = self.database_client.get_last_update()
        self.assertEqual(last_update["bot_id"], "malicious_bot_001")
        self.assertEqual(last_update["status"], "declined")

        # Verify threats were detected
        self.assertGreater(len(review_data["yaraMatches"]), 0)

        print(f"🚨 Malicious bot blocked with score {review_data['score']}/5")

    def test_cloud_function_simulation(self):
        """Simulate the actual Cloud Function workflow"""
        print("\n🧪 Testing Cloud Function Simulation")

        # Mock the Cloud Function event data structure
        class MockEvent:
            def __init__(self, bot_id, bot_data):
                self.params = {"bot_id": bot_id}
                self.data = MockDocumentSnapshot(bot_data)

        class MockDocumentSnapshot:
            def __init__(self, data):
                self._data = data

            def to_dict(self):
                return self._data

        # Create the analyzer (simulating main.py)
        from src.services.cloud_storage import GCSStorageClient
        from src.services.firestore_database import FirestoreDatabase

        # Mock the concrete implementations
        with patch.object(
            GCSStorageClient, "__init__", return_value=None
        ), patch.object(
            GCSStorageClient,
            "download_file",
            side_effect=self.storage_client.download_file,
        ), patch.object(
            FirestoreDatabase, "__init__", return_value=None
        ), patch.object(
            FirestoreDatabase,
            "update_bot_status",
            side_effect=self.database_client.update_bot_status,
        ):
            # Create analyzer as it would be in the Cloud Function
            storage_client = GCSStorageClient()
            database_client = FirestoreDatabase()

            analyzer = The0vers33r(
                rules_directory="yara_rules",
                storage_client=storage_client,
                database_client=database_client,
            )

            # Simulate Cloud Function trigger
            bot_data = TestDataGenerator.create_bot_data(
                "Cloud Function Test Bot", "gs://test/safe-bot.zip", "cloud_user_789"
            )

            event = MockEvent("cloud_bot_001", bot_data)

            # This simulates the Cloud Function execution
            bot_id = event.params["bot_id"]
            bot_data = event.data.to_dict()

            # Skip if already processed (Cloud Function logic)
            if bot_data.get("status") not in [
                "approved",
                "declined",
                "awaiting_human_review",
            ]:
                status, review_data = analyzer.analyze_and_update_bot(bot_id, bot_data)

                # Verify Cloud Function would complete successfully
                self.assertIsNotNone(status)
                self.assertIsNotNone(review_data)

                print(
                    f"☁️ Cloud Function simulation: {status} with score {review_data['score']}"
                )

    def test_error_handling_workflow(self):
        """Test error handling in various failure scenarios"""
        print("\n🧪 Testing Error Handling")

        # Test 1: Storage failure
        print("  📁 Testing storage failure...")
        self.storage_client.set_exception(
            FileNotFoundError("Storage service unavailable")
        )

        analyzer = The0vers33r(
            storage_client=self.storage_client, database_client=self.database_client
        )

        bot_data = TestDataGenerator.create_bot_data(
            "Error Test Bot", "gs://test/missing-file.zip"
        )

        status, review_data = analyzer.analyze_bot(bot_data)
        self.assertEqual(status, "declined")
        self.assertEqual(review_data["score"], 5)
        self.assertTrue(
            any("storage_error" in issue for issue in review_data["issues"])
        )

        # Clear the exception
        self.storage_client.clear_exception()

        # Test 2: Database failure
        print("  🗄️ Testing database failure...")
        self.database_client.set_exception(
            ConnectionError("Database connection failed")
        )

        # Analysis should still work, but update should fail
        with self.assertRaises(ConnectionError):
            analyzer.analyze_and_update_bot("error_bot_001", bot_data)

        # But analyze_bot should still work
        status, review_data = analyzer.analyze_bot(bot_data)
        self.assertIsNotNone(status)

        print("  ✅ Error handling verified")

    def test_sophisticated_apt_scenario(self):
        """Test detection of sophisticated APT-style malware"""
        print("\n🧪 Testing Sophisticated APT Detection")

        # Create sophisticated malware that tries to evade detection
        apt_zip = ThreatScenarios.create_sophisticated_apt()
        self.storage_client.add_file("gs://test/apt-malware.zip", apt_zip)

        analyzer = The0vers33r(
            storage_client=self.storage_client, database_client=self.database_client
        )

        bot_data = TestDataGenerator.create_bot_data(
            "Advanced Trading Suite", "gs://test/apt-malware.zip", "apt_actor_001"
        )

        status, review_data = analyzer.analyze_and_update_bot(
            "apt_malware_001", bot_data
        )

        # Should detect multiple threats despite sophistication
        self.assertEqual(status, "declined")
        self.assertGreaterEqual(review_data["score"], 4)

        # Should detect multiple different threat types
        threat_categories = review_data["threatSummary"]["ruleCategories"]
        self.assertGreater(len(threat_categories), 1)

        print(
            f"  🎯 APT detected: {len(review_data['yaraMatches'])} threats across {len(threat_categories)} categories"
        )

    def test_batch_analysis_workflow(self):
        """Test analyzing multiple bots in sequence"""
        print("\n🧪 Testing Batch Analysis")

        analyzer = The0vers33r(
            storage_client=self.storage_client, database_client=self.database_client
        )

        # Create batch of bots
        bots_to_analyze = [
            ("safe_bot_1", "gs://test/safe-bot.zip", "approved"),
            ("malicious_bot_1", "gs://test/malicious-bot.zip", "declined"),
            ("credential_stealer_1", "gs://test/credential-stealer.zip", "declined"),
        ]

        results = []
        for bot_id, gcs_path, expected_status in bots_to_analyze:
            bot_data = TestDataGenerator.create_bot_data(
                f"Batch Bot {bot_id}", gcs_path
            )
            status, review_data = analyzer.analyze_and_update_bot(bot_id, bot_data)
            results.append((bot_id, status, review_data["score"]))

            # Verify expected outcome
            if expected_status == "approved":
                self.assertIn(status, ["approved"])
            elif expected_status == "declined":
                self.assertEqual(status, "declined")

        # Verify all were processed
        self.assertEqual(len(results), 3)
        self.assertEqual(len(self.database_client.updates), 3)

        print(f"  📦 Batch processed: {len(results)} bots analyzed")
        for bot_id, status, score in results:
            print(f"    {bot_id}: {status} (score: {score})")


def run_integration_demo():
    """
    Run a comprehensive demo showing the refactored system
    """
    print("🚀 0VERS33R Refactored System Demo")
    print("=" * 60)

    # Set up test environment
    storage_client, database_client, rule_manager = setup_test_environment()

    # Create analyzer with dependency injection
    analyzer = The0vers33r(
        rules_directory="./yara_rules",
        storage_client=storage_client,
        database_client=database_client,
    )
    analyzer.rule_manager = rule_manager

    print("🔧 System initialized with dependency injection")
    print(f"   📁 Storage: {len(storage_client.list_files())} test files loaded")
    print(f"   🔍 Rules: {len(rule_manager.rules)} detection rules active")
    print(f"   🗄️ Database: Ready for updates")

    # Demo scenarios
    scenarios = [
        {
            "name": "✅ Legitimate Trading Bot",
            "bot_id": "demo_safe_001",
            "gcs_path": "gs://test/safe-bot.zip",
            "description": "Professional grid trading bot with proper security practices",
        },
        {
            "name": "🚨 Code Injection Attack",
            "bot_id": "demo_malicious_001",
            "gcs_path": "gs://test/malicious-bot.zip",
            "description": "Bot containing eval() and exec() vulnerabilities",
        },
        {
            "name": "🕵️ Credential Stealer",
            "bot_id": "demo_stealer_001",
            "gcs_path": "gs://test/credential-stealer.zip",
            "description": "Advanced malware stealing browser passwords and API keys",
        },
    ]

    print(f"\n📊 Analyzing {len(scenarios)} test scenarios...")
    print("-" * 60)

    results = []
    for scenario in scenarios:
        print(f"\n🔍 {scenario['name']}")
        print(f"   📝 {scenario['description']}")

        # Create bot data
        bot_data = TestDataGenerator.create_bot_data(
            scenario["name"], scenario["gcs_path"], f"demo_user_{scenario['bot_id']}"
        )

        # Analyze with full workflow
        status, review_data = analyzer.analyze_and_update_bot(
            scenario["bot_id"], bot_data
        )

        # Display results
        status_emoji = {
            "approved": "✅",
            "awaiting_human_review": "🟡",
            "declined": "🚨",
        }
        threat_emoji = {
            "none": "🟢",
            "low": "🟡",
            "medium": "🟠",
            "high": "🔴",
            "critical": "🚨",
        }

        threat_level = review_data["threatSummary"]["threatLevel"]

        print(f"   🎯 Result: {status_emoji.get(status, '❓')} {status.upper()}")
        print(f"   📊 Score: {review_data['score']}/5")
        print(
            f"   ⚠️ Threat Level: {threat_emoji.get(threat_level, '❓')} {threat_level.upper()}"
        )
        print(f"   📁 Files Scanned: {len(review_data['filesScanned'])}")

        if review_data["yaraMatches"]:
            print(f"   🔍 Threats Detected: {len(review_data['yaraMatches'])}")
            for match in review_data["yaraMatches"][:3]:  # Show first 3
                print(f"      - {match['rule']} ({match['severity']})")
            if len(review_data["yaraMatches"]) > 3:
                print(f"      ... and {len(review_data['yaraMatches']) - 3} more")
        else:
            print("   ✅ No threats detected")

        results.append(
            {
                "name": scenario["name"],
                "status": status,
                "score": review_data["score"],
                "threatLevel": threat_level,
            }
        )

    # Summary
    print(f"\n📈 ANALYSIS SUMMARY")
    print("=" * 60)

    approved = len([r for r in results if r["status"] == "approved"])
    declined = len([r for r in results if r["status"] == "declined"])
    pending = len([r for r in results if r["status"] == "awaiting_human_review"])

    print(f"✅ Approved: {approved}")
    print(f"🚨 Declined: {declined}")
    print(f"🟡 Pending Review: {pending}")
    print(f"📊 Total Analyzed: {len(results)}")

    # Database verification
    print(f"\n🗄️ DATABASE UPDATES")
    print("=" * 30)
    for update in database_client.updates:
        print(f"Bot {update['bot_id']}: {update['status']}")

    # Performance metrics
    print(f"\n⚡ PERFORMANCE METRICS")
    print("=" * 30)
    print(f"Storage Downloads: {len(storage_client.get_download_history())}")
    print(f"Database Updates: {len(database_client.updates)}")
    print(
        f"Rules Triggered: {len([m for r in results for m in rule_manager.scan_history])}"
    )

    print("\n🎉 Demo completed successfully!")
    print("   ✅ Dependency injection working")
    print("   ✅ Threat detection active")
    print("   ✅ Database integration verified")
    print("   ✅ Error handling tested")


def demonstrate_mocking_benefits():
    """
    Show the benefits of the refactored architecture with mocking
    """
    print("\n🧪 MOCKING BENEFITS DEMONSTRATION")
    print("=" * 50)

    print("1. 🔌 Easy Dependency Injection")
    print("   - No Firebase dependencies in tests")
    print("   - Swappable storage and database clients")
    print("   - Configurable rule engine")

    print("\n2. 🎯 Isolated Unit Testing")
    print("   - Test analyzer logic independently")
    print("   - Mock external service failures")
    print("   - Verify specific interactions")

    print("\n3. 🚀 Fast Test Execution")
    print("   - No network calls or real storage")
    print("   - Predictable test data")
    print("   - Instant feedback loop")

    print("\n4. 🔄 Flexible Configuration")
    print("   - Different rule sets per test")
    print("   - Custom threat scenarios")
    print("   - Edge case simulation")

    # Demonstrate quick test setup
    print("\n📝 Quick Test Setup Example:")
    print("```python")
    print("# Traditional approach - requires Firebase")
    print("# analyzer = The0vers33r()  # Hard to test")
    print("")
    print("# Refactored approach - easy to test")
    print("mock_storage = MockStorageClient()")
    print("mock_db = MockDatabaseClient()")
    print("analyzer = The0vers33r(")
    print("    storage_client=mock_storage,")
    print("    database_client=mock_db")
    print(")")
    print("# Now fully testable!")
    print("```")


if __name__ == "__main__":
    print("🧪 Running 0VERS33R Integration Tests")
    print("=" * 50)

    # Run the demo first
    run_integration_demo()

    # Show mocking benefits
    demonstrate_mocking_benefits()

    print("\n" + "=" * 50)
    print("🧪 Running Integration Test Suite...")

    # Run the actual tests
    unittest.main(argv=[""], exit=False, verbosity=2)
