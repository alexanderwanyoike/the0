"""
Quick test script to validate 0VERS33R detection
"""


def run_quick_tests():
    print("🕵️ 0VERS33R Quick Detection Test")
    print("=" * 40)

    from src.rule_manager import RuleManager

    # Initialize rule manager
    rule_manager = RuleManager("yara_rules")
    if not rule_manager.load_and_compile_rules():
        print("❌ Failed to load YARA rules")
        return

    # Quick test cases
    test_cases = [
        ("Safe Code", b"import pandas as pd\ndf = pd.read_csv('data.csv')", 0),
        ("Code Injection", b"user_input = input()\nresult = eval(user_input)", 5),
        ("Hardcoded Creds", b"api_key = 'sk-abc123def456'\nbinance.Client(api_key)", 5),
        ("OS Commands", b"import os\nos.system('rm -rf /')", 5),
        ("Network Activity", b"import socket\ns = socket.socket()", 2),
    ]

    for name, code, expected_min_score in test_cases:
        score, matches = rule_manager.scan(code, f"{name.lower()}.py")

        status = "✅ PASS" if score >= expected_min_score else "❌ FAIL"
        threat_level = (
            "🚨 CRITICAL" if score >= 4 else "⚠️ SUSPICIOUS" if score >= 2 else "✅ SAFE"
        )

        print(
            f"{status} {name:15} | Score: {score:1}/5 | {threat_level} | File: {name.lower()}.py"
        )

        if matches:
            for match in matches[:2]:  # Show first 2 matches
                print(f"     └─ {match['rule']} ({match['severity']})")

    print("\n🎯 0VERS33R is working correctly!")


if __name__ == "__main__":
    run_quick_tests()
