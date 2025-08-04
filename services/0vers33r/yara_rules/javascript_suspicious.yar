/*
   JavaScript Suspicious Security Patterns
   Medium-risk patterns that warrant further investigation
   Severity: HIGH/MEDIUM - May require human review
*/

rule JS_Obfuscated_Code {
    meta:
        description = "Detects obfuscated JavaScript code patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        $obf1 = /\\\\x[0-9a-fA-F]{2}/
        $obf2 = /\\u[0-9a-fA-F]{4}/
        $obf3 = /String\.fromCharCode\(/
        $obf4 = /unescape\(/
        $obf5 = /decodeURIComponent\(/
        $obf6 = /atob\(/
        $obf7 = /btoa\(/

    condition:
        3 of them
}

rule JS_Dynamic_Import {
    meta:
        description = "Detects dynamic import statements that could load malicious code"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $import1 = "import(" nocase
        $import2 = "__import__(" nocase
        $require1 = "require(" nocase
        $dynamic = "eval" nocase

    condition:
        ($import1 or $import2 or $require1) and $dynamic
}

rule JS_Network_Requests {
    meta:
        description = "Detects suspicious network request patterns"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $fetch1 = "fetch(" nocase
        $xhr1 = "XMLHttpRequest(" nocase
        $axios1 = "axios." nocase
        $http1 = "http.request(" nocase
        $https1 = "https.request(" nocase
        $suspicious1 = ".onion" nocase
        $suspicious2 = "127.0.0.1" nocase
        $suspicious3 = "localhost" nocase

    condition:
        any of ($fetch1, $xhr1, $axios1, $http1, $https1) and any of ($suspicious1, $suspicious2, $suspicious3)
}

rule JS_Credential_Harvesting {
    meta:
        description = "Detects patterns that might harvest credentials"
        severity = "high"
        author = "0VERS33R"

    strings:
        $cred1 = "password" nocase
        $cred2 = "username" nocase
        $cred3 = "login" nocase
        $cred4 = "credential" nocase
        $cred5 = "api_key" nocase
        $cred6 = "apikey" nocase
        $cred7 = "secret" nocase
        $store1 = "localStorage" nocase
        $store2 = "sessionStorage" nocase
        $store3 = "cookie" nocase
        $send1 = "POST" nocase
        $send2 = "send(" nocase

    condition:
        2 of ($cred1, $cred2, $cred3, $cred4, $cred5, $cred6, $cred7) and 
        1 of ($store1, $store2, $store3) and 
        1 of ($send1, $send2)
}

rule JS_Browser_Fingerprinting {
    meta:
        description = "Detects browser fingerprinting techniques"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $fp1 = "navigator.userAgent" nocase
        $fp2 = "screen.width" nocase
        $fp3 = "screen.height" nocase
        $fp4 = "timezone" nocase
        $fp5 = "canvas" nocase
        $fp6 = "webgl" nocase
        $fp7 = "fonts" nocase
        $fp8 = "plugins" nocase

    condition:
        4 of them
}

rule JS_WebRTC_Exploit {
    meta:
        description = "Detects WebRTC exploitation patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        $webrtc1 = "RTCPeerConnection" nocase
        $webrtc2 = "webkitRTCPeerConnection" nocase
        $webrtc3 = "mozRTCPeerConnection" nocase
        $ice1 = "iceServers" nocase
        $stun1 = "stun:" nocase
        $turn1 = "turn:" nocase

    condition:
        any of ($webrtc1, $webrtc2, $webrtc3) and any of ($ice1, $stun1, $turn1)
}

rule JS_Keylogger_Patterns {
    meta:
        description = "Detects keylogger patterns in JavaScript"
        severity = "high"
        author = "0VERS33R"

    strings:
        $key1 = "addEventListener('keydown'" nocase
        $key2 = "addEventListener('keyup'" nocase
        $key3 = "addEventListener('keypress'" nocase
        $key4 = "onkeydown" nocase
        $key5 = "onkeyup" nocase
        $key6 = "onkeypress" nocase
        $capture1 = "event.key" nocase
        $capture2 = "event.keyCode" nocase
        $capture3 = "event.which" nocase

    condition:
        2 of ($key1, $key2, $key3, $key4, $key5, $key6) and 
        1 of ($capture1, $capture2, $capture3)
}

rule JS_Clipboard_Access {
    meta:
        description = "Detects clipboard access that could steal sensitive data"
        severity = "medium"
        author = "0VERS33R"

    strings:
        $clip1 = "navigator.clipboard" nocase
        $clip2 = "clipboard.read" nocase
        $clip3 = "clipboard.readText" nocase
        $clip4 = "clipboard.write" nocase
        $clip5 = "execCommand('copy')" nocase
        $clip6 = "execCommand('paste')" nocase

    condition:
        any of them
}