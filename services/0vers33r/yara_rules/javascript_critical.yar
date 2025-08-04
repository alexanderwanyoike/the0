/*
   JavaScript Critical Security Threats
   High-risk patterns that indicate immediate security concerns
   Severity: CRITICAL - Immediate rejection recommended
*/

rule JS_Code_Execution_Eval {
    meta:
        description = "Detects use of eval() function for code execution"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $eval1 = "eval(" nocase
        $eval2 = "window.eval(" nocase
        $eval3 = "global.eval(" nocase
        $eval4 = ".eval(" nocase

    condition:
        any of them
}

rule JS_Function_Constructor {
    meta:
        description = "Detects Function constructor for dynamic code execution"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $func1 = "new Function(" nocase
        $func2 = "Function(" nocase
        $func3 = "window.Function(" nocase
        $func4 = "global.Function(" nocase

    condition:
        any of them
}

rule JS_VM_RunInContext {
    meta:
        description = "Detects Node.js vm module code execution"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $vm1 = "vm.runInNewContext(" nocase
        $vm2 = "vm.runInThisContext(" nocase
        $vm3 = "vm.runInContext(" nocase
        $vm4 = "require('vm')" nocase
        $vm5 = "require(\"vm\")" nocase

    condition:
        any of them
}

rule JS_Child_Process_Execution {
    meta:
        description = "Detects child process execution in Node.js"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $cp1 = "child_process.exec(" nocase
        $cp2 = "child_process.spawn(" nocase
        $cp3 = "child_process.execSync(" nocase
        $cp4 = "child_process.spawnSync(" nocase
        $cp5 = "require('child_process')" nocase
        $cp6 = "require(\"child_process\")" nocase

    condition:
        any of them
}

rule JS_File_System_Write {
    meta:
        description = "Detects file system write operations"
        severity = "high"
        author = "0VERS33R"

    strings:
        $fs1 = "fs.writeFile(" nocase
        $fs2 = "fs.writeFileSync(" nocase
        $fs3 = "fs.appendFile(" nocase
        $fs4 = "fs.appendFileSync(" nocase
        $fs5 = "fs.createWriteStream(" nocase

    condition:
        any of them
}

rule JS_Reverse_Shell_Patterns {
    meta:
        description = "Detects JavaScript reverse shell patterns"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $net1 = "net.connect(" nocase
        $net2 = "net.createConnection(" nocase
        $shell1 = "/bin/sh" nocase
        $shell2 = "/bin/bash" nocase
        $shell3 = "cmd.exe" nocase
        $pipe1 = ".pipe(" nocase
        $spawn1 = "spawn(" nocase

    condition:
        ($net1 or $net2) and ($shell1 or $shell2 or $shell3) and ($pipe1 or $spawn1)
}

rule JS_Crypto_Mining_Indicators {
    meta:
        description = "Detects cryptocurrency mining indicators"
        severity = "critical"
        author = "0VERS33R"

    strings:
        $mine1 = "coinhive" nocase
        $mine2 = "cryptonight" nocase
        $mine3 = "webassembly" nocase
        $mine4 = "crypto-js" nocase
        $mine5 = "monero" nocase
        $mine6 = "mining" nocase
        $hash1 = "hashrate" nocase
        $hash2 = "difficulty" nocase

    condition:
        2 of them
}

rule JS_DOM_Manipulation_XSS {
    meta:
        description = "Detects dangerous DOM manipulation that could lead to XSS"
        severity = "high"
        author = "0VERS33R"

    strings:
        $dom1 = ".innerHTML" nocase
        $dom2 = ".outerHTML" nocase
        $dom3 = "document.write(" nocase
        $dom4 = "document.writeln(" nocase
        $dom5 = ".insertAdjacentHTML(" nocase
        $dangerous = "<script" nocase

    condition:
        any of ($dom1, $dom2, $dom3, $dom4, $dom5) and $dangerous
}