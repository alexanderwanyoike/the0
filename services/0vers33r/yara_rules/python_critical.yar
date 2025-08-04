/*
   Critical Python Exploit Detection Rules
   Based on real-world CVEs and exploit patterns
   Severity: CRITICAL - Auto-block
*/

rule Python_Code_Injection_CVE_Patterns {
    meta:
        description = "Detects Python code injection patterns from known CVEs"
        severity = "critical"
        author = "0VERS33R"
        reference = "CVE-2021-44228, CVE-2022-40897, CVE-2023-24329"

    strings:
        // Direct code execution
        $eval1 = "eval(" nocase
        $exec1 = "exec(" nocase
        $compile1 = "compile(" nocase

        // Dynamic imports (common in exploits)
        $import1 = "__import__(" nocase
        $importlib = "importlib.import_module(" nocase

        // Dangerous built-ins
        $globals = "globals()[" nocase
        $locals = "locals()[" nocase
        $getattr = "getattr(" nocase
        $setattr = "setattr(" nocase

        // Python pickle exploitation (CVE-2022-40897)
        $pickle_loads = "pickle.loads(" nocase
        $pickle_load = "pickle.load(" nocase
        $cPickle = "cPickle.loads(" nocase

        // Code object manipulation
        $code_obj = ".co_code" nocase
        $code_names = ".co_names" nocase

    condition:
        any of them
}

rule Python_OS_Command_Injection {
    meta:
        description = "Detects OS command injection in Python"
        severity = "critical"
        author = "0VERS33R"

    strings:
        // Direct OS command execution
        $os_system = "os.system(" nocase
        $os_popen = "os.popen(" nocase
        $os_spawn = "os.spawn" nocase
        $os_exec = "os.exec" nocase

        // Subprocess exploitation
        $subprocess_call = "subprocess.call(" nocase
        $subprocess_run = "subprocess.run(" nocase
        $subprocess_popen = "subprocess.Popen(" nocase
        $subprocess_check = "subprocess.check_" nocase

        // Shell injection patterns
        $shell_true = "shell=True" nocase
        $shell_injection = /subprocess\.[a-z]+\([^)]*shell\s*=\s*True/ nocase

        // Platform-specific dangerous calls
        $commands_getoutput = "commands.getoutput(" nocase
        $commands_getstatusoutput = "commands.getstatusoutput(" nocase

    condition:
        any of them
}

rule Python_Deserialization_Exploits {
    meta:
        description = "Detects dangerous Python deserialization patterns"
        severity = "critical"
        author = "0VERS33R"
        reference = "Pickle RCE, YAML unsafe loading"

    strings:
        // Pickle deserialization (RCE vector)
        $pickle_loads = "pickle.loads(" nocase
        $pickle_load = "pickle.load(" nocase
        $cpickle_loads = "cPickle.loads(" nocase

        // YAML unsafe loading (CVE-2017-18342)
        $yaml_load = "yaml.load(" nocase
        $yaml_unsafe = "yaml.unsafe_load(" nocase

        // Marshal exploitation
        $marshal_loads = "marshal.loads(" nocase
        $marshal_load = "marshal.load(" nocase

        // Shelve exploitation
        $shelve_open = "shelve.open(" nocase

        // Custom pickle protocols
        $reduce_ex = "__reduce_ex__" nocase
        $reduce = "__reduce__" nocase
        $setstate = "__setstate__" nocase

    condition:
        any of them
}

rule Python_Path_Traversal_CVE {
    meta:
        description = "Detects path traversal exploitation in Python"
        severity = "critical"
        author = "0VERS33R"
        reference = "CVE-2007-4559, Zip Slip attacks"

    strings:
        // Zip slip patterns
        $zip_slip1 = "../" nocase
        $zip_slip2 = "..\\\\" nocase
        $zip_extractall = ".extractall(" nocase
        $tarfile_extract = "tarfile.extract" nocase

        // Path traversal in file operations
        $path_join_danger = /os\.path\.join\([^)]*\.\.[\/\\]/ nocase
        $file_open_danger = /open\([^)]*\.\.[\/\\]/ nocase

        // Direct path manipulation
        $os_chdir = "os.chdir(" nocase
        $pathlib_resolve = ".resolve()" nocase

    condition:
        2 of them or ($zip_extractall and ($zip_slip1 or $zip_slip2))
}

rule Python_Server_Side_Template_Injection {
    meta:
        description = "Detects Server-Side Template Injection (SSTI) in Python"
        severity = "critical"
        author = "0VERS33R"
        reference = "Jinja2, Django template injection"

    strings:
        // Jinja2 SSTI patterns
        $jinja_subclasses = "__subclasses__" nocase
        $jinja_mro = "__mro__" nocase
        $jinja_globals = "__globals__" nocase
        $jinja_builtins = "__builtins__" nocase

        // Template injection payloads
        $template_exec = /\{\{.*__.*\}\}/ nocase
        $template_import = /\{\{.*__import__.*\}\}/ nocase

        // Django template injection
        $django_debug = "{% debug %}" nocase
        $django_load = "{% load" nocase

        // String formatting injection
        $format_exploit = /\.format\([^)]*__/ nocase
        $percent_format = /%\([^)]*__.*\)/ nocase

    condition:
        any of them
}

rule Python_Memory_Corruption_CVE {
    meta:
        description = "Detects memory corruption exploitation in Python"
        severity = "critical"
        author = "0VERS33R"
        reference = "Buffer overflow, heap corruption"

    strings:
        // C extension exploitation
        $ctypes_import = "import ctypes" nocase
        $ctypes_cdll = "ctypes.CDLL(" nocase
        $ctypes_pointer = "ctypes.pointer(" nocase
        $ctypes_cast = "ctypes.cast(" nocase

        // Direct memory access
        $memoryview = "memoryview(" nocase
        $bytes_decode = ".decode(" nocase
        $buffer_overflow = "b'A' * " nocase

        // Array manipulation
        $array_buffer = "array.array(" nocase
        $bytearray_extend = "bytearray(" nocase

        // Struct exploitation
        $struct_pack = "struct.pack(" nocase
        $struct_unpack = "struct.unpack(" nocase

    condition:
        2 of them
}

rule Python_URL_Parsing_CVE {
    meta:
        description = "Detects URL parsing vulnerabilities in Python"
        severity = "critical"
        author = "0VERS33R"
        reference = "CVE-2023-24329, urllib parsing bypass"

    strings:
        // URL parsing bypass patterns
        $urllib_parse = "urllib.parse" nocase
        $urlparse = "urlparse(" nocase
        $urlsplit = "urlsplit(" nocase

        // Malicious URL patterns
        $url_bypass1 = "http://@" nocase
        $url_bypass2 = "https://@" nocase
        $url_bypass3 = "://:" nocase
        $url_bypass4 = "ftp://" nocase

        // SSRF patterns
        $localhost = "localhost" nocase
        $internal_ip = "127.0.0.1" nocase
        $metadata_ip = "169.254.169.254" nocase

    condition:
        ($urllib_parse or $urlparse or $urlsplit) and
        (any of ($url_bypass*) or any of ($localhost, $internal_ip, $metadata_ip))
}

rule Python_Cryptographic_Weakness_CVE {
    meta:
        description = "Detects weak cryptographic implementations"
        severity = "critical"
        author = "0VERS33R"
        reference = "Weak crypto, key reuse, timing attacks"

    strings:
        // Weak algorithms
        $md5 = "hashlib.md5(" nocase
        $sha1 = "hashlib.sha1(" nocase
        $des = "DES.new(" nocase
        $rc4 = "ARC4.new(" nocase

        // Weak random number generation
        $random_seed = "random.seed(" nocase
        $predictable_random = "random.random()" nocase

        // Hardcoded keys/secrets
        $hardcoded_key = /[a-zA-Z0-9_]*key[a-zA-Z0-9_]*\s*=\s*["'][a-fA-F0-9]{16,}["']/ nocase
        $hardcoded_secret = /[a-zA-Z0-9_]*secret[a-zA-Z0-9_]*\s*=\s*["'][a-zA-Z0-9]{20,}["']/ nocase

        // Crypto timing attacks
        $time_compare = "time.time()" nocase
        $direct_compare = " == " nocase

    condition:
        any of ($md5, $sha1, $des, $rc4) or
        ($random_seed and $predictable_random) or
        any of ($hardcoded_key, $hardcoded_secret) or
        ($time_compare and $direct_compare)
}


rule Credential_Theft_SQLite_Exfil
{
    meta:
        description = "Detects Python code stealing credentials from browser SQLite DBs and exfiltrating them"
        severity = "critical"
        author = "0VERS33R"
    strings:
        $sqlite = "sqlite3.connect"
        $shutil_copy = "shutil.copy"
        $select_creds = /SELECT\s+.*(user(name)?|pass(word)?)[^;]*FROM/i
        $requests_post = "requests.post"
        $exfil_url = /http[s]?:\/\/[a-zA-Z0-9\.\-]+\/collect/
    condition:
        all of ($sqlite, $shutil_copy, $select_creds) and $requests_post and $exfil_url
}