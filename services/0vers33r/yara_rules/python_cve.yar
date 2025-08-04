/*
   Python CVE-Specific Detection Rules
   Based on specific CVE patterns and exploitation techniques
   Severity: CRITICAL/HIGH - Known vulnerabilities
*/

rule CVE_2023_24329_URL_Parsing_Bypass {
    meta:
        description = "Detects CVE-2023-24329 urllib URL parsing bypass"
        severity = "critical"
        author = "0VERS33R"
        reference = "https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2023-24329"
        cve = "CVE-2023-24329"

    strings:
        // URL parsing functions
        $urlparse = "urllib.parse.urlparse(" nocase
        $urlsplit = "urllib.parse.urlsplit(" nocase

        // Bypass patterns
        $bypass1 = "http://@" nocase
        $bypass2 = "https://@" nocase
        $bypass3 = "://:" nocase
        $bypass4 = "http://localhost@" nocase
        $bypass5 = "https://127.0.0.1@" nocase

        // Malicious URL construction
        $url_concat = " + " nocase
        $format_url = ".format(" nocase

    condition:
        ($urlparse or $urlsplit) and any of ($bypass*) and
        (any of ($url_concat, $format_url) or
         (all of ($bypass1, $bypass2, $bypass3, $bypass4, $bypass5)))
}

rule CVE_2022_40897_Setuptools_Wheel_Extraction {
    meta:
        description = "Detects CVE-2022-40897 setuptools wheel extraction vulnerability"
        severity = "critical"
        author = "0VERS33R"
        reference = "Directory traversal in wheel extraction"
        cve = "CVE-2022-40897"

    strings:
        // Wheel/setuptools operations
        $wheel_install = "wheel" nocase
        $setuptools = "setuptools" nocase
        $pip_install = "pip install" nocase

        // Path traversal in wheel files
        $wheel_extract = "extract" nocase
        $zip_extract = "extractall" nocase
        $path_traversal = "../" nocase

        // Malicious wheel patterns
        $wheel_file = ".whl" nocase
        $setup_py = "setup.py" nocase

    condition:
        ($wheel_install or $setuptools or $pip_install) and
        ($wheel_extract or $zip_extract) and
        $path_traversal and
        ($wheel_file or $setup_py)
}

rule CVE_2021_44228_Log4Shell_Python_Exploitation {
    meta:
        description = "Detects Log4Shell exploitation attempts in Python"
        severity = "critical"
        author = "0VERS33R"
        reference = "Log4j JNDI injection adapted for Python"
        cve = "CVE-2021-44228"

    strings:
        // JNDI patterns (could be used in Python logging)
        $jndi_ldap = "${jndi:ldap:" nocase
        $jndi_rmi = "${jndi:rmi:" nocase
        $jndi_dns = "${jndi:dns:" nocase

        // Python logging with format strings
        $logging_format = "logging" nocase
        $format_string = "${" nocase
        $logger_info = ".info(" nocase
        $logger_error = ".error(" nocase

        // Exploitation payloads
        $payload_exec = "/Base64/" nocase
        $payload_download = "/Exploit" nocase

    condition:
        any of ($jndi_*) and $logging_format and
        (any of ($logger_info, $logger_error)) and
        (any of ($payload_exec, $payload_download) or
         $format_string)
}

rule CVE_2007_4559_TarFile_Path_Traversal {
    meta:
        description = "Detects CVE-2007-4559 tarfile path traversal vulnerability"
        severity = "critical"
        author = "0VERS33R"
        reference = "Python tarfile directory traversal"
        cve = "CVE-2007-4559"

    strings:
        // Tarfile operations
        $tarfile_open = "tarfile.open(" nocase
        $tar_extract = ".extractall(" nocase
        $tar_extract_single = ".extract(" nocase

        // Path traversal patterns
        $path_traversal1 = "../" nocase
        $path_traversal2 = "..\\\\" nocase
        $absolute_path = "os.path.isabs(" nocase

        // Unsafe extraction
        $unsafe_extract = "extractall()" nocase
        $no_validation = "extract(" nocase

    condition:
        ($tarfile_open and ($tar_extract or $tar_extract_single)) or
        (any of ($path_traversal1, $path_traversal2) or $absolute_path) and
        ($unsafe_extract or $no_validation)
}

rule CVE_2017_18342_PyYAML_Unsafe_Load {
    meta:
        description = "Detects CVE-2017-18342 PyYAML unsafe loading"
        severity = "critical"
        author = "0VERS33R"
        reference = "YAML deserialization vulnerability"
        cve = "CVE-2017-18342"

    strings:
        // YAML loading functions
        $yaml_load = "yaml.load(" nocase
        $yaml_unsafe = "yaml.unsafe_load(" nocase
        $yaml_fullloader = "FullLoader" nocase

        // Dangerous YAML patterns
        $python_object = "!!python/object:" nocase
        $python_module = "!!python/module:" nocase
        $python_name = "!!python/name:" nocase

        // Code execution in YAML
        $yaml_exec = "__reduce__" nocase
        $yaml_import = "__import__" nocase

    condition:
        ($yaml_load or $yaml_unsafe) and
        (any of ($python_*) or any of ($yaml_exec, $yaml_import)) and
         $yaml_fullloader
}

rule CVE_2022_42969_Py7zr_Path_Traversal {
    meta:
        description = "Detects CVE-2022-42969 py7zr path traversal"
        severity = "critical"
        author = "0VERS33R"
        reference = "py7zr directory traversal vulnerability"
        cve = "CVE-2022-42969"

    strings:
        // py7zr operations
        $py7zr_import = "import py7zr" nocase
        $sevenz_open = "py7zr.SevenZipFile(" nocase
        $archive_extract = ".extractall(" nocase

        // Path traversal
        $path_traversal = "../" nocase
        $windows_traversal = "..\\" nocase

        // Archive file
        $sevenz_file = ".7z" nocase

    condition:
        ($py7zr_import or $sevenz_open) and
        $archive_extract and
        ($path_traversal or $windows_traversal) and
        $sevenz_file
}

rule CVE_2023_43641_Libcurl_Cookie_Injection {
    meta:
        description = "Detects CVE-2023-43641 libcurl cookie injection in Python"
        severity = "high"
        author = "0VERS33R"
        reference = "Cookie injection via pycurl"
        cve = "CVE-2023-43641"

    strings:
        // pycurl usage
        $pycurl_import = "import pycurl" nocase
        $curl_easy = "pycurl.Curl(" nocase
        $curl_setopt = ".setopt(" nocase

        // Cookie manipulation
        $cookie_jar = "COOKIEJAR" nocase
        $cookie_file = "COOKIEFILE" nocase
        $cookie_header = "HTTPHEADER" nocase

        // Injection patterns
        $newline_inject = "\\n" nocase
        $carriage_return = "\\r" nocase
        $cookie_inject = "Set-Cookie:" nocase

    condition:
        ($pycurl_import or $curl_easy) and
        any of ($cookie_*) and
        any of ($newline_inject, $carriage_return, $cookie_inject) or
        ($curl_setopt and ($cookie_jar or $cookie_file or $cookie_header))
}

rule CVE_2023_5752_Pip_Mercurial_Injection {
    meta:
        description = "Detects CVE-2023-5752 pip Mercurial injection"
        severity = "high"
        author = "0VERS33R"
        reference = "pip install from Mercurial repositories"
        cve = "CVE-2023-5752"

    strings:
        // pip operations
        $pip_install = "pip install" nocase
        $pip_subprocess = "subprocess" nocase

        // Mercurial URLs
        $hg_url = "hg+" nocase
        $mercurial_url = "hg+http" nocase
        $hg_ssh = "hg+ssh" nocase

        // Command injection
        $command_inject = "--config" nocase
        $hg_config = "hooks.pre-" nocase

    condition:
        $pip_install and
        any of ($hg_url, $mercurial_url, $hg_ssh) and
        ($command_inject or $hg_config) or
        ($pip_subprocess and ($hg_url or $mercurial_url or $hg_ssh))
}

rule CVE_2022_40023_Mako_Template_Injection {
    meta:
        description = "Detects CVE-2022-40023 Mako template injection"
        severity = "critical"
        author = "0VERS33R"
        reference = "Mako template RCE vulnerability"
        cve = "CVE-2022-40023"

    strings:
        // Mako template usage
        $mako_import = "from mako" nocase
        $mako_template = "mako.template" nocase
        $template_render = ".render(" nocase

        // Template injection patterns
        $template_exec = "${" nocase
        $python_exec = "<%!" nocase
        $mako_exec = "<%" nocase

        // Dangerous functions in templates
        $import_in_template = "__import__" nocase
        $eval_in_template = "eval(" nocase
        $exec_in_template = "exec(" nocase

    condition:
        ($mako_import or $mako_template) and
        $template_render and
        any of ($template_exec, $python_exec, $mako_exec) and
        any of ($import_in_template, $eval_in_template, $exec_in_template)
}

rule CVE_2020_26116_HTTP_Header_Injection {
    meta:
        description = "Detects CVE-2020-26116 HTTP header injection"
        severity = "high"
        author = "0VERS33R"
        reference = "Python HTTP client header injection"
        cve = "CVE-2020-26116"

    strings:
        // HTTP client usage
        $http_client = "http.client" nocase
        $urllib_request = "urllib.request" nocase
        $httplib = "httplib" nocase

        // Header manipulation
        $add_header = "add_header(" nocase
        $putheader = "putheader(" nocase
        $headers_dict = "headers" nocase

        // Injection patterns
        $crlf_inject = "\\r\\n" nocase
        $newline_inject = "\\n" nocase
        $header_inject = ":" nocase

    condition:
        any of ($http_client, $urllib_request, $httplib) and
        any of ($add_header, $putheader, $headers_dict) and
        ($crlf_inject or ($newline_inject and $header_inject))
}

rule CVE_2022_48560_Python_Deserialization {
    meta:
        description = "Detects Python deserialization vulnerabilities"
        severity = "critical"
        author = "0VERS33R"
        reference = "Unsafe deserialization leading to RCE"

    strings:
        // Deserialization functions
        $pickle_loads = "pickle.loads(" nocase
        $pickle_load = "pickle.load(" nocase
        $marshal_loads = "marshal.loads(" nocase
        $dill_loads = "dill.loads(" nocase

        // Dangerous pickle operations
        $pickle_reduce = "__reduce__" nocase
        $pickle_setstate = "__setstate__" nocase
        $pickle_getnewargs = "__getnewargs__" nocase

        // Code execution patterns
        $subprocess_in_pickle = "subprocess" nocase
        $os_system_in_pickle = "os.system" nocase
        $eval_in_pickle = "eval" nocase

    condition:
        any of ($pickle_loads, $pickle_load, $marshal_loads, $dill_loads) and
        (any of ($pickle_reduce, $pickle_setstate, $pickle_getnewargs) or
         any of ($subprocess_in_pickle, $os_system_in_pickle, $eval_in_pickle))
}

rule Python_Zip_Bomb_CVE_Pattern {
    meta:
        description = "Detects zip bomb exploitation patterns"
        severity = "high"
        author = "0VERS33R"
        reference = "Zip bomb DoS attacks"

    strings:
        // Zip operations
        $zipfile_open = "zipfile.ZipFile(" nocase
        $zip_extractall = ".extractall(" nocase
        $zip_extract = ".extract(" nocase

        // Bomb indicators
        $large_ratio = "ratio" nocase
        $zip_info = ".infolist(" nocase
        $file_size = ".file_size" nocase
        $compress_size = ".compress_size" nocase

        // No size validation
        $no_size_check = "extractall()" nocase
        $unlimited_extract = "extract(" nocase

    condition:
        ($zipfile_open and ($zip_extractall or $zip_extract)) and
        ($no_size_check or $unlimited_extract or
         ($zip_info and ($file_size or $compress_size))) and
        $large_ratio
}