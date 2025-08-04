/*
   Suspicious Python Pattern Detection Rules
   Patterns that may indicate malicious intent but could have legitimate uses
   Severity: HIGH/MEDIUM - Human review or conditional approval
*/

rule Python_Network_Operations {
    meta:
        description = "Detects network operations that could be malicious"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Socket operations
        $socket_socket = "socket.socket(" nocase
        $socket_connect = ".connect(" nocase
        $socket_bind = ".bind(" nocase
        $socket_listen = ".listen(" nocase

        // HTTP operations
        $urllib_request = "urllib.request" nocase
        $urllib_urlopen = "urllib.urlopen(" nocase
        $requests_get = "requests.get(" nocase
        $requests_post = "requests.post(" nocase
        $http_client = "http.client" nocase

        // FTP operations
        $ftplib = "ftplib.FTP(" nocase
        $ftp_login = ".login(" nocase

        // SMTP operations
        $smtplib = "smtplib.SMTP(" nocase
        $smtp_login = ".login(" nocase

        // Raw packet manipulation
        $scapy = "from scapy" nocase
        $raw_socket = "socket.AF_PACKET" nocase

    condition:
        2 of them or $scapy or $raw_socket
}

rule Python_File_System_Operations {
    meta:
        description = "Detects potentially suspicious file system operations"
        severity = "medium"
        author = "0VERS33R"

    strings:
        // File operations
        $file_write = /open\([^)]*['"]\w*w/ nocase
        $file_append = /open\([^)]*['"]\w*a/ nocase
        $file_binary = /open\([^)]*['"]\w*b/ nocase

        // Directory operations
        $os_listdir = "os.listdir(" nocase
        $os_walk = "os.walk(" nocase
        $glob_glob = "glob.glob(" nocase

        // File manipulation
        $shutil_copy = "shutil.copy" nocase
        $shutil_move = "shutil.move(" nocase
        $shutil_rmtree = "shutil.rmtree(" nocase
        $os_remove = "os.remove(" nocase
        $os_unlink = "os.unlink(" nocase

        // Path manipulation
        $os_path_join = "os.path.join(" nocase
        $pathlib_path = "pathlib.Path(" nocase

        // Temporary file operations
        $tempfile = "tempfile." nocase
        $mktemp = "mktemp(" nocase

    condition:
        3 of them
}

rule Python_Process_Manipulation {
    meta:
        description = "Detects process manipulation and monitoring"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Process information
        $psutil = "import psutil" nocase
        $ps_process = "psutil.Process(" nocase
        $ps_pids = "psutil.pids(" nocase

        // Process control
        $os_kill = "os.kill(" nocase
        $os_killpg = "os.killpg(" nocase
        $signal_kill = "signal.SIGKILL" nocase
        $signal_term = "signal.SIGTERM" nocase

        // Threading operations
        $threading = "import threading" nocase
        $thread_start = ".start()" nocase
        $multiprocessing = "import multiprocessing" nocase

        // Process spawning
        $os_fork = "os.fork(" nocase
        $os_waitpid = "os.waitpid(" nocase

    condition:
        2 of them
}

rule Python_Credential_Harvesting {
    meta:
        description = "Detects potential credential harvesting patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Common credential patterns
        $password = "password" nocase
        $passwd = "passwd" nocase
        $credential = "credential" nocase
        $api_key = "api_key" nocase
        $secret_key = "secret_key" nocase
        $access_token = "access_token" nocase
        $private_key = "private_key" nocase

        // Environment variable access
        $os_environ = "os.environ" nocase
        $getenv = "os.getenv(" nocase
        $env_get = ".get(" nocase

        // Config file reading
        $configparser = "configparser" nocase
        $config_read = ".read(" nocase
        $json_load = "json.load(" nocase

        // Network operations
        $requests_post = "requests.post(" nocase
        $urllib_request = "urllib.request" nocase
        $socket_connect = ".connect(" nocase

        // Storage operations
        $file_write = /open\([^)]*['"]\w*w/ nocase
        $sqlite = "sqlite3" nocase
        $pickle_dump = "pickle.dump(" nocase

        // System information
        $platform_system = "platform.system(" nocase
        $sys_platform = "sys.platform" nocase
        $os_name = "os.name" nocase

    condition:
        (2 of ($password, $passwd, $credential, $api_key, $secret_key, $access_token, $private_key)) and
        (any of ($os_environ, $getenv, $env_get, $configparser, $config_read, $json_load)) and
        (any of ($requests_post, $urllib_request, $socket_connect, $file_write, $sqlite, $pickle_dump) or
         2 of ($platform_system, $sys_platform, $os_name))
}

rule Python_Obfuscation_Techniques {
    meta:
        description = "Detects code obfuscation techniques"
        severity = "medium"
        author = "0VERS33R"

    strings:
        // String obfuscation
        $base64_decode = "base64.b64decode(" nocase
        $base64_encode = "base64.b64encode(" nocase
        $hex_decode = ".decode('hex')" nocase
        $rot13 = ".encode('rot13')" nocase

        // Character manipulation
        $chr_function = "chr(" nocase
        $ord_function = "ord(" nocase
        $hex_function = "hex(" nocase
        $bin_function = "bin(" nocase

        // Compression
        $zlib_compress = "zlib.compress(" nocase
        $zlib_decompress = "zlib.decompress(" nocase
        $gzip_compress = "gzip.compress(" nocase

        // Unicode obfuscation
        $unicode_escape = "unicode_escape" nocase
        $utf8_decode = "utf-8" nocase

        // Hex strings
        $hex_pattern = /\\x[0-9a-fA-F]{2}/ nocase

    condition:
        3 of them or $hex_pattern
}

rule Python_Anti_Analysis {
    meta:
        description = "Detects anti-analysis and evasion techniques"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Debugging detection
        $sys_gettrace = "sys.gettrace(" nocase
        $debugger_check = "debugger" nocase
        $pdb_import = "import pdb" nocase

        // Virtual machine detection
        $vm_check = "virtual" nocase
        $sandbox_check = "sandbox" nocase
        $vmware = "vmware" nocase
        $virtualbox = "virtualbox" nocase

        // Timing checks
        $time_time = "time.time(" nocase
        $time_sleep = "time.sleep(" nocase
        $datetime_now = "datetime.now(" nocase

        // Environment checks
        $platform_system = "platform.system(" nocase
        $os_name = "os.name" nocase
        $sys_platform = "sys.platform" nocase

        // Analysis tools detection
        $ida_check = "ida" nocase
        $ollydbg = "ollydbg" nocase
        $wireshark = "wireshark" nocase

    condition:
        3 of them
}

rule Python_Persistence_Mechanisms {
    meta:
        description = "Detects persistence and startup mechanisms"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Cron/scheduled tasks
        $crontab = "crontab" nocase
        $systemd = "systemd" nocase
        $launchd = "launchd" nocase

        // Startup locations
        $startup = "startup" nocase
        $autostart = "autostart" nocase
        $init_d = "/etc/init.d" nocase
        $rc_local = "/etc/rc.local" nocase

        // User profile modification
        $bashrc = ".bashrc" nocase
        $bash_profile = ".bash_profile" nocase
        $profile = ".profile" nocase
        $zshrc = ".zshrc" nocase

        // Registry operations (Windows)
        $winreg = "winreg" nocase
        $registry = "registry" nocase
        $hkey_current_user = "HKEY_CURRENT_USER" nocase

        // Service installation
        $service_install = "service" nocase
        $daemon = "daemon" nocase

    condition:
        2 of them
}

rule Python_Encryption_Ransomware_Indicators {
    meta:
        description = "Detects potential ransomware encryption patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Encryption imports
        $crypto_cipher = "Crypto.Cipher" nocase
        $cryptography_fernet = "cryptography.fernet" nocase
        $aes_new = "AES.new(" nocase
        $rsa_new = "RSA.new(" nocase

        // File extension patterns
        $encrypt_suffix = ".encrypt" nocase
        $locked_suffix = ".locked" nocase
        $ransom_suffix = ".ransom" nocase

        // Bulk file operations
        $os_walk = "os.walk(" nocase
        $glob_recursive = "glob.glob(" nocase
        $file_encrypt = ".encrypt(" nocase

        // Ransom note indicators
        $ransom_note = "ransom" nocase
        $bitcoin = "bitcoin" nocase
        $payment = "payment" nocase
        $decrypt = "decrypt" nocase

        // Key generation
        $random_bytes = "random.bytes(" nocase
        $urandom = "os.urandom(" nocase
        $secrets_token = "secrets.token_bytes(" nocase

    condition:
        all of ($crypto_cipher, $cryptography_fernet, $aes_new, $rsa_new) and
        any of ($os_walk, $glob_recursive, $file_encrypt, $random_bytes, $urandom, $secrets_token) and
        any of ($ransom_note, $bitcoin, $payment, $decrypt, $encrypt_suffix, $locked_suffix, $ransom_suffix)
}

rule Python_Data_Exfiltration {
    meta:
        description = "Detects potential data exfiltration patterns"
        severity = "high"
        author = "0VERS33R"

    strings:
        // Archive creation
        $zipfile = "zipfile.ZipFile(" nocase
        $tarfile = "tarfile.open(" nocase
        $archive_write = ".write(" nocase

        // Network upload
        $requests_post = "requests.post(" nocase
        $urllib_request = "urllib.request.urlopen(" nocase
        $ftp_storbinary = ".storbinary(" nocase
        $ftp_storlines = ".storlines(" nocase

        // Email sending
        $smtplib_send = "smtplib" nocase
        $email_mime = "email.mime" nocase
        $send_message = ".send_message(" nocase

        // Cloud storage APIs
        $boto3 = "boto3" nocase
        $dropbox_api = "dropbox" nocase
        $google_drive = "googleapiclient" nocase

        // Data collection patterns
        $sensitive_dirs = /['"](Documents|Downloads|Desktop|Pictures)['"]/
        $browser_data = /['"](chrome|firefox|safari)['"]/

    condition:
        3 of them

}
