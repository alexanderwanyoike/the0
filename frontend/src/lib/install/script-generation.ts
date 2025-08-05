import type { PlatformId, ScriptGenerationConfig } from "@/types/install";

// Script template generation functions
export function generateInstallScript(
  platformId: PlatformId,
  config: ScriptGenerationConfig = {},
): string {
  const {
    baseUrl = process.env.CLI_DOWNLOAD_BASE_URL ||
      "https://github.com/theo-cli/theo/releases/latest/download",
    installBaseUrl = process.env.CLI_INSTALL_BASE_URL || "",
    channel = "latest",
  } = config;

  const timestamp = new Date().toISOString();
  const isWindows = platformId.startsWith("windows");

  if (isWindows) {
    return generatePowerShellScript(
      platformId,
      baseUrl,
      installBaseUrl,
      timestamp,
      channel,
    );
  } else {
    return generateBashScript(
      platformId,
      baseUrl,
      installBaseUrl,
      timestamp,
      channel,
    );
  }
}

// Generate Bash script for Unix-like systems (macOS, Linux)
function generateBashScript(
  platformId: PlatformId,
  baseUrl: string,
  installBaseUrl: string,
  timestamp: string,
  channel: string,
): string {
  const binaryName = "the0";
  const downloadUrl = `${baseUrl}/${binaryName}-${platformId}`;
  const installDir = "$HOME/bin";

  return `#!/bin/bash
# THE0 CLI Installation Script
# Platform: ${platformId}
# Generated: ${timestamp}
# Channel: ${channel}

set -e

# Colors for output
RED='\\033[0;31m'
GREEN='\\033[0;32m'
YELLOW='\\033[1;33m'
BLUE='\\033[0;34m'
NC='\\033[0m' # No Color

# Print colored output
print_status() {
    echo -e "\${BLUE}[INFO]\${NC} \$1"
}

print_success() {
    echo -e "\${GREEN}[SUCCESS]\${NC} \$1"
}

print_warning() {
    echo -e "\${YELLOW}[WARNING]\${NC} \$1"
}

print_error() {
    echo -e "\${RED}[ERROR]\${NC} \$1"
}

# Check if running as root for system installation
check_permissions() {
    # Always install to user's home directory
    INSTALL_DIR="${installDir}"
    NEEDS_SUDO=false
    
    # Create the bin directory if it doesn't exist
    if [[ ! -d "\$INSTALL_DIR" ]]; then
        print_status "Creating directory: \$INSTALL_DIR"
        mkdir -p "\$INSTALL_DIR"
    fi
    
    # Check if the directory is writable
    if [[ ! -w "\$INSTALL_DIR" ]]; then
        print_error "Cannot write to \$INSTALL_DIR"
        print_error "Please check directory permissions and try again"
        exit 1
    fi
}

# Detect system architecture and validate
detect_arch() {
    local arch=\$(uname -m)
    case \$arch in
        x86_64)
            if [[ "${platformId}" != *"amd64" ]]; then
                print_error "Architecture mismatch: detected x86_64 but script is for ${platformId}"
                exit 1
            fi
            ;;
        arm64|aarch64)
            if [[ "${platformId}" != *"arm64" ]]; then
                print_error "Architecture mismatch: detected arm64 but script is for ${platformId}"
                exit 1
            fi
            ;;
        *)
            print_warning "Unknown architecture: \$arch. Proceeding anyway..."
            ;;
    esac
}

# Check system requirements
check_requirements() {
    # Check if curl is available
    if ! command -v curl &> /dev/null; then
        print_error "curl is required but not installed. Please install curl and try again."
        exit 1
    fi

    # Check if tar is available
    if ! command -v tar &> /dev/null; then
        print_error "tar is required but not installed. Please install tar and try again."
        exit 1
    fi
}

# Download and install THE0 CLI
install_cli() {
    local download_url="${downloadUrl}"
    local temp_dir=\$(mktemp -d)
    local binary_path="\$temp_dir/${binaryName}"

    print_status "Downloading THE0 CLI from \$download_url"
    
    # Download with progress bar
    if ! curl -fSL --progress-bar "\$download_url" -o "\$binary_path"; then
        print_error "Failed to download THE0 CLI from \$download_url"
        print_error "Please check your internet connection and try again."
        rm -rf "\$temp_dir"
        exit 1
    fi

    # Verify download
    if [[ ! -f "\$binary_path" ]]; then
        print_error "Download failed: binary not found"
        rm -rf "\$temp_dir"
        exit 1
    fi

    # Make binary executable
    chmod +x "\$binary_path"

    # Install binary
    print_status "Installing THE0 CLI to \$INSTALL_DIR"
    
    if [[ "\$NEEDS_SUDO" == "true" ]]; then
        if ! sudo cp "\$binary_path" "\$INSTALL_DIR/${binaryName}"; then
            print_error "Failed to install THE0 CLI. Check permissions for \$INSTALL_DIR"
            rm -rf "\$temp_dir"
            exit 1
        fi
    else
        if ! cp "\$binary_path" "\$INSTALL_DIR/${binaryName}"; then
            print_error "Failed to install THE0 CLI. Check permissions for \$INSTALL_DIR"
            rm -rf "\$temp_dir"
            exit 1
        fi
    fi

    # Clean up
    rm -rf "\$temp_dir"
}

# Add to PATH if needed
setup_path() {
    # Check if $HOME/bin is already in PATH
    if [[ ":$PATH:" != *":\$INSTALL_DIR:"* ]]; then
        print_status "Adding \$INSTALL_DIR to PATH for this session"
        export PATH="\$INSTALL_DIR:\$PATH"
        
        # Add to shell profile for permanent PATH
        local shell_profile=""
        if [[ "\$SHELL" == *"zsh"* ]]; then
            shell_profile="\$HOME/.zshrc"
        elif [[ "\$SHELL" == *"bash"* ]]; then
            if [[ -f "\$HOME/.bashrc" ]]; then
                shell_profile="\$HOME/.bashrc"
            elif [[ -f "\$HOME/.bash_profile" ]]; then
                shell_profile="\$HOME/.bash_profile"
            fi
        fi
        
        if [[ -n "\$shell_profile" ]]; then
            local path_line="export PATH=\"\$HOME/bin:\\\$PATH\""
            if ! grep -q "export PATH.*\$HOME/bin" "\$shell_profile" 2>/dev/null; then
                print_status "Adding \$HOME/bin to PATH in \$shell_profile"
                echo "" >> "\$shell_profile"
                echo "# Added by THE0 CLI installer" >> "\$shell_profile"
                echo "\$path_line" >> "\$shell_profile"
                print_status "PATH updated in \$shell_profile"
            fi
        else
            print_status "Please add \$HOME/bin to your PATH manually:"
            print_status "export PATH=\"\$HOME/bin:\\\$PATH\""
        fi
    fi
}

# Verify installation
verify_installation() {
    if command -v ${binaryName} &> /dev/null; then
        local version=\$(${binaryName} --version 2>/dev/null || echo "unknown")
        print_success "THE0 CLI installed successfully!"
        print_success "Version: \$version"
        print_status "Run '${binaryName} --help' to get started"
    else
        print_warning "THE0 CLI was installed to \$INSTALL_DIR but may not be in your current PATH"
        print_status "You may need to restart your shell or run: source ~/.bashrc (or ~/.zshrc)"
        print_status "Alternatively, you can run the CLI directly: \$INSTALL_DIR/${binaryName}"
    fi
}

# Main installation process
main() {
    print_status "Starting THE0 CLI installation for ${platformId}"
    
    check_requirements
    detect_arch
    check_permissions
    install_cli
    setup_path
    verify_installation
    
    print_success "Installation complete!"
    echo
    print_status "Next steps:"
    print_status "1. Run '${binaryName} auth login' to authenticate"
    print_status "2. Run '${binaryName} --help' to see available commands"
    print_status "3. Visit ${installBaseUrl || "https://the0.dev/docs/the0-CLI"} for documentation"
}

# Run main function
main "\$@"
`;
}

// Generate PowerShell script for Windows
function generatePowerShellScript(
  platformId: PlatformId,
  baseUrl: string,
  installBaseUrl: string,
  timestamp: string,
  channel: string,
): string {
  const binaryName = "the0.exe";
  const downloadUrl = `${baseUrl}/${binaryName.replace(".exe", "")}-${platformId}.exe`;

  return `# THE0 CLI Installation Script for Windows
# Platform: ${platformId}
# Generated: ${timestamp}
# Channel: ${channel}

\$ErrorActionPreference = "Stop"

# Function to write colored output
function Write-ColoredOutput {
    param(
        [string]\$Message,
        [string]\$Color = "White"
    )
    Write-Host \$Message -ForegroundColor \$Color
}

function Write-Status {
    param([string]\$Message)
    Write-ColoredOutput "[INFO] \$Message" "Cyan"
}

function Write-Success {
    param([string]\$Message)
    Write-ColoredOutput "[SUCCESS] \$Message" "Green"
}

function Write-Warning {
    param([string]\$Message)
    Write-ColoredOutput "[WARNING] \$Message" "Yellow"
}

function Write-Error {
    param([string]\$Message)
    Write-ColoredOutput "[ERROR] \$Message" "Red"
}

# Check PowerShell version
function Test-PowerShellVersion {
    if (\$PSVersionTable.PSVersion.Major -lt 3) {
        Write-Error "PowerShell 3.0 or higher is required. Please upgrade PowerShell."
        exit 1
    }
}

# Check if running as administrator
function Test-Administrator {
    \$currentUser = [Security.Principal.WindowsIdentity]::GetCurrent()
    \$principal = New-Object Security.Principal.WindowsPrincipal(\$currentUser)
    return \$principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

# Get installation directory
function Get-InstallDirectory {
    # Install to user's local bin directory
    \$userBin = Join-Path \$env:USERPROFILE "bin"
    
    # Create directory if it doesn't exist
    if (-not (Test-Path \$userBin)) {
        try {
            New-Item -ItemType Directory -Path \$userBin -Force | Out-Null
            Write-Status "Created installation directory: \$userBin"
        }
        catch {
            Write-Error "Failed to create \$userBin"
            Write-Error "Please check directory permissions and try again"
            exit 1
        }
    }
    
    return \$userBin
}

# Download THE0 CLI
function Get-CliBinary {
    param(
        [string]\$DownloadUrl,
        [string]\$InstallDir
    )
    
    \$binaryPath = Join-Path \$InstallDir "${binaryName}"
    \$tempFile = Join-Path \$env:TEMP "the0-installer.exe"
    
    Write-Status "Downloading THE0 CLI from \$DownloadUrl"
    
    try {
        # Use .NET WebClient for better progress reporting
        \$webClient = New-Object System.Net.WebClient
        \$webClient.DownloadFile(\$DownloadUrl, \$tempFile)
        
        # Verify download
        if (-not (Test-Path \$tempFile)) {
            throw "Download failed: file not found"
        }
        
        # Move to installation directory
        Move-Item \$tempFile \$binaryPath -Force
        Write-Success "Downloaded and installed binary to \$binaryPath"
        
        return \$binaryPath
    }
    catch {
        Write-Error "Failed to download THE0 CLI: \$_"
        if (Test-Path \$tempFile) {
            Remove-Item \$tempFile -Force
        }
        exit 1
    }
}

# Add to PATH
function Add-ToPath {
    param([string]\$InstallDir)
    
    \$currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    
    if (\$currentPath -notlike "*\$InstallDir*") {
        \$newPath = "\$currentPath;\$InstallDir"
        [Environment]::SetEnvironmentVariable("PATH", \$newPath, "User")
        Write-Success "Added \$InstallDir to user PATH"
        Write-Status "Please restart your terminal or run: \\\$env:PATH += \";\$InstallDir\""
    }
    else {
        Write-Status "Installation directory already in PATH"
    }
}

# Verify installation
function Test-Installation {
    param([string]\$BinaryPath)
    
    try {
        \$version = & \$BinaryPath --version 2>&1
        if (\$LASTEXITCODE -eq 0) {
            Write-Success "THE0 CLI installed successfully!"
            Write-Success "Version: \$version"
        }
        else {
            Write-Warning "THE0 CLI installed but version check failed"
        }
    }
    catch {
        Write-Warning "THE0 CLI installed but verification failed: \$_"
    }
}

# Main installation process
function Install-TheoCli {
    Write-Status "Starting THE0 CLI installation for ${platformId}"
    
    # Check requirements
    Test-PowerShellVersion
    
    # Check if running as administrator
    if (Test-Administrator) {
        Write-Status "Running as administrator - installing system-wide"
    }
    else {
        Write-Status "Running as regular user - installing to user directory"
    }
    
    # Get installation directory
    \$installDir = Get-InstallDirectory
    Write-Status "Installing to: \$installDir"
    
    # Download and install
    \$downloadUrl = "${downloadUrl}"
    \$binaryPath = Get-CliBinary -DownloadUrl \$downloadUrl -InstallDir \$installDir
    
    # Add to PATH
    Add-ToPath -InstallDir \$installDir
    
    # Verify installation
    Test-Installation -BinaryPath \$binaryPath
    
    Write-Success "Installation complete!"
    Write-Host ""
    Write-Status "Next steps:"
    Write-Status "1. Restart your terminal or PowerShell session"
    Write-Status "2. Run 'the0 auth login' to authenticate"
    Write-Status "3. Run 'the0 --help' to see available commands"
    Write-Status "4. Visit ${installBaseUrl || "https://the0.dev/docs/the0-CLI"} for documentation"
}

# Run installation
try {
    Install-TheoCli
}
catch {
    Write-Error "Installation failed: \$_"
    exit 1
}
`;
}

// Validate script content for security
export function validateScriptContent(
  script: string,
  fileType: "sh" | "ps1",
): boolean {
  // Basic security checks to prevent malicious content
  const dangerousPatterns = [
    /rm\s+-rf\s+\/[^\/\s]*/gi, // Dangerous rm commands
    /del\s+\/[sq]\s+[a-z]:/gi, // Dangerous Windows del commands
    /format\s+[a-z]:/gi, // Windows format commands
    />\s*\/dev\/sd[a-z]/gi, // Direct disk writes
    /curl.*\|\s*sh.*rm/gi, // Chained dangerous commands
    /wget.*\|\s*sh.*rm/gi, // Chained dangerous commands
    /eval.*\$\(/gi, // Eval with command substitution
    /\$\(.*rm.*\)/gi, // Command substitution with rm
  ];

  // Check for dangerous patterns
  for (const pattern of dangerousPatterns) {
    if (pattern.test(script)) {
      console.warn(`Dangerous pattern detected in script: ${pattern}`);
      return false;
    }
  }

  // Validate basic script structure
  if (fileType === "sh") {
    return script.includes("#!/bin/bash") || script.includes("#!/bin/sh");
  } else if (fileType === "ps1") {
    return (
      script.includes("$ErrorActionPreference") || script.includes("param(")
    );
  }

  return true;
}

// Get script template for a platform without full generation
export function getScriptTemplate(platformId: PlatformId): {
  fileExtension: "sh" | "ps1";
  shellCommand: string;
  contentType: string;
} {
  const isWindows = platformId.startsWith("windows");

  return {
    fileExtension: isWindows ? "ps1" : "sh",
    shellCommand: isWindows ? "powershell" : "bash",
    contentType: isWindows ? "application/x-powershell" : "application/x-sh",
  };
}

// Generate checksum for script content
export function generateScriptChecksum(script: string): string {
  // Simple hash function for script verification
  let hash = 0;
  for (let i = 0; i < script.length; i++) {
    const char = script.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash).toString(16);
}
