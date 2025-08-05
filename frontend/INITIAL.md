## FEATURE: CLI Installation Page

Create a comprehensive web-based installation page for the THE0 CLI that provides platform-specific installation scripts, detects user's operating system, and offers multiple installation methods with clear instructions and troubleshooting guidance.

### Goals:

- **Auto-detection**: Automatically detect user's platform and show relevant installation instructions
- **Multiple methods**: Provide curl/bash script for Unix and PowerShell script for Windows
- **Copy-paste ready**: One-click copy commands that users can paste directly into their terminal
- **Verification**: Include post-installation verification steps and troubleshooting
- **Responsive design**: Works seamlessly across desktop, tablet, and mobile devices
- **Analytics**: Track installation attempts and success rates by platform

### Core Components:

1. **Platform Detection**: Client-side OS/architecture detection with manual override options
2. **Installation Scripts**: Generated bash/PowerShell scripts that download and install CLI
3. **Interactive UI**: Tabbed interface with copy buttons and syntax highlighting
4. **Verification Guide**: Post-installation steps to verify successful installation
5. **Troubleshooting**: Common issues and solutions organized by platform
6. **Alternative Methods**: Manual download links and package manager options

## EXAMPLES:

### 1. Automatic Platform Detection

```javascript
// User visits /cli-install on macOS
// Page automatically shows platform-specific one-liner:

curl -fsSL https://the0.dev/cli-install/darwin-arm64.sh | bash

// With detected platform indicator:
✅ Detected: macOS (Apple Silicon)

// Option to switch platforms shows different URLs:
// macOS Intel:     curl -fsSL https://the0.dev/cli-install/darwin-amd64.sh | bash
// macOS Apple:     curl -fsSL https://the0.dev/cli-install/darwin-arm64.sh | bash
// Linux x64:       curl -fsSL https://the0.dev/cli-install/linux-amd64.sh | bash
// Linux ARM64:     curl -fsSL https://the0.dev/cli-install/linux-arm64.sh | bash
// Windows:         iwr https://the0.dev/cli-install/windows-amd64.ps1 | iex
```

### 2. Dynamic Script URL Generation and API Routes

#### Next.js API Routes for Script Generation

```javascript
// /app/api/install/[platform]/route.js
import { generateInstallScript } from "@/lib/install-scripts";
import { NextResponse } from "next/server";

export async function GET(request, { params }) {
  const { platform } = params;

  // Validate platform
  const validPlatforms = [
    "darwin-amd64",
    "darwin-arm64",
    "linux-amd64",
    "linux-arm64",
    "windows-amd64",
  ];
  if (!validPlatforms.includes(platform)) {
    return NextResponse.json({ error: "Invalid platform" }, { status: 400 });
  }

  try {
    const script = generateInstallScript(platform, {
      baseUrl: process.env.CLI_DOWNLOAD_BASE_URL,
      installBaseUrl: process.env.CLI_INSTALL_BASE_URL,
    });

    // Set appropriate content type based on platform
    const contentType = platform.startsWith("windows")
      ? "application/x-powershell"
      : "application/x-sh";

    const headers = new Headers({
      "Content-Type": contentType,
      "Content-Disposition": `attachment; filename="install-${platform}.${platform.startsWith("windows") ? "ps1" : "sh"}"`,
    });

    return new NextResponse(script, { status: 200, headers });
  } catch (error) {
    return NextResponse.json(
      { error: "Failed to generate install script" },
      { status: 500 },
    );
  }
}
```

#### Environment Variable Configuration

```bash
# .env.local / .env.production
CLI_DOWNLOAD_BASE_URL=https://storage.googleapis.com/the0-cli-releases
CLI_INSTALL_BASE_URL=https://the0.dev/cli-install

# For development
CLI_DOWNLOAD_BASE_URL=https://storage.googleapis.com/the0-cli-releases-staging
CLI_INSTALL_BASE_URL=http://localhost:3000
```

#### Generated Installation Scripts with Dynamic URLs

#### macOS/Linux Installation Script (Generated at /api/install/darwin-arm64)

```bash
#!/bin/bash
# THE0 CLI Installation Script
# Platform: darwin-arm64
# Generated: {{timestamp}}
# Download URL: {{CLI_INSTALL_BASE_URL}}/api/install/darwin-arm64

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration (dynamically injected)
BASE_URL="{{CLI_DOWNLOAD_BASE_URL}}/latest"
PLATFORM="darwin-arm64"
BINARY_NAME="the0-${PLATFORM}"
INSTALL_DIR="${THE0_INSTALL_DIR:-/usr/local/bin}"

echo -e "${GREEN}🚀 Installing THE0 CLI for ${PLATFORM}...${NC}"
echo -e "${GREEN}📦 Download source: ${BASE_URL}${NC}"

# Check if running as root
if [[ $EUID -eq 0 ]]; then
    echo -e "${YELLOW}⚠️  Running as root. Installing to /usr/local/bin${NC}"
else
    # Check if user has write access to /usr/local/bin
    if [[ ! -w "$INSTALL_DIR" ]]; then
        echo -e "${YELLOW}⚠️  No write access to $INSTALL_DIR. You may need to run with sudo${NC}"
        INSTALL_DIR="$HOME/.local/bin"
        mkdir -p "$INSTALL_DIR"
        echo -e "${YELLOW}📁 Installing to $INSTALL_DIR instead${NC}"
    fi
fi

# Download binary
echo -e "${GREEN}⬇️  Downloading THE0 CLI...${NC}"
if command -v curl >/dev/null 2>&1; then
    curl -fL "$BASE_URL/$BINARY_NAME" -o "$INSTALL_DIR/the0"
elif command -v wget >/dev/null 2>&1; then
    wget -q "$BASE_URL/$BINARY_NAME" -O "$INSTALL_DIR/the0"
else
    echo -e "${RED}❌ Error: curl or wget is required${NC}"
    exit 1
fi

# Make executable
chmod +x "$INSTALL_DIR/the0"

# Verify installation
if "$INSTALL_DIR/the0" version >/dev/null 2>&1; then
    VERSION=$("$INSTALL_DIR/the0" version 2>/dev/null | head -n1 || echo "unknown")
    echo -e "${GREEN}✅ THE0 CLI installed successfully!${NC}"
    echo -e "${GREEN}📍 Version: $VERSION${NC}"
    echo -e "${GREEN}📍 Location: $INSTALL_DIR/the0${NC}"

    # Check if in PATH
    if command -v the0 >/dev/null 2>&1; then
        echo -e "${GREEN}🎉 Ready to use! Try: the0 --help${NC}"
    else
        echo -e "${YELLOW}⚠️  Add $INSTALL_DIR to your PATH to use 'the0' command${NC}"
        echo -e "${YELLOW}💡 Add this to your shell profile (~/.bashrc, ~/.zshrc):${NC}"
        echo -e "${YELLOW}   export PATH=\"$INSTALL_DIR:\$PATH\"${NC}"
    fi

    echo -e "${GREEN}🚀 Next steps:${NC}"
    echo -e "${GREEN}   1. the0 auth login${NC}"
    echo -e "${GREEN}   2. the0 bot list${NC}"
else
    echo -e "${RED}❌ Installation failed. Please check permissions and try again.${NC}"
    exit 1
fi
```

#### Windows PowerShell Installation Script (Generated at /api/install/windows-amd64)

```powershell
# THE0 CLI Installation Script for Windows
# Platform: windows-amd64
# Generated: {{timestamp}}
# Download URL: {{CLI_INSTALL_BASE_URL}}/api/install/windows-amd64

$ErrorActionPreference = "Stop"

# Configuration (dynamically injected)
$BaseUrl = "{{CLI_DOWNLOAD_BASE_URL}}/latest"
$Platform = "windows-amd64"
$BinaryName = "the0-$Platform.exe"
$InstallDir = if ($env:THE0_INSTALL_DIR) { $env:THE0_INSTALL_DIR } else { "$env:LOCALAPPDATA\the0\bin" }

Write-Host "🚀 Installing THE0 CLI for $Platform..." -ForegroundColor Green
Write-Host "📦 Download source: $BaseUrl" -ForegroundColor Green

# Create install directory
if (!(Test-Path $InstallDir)) {
    New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
    Write-Host "📁 Created directory: $InstallDir" -ForegroundColor Yellow
}

# Download binary
Write-Host "⬇️  Downloading THE0 CLI..." -ForegroundColor Green
$BinaryPath = Join-Path $InstallDir "the0.exe"
try {
    Invoke-WebRequest -Uri "$BaseUrl/$BinaryName" -OutFile $BinaryPath -UseBasicParsing
} catch {
    Write-Host "❌ Download failed: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "🔗 Try manual download: $BaseUrl/$BinaryName" -ForegroundColor Yellow
    exit 1
}

# Verify installation
try {
    $Version = & $BinaryPath version 2>$null
    Write-Host "✅ THE0 CLI installed successfully!" -ForegroundColor Green
    Write-Host "📍 Version: $Version" -ForegroundColor Green
    Write-Host "📍 Location: $BinaryPath" -ForegroundColor Green

    # Check if in PATH
    if (Get-Command the0 -ErrorAction SilentlyContinue) {
        Write-Host "🎉 Ready to use! Try: the0 --help" -ForegroundColor Green
    } else {
        Write-Host "⚠️  Add $InstallDir to your PATH to use 'the0' command" -ForegroundColor Yellow
        Write-Host "💡 Run this command to add to PATH for current session:" -ForegroundColor Yellow
        Write-Host "   `$env:PATH += `";$InstallDir`"" -ForegroundColor Yellow
        Write-Host "💡 Or add permanently via System Properties > Environment Variables" -ForegroundColor Yellow
    }

    Write-Host "🚀 Next steps:" -ForegroundColor Green
    Write-Host "   1. the0 auth login" -ForegroundColor Green
    Write-Host "   2. the0 bot list" -ForegroundColor Green
} catch {
    Write-Host "❌ Installation failed. Please check permissions and try again." -ForegroundColor Red
    Write-Host "🔗 Try manual download: $BaseUrl/$BinaryName" -ForegroundColor Yellow
    exit 1
}
```

### 3. React Component Structure with Dynamic URLs

```jsx
// /pages/cli-install.tsx
import { useState, useEffect } from "react";
import { detectPlatform } from "@/lib/cli-install";

export default function CLIInstallPage() {
  const [platform, setPlatform] = useState(null);
  const [installMethod, setInstallMethod] = useState("script");
  const [copied, setCopied] = useState(false);

  useEffect(() => {
    setPlatform(detectPlatform());
  }, []);

  // Generate install URLs based on environment
  const getInstallUrl = (targetPlatform) => {
    const baseUrl =
      process.env.NEXT_PUBLIC_CLI_INSTALL_BASE_URL ||
      "https://the0.dev/cli-install";
    if (targetPlatform?.startsWith("windows")) {
      return `iwr ${baseUrl}/api/install/${targetPlatform} | iex`;
    }
    return `curl -fsSL ${baseUrl}/api/install/${targetPlatform} | bash`;
  };

  const installCommand = platform ? getInstallUrl(platform) : "";

  return (
    <div className="cli-install-page">
      <Hero />
      <PlatformSelector
        platform={platform}
        onChange={setPlatform}
        getInstallUrl={getInstallUrl}
      />
      <InstallationTabs
        platform={platform}
        method={installMethod}
        onChange={setInstallMethod}
        installCommand={installCommand}
      />
      <QuickCopySection command={installCommand} />
      <VerificationSteps platform={platform} />
      <TroubleshootingSection platform={platform} />
      <AlternativeInstallMethods platform={platform} />
    </div>
  );
}

// Component for quick copy of install command
function QuickCopySection({ command }) {
  const [copied, setCopied] = useState(false);

  const copyToClipboard = async () => {
    await navigator.clipboard.writeText(command);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <div className="bg-gray-50 p-6 rounded-lg border">
      <h3 className="text-lg font-semibold mb-3">Quick Install</h3>
      <div className="flex items-center gap-3">
        <code className="flex-1 bg-black text-green-400 p-3 rounded font-mono text-sm">
          {command}
        </code>
        <button
          onClick={copyToClipboard}
          className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
        >
          {copied ? "Copied!" : "Copy"}
        </button>
      </div>
    </div>
  );
}
```

### 4. Platform-Specific URL Mapping

```javascript
// /lib/install-scripts.js
export const PLATFORM_URLS = {
  "darwin-amd64": {
    scriptUrl: "/api/install/darwin-amd64",
    command: (baseUrl) =>
      `curl -fsSL ${baseUrl}/api/install/darwin-amd64 | bash`,
    fileExtension: "sh",
  },
  "darwin-arm64": {
    scriptUrl: "/api/install/darwin-arm64",
    command: (baseUrl) =>
      `curl -fsSL ${baseUrl}/api/install/darwin-arm64 | bash`,
    fileExtension: "sh",
  },
  "linux-amd64": {
    scriptUrl: "/api/install/linux-amd64",
    command: (baseUrl) =>
      `curl -fsSL ${baseUrl}/api/install/linux-amd64 | bash`,
    fileExtension: "sh",
  },
  "linux-arm64": {
    scriptUrl: "/api/install/linux-arm64",
    command: (baseUrl) =>
      `curl -fsSL ${baseUrl}/api/install/linux-arm64 | bash`,
    fileExtension: "sh",
  },
  "windows-amd64": {
    scriptUrl: "/api/install/windows-amd64",
    command: (baseUrl) => `iwr ${baseUrl}/api/install/windows-amd64 | iex`,
    fileExtension: "ps1",
  },
};

export function generateInstallScript(platform, options = {}) {
  const {
    baseUrl = process.env.CLI_DOWNLOAD_BASE_URL,
    installBaseUrl = process.env.CLI_INSTALL_BASE_URL,
    channel = "latest",
  } = options;

  const template = getScriptTemplate(platform);

  return template
    .replace(/\{\{CLI_DOWNLOAD_BASE_URL\}\}/g, baseUrl)
    .replace(/\{\{CLI_INSTALL_BASE_URL\}\}/g, installBaseUrl)
    .replace(/\{\{timestamp\}\}/g, new Date().toISOString())
    .replace(/\{\{platform\}\}/g, platform);
}
```

### 5. Script URL Generation and Caching

```jsx
// Advanced installation interface with URL generation
<Tabs value={installMethod} onValueChange={setInstallMethod}>
  <TabsList>
    <TabsTrigger value="oneliner">🚀 One-Liner</TabsTrigger>
    <TabsTrigger value="script">📜 Download Script</TabsTrigger>
    <TabsTrigger value="manual">📥 Manual Download</TabsTrigger>
    <TabsTrigger value="package">📦 Package Managers</TabsTrigger>
  </TabsList>

  <TabsContent value="oneliner">
    <OneLinerPanel platform={platform} command={getInstallUrl(platform)} />
  </TabsContent>

  <TabsContent value="script">
    <DownloadScriptPanel
      platform={platform}
      scriptUrl={`${process.env.NEXT_PUBLIC_CLI_INSTALL_BASE_URL}/api/install/${platform}`}
    />
  </TabsContent>

  <TabsContent value="manual">
    <ManualDownloadPanel platform={platform} />
  </TabsContent>

  <TabsContent value="package">
    <PackageManagerPanel platform={platform} />
  </TabsContent>
</Tabs>;

// One-liner component with copy functionality
function OneLinerPanel({ platform, command }) {
  return (
    <div className="space-y-4">
      <p className="text-gray-600">
        Copy and paste this command into your terminal:
      </p>
      <CodeBlockWithCopy code={command} />
      <div className="text-sm text-gray-500">
        This command downloads and executes the installation script for{" "}
        {platform}
      </div>
    </div>
  );
}

// Download script panel
function DownloadScriptPanel({ platform, scriptUrl }) {
  return (
    <div className="space-y-4">
      <p className="text-gray-600">
        Download the installation script to review before executing:
      </p>
      <div className="flex gap-3">
        <a
          href={scriptUrl}
          className="btn btn-primary"
          download={`install-${platform}.${platform.startsWith("windows") ? "ps1" : "sh"}`}
        >
          📥 Download Script
        </a>
        <a href={scriptUrl} target="_blank" className="btn btn-secondary">
          👀 View Script
        </a>
      </div>
      <div className="text-sm text-gray-500">
        After downloading, make executable and run:
        {platform.startsWith("windows") ? (
          <code className="block mt-1">
            PowerShell -ExecutionPolicy Bypass -File install-{platform}.ps1
          </code>
        ) : (
          <code className="block mt-1">
            chmod +x install-{platform}.sh && ./install-{platform}.sh
          </code>
        )}
      </div>
    </div>
  );
}
```

### 5. Verification Steps

```jsx
// Post-installation verification guide
<VerificationSteps>
  <Step number={1} title="Verify Installation">
    <CodeBlock>the0 version</CodeBlock>
    <p>Should output: THE0 CLI v2024.01.20-456</p>
  </Step>

  <Step number={2} title="Check Authentication">
    <CodeBlock>the0 auth login</CodeBlock>
    <p>Follow the browser authentication flow</p>
  </Step>

  <Step number={3} title="Test Basic Command">
    <CodeBlock>the0 bot list</CodeBlock>
    <p>Lists your trading bots (may be empty initially)</p>
  </Step>
</VerificationSteps>
```

## DOCUMENTATION:

### 1. **Dynamic Script URL Generation**

- Next.js API routes at `/api/install/[platform]` that generate platform-specific scripts
- Environment variable configuration for CLI download URLs (`CLI_DOWNLOAD_BASE_URL`)
- Template-based script generation with dynamic URL injection
- Support for staging/production environment switching

### 2. **Platform-Specific URL Mapping**

- Consistent URL pattern: `/api/install/{platform}` (e.g., `/api/install/darwin-arm64`)
- File extension handling: `.sh` for Unix systems, `.ps1` for Windows
- Command generation with appropriate tools (`curl`/`bash` vs `iwr`/`iex`)
- Download headers and content-type configuration

### 3. **Script Generation and Templating**

- Dynamic template replacement for base URLs, platforms, and timestamps
- Error handling and fallback mechanisms in generated scripts
- Script validation and testing infrastructure
- Version information and source attribution in generated scripts

### 2. **Environment Variable Configuration**

```bash
# Production environment
CLI_DOWNLOAD_BASE_URL=https://storage.googleapis.com/the0-cli-releases
CLI_INSTALL_BASE_URL=https://the0.dev/cli-install
NEXT_PUBLIC_CLI_INSTALL_BASE_URL=https://the0.dev/cli-install

# Development environment
CLI_DOWNLOAD_BASE_URL=https://storage.googleapis.com/the0-cli-releases-staging
CLI_INSTALL_BASE_URL=http://localhost:3000
NEXT_PUBLIC_CLI_INSTALL_BASE_URL=http://localhost:3000
```

### 3. **GCS Binary URLs and Script Integration**

- Production: `https://storage.googleapis.com/the0-cli-releases/latest/`
- Staging: `https://storage.googleapis.com/the0-cli-releases-staging/latest/`
- Binary naming convention: `the0-{os}-{arch}` or `the0-{os}-{arch}.exe`
- Checksum files: `checksums.txt` for verification
- Script URLs: `https://the0.dev/cli-install/api/install/{platform}`

### 4. **Platform Detection and URL Generation**

- Client-side OS detection using `navigator.userAgent` and `navigator.platform`
- Server-side User-Agent parsing for accurate platform detection
- Automatic command generation based on detected platform
- Manual platform override with URL regeneration

### 4. **Next.js Implementation Patterns**

- Server-side rendering for SEO optimization
- Client-side platform detection for personalization
- API routes for analytics and telemetry
- Static generation for performance

### 5. **Analytics and Tracking**

- Installation attempt tracking by platform
- Success/failure rate monitoring
- User journey analysis from landing to successful CLI usage
- Error reporting for failed installations

## OTHER CONSIDERATIONS:

### 1. **Dynamic URL Generation and Caching**

- **Script URL consistency**: All install scripts accessible via predictable URLs (`/api/install/{platform}`)
- **Environment variable precedence**: `CLI_INSTALL_BASE_URL` for script hosting, `CLI_DOWNLOAD_BASE_URL` for binary downloads
- **Client-side URL generation**: Frontend generates appropriate one-liner commands based on detected platform
- **Caching strategy**: Generated scripts can be cached with proper cache headers and ETags

### 2. **Platform-Specific Installation Gotchas**

- **macOS**: Gatekeeper warnings for unsigned binaries - provide instructions for bypassing
- **Windows**: PowerShell execution policy may block scripts - include `Set-ExecutionPolicy` instructions
- **Linux**: Various distributions have different default shell configurations
- **Corporate environments**: Proxy settings and firewall restrictions may block downloads
- **Script hosting**: Ensure install domain (the0.dev/cli-install) has proper CORS and security headers

### 3. **Security and Trust**

- **HTTPS enforcement**: All downloads must use HTTPS for both scripts and binaries
- **Checksum verification**: Optional but recommended checksum verification in scripts
- **Domain reputation**: Ensure the0.dev/cli-install subdomain is properly configured with SSL
- **Script transparency**: Make installation scripts viewable at URLs before execution
- **Content-Type headers**: Proper MIME types for .sh and .ps1 files to prevent browser execution

### 4. **User Experience Priorities**

- **One-liner commands**: Primary installation method should be single copy-paste command
- **URL predictability**: Users can manually construct install URLs if needed
- **Progressive disclosure**: Show simple one-liner first, advanced options in tabs
- **Error recovery**: Clear instructions for common failure scenarios with fallback to manual download
- **Mobile experience**: Installation commands should be easily copyable on mobile devices

### 5. **Analytics and Optimization**

- **Conversion tracking**: Measure funnel from page visit to successful CLI usage
- **Platform distribution**: Track which platforms are most popular
- **Error analysis**: Collect and analyze installation failure patterns
- **A/B testing**: Test different copy and installation flows

### 6. **Alternative Installation Methods**

- **Package managers**: Future homebrew, apt, chocolatey support
- **Container images**: Docker image with CLI pre-installed
- **IDE plugins**: VS Code extension with embedded CLI
- **GitHub releases**: Direct download from GitHub releases page

### 7. **Accessibility Considerations**

- **Screen reader compatibility**: Proper ARIA labels and semantic HTML
- **Keyboard navigation**: All interactive elements accessible via keyboard
- **Color contrast**: High contrast for code blocks and important information
- **Text scaling**: Layout should work with browser zoom up to 200%

### 8. **SEO and Discovery**

- **Meta tags**: Proper title, description, and Open Graph tags
- **Structured data**: Schema.org markup for software application
- **Internal linking**: Link from main documentation and getting started guides
- **External linking**: Encourage linking from developer blogs and tutorials

### 9. **Internationalization Preparation**

- **Text externalization**: Prepare for future translation of installation instructions
- **Platform naming**: Consistent platform terminology across languages
- **Script localization**: Consider localized error messages in installation scripts
- **RTL support**: Basic RTL layout support for future expansion

### 10. **Testing and Quality Assurance**

- **Cross-platform testing**: Test installation on all supported platforms
- **Browser compatibility**: Ensure detection works across major browsers
- **Script validation**: Automated testing of generated installation scripts
- **Dead link monitoring**: Regular checks for broken download URLs
- **Performance monitoring**: Page load speed and script generation time
