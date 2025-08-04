import type { PlatformId, InstallConfig } from '@/types/install';

// URL generation and mapping configuration
export class InstallUrlGenerator {
  private baseUrl: string;
  private installBaseUrl: string;
  private channel: string;

  constructor(config: Partial<InstallConfig> = {}) {
    this.baseUrl =
      config.baseUrl ||
      process.env.CLI_DOWNLOAD_BASE_URL ||
      'https://github.com/theo-cli/theo/releases/latest/download';
    this.installBaseUrl =
      config.installBaseUrl ||
      process.env.CLI_INSTALL_BASE_URL ||
      (typeof window !== 'undefined' ? window.location.origin : '');
    this.channel = 'latest'; // Default to latest stable release
  }

  // Generate download URL for binary
  getBinaryDownloadUrl(platformId: PlatformId): string {
    const binaryName = this.getBinaryName(platformId);
    return `${this.baseUrl}/${binaryName}`;
  }

  // Generate script download URL
  getScriptUrl(platformId: PlatformId): string {
    return `${this.installBaseUrl}/api/install/${platformId}`;
  }

  // Generate one-liner install command
  getInstallCommand(platformId: PlatformId): string {
    const scriptUrl = this.getScriptUrl(platformId);

    if (platformId.startsWith('windows')) {
      return `iwr ${scriptUrl} -useb | iex`;
    } else {
      return `curl -fsSL ${scriptUrl} | bash`;
    }
  }

  // Get binary name for platform
  private getBinaryName(platformId: PlatformId): string {
    const baseName = 'the0';

    if (platformId.startsWith('windows')) {
      return `${baseName}-${platformId}.exe`;
    } else {
      return `${baseName}-${platformId}`;
    }
  }

  // Generate manual download URLs
  getManualDownloadUrls(platformId: PlatformId): {
    binary: string;
    script: string;
    checksum?: string;
  } {
    return {
      binary: this.getBinaryDownloadUrl(platformId),
      script: this.getScriptUrl(platformId),
      // checksum: `${this.getBinaryDownloadUrl(platformId)}.sha256` // Future enhancement
    };
  }

  // Get alternative installation methods
  getAlternativeInstallMethods(platformId: PlatformId): Array<{
    method: string;
    title: string;
    description: string;
    command?: string;
    url?: string;
    available: boolean;
  }> {
    const alternatives = [];

    // Homebrew for macOS
    if (platformId.startsWith('darwin')) {
      alternatives.push({
        method: 'homebrew',
        title: 'Homebrew',
        description: 'Install via Homebrew package manager',
        command: 'brew install theo-cli',
        available: false, // Not yet available
      });
    }

    // Chocolatey for Windows
    if (platformId.startsWith('windows')) {
      alternatives.push({
        method: 'chocolatey',
        title: 'Chocolatey',
        description: 'Install via Chocolatey package manager',
        command: 'choco install theo-cli',
        available: false, // Not yet available
      });

      alternatives.push({
        method: 'winget',
        title: 'Windows Package Manager',
        description: 'Install via winget',
        command: 'winget install theo-cli',
        available: false, // Not yet available
      });
    }

    // Snap for Linux
    if (platformId.startsWith('linux')) {
      alternatives.push({
        method: 'snap',
        title: 'Snap Package',
        description: 'Install via Snap package manager',
        command: 'sudo snap install theo-cli',
        available: false, // Not yet available
      });

      alternatives.push({
        method: 'apt',
        title: 'APT Repository',
        description: 'Install via APT package manager (Debian/Ubuntu)',
        command: 'sudo apt install theo-cli',
        available: false, // Not yet available
      });
    }

    // Docker (available for all platforms)
    alternatives.push({
      method: 'docker',
      title: 'Docker',
      description: 'Run THE0 CLI in a Docker container',
      command: 'docker run --rm -it theo/cli:latest',
      available: false, // Not yet available
    });

    // NPM global install (if available)
    alternatives.push({
      method: 'npm',
      title: 'NPM Global Install',
      description: 'Install globally via npm',
      command: 'npm install -g @theo/cli',
      available: false, // Not yet available
    });

    return alternatives;
  }
}

// Default URL generator instance
export const urlGenerator = new InstallUrlGenerator();

// Platform-specific URL mappings
export const PLATFORM_URLS: Record<
  PlatformId,
  {
    binary: string;
    script: string;
    docs: string;
  }
> = {
  'darwin-amd64': {
    binary: urlGenerator.getBinaryDownloadUrl('darwin-amd64'),
    script: urlGenerator.getScriptUrl('darwin-amd64'),
    docs: `${process.env.NEXT_BASE_URL}/docs/the0-CLI/installation`,
  },
  'darwin-arm64': {
    binary: urlGenerator.getBinaryDownloadUrl('darwin-arm64'),
    script: urlGenerator.getScriptUrl('darwin-arm64'),
    docs: `${process.env.NEXT_BASE_URL}/docs/the0-CLI/installation`,
  },
  'linux-amd64': {
    binary: urlGenerator.getBinaryDownloadUrl('linux-amd64'),
    script: urlGenerator.getScriptUrl('linux-amd64'),
    docs: `${process.env.NEXT_BASE_URL}/docs/the0-CLI/installation`,
  },
  'linux-arm64': {
    binary: urlGenerator.getBinaryDownloadUrl('linux-arm64'),
    script: urlGenerator.getScriptUrl('linux-arm64'),
    docs: `${process.env.NEXT_BASE_URL}/docs/the0-CLI/installation`,
  },
  'windows-amd64': {
    binary: urlGenerator.getBinaryDownloadUrl('windows-amd64'),
    script: urlGenerator.getScriptUrl('windows-amd64'),
    docs: `${process.env.NEXT_BASE_URL}/docs/the0-CLI/installation`,
  },
};

// Generate platform-specific installation commands
export function generatePlatformCommands(platformId: PlatformId): {
  oneLiner: string;
  scriptDownload: string;
  manualSteps: string[];
  verification: string[];
} {
  const isWindows = platformId.startsWith('windows');
  const binaryName = isWindows ? 'the0.exe' : 'the0';
  const scriptUrl = urlGenerator.getScriptUrl(platformId);
  const binaryUrl = urlGenerator.getBinaryDownloadUrl(platformId);

  if (isWindows) {
    return {
      oneLiner: `iwr ${scriptUrl} -useb | iex`,
      scriptDownload: `iwr ${scriptUrl} -OutFile install.ps1; .\\install.ps1`,
      manualSteps: [
        `Download the binary from: ${binaryUrl}`,
        'Create a directory: C:\\Program Files\\THE0',
        'Move the downloaded file to: C:\\Program Files\\THE0\\the0.exe',
        'Add C:\\Program Files\\THE0 to your PATH environment variable',
        'Restart your terminal or PowerShell session',
      ],
      verification: ['the0 --version', 'the0 --help', 'the0 auth login'],
    };
  } else {
    const installDir = platformId.startsWith('darwin')
      ? '/usr/local/bin'
      : '/usr/local/bin';

    return {
      oneLiner: `curl -fsSL ${scriptUrl} | bash`,
      scriptDownload: `curl -fsSL ${scriptUrl} -o install.sh && bash install.sh`,
      manualSteps: [
        `Download the binary: curl -fsSL ${binaryUrl} -o ${binaryName}`,
        `Make it executable: chmod +x ${binaryName}`,
        `Move to system path: sudo mv ${binaryName} ${installDir}/`,
        'Verify installation with: the0 --version',
      ],
      verification: ['the0 --version', 'the0 --help', 'the0 auth login'],
    };
  }
}

// Get platform-specific troubleshooting URLs
export function getTroubleshootingUrls(platformId: PlatformId): {
  general: string;
  platform: string;
  community: string;
} {
  const baseDocsUrl = 'https://the0.dev/docs/the0-CLI';

  return {
    general: `${baseDocsUrl}/installation`,
    platform: PLATFORM_URLS[platformId].docs,
    community: 'https://discord.gg/qafCfTA5',
  };
}

// Cache management for URLs
export class UrlCache {
  private cache = new Map<string, { value: string; timestamp: number }>();
  private ttl = 5 * 60 * 1000; // 5 minutes

  get(key: string): string | null {
    const cached = this.cache.get(key);
    if (!cached) return null;

    if (Date.now() - cached.timestamp > this.ttl) {
      this.cache.delete(key);
      return null;
    }

    return cached.value;
  }

  set(key: string, value: string): void {
    this.cache.set(key, {
      value,
      timestamp: Date.now(),
    });
  }

  clear(): void {
    this.cache.clear();
  }
}

// Global URL cache instance
export const urlCache = new UrlCache();

// Helper function to build URLs with caching
export function getCachedUrl(key: string, generator: () => string): string {
  const cached = urlCache.get(key);
  if (cached) return cached;

  const url = generator();
  urlCache.set(key, url);
  return url;
}

// Validate URL format
export function validateUrl(url: string): boolean {
  try {
    new URL(url);
    return true;
  } catch {
    return false;
  }
}

// Generate tracking parameters for analytics
export function addTrackingParams(
  url: string,
  params: Record<string, string>,
): string {
  try {
    const urlObj = new URL(url);
    Object.entries(params).forEach(([key, value]) => {
      urlObj.searchParams.set(key, value);
    });
    return urlObj.toString();
  } catch {
    return url; // Return original URL if parsing fails
  }
}
