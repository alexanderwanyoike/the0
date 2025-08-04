import type {
  PlatformInfo,
  PlatformDetectionResult,
  PlatformId,
  SUPPORTED_PLATFORMS,
} from '@/types/install';

// Platform detection utilities for client-side platform identification
export function detectPlatform(): PlatformInfo | null {
  // Server-side safe check
  if (typeof window === 'undefined') return null;

  const userAgent = window.navigator.userAgent.toLowerCase();
  const platform = window.navigator.platform.toLowerCase();

  // Detect macOS
  if (platform.includes('mac') || userAgent.includes('mac')) {
    const isAppleSilicon =
      userAgent.includes('arm') ||
      (window.navigator as any)?.userAgentData?.architecture === 'arm';

    const platformId: PlatformId = isAppleSilicon
      ? 'darwin-arm64'
      : 'darwin-amd64';

    return {
      id: platformId,
      os: 'macOS',
      arch: isAppleSilicon ? 'arm64' : 'x64',
      displayName: `macOS (${isAppleSilicon ? 'Apple Silicon' : 'Intel'})`,
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'sh',
      shellCommand: 'bash',
    };
  }

  // Detect Windows
  if (platform.includes('win') || userAgent.includes('windows')) {
    const platformId: PlatformId = 'windows-amd64';

    return {
      id: platformId,
      os: 'Windows',
      arch: 'x64',
      displayName: 'Windows',
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'ps1',
      shellCommand: 'powershell',
    };
  }

  // Detect Linux
  if (platform.includes('linux') || userAgent.includes('linux')) {
    // Detect ARM vs x64 architecture
    const isARM =
      userAgent.includes('arm') ||
      platform.includes('arm') ||
      platform.includes('aarch64');
    const platformId: PlatformId = isARM ? 'linux-arm64' : 'linux-amd64';

    return {
      id: platformId,
      os: 'Linux',
      arch: isARM ? 'arm64' : 'x64',
      displayName: `Linux (${isARM ? 'ARM64' : 'x64'})`,
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'sh',
      shellCommand: 'bash',
    };
  }

  return null; // Unknown platform
}

// Enhanced platform detection with confidence scoring
export function detectPlatformWithConfidence(): PlatformDetectionResult {
  if (typeof window === 'undefined') {
    return {
      platform: null,
      confidence: 'low',
      detectionMethod: 'server-side',
      fallback: true,
    };
  }

  const userAgent = window.navigator.userAgent.toLowerCase();
  const platform = window.navigator.platform.toLowerCase();
  const userAgentData = (window.navigator as any)?.userAgentData;

  let confidence: 'high' | 'medium' | 'low' = 'medium';
  let detectionMethod = 'user-agent';

  // Use User-Agent Client Hints if available (modern browsers)
  if (userAgentData) {
    confidence = 'high';
    detectionMethod = 'user-agent-client-hints';
  }

  // Check for specific platform indicators
  const hasStrongIndicators =
    platform.includes('mac') ||
    platform.includes('win32') ||
    platform.includes('linux') ||
    userAgent.includes('macintosh') ||
    userAgent.includes('windows nt') ||
    userAgent.includes('x11; linux');

  if (hasStrongIndicators) {
    confidence = confidence === 'high' ? 'high' : 'medium';
  } else {
    confidence = 'low';
  }

  const detectedPlatform = detectPlatform();

  return {
    platform: detectedPlatform,
    confidence,
    detectionMethod,
    fallback: !detectedPlatform,
  };
}

// Generate install command for a given platform
function generateInstallCommand(platformId: PlatformId): string {
  const baseUrl =
    process.env.NEXT_PUBLIC_CLI_INSTALL_BASE_URL ||
    (typeof window !== 'undefined' ? window.location.origin : '');

  switch (platformId) {
    case 'darwin-amd64':
    case 'darwin-arm64':
    case 'linux-amd64':
    case 'linux-arm64':
      return `curl -fsSL ${baseUrl}/api/install/${platformId} | bash`;

    case 'windows-amd64':
      return `iwr ${baseUrl}/api/install/${platformId} -useb | iex`;

    default:
      return `# Unknown platform: ${platformId}`;
  }
}

// Validate if platform is supported
export function validatePlatform(platformId: string): platformId is PlatformId {
  const supportedPlatforms: PlatformId[] = [
    'darwin-amd64',
    'darwin-arm64',
    'linux-amd64',
    'linux-arm64',
    'windows-amd64',
  ];

  return supportedPlatforms.includes(platformId as PlatformId);
}

// Get human-readable platform display name
export function getPlatformDisplayName(platformId: PlatformId): string {
  const displayNames: Record<PlatformId, string> = {
    'darwin-amd64': 'macOS (Intel)',
    'darwin-arm64': 'macOS (Apple Silicon)',
    'linux-amd64': 'Linux (x64)',
    'linux-arm64': 'Linux (ARM64)',
    'windows-amd64': 'Windows',
  };

  return displayNames[platformId] || platformId;
}

// Get all available platforms for manual selection
export function getAllPlatforms(): PlatformInfo[] {
  const supportedPlatforms: PlatformId[] = [
    'darwin-amd64',
    'darwin-arm64',
    'linux-amd64',
    'linux-arm64',
    'windows-amd64',
  ];

  return supportedPlatforms.map((platformId) => {
    const isWindows = platformId.startsWith('windows');

    return {
      id: platformId,
      os: platformId.startsWith('darwin')
        ? 'macOS'
        : platformId.startsWith('linux')
          ? 'Linux'
          : 'Windows',
      arch: platformId.includes('arm64') ? 'arm64' : 'x64',
      displayName: getPlatformDisplayName(platformId),
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: isWindows ? 'ps1' : 'sh',
      shellCommand: isWindows ? 'powershell' : 'bash',
    };
  });
}

// Server-side platform suggestion based on request headers
export function suggestPlatformFromHeaders(
  userAgent?: string,
): PlatformInfo | null {
  if (!userAgent) return null;

  const ua = userAgent.toLowerCase();

  // macOS detection
  if (ua.includes('macintosh') || ua.includes('mac os x')) {
    // Try to detect Apple Silicon vs Intel
    const isAppleSilicon = ua.includes('arm64') || ua.includes('aarch64');
    const platformId: PlatformId = isAppleSilicon
      ? 'darwin-arm64'
      : 'darwin-amd64';

    return {
      id: platformId,
      os: 'macOS',
      arch: isAppleSilicon ? 'arm64' : 'x64',
      displayName: getPlatformDisplayName(platformId),
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'sh',
      shellCommand: 'bash',
    };
  }

  // Windows detection
  if (
    ua.includes('windows nt') ||
    ua.includes('win32') ||
    ua.includes('win64')
  ) {
    const platformId: PlatformId = 'windows-amd64';

    return {
      id: platformId,
      os: 'Windows',
      arch: 'x64',
      displayName: getPlatformDisplayName(platformId),
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'ps1',
      shellCommand: 'powershell',
    };
  }

  // Linux detection
  if (ua.includes('linux') || ua.includes('x11')) {
    const isARM = ua.includes('arm') || ua.includes('aarch64');
    const platformId: PlatformId = isARM ? 'linux-arm64' : 'linux-amd64';

    return {
      id: platformId,
      os: 'Linux',
      arch: isARM ? 'arm64' : 'x64',
      displayName: getPlatformDisplayName(platformId),
      command: generateInstallCommand(platformId),
      scriptUrl: `/api/install/${platformId}`,
      fileExtension: 'sh',
      shellCommand: 'bash',
    };
  }

  return null;
}

// Feature detection for clipboard API support
export function supportsClipboardAPI(): boolean {
  return (
    typeof navigator !== 'undefined' &&
    'clipboard' in navigator &&
    'writeText' in navigator.clipboard
  );
}

// Feature detection for User-Agent Client Hints
export function supportsUserAgentClientHints(): boolean {
  return typeof navigator !== 'undefined' && 'userAgentData' in navigator;
}
