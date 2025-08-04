// Platform identification and command generation types
export interface PlatformInfo {
  id: string; // 'darwin-arm64', 'linux-amd64', etc.
  os: string; // 'macOS', 'Linux', 'Windows'
  arch: string; // 'x64', 'arm64'
  displayName: string; // 'macOS (Apple Silicon)'
  command: string; // Generated install command
  scriptUrl: string; // API endpoint for script download
  fileExtension: 'sh' | 'ps1'; // Script file type
  shellCommand: 'bash' | 'powershell'; // Execution shell
}

// Installation method types
export type InstallMethod = 'oneliner' | 'script' | 'manual' | 'package';

// Component prop interfaces
export interface InstallationPageProps {
  detectedPlatform: PlatformInfo | null;
  availablePlatforms: PlatformInfo[];
}

export interface PlatformDetectorProps {
  onPlatformDetected: (platform: PlatformInfo | null) => void;
  children?: React.ReactNode;
}

export interface PlatformSelectorProps {
  platforms: PlatformInfo[];
  selectedPlatform: PlatformInfo | null;
  onPlatformChange: (platform: PlatformInfo) => void;
}

export interface QuickCopySectionProps {
  command: string;
  platform?: string;
}

export interface InstallationTabsProps {
  platform: PlatformInfo | null;
}

export interface VerificationStepsProps {
  platform?: string;
}

export interface TroubleshootingProps {
  platform: string;
}

// API response interfaces
export interface ScriptGenerationResponse {
  script: string;
  platform: string;
  timestamp: string;
  checksum?: string;
}

export interface ScriptGenerationError {
  error: string;
  platform?: string;
  details?: string;
}

// Environment configuration interface
export interface InstallConfig {
  baseUrl: string; // CLI download base URL
  installBaseUrl: string; // Installation page base URL
  supportedPlatforms: string[]; // Supported platform IDs
  channel?: string; // Release channel (latest, beta, etc.)
}

// Script generation configuration
export interface ScriptGenerationConfig {
  baseUrl?: string;
  installBaseUrl?: string;
  channel?: string;
}

// Platform detection result
export interface PlatformDetectionResult {
  platform: PlatformInfo | null;
  confidence: 'high' | 'medium' | 'low';
  detectionMethod: string;
  fallback?: boolean;
}

// Validation types
export type PlatformId =
  | 'darwin-amd64'
  | 'darwin-arm64'
  | 'linux-amd64'
  | 'linux-arm64'
  | 'windows-amd64';

export const SUPPORTED_PLATFORMS: PlatformId[] = [
  'darwin-amd64',
  'darwin-arm64',
  'linux-amd64',
  'linux-arm64',
  'windows-amd64',
];

// Verification step interface
export interface VerificationStep {
  id: string;
  title: string;
  description: string;
  command: string;
  expectedOutput?: string;
  troubleshootingLink?: string;
}

// Troubleshooting item interface
export interface TroubleshootingItem {
  id: string;
  title: string;
  description: string;
  solution: string;
  platform?: PlatformId;
  category: 'permissions' | 'network' | 'platform' | 'configuration';
}

// Installation analytics event
export interface InstallationEvent {
  eventType:
    | 'platform_detected'
    | 'command_copied'
    | 'script_downloaded'
    | 'installation_completed';
  platform: PlatformId;
  installMethod: InstallMethod;
  timestamp: string;
  userAgent?: string;
  success?: boolean;
  errorMessage?: string;
}
