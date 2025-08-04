import {
  generateInstallScript,
  validateScriptContent,
  getScriptTemplate,
  generateScriptChecksum,
} from '../script-generation';
import type { PlatformId, ScriptGenerationConfig } from '@/types/install';

describe('Script Generation - Simple Tests', () => {
  const mockConfig: ScriptGenerationConfig = {
    baseUrl: 'https://github.com/theo-cli/theo/releases/latest/download',
    installBaseUrl: 'https://the0.dev',
    channel: 'latest',
  };

  beforeEach(() => {
    process.env.CLI_DOWNLOAD_BASE_URL = mockConfig.baseUrl;
    process.env.CLI_INSTALL_BASE_URL = mockConfig.installBaseUrl;
  });

  describe('generateInstallScript', () => {
    it('should generate Bash script for macOS Intel', () => {
      const script = generateInstallScript('darwin-amd64', mockConfig);

      expect(script).toContain('#!/bin/bash');
      expect(script).toContain('# THE0 CLI Installation Script');
      expect(script).toContain('# Platform: darwin-amd64');
      expect(script).toContain('# Channel: latest');
      expect(script).toContain('the0-darwin-amd64');
      expect(script).toContain('$HOME/bin');
    });

    it('should generate PowerShell script for Windows', () => {
      const script = generateInstallScript('windows-amd64', mockConfig);

      expect(script).toContain('# THE0 CLI Installation Script for Windows');
      expect(script).toContain('# Platform: windows-amd64');
      expect(script).toContain('$ErrorActionPreference = "Stop"');
      expect(script).toContain('the0-windows-amd64.exe');
      expect(script).toContain('$env:USERPROFILE');
    });

    it('should include timestamp in generated script', () => {
      const script = generateInstallScript('darwin-amd64', mockConfig);

      expect(script).toMatch(
        /# Generated: \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/,
      );
    });
  });

  describe('validateScriptContent', () => {
    it('should validate safe Bash scripts', () => {
      const safeScript = `#!/bin/bash
echo "Installing CLI"
curl -fsSL https://example.com/install.sh | bash`;

      expect(validateScriptContent(safeScript, 'sh')).toBe(true);
    });

    it('should validate safe PowerShell scripts', () => {
      const safeScript = `$ErrorActionPreference = "Stop"
Write-Host "Installing CLI"`;

      expect(validateScriptContent(safeScript, 'ps1')).toBe(true);
    });

    it('should reject dangerous rm commands', () => {
      const dangerousScript = `#!/bin/bash
rm -rf /usr`;

      expect(validateScriptContent(dangerousScript, 'sh')).toBe(false);
    });

    it('should reject scripts without proper headers', () => {
      const badScript = `echo "no shebang"`;

      expect(validateScriptContent(badScript, 'sh')).toBe(false);
    });
  });

  describe('getScriptTemplate', () => {
    it('should return shell template for Unix platforms', () => {
      const templates = [
        getScriptTemplate('darwin-amd64'),
        getScriptTemplate('darwin-arm64'),
        getScriptTemplate('linux-amd64'),
        getScriptTemplate('linux-arm64'),
      ];

      templates.forEach((template) => {
        expect(template.fileExtension).toBe('sh');
        expect(template.shellCommand).toBe('bash');
        expect(template.contentType).toBe('application/x-sh');
      });
    });

    it('should return PowerShell template for Windows', () => {
      const template = getScriptTemplate('windows-amd64');

      expect(template.fileExtension).toBe('ps1');
      expect(template.shellCommand).toBe('powershell');
      expect(template.contentType).toBe('application/x-powershell');
    });
  });

  describe('generateScriptChecksum', () => {
    it('should generate consistent checksums', () => {
      const script = 'test script content';
      const checksum1 = generateScriptChecksum(script);
      const checksum2 = generateScriptChecksum(script);

      expect(checksum1).toBe(checksum2);
      expect(typeof checksum1).toBe('string');
      expect(checksum1.length).toBeGreaterThan(0);
    });

    it('should generate different checksums for different content', () => {
      const script1 = 'test script 1';
      const script2 = 'test script 2';

      const checksum1 = generateScriptChecksum(script1);
      const checksum2 = generateScriptChecksum(script2);

      expect(checksum1).not.toBe(checksum2);
    });

    it('should return hexadecimal string', () => {
      const script = 'test script';
      const checksum = generateScriptChecksum(script);

      expect(checksum).toMatch(/^[0-9a-f]+$/);
    });
  });
});
