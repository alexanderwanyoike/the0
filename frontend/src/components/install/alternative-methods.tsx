'use client';

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { CodeBlock } from './quick-copy-section';
import { InstallUrlGenerator } from '@/lib/install/install-urls';
import {
  Package,
  Download,
  Container as DockerIcon,
  ExternalLink,
  Github,
  Globe,
  Coffee,
  Box,
  HardDrive,
  Info,
} from 'lucide-react';
import type { PlatformInfo } from '@/types/install';

interface AlternativeMethodsProps {
  platform: PlatformInfo;
}

export function AlternativeMethods({ platform }: AlternativeMethodsProps) {
  const urlGenerator = new InstallUrlGenerator();
  const alternatives = urlGenerator.getAlternativeInstallMethods(
    platform.id as any,
  );
  const manualUrls = urlGenerator.getManualDownloadUrls(platform.id as any);

  return (
    <div className="space-y-6">
      {/* Package Managers */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <Package className="h-5 w-5" />
            <span>Package Managers</span>
          </CardTitle>
          <CardDescription>
            Install THE0 CLI using your system&apos;s package manager for easier
            updates and management.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <PackageManagerGrid platform={platform} alternatives={alternatives} />
        </CardContent>
      </Card>

      {/* Manual Downloads */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <Download className="h-5 w-5" />
            <span>Manual Downloads</span>
          </CardTitle>
          <CardDescription>
            Download binaries directly for offline installation or air-gapped
            environments.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ManualDownloadSection platform={platform} urls={manualUrls} />
        </CardContent>
      </Card>

      {/* Container Images */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <DockerIcon className="h-5 w-5" />
            <span>Container Images</span>
          </CardTitle>
          <CardDescription>
            Run THE0 CLI in containers for development environments and CI/CD
            pipelines.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ContainerSection />
        </CardContent>
      </Card>

      {/* Source Code */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center space-x-2">
            <Github className="h-5 w-5" />
            <span>Build from Source</span>
          </CardTitle>
          <CardDescription>
            Build THE0 CLI from source code for development or custom
            configurations.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <SourceBuildSection />
        </CardContent>
      </Card>
    </div>
  );
}

// Package manager grid component
function PackageManagerGrid({
  platform,
  alternatives,
}: {
  platform: PlatformInfo;
  alternatives: Array<{
    method: string;
    title: string;
    description: string;
    command?: string;
    url?: string;
    available: boolean;
  }>;
}) {
  if (alternatives.length === 0) {
    return (
      <Alert>
        <Info className="h-4 w-4" />
        <AlertDescription>
          No package managers are currently available for {platform.displayName}
          . Please use the one-liner installation method above.
        </AlertDescription>
      </Alert>
    );
  }

  return (
    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
      {alternatives.map((alt) => (
        <PackageManagerCard key={alt.method} method={alt} />
      ))}
    </div>
  );
}

// Individual package manager card
function PackageManagerCard({
  method,
}: {
  method: {
    method: string;
    title: string;
    description: string;
    command?: string;
    url?: string;
    available: boolean;
  };
}) {
  const icon = getPackageManagerIcon(method.method);
  const Icon = icon;

  return (
    <div className="p-4 border rounded-lg space-y-3">
      <div className="flex items-center justify-between">
        <div className="flex items-center space-x-2">
          <Icon className="h-5 w-5" />
          <h4 className="font-medium">{method.title}</h4>
        </div>
        <Badge variant={method.available ? 'default' : 'secondary'}>
          {method.available ? 'Available' : 'Coming Soon'}
        </Badge>
      </div>

      <p className="text-sm text-muted-foreground">{method.description}</p>

      {method.command && (
        <div className="space-y-2">
          <CodeBlock code={method.command} showCopy={method.available} />
          {!method.available && (
            <p className="text-xs text-muted-foreground">
              This command will be available once the package is published.
            </p>
          )}
        </div>
      )}

      {method.url && (
        <Button variant="outline" size="sm" asChild>
          <a href={method.url} target="_blank" rel="noopener noreferrer">
            <ExternalLink className="h-4 w-4 mr-2" />
            Learn More
          </a>
        </Button>
      )}
    </div>
  );
}

// Manual download section
function ManualDownloadSection({
  platform,
  urls,
}: {
  platform: PlatformInfo;
  urls: {
    binary: string;
    script: string;
    checksum?: string;
  };
}) {
  const binaryName = platform.os === 'Windows' ? 'the0.exe' : 'the0';

  return (
    <div className="space-y-4">
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="p-4 border rounded-lg space-y-3">
          <div className="flex items-center space-x-2">
            <HardDrive className="h-5 w-5" />
            <h4 className="font-medium">Binary Download</h4>
          </div>
          <p className="text-sm text-muted-foreground">
            Download the compiled binary for {platform.displayName}
          </p>
          <div className="space-y-2">
            <p className="text-xs text-muted-foreground">File: {binaryName}</p>
            <Button asChild className="w-full">
              <a href={urls.binary} download>
                <Download className="h-4 w-4 mr-2" />
                Download Binary
              </a>
            </Button>
          </div>
        </div>

        <div className="p-4 border rounded-lg space-y-3">
          <div className="flex items-center space-x-2">
            <Box className="h-5 w-5" />
            <h4 className="font-medium">Installation Script</h4>
          </div>
          <p className="text-sm text-muted-foreground">
            Download the installation script for offline use
          </p>
          <div className="space-y-2">
            <p className="text-xs text-muted-foreground">
              File: install.{platform.fileExtension}
            </p>
            <Button variant="outline" asChild className="w-full">
              <a href={urls.script} download>
                <Download className="h-4 w-4 mr-2" />
                Download Script
              </a>
            </Button>
          </div>
        </div>
      </div>

      <Alert>
        <Info className="h-4 w-4" />
        <AlertDescription>
          <strong>Security note:</strong> Always verify downloads from trusted
          sources. Check the file integrity and review installation scripts
          before execution.
        </AlertDescription>
      </Alert>
    </div>
  );
}

// Container section
function ContainerSection() {
  return (
    <div className="space-y-4">
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="p-4 border rounded-lg space-y-3">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-2">
              <DockerIcon className="h-5 w-5" />
              <h4 className="font-medium">Docker</h4>
            </div>
            <Badge variant="secondary">Coming Soon</Badge>
          </div>
          <p className="text-sm text-muted-foreground">
            Official Docker image for containerized environments
          </p>
          <CodeBlock
            code="docker run --rm -it the0/cli:latest --help"
            showCopy={false}
          />
        </div>

        <div className="p-4 border rounded-lg space-y-3">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-2">
              <Box className="h-5 w-5" />
              <h4 className="font-medium">Podman</h4>
            </div>
            <Badge variant="secondary">Coming Soon</Badge>
          </div>
          <p className="text-sm text-muted-foreground">
            Compatible with Podman for rootless containers
          </p>
          <CodeBlock
            code="podman run --rm -it the0/cli:latest --help"
            showCopy={false}
          />
        </div>
      </div>

      <div className="p-4 bg-muted rounded-lg">
        <h4 className="font-medium mb-2">Container Registry</h4>
        <p className="text-sm text-muted-foreground mb-3">
          Images will be available on multiple container registries:
        </p>
        <ul className="text-sm space-y-1">
          <li>
            • Docker Hub: <code>the0/cli</code>
          </li>
          <li>
            • GitHub Container Registry: <code>ghcr.io/the0-org/cli</code>
          </li>
          <li>
            • AWS ECR Public: <code>public.ecr.aws/the0/cli</code>
          </li>
        </ul>
      </div>
    </div>
  );
}

// Source build section
function SourceBuildSection() {
  return (
    <div className="space-y-4">
      <Alert>
        <Info className="h-4 w-4" />
        <AlertDescription>
          Building from source requires Go 1.21+ and basic development tools
          installed.
        </AlertDescription>
      </Alert>

      <div className="space-y-3">
        <h4 className="font-medium">Prerequisites</h4>
        <CodeBlock code="go version" />

        <h4 className="font-medium">Clone and Build</h4>
        <CodeBlock
          code={`git clone https://github.com/the0-org/cli.git
cd cli
make build
sudo make install`}
        />

        <h4 className="font-medium">Development Build</h4>
        <CodeBlock
          code={`git clone https://github.com/the0-org/cli.git
cd cli
make dev
./bin/the0 --version`}
        />
      </div>

      <div className="flex space-x-2">
        <Button variant="outline" size="sm" asChild>
          <a
            href="https://github.com/the0-org/cli"
            target="_blank"
            rel="noopener noreferrer"
          >
            <Github className="h-4 w-4 mr-2" />
            View Source
          </a>
        </Button>
        <Button variant="outline" size="sm" asChild>
          <a
            href="https://docs.the0.dev/development"
            target="_blank"
            rel="noopener noreferrer"
          >
            <ExternalLink className="h-4 w-4 mr-2" />
            Build Guide
          </a>
        </Button>
      </div>
    </div>
  );
}

// Get icon for package manager
function getPackageManagerIcon(method: string) {
  const icons = {
    homebrew: Coffee,
    chocolatey: Box,
    winget: Package,
    scoop: Box,
    apt: Package,
    snap: Package,
    flatpak: Package,
    npm: Globe,
    docker: DockerIcon,
    default: Package,
  };

  return icons[method as keyof typeof icons] || icons.default;
}

// Compact alternative methods for smaller spaces
export function CompactAlternativeMethods({
  platform,
}: AlternativeMethodsProps) {
  const urlGenerator = new InstallUrlGenerator();
  const manualUrls = urlGenerator.getManualDownloadUrls(platform.id as any);

  return (
    <Card>
      <CardHeader className="pb-3">
        <CardTitle className="text-base">Other Options</CardTitle>
      </CardHeader>
      <CardContent className="space-y-3">
        <Button variant="outline" asChild className="w-full justify-start">
          <a href={manualUrls.binary} download>
            <Download className="h-4 w-4 mr-2" />
            Manual Download
          </a>
        </Button>
        <Button variant="outline" asChild className="w-full justify-start">
          <a
            href="https://github.com/the0-org/cli"
            target="_blank"
            rel="noopener noreferrer"
          >
            <Github className="h-4 w-4 mr-2" />
            Build from Source
          </a>
        </Button>
        <Button variant="outline" disabled className="w-full justify-start">
          <DockerIcon className="h-4 w-4 mr-2" />
          Docker (Coming Soon)
        </Button>
      </CardContent>
    </Card>
  );
}
