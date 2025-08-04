'use client';

import { useState } from 'react';
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card';
import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from '@/components/ui/collapsible';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { CodeBlock } from './quick-copy-section';
import { getTroubleshootingUrls } from '@/lib/install/install-urls';
import {
  ChevronDown,
  ChevronRight,
  AlertTriangle,
  HelpCircle,
  ExternalLink,
  Shield,
  Network,
  Settings,
  Terminal,
} from 'lucide-react';
import type {
  TroubleshootingProps,
  TroubleshootingItem,
  PlatformId,
} from '@/types/install';

export function TroubleshootingSection({ platform }: TroubleshootingProps) {
  const [openItems, setOpenItems] = useState<Set<string>>(new Set());

  const troubleshootingItems = getTroubleshootingItems(platform as PlatformId);
  const urls = getTroubleshootingUrls(platform as PlatformId);

  const toggleItem = (itemId: string) => {
    const newOpenItems = new Set(openItems);
    if (newOpenItems.has(itemId)) {
      newOpenItems.delete(itemId);
    } else {
      newOpenItems.add(itemId);
    }
    setOpenItems(newOpenItems);
  };

  const groupedItems = troubleshootingItems.reduce(
    (acc, item) => {
      if (!acc[item.category]) {
        acc[item.category] = [];
      }
      acc[item.category].push(item);
      return acc;
    },
    {} as Record<string, TroubleshootingItem[]>,
  );

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <HelpCircle className="h-5 w-5" />
          <span>Troubleshooting</span>
        </CardTitle>
        <CardDescription>
          Common installation issues and solutions for{' '}
          {platform ? `${platform}` : 'your platform'}.
        </CardDescription>
      </CardHeader>

      <CardContent className="space-y-6">
        {/* Quick help links */}
        <div className="flex flex-wrap gap-2">
          <Button variant="outline" size="sm" asChild>
            <a href={urls.general} target="_blank" rel="noopener noreferrer">
              <ExternalLink className="h-4 w-4 mr-2" />
              General Help
            </a>
          </Button>
          <Button variant="outline" size="sm" asChild>
            <a href={urls.platform} target="_blank" rel="noopener noreferrer">
              <ExternalLink className="h-4 w-4 mr-2" />
              Platform Guide
            </a>
          </Button>
          <Button variant="outline" size="sm" asChild>
            <a href={urls.community} target="_blank" rel="noopener noreferrer">
              <ExternalLink className="h-4 w-4 mr-2" />
              Community Help
            </a>
          </Button>
        </div>

        {/* Troubleshooting categories */}
        {Object.entries(groupedItems).map(([category, items]) => (
          <TroubleshootingCategory
            key={category}
            category={category}
            items={items}
            openItems={openItems}
            onToggleItem={toggleItem}
          />
        ))}

        {/* Still need help section */}
        <div className="pt-4 border-t">
          <h4 className="font-medium mb-2">Still Need Help?</h4>
          <div className="space-y-2 text-sm text-muted-foreground">
            <p>
              If you&apos;re still experiencing issues, here are additional
              resources:
            </p>
            <ul className="space-y-1 ml-4">
              <li>
                • Check our{' '}
                <a
                  href={urls.general}
                  className="text-blue-600 hover:underline"
                >
                  troubleshooting documentation
                </a>
              </li>
              <li>
                • Ask the community on our{' '}
                <a
                  href={urls.community}
                  className="text-blue-600 hover:underline"
                >
                  support forum
                </a>
              </li>
              <li>
                • Report bugs on{' '}
                <a
                  href="https://github.com/the0-org/cli/issues"
                  className="text-blue-600 hover:underline"
                >
                  GitHub
                </a>
              </li>
              <li>• Contact support via email: support@the0.dev</li>
            </ul>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

// Troubleshooting category component
function TroubleshootingCategory({
  category,
  items,
  openItems,
  onToggleItem,
}: {
  category: string;
  items: TroubleshootingItem[];
  openItems: Set<string>;
  onToggleItem: (itemId: string) => void;
}) {
  const categoryIcon =
    {
      permissions: Shield,
      network: Network,
      platform: Settings,
      configuration: Terminal,
    }[category] || AlertTriangle;

  const CategoryIcon = categoryIcon;
  const categoryTitle =
    {
      permissions: 'Permissions & Security',
      network: 'Network & Downloads',
      platform: 'Platform Specific',
      configuration: 'Configuration',
    }[category] || 'Other Issues';

  return (
    <div className="space-y-3">
      <div className="flex items-center space-x-2">
        <CategoryIcon className="h-4 w-4" />
        <h4 className="font-medium">{categoryTitle}</h4>
        <Badge variant="secondary" className="text-xs">
          {items.length}
        </Badge>
      </div>

      <div className="space-y-2">
        {items.map((item) => (
          <TroubleshootingItem
            key={item.id}
            item={item}
            isOpen={openItems.has(item.id)}
            onToggle={() => onToggleItem(item.id)}
          />
        ))}
      </div>
    </div>
  );
}

// Individual troubleshooting item
function TroubleshootingItem({
  item,
  isOpen,
  onToggle,
}: {
  item: TroubleshootingItem;
  isOpen: boolean;
  onToggle: () => void;
}) {
  return (
    <Collapsible open={isOpen} onOpenChange={onToggle}>
      <CollapsibleTrigger asChild>
        <Button variant="ghost" className="w-full justify-between p-3 h-auto">
          <div className="flex items-start space-x-2 text-left">
            <AlertTriangle className="h-4 w-4 mt-0.5 text-orange-500 flex-shrink-0" />
            <div>
              <p className="font-medium">{item.title}</p>
              <p className="text-sm text-muted-foreground">
                {item.description}
              </p>
            </div>
          </div>
          {isOpen ? (
            <ChevronDown className="h-4 w-4 flex-shrink-0" />
          ) : (
            <ChevronRight className="h-4 w-4 flex-shrink-0" />
          )}
        </Button>
      </CollapsibleTrigger>

      <CollapsibleContent className="px-3 pb-3">
        <div className="space-y-3 mt-2">
          <div className="bg-muted p-3 rounded-md">
            <h5 className="font-medium text-sm mb-2">Solution:</h5>
            <div className="text-sm space-y-2">
              {item.solution.split('\n').map((line, index) => {
                if (line.trim().startsWith('```')) {
                  return null; // Skip code block markers
                }
                if (line.trim().includes('$') || line.trim().includes('>')) {
                  return (
                    <CodeBlock key={index} code={line.trim()} showCopy={true} />
                  );
                }
                return line.trim() ? <p key={index}>{line.trim()}</p> : null;
              })}
            </div>
          </div>
        </div>
      </CollapsibleContent>
    </Collapsible>
  );
}

// Get troubleshooting items for a platform
function getTroubleshootingItems(platform: PlatformId): TroubleshootingItem[] {
  const commonItems: TroubleshootingItem[] = [
    {
      id: 'command-not-found',
      title: 'Command not found: the0',
      description: 'The CLI was installed but the command is not recognized.',
      solution: `The installation directory might not be in your PATH. Try these solutions:

1. Restart your terminal/shell
2. Add the installation directory to your PATH
3. Use the full path to the binary

export PATH="/usr/local/bin:$PATH"

Or find where it was installed:
which the0`,
      category: 'configuration',
    },
    {
      id: 'download-failed',
      title: 'Download failed or timed out',
      description: 'The installation script cannot download the CLI binary.',
      solution: `This is usually a network connectivity issue:

1. Check your internet connection
2. Try again with a VPN if behind a corporate firewall
3. Download manually from the releases page
4. Check if antivirus is blocking the download

curl -v https://github.com/the0-org/cli/releases/latest`,
      category: 'network',
    },
    {
      id: 'checksum-mismatch',
      title: 'Checksum verification failed',
      description: 'The downloaded binary failed integrity verification.',
      solution: `The download may be corrupted or incomplete:

1. Delete the downloaded file and try again
2. Check available disk space
3. Try downloading from a different network
4. Report this issue if it persists

rm -f /tmp/the0-installer*`,
      category: 'network',
    },
  ];

  // Platform-specific items
  const platformItems: Record<PlatformId, TroubleshootingItem[]> = {
    'darwin-amd64': [
      {
        id: 'gatekeeper-block',
        title: 'macOS Gatekeeper blocking execution',
        description:
          '"the0" cannot be opened because it is from an unidentified developer.',
        solution: `macOS Gatekeeper is blocking the unsigned binary:

1. Right-click the binary and select "Open"
2. Or use the command line to bypass:
   xattr -d com.apple.quarantine /usr/local/bin/the0

3. Or allow in System Preferences > Security & Privacy`,
        category: 'platform',
      },
      {
        id: 'rosetta-required',
        title: 'Architecture mismatch on Apple Silicon',
        description: 'The Intel binary was installed on Apple Silicon Mac.',
        solution: `You might need to install Rosetta 2 or use the ARM64 version:

1. Install Rosetta 2:
   softwareupdate --install-rosetta

2. Or download the ARM64 version specifically for Apple Silicon`,
        category: 'platform',
      },
    ],
    'darwin-arm64': [
      {
        id: 'gatekeeper-block-arm64',
        title: 'macOS Gatekeeper blocking execution',
        description:
          '"the0" cannot be opened because it is from an unidentified developer.',
        solution: `macOS Gatekeeper is blocking the unsigned binary:

1. Right-click the binary and select "Open"
2. Or use the command line to bypass:
   xattr -d com.apple.quarantine /usr/local/bin/the0

3. Or allow in System Preferences > Security & Privacy`,
        category: 'platform',
      },
    ],
    'linux-amd64': [
      {
        id: 'permission-denied',
        title: 'Permission denied when running the0',
        description: 'The binary file does not have execute permissions.',
        solution: `Make the binary executable:

chmod +x /usr/local/bin/the0

Or if installed elsewhere:
chmod +x /path/to/the0`,
        category: 'permissions',
      },
      {
        id: 'missing-dependencies',
        title: 'Missing system dependencies',
        description: 'Error about missing shared libraries or system packages.',
        solution: `Install required system dependencies:

# Ubuntu/Debian:
sudo apt update && sudo apt install ca-certificates curl

# CentOS/RHEL/Fedora:
sudo yum install ca-certificates curl

# Or:
sudo dnf install ca-certificates curl`,
        category: 'platform',
      },
    ],
    'linux-arm64': [
      {
        id: 'permission-denied-arm64',
        title: 'Permission denied when running the0',
        description: 'The binary file does not have execute permissions.',
        solution: `Make the binary executable:

chmod +x /usr/local/bin/the0

Or if installed elsewhere:
chmod +x /path/to/the0`,
        category: 'permissions',
      },
      {
        id: 'architecture-error',
        title: 'Cannot execute binary file',
        description: 'The binary format is not compatible with your system.',
        solution: `This usually means architecture mismatch:

1. Verify your architecture:
   uname -m

2. Ensure you downloaded the ARM64 version
3. Check if you're in a container with different architecture`,
        category: 'platform',
      },
    ],
    'windows-amd64': [
      {
        id: 'execution-policy',
        title: 'PowerShell execution policy restriction',
        description: 'Cannot run installation script due to execution policy.',
        solution: `Temporarily allow script execution:

Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

Or run the installer directly:
powershell -ExecutionPolicy Bypass -File install.ps1`,
        category: 'permissions',
      },
      {
        id: 'antivirus-blocking',
        title: 'Antivirus software blocking installation',
        description:
          'Windows Defender or other antivirus blocking the download.',
        solution: `Temporarily disable real-time protection:

1. Open Windows Security
2. Go to Virus & threat protection
3. Temporarily disable real-time protection
4. Run the installation again
5. Re-enable protection after installation`,
        category: 'permissions',
      },
      {
        id: 'path-not-updated',
        title: 'PATH not updated after installation',
        description: 'Command not found even after successful installation.',
        solution: `Restart PowerShell or update PATH manually:

1. Close and reopen PowerShell/Command Prompt
2. Or add to PATH manually:
   $env:PATH += ";C:\\Program Files\\THE0"

3. For permanent PATH update, use System Properties`,
        category: 'configuration',
      },
    ],
  };

  return [...commonItems, ...(platformItems[platform] || [])];
}

// Compact troubleshooting component
export function CompactTroubleshooting({ platform }: TroubleshootingProps) {
  const items = getTroubleshootingItems(platform as PlatformId);
  const topIssues = items.slice(0, 3); // Show only top 3 issues

  return (
    <Card>
      <CardHeader className="pb-3">
        <CardTitle className="text-base">Common Issues</CardTitle>
      </CardHeader>
      <CardContent className="space-y-2">
        {topIssues.map((item) => (
          <div key={item.id} className="p-2 border rounded-md">
            <h5 className="text-sm font-medium">{item.title}</h5>
            <p className="text-xs text-muted-foreground">{item.description}</p>
          </div>
        ))}
        <Button variant="outline" size="sm" className="w-full">
          <HelpCircle className="h-4 w-4 mr-2" />
          View All Issues
        </Button>
      </CardContent>
    </Card>
  );
}
