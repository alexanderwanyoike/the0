'use client';

import { useState } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
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
import { QuickCopySection, CodeBlock } from './quick-copy-section';
import { generatePlatformCommands } from '@/lib/install/install-urls';
import { Download, Terminal, Zap, FileText } from 'lucide-react';
import type { InstallationTabsProps, PlatformInfo } from '@/types/install';

export function InstallationTabs({ platform }: InstallationTabsProps) {
  const [activeTab, setActiveTab] = useState('oneliner');

  if (!platform) {
    return (
      <Card>
        <CardContent className="py-12 text-center">
          <Terminal className="h-12 w-12 mx-auto text-muted-foreground mb-4" />
          <h3 className="text-lg font-semibold mb-2">Select a Platform</h3>
          <p className="text-muted-foreground">
            Choose your operating system above to see installation instructions.
          </p>
        </CardContent>
      </Card>
    );
  }

  const commands = generatePlatformCommands(platform.id as any);
  const isWindows = platform.os === 'Windows';

  return (
    <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
      <TabsList className="grid w-full grid-cols-2 h-auto">
        <TabsTrigger
          value="oneliner"
          className="flex items-center justify-center space-x-2 text-sm py-3"
        >
          <Zap className="h-4 w-4" />
          <span>Quick Install</span>
        </TabsTrigger>
        <TabsTrigger
          value="manual"
          className="flex items-center justify-center space-x-2 text-sm py-3"
        >
          <Download className="h-4 w-4" />
          <span>Manual Download</span>
        </TabsTrigger>
      </TabsList>

      {/* One-liner Installation */}
      <TabsContent value="oneliner" className="space-y-4">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Zap className="h-5 w-5" />
              <span>Quick Install</span>
              <Badge variant="default">Recommended</Badge>
            </CardTitle>
            <CardDescription>
              The fastest way to install THE0 CLI. Copy and paste this command
              into your terminal.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <QuickCopySection
              command={commands.oneLiner}
              platform={platform.displayName}
            />

            <div className="mt-4 space-y-2">
              <h4 className="text-sm font-medium">What this command does:</h4>
              <ul className="text-sm text-muted-foreground space-y-1 ml-4">
                <li>• Downloads the installation script from our servers</li>
                <li>• Detects your system architecture automatically</li>
                <li>• Downloads and installs the appropriate binary</li>
                <li>• Adds THE0 CLI to your system PATH</li>
                <li>• Verifies the installation works correctly</li>
              </ul>
            </div>

            {isWindows && (
              <Alert className="mt-4">
                <Terminal className="h-4 w-4" />
                <AlertDescription>
                  <strong>PowerShell required:</strong> Make sure you&apos;re
                  running this command in PowerShell, not Command Prompt. You
                  may need to run PowerShell as Administrator.
                </AlertDescription>
              </Alert>
            )}
          </CardContent>
        </Card>
      </TabsContent>

      {/* Manual Download */}
      <TabsContent value="manual" className="space-y-4">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center space-x-2">
              <Download className="h-5 w-5" />
              <span>Manual Installation</span>
            </CardTitle>
            <CardDescription>
              Download the binary directly and install it manually.
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-6">
            <div className="space-y-4">
              <h4 className="font-medium">Step-by-step instructions:</h4>
              <ol className="space-y-3">
                {commands.manualSteps.map((step, index) => (
                  <li key={index} className="flex space-x-3">
                    <span className="flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-primary text-xs text-white">
                      {index + 1}
                    </span>
                    <div className="flex-1">
                      {step.startsWith('curl') ||
                      step.startsWith('Download') ? (
                        <CodeBlock code={step} showCopy={true} />
                      ) : (
                        <p className="text-sm">{step}</p>
                      )}
                    </div>
                  </li>
                ))}
              </ol>
            </div>

            <div className="p-4 bg-muted rounded-md">
              <h4 className="font-medium mb-2">Direct download links:</h4>
              <div className="space-y-2">
                <Button
                  variant="outline"
                  asChild
                  className="w-full justify-start"
                >
                  <a href={`/api/install/${platform.id}`} target="_blank">
                    <Download className="h-4 w-4 mr-2" />
                    Download binary for {platform.displayName}
                  </a>
                </Button>
                <Button
                  variant="outline"
                  asChild
                  className="w-full justify-start"
                >
                  <a href={platform.scriptUrl} target="_blank">
                    <FileText className="h-4 w-4 mr-2" />
                    Download installation script
                  </a>
                </Button>
              </div>
            </div>
          </CardContent>
        </Card>
      </TabsContent>
    </Tabs>
  );
}
