'use client';

import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Copy, Check, Download, ExternalLink, Terminal } from 'lucide-react';
import type { QuickCopySectionProps } from '@/types/install';

export function QuickCopySection({ command, platform }: QuickCopySectionProps) {
  const [copied, setCopied] = useState(false);
  const [copying, setCopying] = useState(false);

  const copyToClipboard = async () => {
    setCopying(true);

    try {
      // Try modern Clipboard API first
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(command);
      } else {
        // Fallback for older browsers or non-HTTPS contexts
        await fallbackCopyToClipboard(command);
      }

      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch (error) {
      console.error('Failed to copy to clipboard:', error);
      // Still show success feedback even if copy failed
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } finally {
      setCopying(false);
    }
  };

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-lg">Quick Install</CardTitle>
          {platform && (
            <Badge variant="secondary" className="text-xs">
              {platform}
            </Badge>
          )}
        </div>
        <p className="text-sm text-muted-foreground">
          Copy and paste this command into your terminal to install THE0 CLI.
        </p>
      </CardHeader>

      <CardContent className="space-y-4">
        <div className="relative max-w-full">
          <pre className="bg-muted p-4 rounded-md font-mono text-sm overflow-x-auto break-all">
            <code className="whitespace-pre-wrap break-all">{command}</code>
          </pre>

          <Button
            onClick={copyToClipboard}
            variant="outline"
            size="sm"
            className="absolute top-2 right-2"
            disabled={copying}
          >
            {copying ? (
              <div className="h-4 w-4 animate-spin rounded-full border-2 border-primary border-t-transparent" />
            ) : copied ? (
              <>
                <Check className="h-4 w-4 mr-2" />
                Copied!
              </>
            ) : (
              <>
                <Copy className="h-4 w-4 mr-2" />
                Copy
              </>
            )}
          </Button>
        </div>

        <div className="flex items-center space-x-2 text-xs text-muted-foreground">
          <Terminal className="h-3 w-3" />
          <span>Paste this command in your terminal and press Enter</span>
        </div>
      </CardContent>
    </Card>
  );
}

// Compact version for inline use
export function InlineQuickCopy({ command, platform }: QuickCopySectionProps) {
  const [copied, setCopied] = useState(false);

  const copyToClipboard = async () => {
    try {
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(command);
      } else {
        await fallbackCopyToClipboard(command);
      }

      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    } catch (error) {
      console.error('Copy failed:', error);
    }
  };

  return (
    <div className="flex items-center space-x-2 p-2 bg-muted rounded-md">
      <code className="flex-1 text-sm font-mono truncate">{command}</code>

      <Button
        onClick={copyToClipboard}
        variant="ghost"
        size="sm"
        className="shrink-0 h-8 w-8 p-0"
      >
        {copied ? <Check className="h-3 w-3" /> : <Copy className="h-3 w-3" />}
      </Button>
    </div>
  );
}

// Enhanced version with additional options
export function EnhancedQuickCopySection({
  command,
  platform,
  scriptUrl,
  showAlternatives = true,
}: QuickCopySectionProps & {
  scriptUrl?: string;
  showAlternatives?: boolean;
}) {
  const [copied, setCopied] = useState(false);
  const [copying, setCopying] = useState(false);

  const copyToClipboard = async () => {
    setCopying(true);

    try {
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(command);
      } else {
        await fallbackCopyToClipboard(command);
      }

      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch (error) {
      console.error('Copy failed:', error);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } finally {
      setCopying(false);
    }
  };

  const downloadScript = () => {
    if (scriptUrl) {
      window.open(scriptUrl, '_blank');
    }
  };

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="text-lg">Installation Command</CardTitle>
          {platform && <Badge variant="secondary">{platform}</Badge>}
        </div>
      </CardHeader>

      <CardContent className="space-y-4">
        {/* Main command section */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <h4 className="text-sm font-medium">One-liner install:</h4>
            <div className="flex space-x-2">
              <Button
                onClick={copyToClipboard}
                variant="outline"
                size="sm"
                disabled={copying}
              >
                {copying ? (
                  <div className="h-4 w-4 animate-spin rounded-full border-2 border-primary border-t-transparent" />
                ) : copied ? (
                  <>
                    <Check className="h-4 w-4 mr-2" />
                    Copied!
                  </>
                ) : (
                  <>
                    <Copy className="h-4 w-4 mr-2" />
                    Copy
                  </>
                )}
              </Button>

              {scriptUrl && (
                <Button onClick={downloadScript} variant="outline" size="sm">
                  <Download className="h-4 w-4 mr-2" />
                  Download Script
                </Button>
              )}
            </div>
          </div>

          <pre className="bg-muted p-4 rounded-md font-mono text-sm overflow-x-auto">
            <code>{command}</code>
          </pre>
        </div>

        {/* Instructions */}
        <div className="space-y-2 text-sm text-muted-foreground">
          <div className="flex items-center space-x-2">
            <Terminal className="h-4 w-4" />
            <span>Open your terminal and paste the command above</span>
          </div>
          <div className="flex items-center space-x-2">
            <span className="ml-6">
              • Press Enter to execute the installation
            </span>
          </div>
          <div className="flex items-center space-x-2">
            <span className="ml-6">• Follow any prompts that appear</span>
          </div>
        </div>

        {/* Alternative methods */}
        {showAlternatives && (
          <div className="pt-4 border-t">
            <h4 className="text-sm font-medium mb-2">Alternative methods:</h4>
            <div className="space-y-2 text-sm">
              <button className="flex items-center space-x-2 text-blue-600 hover:text-blue-800">
                <ExternalLink className="h-3 w-3" />
                <span>Manual download</span>
              </button>
              <button className="flex items-center space-x-2 text-blue-600 hover:text-blue-800">
                <ExternalLink className="h-3 w-3" />
                <span>Package managers</span>
              </button>
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}

// Code block component for displaying commands with syntax highlighting
export function CodeBlock({
  code,
  language = 'bash',
  showCopy = true,
}: {
  code: string;
  language?: string;
  showCopy?: boolean;
}) {
  const [copied, setCopied] = useState(false);

  const copyCode = async () => {
    try {
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(code);
      } else {
        await fallbackCopyToClipboard(code);
      }

      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    } catch (error) {
      console.error('Copy failed:', error);
    }
  };

  return (
    <div className="relative max-w-full">
      <pre
        className={`bg-muted p-4 rounded-md font-mono text-sm overflow-x-auto break-all language-${language}`}
      >
        <code className="whitespace-pre-wrap break-all">{code}</code>
      </pre>

      {showCopy && (
        <Button
          onClick={copyCode}
          variant="ghost"
          size="sm"
          className="absolute top-2 right-2 h-8 w-8 p-0"
        >
          {copied ? (
            <Check className="h-3 w-3" />
          ) : (
            <Copy className="h-3 w-3" />
          )}
        </Button>
      )}
    </div>
  );
}

// Fallback copy function for older browsers
async function fallbackCopyToClipboard(text: string): Promise<void> {
  return new Promise((resolve, reject) => {
    // Create a textarea element
    const textArea = document.createElement('textarea');
    textArea.value = text;

    // Make the textarea invisible
    textArea.style.position = 'fixed';
    textArea.style.left = '-999999px';
    textArea.style.top = '-999999px';

    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      // Use the deprecated execCommand as fallback
      const successful = document.execCommand('copy');
      document.body.removeChild(textArea);

      if (successful) {
        resolve();
      } else {
        reject(new Error('execCommand copy failed'));
      }
    } catch (error) {
      document.body.removeChild(textArea);
      reject(error);
    }
  });
}
