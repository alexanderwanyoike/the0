import React from 'react';
import Link from 'next/link';
import { Button } from '@/components/ui/button';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { FileText, ArrowLeft, Home, Search } from 'lucide-react';

export default function DocsNotFound() {
  return (
    <div className="min-h-screen bg-background">
      <div className="container mx-auto px-4 py-8">
        <div className="space-y-6">
          <Alert>
            <FileText className="h-4 w-4" />
            <AlertDescription>
              The documentation page you&apos;re looking for doesn&apos;t exist.
            </AlertDescription>
          </Alert>

          <div className="text-center py-12">
            <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6 mx-auto">
              <FileText className="w-10 h-10 text-muted-foreground" />
            </div>

            <h1 className="text-2xl font-bold mb-2">Documentation Not Found</h1>
            <p className="text-muted-foreground mb-8 max-w-md mx-auto">
              The documentation page you&apos;re looking for doesn&apos;t exist
              or may have been moved.
            </p>

            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Button asChild variant="outline">
                <Link href="/docs" className="gap-2">
                  <ArrowLeft className="w-4 h-4" />
                  Back to Documentation
                </Link>
              </Button>
              <Button asChild>
                <Link href="/dashboard" className="gap-2">
                  <Home className="w-4 h-4" />
                  Go to Dashboard
                </Link>
              </Button>
            </div>

            <div className="mt-8 pt-6 border-t border-border">
              <h3 className="font-medium mb-4">
                Popular Documentation Sections
              </h3>
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-3 max-w-md mx-auto">
                <Button variant="ghost" size="sm" asChild>
                  <Link href="/docs/getting-started">Getting Started</Link>
                </Button>
                <Button variant="ghost" size="sm" asChild>
                  <Link href="/docs/api">API Reference</Link>
                </Button>
                <Button variant="ghost" size="sm" asChild>
                  <Link href="/docs/guides">Guides</Link>
                </Button>
                <Button variant="ghost" size="sm" asChild>
                  <Link href="/docs/components">Components</Link>
                </Button>
              </div>
            </div>

            <div className="mt-6 text-sm text-muted-foreground">
              <p>
                Can&apos;t find what you&apos;re looking for? Try using the
                search feature or{' '}
                <Link href="/support" className="text-primary hover:underline">
                  contact support
                </Link>
                .
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
