"use client";

import React from "react";
import { useEffect } from "react";
import { DocsLayout } from "@/components/docs/docs-layout";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { Button } from "@/components/ui/button";
import { AlertTriangle, RefreshCw, Home } from "lucide-react";
import Link from "next/link";

interface ErrorProps {
  error: Error & { digest?: string };
  reset: () => void;
}

export default function DocsError({ error, reset }: ErrorProps) {
  useEffect(() => {
    console.error("Documentation page error:", error);
  }, [error]);

  return (
    <DocsLayout>
      <div className="space-y-6">
        <Alert variant="destructive">
          <AlertTriangle className="h-4 w-4" />
          <AlertTitle>Error Loading Documentation</AlertTitle>
          <AlertDescription className="mt-2">
            <p className="mb-4">
              There was an error loading this documentation page. This could be
              due to a missing file, network issue, or server problem.
            </p>
            <div className="flex gap-2 flex-wrap">
              <Button
                variant="outline"
                size="sm"
                onClick={reset}
                className="gap-2"
              >
                <RefreshCw className="h-4 w-4" />
                Try again
              </Button>
              <Button variant="outline" size="sm" asChild>
                <Link href="/docs" className="gap-2">
                  <Home className="h-4 w-4" />
                  Back to Docs
                </Link>
              </Button>
            </div>
          </AlertDescription>
        </Alert>

        <div className="text-center py-8">
          <h2 className="text-xl font-semibold mb-2">Documentation Error</h2>
          <p className="text-muted-foreground mb-6">
            We encountered an issue while trying to load this documentation
            page.
          </p>

          <div className="space-y-2 text-sm text-muted-foreground">
            <p>You can try:</p>
            <ul className="list-disc list-inside space-y-1">
              <li>Refreshing the page</li>
              <li>Checking your internet connection</li>
              <li>Going back to the documentation home</li>
              <li>Searching for the content you need</li>
            </ul>
          </div>
        </div>
      </div>
    </DocsLayout>
  );
}
